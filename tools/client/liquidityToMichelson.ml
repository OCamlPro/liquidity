open LiquidClientSigs
open LiquidTypes
open Dune_Network_Lib

(* Liquidity *)
module Liquidity = LiquidityLang

(* Michelson *)
module Michelson = struct

  open Dune_Network_Lib.Micheline

  type const = LiquidMichelineTypes.expr
  type contract = LiquidMichelineTypes.contract
  type location = Micheline.canonical_location
  type datatype = LiquidMichelineTypes.expr

  let unit =
    let open Micheline in
    strip_locations (Prim ((), "Unit", [], []))


  let compare_loc = Pervasives.compare

  let next_loc l = l + 1

  let loc_encoding = Json_encoding.int

  let const_encoding = LiquidToMicheline.const_encoding
  let contract_encoding = LiquidToMicheline.contract_encoding
  let datatype_encoding = LiquidToMicheline.const_encoding

end

module Source = Liquidity
module Target = Michelson

let global_ty_env = ref (LiquidFromParsetree.initial_env "")
let global_contract_sig = ref LiquidTypes.dummy_contract_sig
let global_type_annots = ref (Hashtbl.create 0)
let global_types = ref []

type compiled_init =
  | No_init
  | Init_constant of Source.const
  | Init_code of Source.contract * (string * Source.datatype) list

let compile_contract syntax_ast =
  let typed_ast = LiquidCheck.typecheck_contract
      ~warnings:true ~decompiling:false syntax_ast in
  global_ty_env := typed_ast.ty_env;
  let contract_sig = LiquidTypes.full_sig_of_contract typed_ast in
  global_contract_sig := contract_sig;
  let contract_sig = LiquidTypes.full_sig_of_contract typed_ast in
  let encoded_ast, to_inline =
    LiquidEncode.encode_contract ~annot:true typed_ast in
  let live_ast =
    if !LiquidOptions.simplify then
      let to_inline =
        if !LiquidOptions.inline then to_inline
        else StringMap.empty, StringMap.empty in
      LiquidSimplify.simplify_contract encoded_ast to_inline
    else encoded_ast in
  let pre_michelson = LiquidMichelson.translate live_ast in
  let pre_michelson =
    if !LiquidOptions.peephole then
      LiquidPeephole.simplify pre_michelson
    else
      pre_michelson
  in
  let c, loc_table =
    LiquidToMicheline.convert_contract ~expand:true pre_michelson in
  let pre_init = match typed_ast.c_init with
    | None -> None
    | Some init ->
      Some (
        LiquidInit.compile_liquid_init live_ast.ty_env contract_sig init,
        init.init_args) in
  let comp_init = match pre_init with
    | None -> No_init
    | Some (LiquidInit.Init_constant c, _) ->
      Init_constant (LiquidUntype.untype_const c)
    | Some (LiquidInit.Init_code c, args) ->
      Init_code (LiquidUntype.untype_contract c,
                 List.map (fun (x,_,ty) -> x, ty) args)
  in
  (c, comp_init, loc_table)

let decompile_contract code =
  let env = LiquidMichelineTypes.empty_env "mic_code" in
  let c = LiquidFromMicheline.convert_contract env code in
  let c = LiquidClean.clean_contract c in
  let c = LiquidInterp.interp c in
  if !LiquidOptions.parseonly then exit 0;
  let c = LiquidDecomp.decompile env c in
  let annoted_tz, type_annots, types = LiquidFromMicheline.infos_env env in
  global_types := types;
  global_type_annots := type_annots;
  let typed_ast = LiquidCheck.typecheck_contract
      ~keep_tvars:true ~warnings:false ~decompiling:true c in
  global_ty_env := typed_ast.ty_env;
  global_contract_sig := LiquidTypes.full_sig_of_contract typed_ast;
  let encode_ast, to_inline =
    LiquidEncode.encode_contract ~decompiling:true typed_ast in
  let live_ast = LiquidSimplify.simplify_contract
      ~decompile_annoted:annoted_tz encode_ast to_inline in
  let multi_ast = LiquidDecode.decode_contract live_ast in
  let untyped_ast = LiquidUntype.untype_contract multi_ast in
  untyped_ast

let compile_const ?ty const =
    let env = !global_ty_env in
    let tenv = empty_typecheck_env ~warnings:true
        LiquidTypes.dummy_contract_sig env in
    (* LiquidData.translate_const_exp const *)
    const
    |> LiquidCheck.typecheck_const tenv ?expected_ty:ty
    |> LiquidEncode.encode_const !global_ty_env !global_contract_sig
    |> LiquidMichelson.compile_const
    |> LiquidToMicheline.convert_const ~expand:true

let decompile_const ?ty e =
  let env = LiquidMichelineTypes.empty_env "decompile_const" in
  let mic_e, loc = match ty with
    | Some ty -> LiquidFromMicheline.convert_const_type env e ty
    | None -> LiquidFromMicheline.convert_const_notype env e in
  let nod_e = LiquidInterp.decompile_const loc mic_e in
  let syn_e = LiquidDecomp.decompile_const nod_e in
  let tenv =
    LiquidTypes.empty_typecheck_env ~warnings:false
      LiquidTypes.dummy_contract_sig
      (LiquidFromParsetree.initial_env "") in
  LiquidCheck.typecheck_const tenv ?expected_ty:ty ~loc syn_e
  |> LiquidSimplify.simplify_const
  |> LiquidUntype.untype_const

let compile_datatype ty =
  LiquidToMicheline.convert_type (LiquidEncode.encode_type ty)


module NoConverter :
  (CONVERTER with type source_from_datatype := Liquidity.datatype
              and type source_from_const := Liquidity.const
              and type source_from_contract := Liquidity.contract
              and type source_dest_datatype := Liquidity.datatype
              and type source_dest_const := Liquidity.const
              and type source_dest_contract := Liquidity.contract
              and type target_from_datatype := Michelson.datatype
              and type target_from_const := Michelson.const
              and type target_from_contract := Michelson.contract
              and type target_dest_datatype := Michelson.datatype
              and type target_dest_const := Michelson.const
              and type target_dest_contract := Michelson.contract)
= struct
  type source_from_datatype = Liquidity.datatype
  type source_from_const = Liquidity.const
  type source_from_contract = Liquidity.contract
  type source_dest_datatype = Liquidity.datatype
  type source_dest_const = Liquidity.const
  type source_dest_contract = Liquidity.contract
  type target_from_datatype = Michelson.datatype
  type target_from_const = Michelson.const
  type target_from_contract = Michelson.contract
  type target_dest_datatype = Michelson.datatype
  type target_dest_const = Michelson.const
  type target_dest_contract = Michelson.contract
  module SourceConv = struct
    let parse_const x = x
    let parse_contract x = x
    let parse_datatype x = x
    let print_const x = x
    let print_contract x = x
    let print_datatype x = x
  end
  module TargetConv = struct
    let parse_const x = x
    let parse_contract x = x
    let parse_datatype x = x
    let print_const x = x
    let print_contract x = x
    let print_datatype x = x
  end
end

type from =
  | From_strings of string list
  | From_files of string list

module StringLiquidity = struct
  type datatype = string
  type const = string
  type contract = from
end

module StringMichelson = struct
  type datatype = string
  type const = string
  type contract = string
end

module JsonMichelson = struct
  type datatype = Ezjsonm.value
  type const = Ezjsonm.value
  type contract = Ezjsonm.value
end

module StringLiquidityConv :
  (CONV
   with type dest_const := Liquidity.const
    and type dest_contract := Liquidity.contract
    and type dest_datatype := Liquidity.datatype
    and type from_const := string
    and type from_contract := from
    and type from_datatype := string) = struct

  let parse_const s =
    let env = !global_ty_env in
    s
    |> LiquidFromParsetree.expression_of_string
    |> LiquidFromParsetree.translate_expression env
    (* |> LiquidCheck.typecheck_code tenv *)
    |> LiquidData.translate_const_exp
    (* |> LiquidCheck.typecheck_const tenv *)

  let parse_contract s =
    let ocaml_asts = match s with
      | From_strings ss ->
        List.map (fun s ->
            "liquidity_buffer",
            LiquidFromParsetree.structure_of_string ~filename:"liquidity_buffer"
              s) ss
      | From_files files ->
        List.map (fun f -> f, LiquidFromParsetree.read_file f) files
    in
    let syntax_ast = LiquidFromParsetree.translate_multi ocaml_asts in
    syntax_ast
    (* let typed_ast = LiquidCheck.typecheck_contract
     *     ~warnings:true ~decompiling:false syntax_ast in
     * global_ty_env := typed_ast.ty_env;
     * let contract_sig = LiquidTypes.full_sig_of_contract typed_ast in
     * global_contract_sig := contract_sig;
     * typed_ast *)

  let parse_datatype s =
    LiquidFromParsetree.type_of_string s
    |> LiquidFromParsetree.translate_type !global_ty_env

  let print_const c =
    let env = !global_ty_env in
    let tenv = empty_typecheck_env ~warnings:true
        LiquidTypes.dummy_contract_sig env in
    (* LiquidData.translate_const_exp const *)
    c
    |> LiquidCheck.typecheck_const tenv
    (* |> LiquidSimplify.simplify_const *)
    (* |> LiquidDecode.decode_const *)
    (* |> LiquidUntype.untype_const *)
    |> LiquidPrinter.Liquid.string_of_const

  let print_contract c =
    let untyped_ast =
      LiquidCheck.typecheck_contract
        ~keep_tvars:true ~warnings:false ~decompiling:true c
    in
    From_strings [LiquidPrinter.Syntax.string_of_structure
                    (LiquidToParsetree.structure_of_contract
                       ~type_annots:!global_type_annots ~types:!global_types untyped_ast) []
                 ]


  let print_datatype ty =
    LiquidPrinter.Liquid.string_of_type ty
end

module StringMichelsonConv :
  (CONV
   with type dest_const := Michelson.const
    and type dest_contract := Michelson.contract
    and type dest_datatype := Michelson.datatype
    and type from_const := string
    and type from_contract := string
    and type from_datatype := string) = struct

  let parse_const s =
    match LiquidFromMicheline.const_of_string "michelson" s with
    | None -> assert false
    | Some (c, _) -> c

  let parse_datatype s = parse_const s

  let parse_contract s =
    match LiquidFromMicheline.contract_of_string "michelson" s with
    | None -> assert false
    | Some (c, _) -> c

  let print_const c =
    if !LiquidOptions.singleline
    then LiquidToMicheline.line_of_const c
    else LiquidToMicheline.string_of_const c

  let print_datatype c =
    if !LiquidOptions.singleline
    then LiquidToMicheline.line_of_const c
    else LiquidToMicheline.string_of_const c

  let print_contract c =
    if !LiquidOptions.singleline
    then LiquidToMicheline.line_of_contract c
    else LiquidToMicheline.string_of_contract c
end

module StringStringConverter :
  (CONVERTER with type source_from_datatype := string
              and type source_from_const := string
              and type source_from_contract := from
              and type source_dest_datatype := Liquidity.datatype
              and type source_dest_const := Liquidity.const
              and type source_dest_contract := Liquidity.contract
              and type target_from_datatype := string
              and type target_from_const := string
              and type target_from_contract := string
              and type target_dest_datatype := Michelson.datatype
              and type target_dest_const := Michelson.const
              and type target_dest_contract := Michelson.contract) = struct
  module SourceConv = StringLiquidityConv
  module TargetConv = StringMichelsonConv
end

module StringJsonConverter :
  (CONVERTER with type source_from_datatype := string
              and type source_from_const := string
              and type source_from_contract := from
              and type source_dest_datatype := Liquidity.datatype
              and type source_dest_const := Liquidity.const
              and type source_dest_contract := Liquidity.contract
              and type target_from_datatype := Ezjsonm.value
              and type target_from_const := Ezjsonm.value
              and type target_from_contract := Ezjsonm.value
              and type target_dest_datatype := Michelson.datatype
              and type target_dest_const := Michelson.const
              and type target_dest_contract := Michelson.contract) = struct

  module SourceConv = StringLiquidityConv

  module TargetConv = struct
    let parse_const = Json_encoding.destruct Target.const_encoding
    let parse_datatype = Json_encoding.destruct Target.datatype_encoding
    let parse_contract = Json_encoding.destruct Target.contract_encoding
    let print_const = Json_encoding.construct Target.const_encoding
    let print_datatype = Json_encoding.construct Target.datatype_encoding
    let print_contract = Json_encoding.construct Target.contract_encoding
  end
end

module MultiLiquidity = struct
  type const = Source.const Lazy_superposed.t
  type contract = Source.contract Lazy_superposed.t
  type datatype = Source.datatype Lazy_superposed.t
end

module MultiMichelson = struct
  type const = Target.const Lazy_superposed.t
  type contract = Target.contract Lazy_superposed.t
  type datatype = Target.datatype Lazy_superposed.t
end

module LiquidityConstMulti = Lazy_superposed.Make(struct
    type t = Liquidity.const
    let parse = StringLiquidityConv.parse_const
    let print = StringLiquidityConv.print_const
    let encoding = Liquidity.const_encoding
  end)

module LiquidityContractMulti = Lazy_superposed.Make(struct
    type t = Liquidity.contract
    let parse s = StringLiquidityConv.parse_contract (From_strings [s])
    let print c = match StringLiquidityConv.print_contract c with
      | From_strings [s] -> s
      | _ -> assert false
    let encoding = Liquidity.contract_encoding
  end)

module LiquidityDatatypeMulti = Lazy_superposed.Make(struct
    type t = Liquidity.datatype
    let parse = StringLiquidityConv.parse_datatype
    let print = StringLiquidityConv.print_datatype
    let encoding = Liquidity.datatype_encoding
  end)

module MichelsonConstMulti = Lazy_superposed.Make(struct
    type t = Michelson.const
    let parse = StringMichelsonConv.parse_const
    let print = StringMichelsonConv.print_const
    let encoding = Michelson.const_encoding
  end)

module MichelsonContractMulti = Lazy_superposed.Make(struct
    type t = Michelson.contract
    let parse = StringMichelsonConv.parse_contract
    let print = StringMichelsonConv.print_contract
    let encoding = Michelson.contract_encoding
  end)

module MichelsonDatatypeMulti = Lazy_superposed.Make(struct
    type t = Michelson.datatype
    let parse = StringMichelsonConv.parse_datatype
    let print = StringMichelsonConv.print_datatype
    let encoding = Michelson.datatype_encoding
  end)

module MultiConverter :
  (CONVERTER with type source_from_datatype := Liquidity.datatype Lazy_superposed.t
              and type source_from_const := Liquidity.const Lazy_superposed.t
              and type source_from_contract := Liquidity.contract Lazy_superposed.t
              and type source_dest_datatype := Liquidity.datatype
              and type source_dest_const := Liquidity.const
              and type source_dest_contract := Liquidity.contract
              and type target_from_datatype := Michelson.datatype Lazy_superposed.t
              and type target_from_const := Michelson.const Lazy_superposed.t
              and type target_from_contract := Michelson.contract Lazy_superposed.t
              and type target_dest_datatype := Michelson.datatype
              and type target_dest_const := Michelson.const
              and type target_dest_contract := Michelson.contract) = struct

  module SourceConv = struct
    let parse_const = LiquidityConstMulti.force_ast
    let parse_datatype = LiquidityDatatypeMulti.force_ast
    let parse_contract = LiquidityContractMulti.force_ast
    let print_const x = Lazy_superposed.ast x
    let print_datatype x = Lazy_superposed.ast x
    let print_contract x = Lazy_superposed.ast x
  end

  module TargetConv = struct
    let parse_const = MichelsonConstMulti.force_ast
    let parse_datatype = MichelsonDatatypeMulti.force_ast
    let parse_contract = MichelsonContractMulti.force_ast
    let print_const x = Lazy_superposed.ast x
    let print_datatype x = Lazy_superposed.ast x
    let print_contract x = Lazy_superposed.ast x
  end
end
