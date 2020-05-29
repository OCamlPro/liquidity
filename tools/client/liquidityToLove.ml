open LiquidTypes
open Dune_Network_Lib
open Tezos_protocol.Protocol
open LiquidClientSigs
open Client

(* Liquidity *)
module Liquidity = LiquidityLang

(* Love *)
module Love = struct

  open Dune_Network_Lib

  type const = Love_value.Value.t
  type contract = Love_ast.top_contract
  type location = Love_ast.location
  type datatype = Love_type.t

  let unit : const = VUnit

  let compare_loc : location -> location -> int = Pervasives.compare

  let next_loc (loc : location) : location =
    Love_ast.{
      pos_lnum = loc.pos_lnum + 1;
      pos_bol = 0;
      pos_cnum = 0
    }

  let loc_encoding : location Json_encoding.encoding =
    Json_encoding.(
      conv
        (fun Love_ast.{pos_lnum; pos_bol; pos_cnum} -> (pos_lnum, pos_bol, pos_cnum))
        (fun (pos_lnum, pos_bol, pos_cnum) -> Love_ast.{pos_lnum; pos_bol; pos_cnum})
        (obj3
           (req "lnum" int)
           (req "bol" int)
           (req "cnum" int)
        )
    )

  let love_enc_to_json_enc
      (encoding : 'a Environment.Data_encoding.encoding)
    : 'a Json_encoding.encoding =
    Json_encoding.conv
      (fun (v : 'a) ->
         let json =
           Environment.Data_encoding.Json.construct encoding v in
         Environment.Data_encoding.Json.to_string json
      )
      (fun str : 'a ->
         match Environment.Data_encoding.Json.from_string str
         with
         | Ok json ->
           Environment.Data_encoding.Json.destruct encoding json
         | Error s ->
           failwith @@
           "Error while destruct of love element: " ^ s )
      Json_encoding.string

  let const_encoding : Love_value.Value.t Json_encoding.encoding =
    love_enc_to_json_enc Love_json_encoding.Value.encoding

  let contract_encoding =
    love_enc_to_json_enc Love_json_encoding.Ast.top_contract_encoding

  let datatype_encoding : Love_type.t Json_encoding.encoding =
    love_enc_to_json_enc Love_json_encoding.Type.encoding

end

module Source = Liquidity
module Target = Love

let global_ty_env = ref (LiquidFromParsetree.initial_env "")
let global_contract_sig = ref LiquidTypes.dummy_contract_sig
let global_type_annots = ref (Hashtbl.create 0)
let global_types = ref []

type compiled_init =
  | No_init
  | Init_constant of Source.const
  | Init_code of Source.contract * (string * Source.datatype) list

let global_ty_env = ref (LiquidFromParsetree.initial_env "")

let compile_contract syntax_ast =
  let typed_ast = LiquidCheck.typecheck_contract
      ~warnings:true ~decompiling:false ~monomorphise:true ~keep_tvars:true syntax_ast in
  global_ty_env := typed_ast.ty_env;
  let typed_ast_no_tfail = Preprocess.contract_ttfail_to_tvar typed_ast in
  let love_ast, _ = Liq2love.liqcontract_to_lovecontract ~ctr_name:"main" typed_ast_no_tfail in
  Love_ast.{version = (1, 0); code = love_ast}, No_init, []

let decompile_contract _code = failwith "Todo: decompile contract"

let compile_const ?ty const =
    let env = !global_ty_env in
    let tenv = empty_typecheck_env ~warnings:true LiquidTypes.dummy_contract_sig env in
    (* LiquidData.translate_const_exp const *)
    const
    |> LiquidCheck.typecheck_const tenv ?expected_ty:ty
    |> Liq2love.liqconst_to_lovevalue

let decompile_const ?ty _ = failwith "Todo: Love decompile const"

let compile_datatype ty =
  Liq2love.liqtype_to_lovetype (Love_tenv.empty (Contract []) ()) ty

module NoConverter :
  (CONVERTER with type source_from_datatype := Liquidity.datatype
              and type source_from_const := Liquidity.const
              and type source_from_contract := Liquidity.contract
              and type source_dest_datatype := Liquidity.datatype
              and type source_dest_const := Liquidity.const
              and type source_dest_contract := Liquidity.contract
              and type target_from_datatype := Love.datatype
              and type target_from_const := Love.const
              and type target_from_contract := Love.contract
              and type target_dest_datatype := Love.datatype
              and type target_dest_const := Love.const
              and type target_dest_contract := Love.contract)
= struct
  type source_from_datatype = Liquidity.datatype
  type source_from_const = Liquidity.const
  type source_from_contract = Liquidity.contract
  type source_dest_datatype = Liquidity.datatype
  type source_dest_const = Liquidity.const
  type source_dest_contract = Liquidity.contract
  type target_from_datatype = Love.datatype
  type target_from_const = Love.const
  type target_from_contract = Love.contract
  type target_dest_datatype = Love.datatype
  type target_dest_const = Love.const
  type target_dest_contract = Love.contract
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

module StringLove = struct
  type datatype = string
  type const = string
  type contract = string
end

module JsonLove = struct
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

module StringLoveConv :
  (CONV
   with type dest_const := Love.const
    and type dest_contract := Love.contract
    and type dest_datatype := Love.datatype
    and type from_const := string
    and type from_contract := string
    and type from_datatype := string) = struct

  let parse_const s =
    let lb = Lexing.from_string s in
    match Love_parser.top_value Love_lexer.token lb with
      Value v -> v
    | Type t -> failwith @@ Format.asprintf "Type %a is not a value" Love_type.pretty t

  let parse_datatype s =
    let lb = Lexing.from_string s in
    match Love_parser.top_value Love_lexer.token lb with
      Value v -> failwith @@ Format.asprintf "Value %a is not a type" Love_printer.Value.print v
    | Type t -> t

  let parse_contract s =
    let lb = Lexing.from_string s in
    Love_parser.top_contract Love_lexer.token lb

  let print_const = Format.asprintf "%a" Love_printer.Value.print

  let print_datatype = Format.asprintf "%a" Love_type.pretty

  let print_contract (c : Love.contract) =
    Format.asprintf "%a" Love_printer.Ast.print_structure c.code
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
              and type target_dest_datatype := Love.datatype
              and type target_dest_const := Love.const
              and type target_dest_contract := Love.contract) = struct
  module SourceConv = StringLiquidityConv
  module TargetConv = StringLoveConv
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
              and type target_dest_datatype := Love.datatype
              and type target_dest_const := Love.const
              and type target_dest_contract := Love.contract) = struct

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

module MultiLove = struct
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

module LoveConstMulti = Lazy_superposed.Make(struct
    type t = Love.const
    let parse = StringLoveConv.parse_const
    let print = StringLoveConv.print_const
    let encoding = Love.const_encoding
  end)

module LoveContractMulti = Lazy_superposed.Make(struct
    type t = Love.contract
    let parse = StringLoveConv.parse_contract
    let print = StringLoveConv.print_contract
    let encoding = Love.contract_encoding
  end)

module LoveDatatypeMulti = Lazy_superposed.Make(struct
    type t = Love.datatype
    let parse = StringLoveConv.parse_datatype
    let print = StringLoveConv.print_datatype
    let encoding = Love.datatype_encoding
  end)

module MultiConverter :
  (CONVERTER with type source_from_datatype := Liquidity.datatype Lazy_superposed.t
              and type source_from_const := Liquidity.const Lazy_superposed.t
              and type source_from_contract := Liquidity.contract Lazy_superposed.t
              and type source_dest_datatype := Liquidity.datatype
              and type source_dest_const := Liquidity.const
              and type source_dest_contract := Liquidity.contract
              and type target_from_datatype := Love.datatype Lazy_superposed.t
              and type target_from_const := Love.const Lazy_superposed.t
              and type target_from_contract := Love.contract Lazy_superposed.t
              and type target_dest_datatype := Love.datatype
              and type target_dest_const := Love.const
              and type target_dest_contract := Love.contract) = struct

  module SourceConv = struct
    let parse_const = LiquidityConstMulti.force_ast
    let parse_datatype = LiquidityDatatypeMulti.force_ast
    let parse_contract = LiquidityContractMulti.force_ast
    let print_const x = Lazy_superposed.ast x
    let print_datatype x = Lazy_superposed.ast x
    let print_contract x = Lazy_superposed.ast x
  end

  module TargetConv = struct
    let parse_const = LoveConstMulti.force_ast
    let parse_datatype = LoveDatatypeMulti.force_ast
    let parse_contract = LoveContractMulti.force_ast
    let print_const x = Lazy_superposed.ast x
    let print_datatype x = Lazy_superposed.ast x
    let print_contract x = Lazy_superposed.ast x
  end
end
