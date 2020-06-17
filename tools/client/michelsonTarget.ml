open LiquidClientSigs
open LiquidTypes
open Dune_Network_Lib

module Liquidity = LiquidityLang

module Target = struct

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

  let const = new Lazy_superposed.superposer (object
    method parse = parse_const
    method print = print_const
    method encoding = const_encoding
  end)

  let contract = new Lazy_superposed.superposer (object
    method parse = parse_contract
    method print = print_contract
    method encoding = contract_encoding
  end)

  let datatype = new Lazy_superposed.superposer (object
    method parse = parse_datatype
    method print = print_datatype
    method encoding = datatype_encoding
  end)

end

module Compiler = struct

  let compile_contract syntax_ast =
    let typed_ast = LiquidCheck.typecheck_contract
        ~warnings:true ~decompiling:false syntax_ast in
    Liquidity.global_ty_env := typed_ast.ty_env;
    let contract_sig = LiquidTypes.full_sig_of_contract typed_ast in
    Liquidity.global_contract_sig := contract_sig;
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
      | None -> Liquidity.No_init
      | Some (LiquidInit.Init_constant c, _) ->
        Liquidity.Init_constant (LiquidUntype.untype_const c)
      | Some (LiquidInit.Init_code c, args) ->
        Liquidity.Init_code (LiquidUntype.untype_contract c,
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
    Liquidity.global_types := types;
    Liquidity.global_type_annots := type_annots;
    let typed_ast = LiquidCheck.typecheck_contract
        ~keep_tvars:true ~warnings:false ~decompiling:true c in
    Liquidity.global_ty_env := typed_ast.ty_env;
    Liquidity.global_contract_sig := LiquidTypes.full_sig_of_contract typed_ast;
    let encode_ast, to_inline =
      LiquidEncode.encode_contract ~decompiling:true typed_ast in
    let live_ast = LiquidSimplify.simplify_contract
        ~decompile_annoted:annoted_tz encode_ast to_inline in
    let multi_ast = LiquidDecode.decode_contract live_ast in
    let untyped_ast = LiquidUntype.untype_contract multi_ast in
    untyped_ast

  let compile_const ?ty const =
    let env = !Liquidity.global_ty_env in
    let tenv = empty_typecheck_env ~warnings:true
        LiquidTypes.dummy_contract_sig env in
    (* LiquidData.translate_const_exp const *)
    const
    |> LiquidCheck.typecheck_const tenv ?expected_ty:ty
    |> LiquidEncode.encode_const !Liquidity.global_ty_env !Liquidity.global_contract_sig
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

end

include Compiler

module Client = LiquidClient.Make(struct
    module Target = Target
    include Compiler
  end
  )
