open LiquidTypes
open Dune_Network_Lib
open Protocol
open LiquidClientSigs
open Love_parsing
open Love_type
open Love_ast_types

module Liquidity = LiquidityLang

module Target = struct

  open Dune_Network_Lib

  type const = Love_value.Value.t
  type contract = AST.top_contract
  type location = AST.location
  type datatype = TYPE.t

  let unit : const = VUnit

  let compare_loc : location -> location -> int = Pervasives.compare

  let next_loc (loc : location) : location =
    AST.{
      pos_lnum = loc.pos_lnum + 1;
      pos_bol = 0;
      pos_cnum = 0
    }

  let loc_encoding : location Json_encoding.encoding =
    Json_encoding.(
      conv
        (fun AST.{pos_lnum; pos_bol; pos_cnum} -> (pos_lnum, pos_bol, pos_cnum))
        (fun (pos_lnum, pos_bol, pos_cnum) -> AST.{pos_lnum; pos_bol; pos_cnum})
        (obj3
           (req "lnum" int)
           (req "bol" int)
           (req "cnum" int)
        )
    )

  let const_encoding = Liq2love.const_encoding

  let contract_encoding = Liq2love.contract_encoding

  let datatype_encoding = Liq2love.datatype_encoding

  let parse_const s =
    match Love_lexer.parse_top_value s with
    | AST.VALUE v -> Love_value.value_of_value v
    | AST.TYPE t ->
      Format.kasprintf failwith "Type %a is not a value" Love_type.pretty t

  let parse_datatype s =
    match Love_lexer.parse_top_value s with
    | AST.VALUE v ->
      Format.kasprintf failwith "Value %a is not a type"
        Love_printer.Value.print (Love_value.value_of_value v)
    | AST.TYPE t -> t

  let parse_contract s =
    Love_lexer.parse_top_contract s

  let print_const = Format.asprintf "%a" Love_printer.Value.print

  let print_datatype = Format.asprintf "%a" Love_type.pretty

  let print_contract (c : contract) =
     Format.asprintf "#love\n%a" Love_printer.Ast.print_structure c.code

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
        ~warnings:true ~decompiling:false ~monomorphise:true ~keep_tvars:true syntax_ast in
    Liquidity.global_ty_env := typed_ast.ty_env;
    let typed_ast_no_tfail = Preprocess.contract_ttfail_to_tvar typed_ast in
    let love_ast, _ = Liq2love.liqcontract_to_lovecontract ~ctr_name:"main" typed_ast_no_tfail in
    let init = match typed_ast.c_init with
      | None -> Liquidity.No_init
      | Some init ->
        Liquidity.Init_components
          (List.map (fun (n, _, ty) -> n, ty) init.init_args) in
    AST.{version = (1, 0); code = love_ast}, init, []

  let decompile_contract _code = failwith "Cannot decompile Love contract yet"

  let compile_const ?ty const =
    let env = !Liquidity.global_ty_env in
    let tenv = empty_typecheck_env ~warnings:true LiquidTypes.dummy_contract_sig env in
    (* LiquidData.translate_const_exp const *)
    const
    |> LiquidCheck.typecheck_const tenv ?expected_ty:ty
    |> Liq2love.liqconst_to_lovevalue

  let decompile_const ?ty _ = failwith "Cannot decompile Love constant yet"

  let compile_datatype ty =
    Liq2love.liqtype_to_lovetype (Love_tenv.empty (Contract []) ()) ty
end

include Compiler

module Client = LiquidClient.Make(struct
  module Target = Target
  include Compiler
end)
