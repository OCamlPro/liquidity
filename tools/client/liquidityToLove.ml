open LiquidTypes
open Dune_Network_Lib
open Protocol
open LiquidClientSigs
open Love_client

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

  let parse_const s =
    let lb = Lexing.from_string s in
    match Love_parser.top_value Love_lexer.token lb with
      Value v -> v
    | Type t -> failwith @@ Format.asprintf "Type %a is not a value" Love_type.pretty t

  let parse_datatype s =
    let lb = Lexing.from_string s in
    match Love_parser.top_value Love_lexer.token lb with
    | Value v -> failwith @@ Format.asprintf "Value %a is not a type"
        Love_printer.Value.print v
    | Type t -> t

  let parse_contract s =
    let lb = Lexing.from_string s in
    Love_parser.top_contract Love_lexer.token lb

  let print_const = Format.asprintf "%a" Love_printer.Value.print

  let print_datatype = Format.asprintf "%a" Love_type.pretty

  let print_contract (c : contract) =
    Format.asprintf "%a" Love_printer.Ast.print_structure c.code

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

  type compiled_init =
    | No_init
    | Init_constant of Liquidity.const
    | Init_code of Liquidity.contract * (string * Liquidity.datatype) list

  let compile_contract syntax_ast =
    let typed_ast = LiquidCheck.typecheck_contract
        ~warnings:true ~decompiling:false ~monomorphise:true ~keep_tvars:true syntax_ast in
    Liquidity.global_ty_env := typed_ast.ty_env;
    let typed_ast_no_tfail = Preprocess.contract_ttfail_to_tvar typed_ast in
    let love_ast, _ = Liq2love.liqcontract_to_lovecontract ~ctr_name:"main" typed_ast_no_tfail in
    Love_ast.{version = (1, 0); code = love_ast}, No_init, []

  let decompile_contract _code = failwith "Todo: decompile contract"

  let compile_const ?ty const =
    let env = !Liquidity.global_ty_env in
    let tenv = empty_typecheck_env ~warnings:true LiquidTypes.dummy_contract_sig env in
    (* LiquidData.translate_const_exp const *)
    const
    |> LiquidCheck.typecheck_const tenv ?expected_ty:ty
    |> Liq2love.liqconst_to_lovevalue

  let decompile_const ?ty _ = failwith "Todo: Love decompile const"

  let compile_datatype ty =
    Liq2love.liqtype_to_lovetype (Love_tenv.empty (Contract []) ()) ty
end

include Compiler

module Lang = struct
  module Source = Liquidity
  module Target = Love
  include Compiler
end

module Client = LiquidClient.Make(Lang)
