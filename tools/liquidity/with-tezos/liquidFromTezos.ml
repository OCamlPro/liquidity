(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes
open LiquidTezosTypes

open Micheline;;

exception Missing_program_field of string

let liquid_loc_of_script_loc f { Micheline_parser.start; stop } =
  { loc_file = f;
    loc_pos = Some (
        (start.Micheline_parser.line, start.Micheline_parser.column),
        (stop.Micheline_parser.line, stop.Micheline_parser.column))
  }

let loc_error filename =
  let open Micheline_parser in
  function
  | Invalid_utf8_sequence (pt, _)
  | Unexpected_character (pt, _)
  | Undefined_escape_sequence (pt, _)
  | Missing_break_after_number pt ->
    liquid_loc_of_script_loc filename { start = pt; stop = pt }
  | Unterminated_string loc
  | Unterminated_integer loc
  | Unterminated_comment loc
  | Unclosed { loc }
  | Unexpected { loc }
  | Extra { loc } ->
    liquid_loc_of_script_loc filename loc
  | Misaligned n ->
    let loc = Micheline.location n in
    liquid_loc_of_script_loc filename loc
  | _ -> LiquidLoc.loc_in_file filename

let string_of_token = function
  | Micheline_parser.Open_paren
  | Micheline_parser.Close_paren -> "parenthesis"
  | Micheline_parser.Open_brace
  | Micheline_parser.Close_brace -> "curly brace"
  | Micheline_parser.String _ -> "string constant"
  | Micheline_parser.Int _ -> "integer constant"
  | Micheline_parser.Ident _ -> "identifier"
  | Micheline_parser.Annot _ -> "annotation"
  | Micheline_parser.Comment _
  | Micheline_parser.Eol_comment _ -> "comment"
  | Micheline_parser.Semi -> "semi colon"

let msg_error =
  let open Micheline_parser in
  function
  | Invalid_utf8_sequence (point, msg) ->
    Printf.sprintf "Invalid UTF-8 sequence %S" msg
  | Unexpected_character (point, msg) ->
    Printf.sprintf "Unexpected character %s" msg
  | Undefined_escape_sequence (point, msg) ->
    Printf.sprintf "Undefined escape character %S" msg
  | Missing_break_after_number point -> "Missing break"
  | Unterminated_string loc -> "Unterminated string"
  | Unterminated_integer loc -> "Unterminated integer"
  | Unterminated_comment loc -> "Unterminated comment"
  | Unclosed { loc ; token } ->
    Printf.sprintf "Unclosed %s" (string_of_token token)
  | Unexpected { loc ; token } ->
    Printf.sprintf "Unexpected %s" (string_of_token token)
  | Extra { loc ; token } ->
    Printf.sprintf "Extra %s" (string_of_token token)
  | Misaligned node -> "Misaligned expression"
  | Empty -> "Empty expression"
  | _ -> ""

let error_to_liqerror filename e = {
  err_loc = loc_error filename e;
  err_msg = msg_error e
}

let loc_of_int env index =
  try IntMap.find index env.loc_table
  with Not_found -> LiquidLoc.noloc

let unknown_expr env msg expr =
  let loc = loc_of_int env (Micheline.location expr) in
  LiquidLoc.raise_error ~loc "in %s, %s" msg
    (match expr with
     | Seq (_loc, _exprs, _debug) -> "unknwon sequence"
     | String (_loc, s) ->
       Printf.sprintf "unknwon string %S" s
     | Int (_loc, s) ->
       Printf.sprintf "unknown integer %s" s
     | Prim(i, s, args, _debug) ->
       Printf.sprintf "unknown primitive %S with %d arguments"
         s (List.length args)
    )

let rec convert_const env expr =
  match expr with
  | Int (_loc, n) -> CInt (LiquidPrinter.integer_of_mic n)
  | String (_loc, s) -> CString s
  | Prim(_, "Unit", [], _debug) -> CUnit
  | Prim(_, "True", [], _debug) -> CBool true
  | Prim(_, "False", [], _debug) -> CBool false
  | Prim(_, "None", [], _debug) -> CNone

  | Prim(_, "Some", [x], _debug) -> CSome (convert_const env x)
  | Prim(_, "Left", [x], _debug) -> CLeft (convert_const env x)
  | Prim(_, "Right", [x], _debug) -> CRight (convert_const env x)
  | Prim(_, "Pair", [x;y], _debug) -> CTuple [convert_const env x;
                                              convert_const env y]
  | Prim(_, "List", args, _debug) -> CList (List.map (convert_const env) args)
  | Prim(_, "Map", args, _debug) ->
     CMap (List.map (function
                     | Prim(_, "Item", [x;y], _debug) ->
                        convert_const env x, convert_const env y
                     | expr ->
                        unknown_expr env "convert_const_item" expr) args)
  | Prim(_, "Set", args, _debug) -> CSet (List.map (convert_const env) args)
  | _ -> unknown_expr env "convert_const" expr

let rec convert_type env expr =
  match expr with
  | Prim(_, "unit", [], _debug) -> Tunit
  | Prim(_, "timestamp", [], _debug) -> Ttimestamp
  | Prim(_, "tez", [], _debug) -> Ttez
  | Prim(_, "int", [], _debug) -> Tint
  | Prim(_, "nat", [], _debug) -> Tnat
  | Prim(_, "bool", [], _debug) -> Tbool
  | Prim(_, "key", [], _debug) -> Tkey
  | Prim(_, "key_hash", [], _debug) -> Tkey_hash
  | Prim(_, "signature", [], _debug) -> Tsignature
  | Prim(_, "string", [], _debug) -> Tstring
  | Prim(_, "pair", [x;y], _debug) -> Ttuple [convert_type env x;
                                              convert_type env y]
  | Prim(_, "or", [x;y], _debug) -> Tor (convert_type env x,
                                         convert_type env y)
  | Prim(_, "contract", [x;y], _debug) -> Tcontract
                                                (convert_type env x,
                                                 convert_type env y)
  | Prim(_, "lambda", [x;y], _debug) -> Tlambda
                                                (convert_type env x,
                                                 convert_type env y)
  | Prim(_, "map", [x;y], _debug) -> Tmap
                                           (convert_type env x,
                                            convert_type env y)
  | Prim(_, "set", [x], _debug) -> Tset (convert_type env x)
  | Prim(_, "list", [x], _debug) -> Tlist (convert_type env x)
  | Prim(_, "option", [x], _debug) -> Toption (convert_type env x)
  | _ -> unknown_expr env "convert_type" expr

let is_liq_annot name =
  try Scanf.sscanf name "@liq_" true
  with _ -> false

let liq_annot name =
  Scanf.sscanf name "@liq_%s" (fun s -> s)

let rec convert_code env expr =
  match expr with
  | Seq (index, [], Some name) when is_liq_annot name ->
    env.annoted <- true;
    mic_loc (loc_of_int env index) (ANNOT (liq_annot name))
  | Seq (index, exprs, _debug) ->
    mic_loc (loc_of_int env index)
      (SEQ (List.map (convert_code env) exprs))
  | Prim(index, "DUP", [], _debug) ->
    mic_loc (loc_of_int env index) (DUP 1)
  | Prim(index, "DROP", [], _debug) ->
    mic_loc (loc_of_int env index) (DROP)
  | Prim(index, "DIP", [ arg ], _debug) ->
    mic_loc (loc_of_int env index) (DIP (1, convert_code env arg))
  | Prim(index, "CAR", [], _debug) ->
    mic_loc (loc_of_int env index) (CAR)
  | Prim(index, "CDR", [], _debug) ->
    mic_loc (loc_of_int env index) (CDR)
  | Prim(index, "SWAP", [], _debug) ->
    mic_loc (loc_of_int env index) (SWAP)
  | Prim(index, "IF", [x;y], _debug) ->
    mic_loc (loc_of_int env index)
      (IF (convert_code env x, convert_code env y))
  | Prim(index, "IF_NONE", [x;y], _debug) ->
    mic_loc (loc_of_int env index)
      (IF_NONE (convert_code env x, convert_code env y))
  | Prim(index, "IF_LEFT", [x;y], _debug) ->
    mic_loc (loc_of_int env index)
      (IF_LEFT (convert_code env x, convert_code env y))
  | Prim(index, "IF_CONS", [x;y], _debug) ->
    mic_loc (loc_of_int env index)
      (IF_CONS (convert_code env x, convert_code env y))
  | Prim(index, "NOW", [], _debug) ->
    mic_loc (loc_of_int env index) (NOW)
  | Prim(index, "PAIR", [], _debug) ->
    mic_loc (loc_of_int env index) (PAIR)
  | Prim(index, "BALANCE", [], _debug) ->
    mic_loc (loc_of_int env index) (BALANCE)
  | Prim(index, "SUB", [], _debug) ->
    mic_loc (loc_of_int env index) (SUB)
  | Prim(index, "ADD", [], _debug) ->
    mic_loc (loc_of_int env index) (ADD)
  | Prim(index, "MUL", [], _debug) ->
    mic_loc (loc_of_int env index) (MUL)
  | Prim(index, "NEQ", [], _debug) ->
    mic_loc (loc_of_int env index) (NEQ)
  | Prim(index, "EQ", [], _debug) ->
    mic_loc (loc_of_int env index) (EQ)
  | Prim(index, "LT", [], _debug) ->
    mic_loc (loc_of_int env index) (LT)
  | Prim(index, "LE", [], _debug) ->
    mic_loc (loc_of_int env index) (LE)
  | Prim(index, "GT", [], _debug) ->
    mic_loc (loc_of_int env index) (GT)
  | Prim(index, "GE", [], _debug) ->
    mic_loc (loc_of_int env index) (GE)
  | Prim(index, "GET", [], _debug) ->
    mic_loc (loc_of_int env index) (GET)
  | Prim(index, "UPDATE", [], _debug) ->
    mic_loc (loc_of_int env index) (UPDATE)
  | Prim(index, "MEM", [], _debug) ->
    mic_loc (loc_of_int env index) (MEM)
  | Prim(index, "SOME", [], _debug) ->
    mic_loc (loc_of_int env index) (SOME)
  | Prim(index, "MANAGER", [], _debug) ->
    mic_loc (loc_of_int env index) (MANAGER)
  | Prim(index, "SOURCE", [ty1; ty2], _debug) ->
    mic_loc (loc_of_int env index)
      (SOURCE (convert_type env ty1, convert_type env ty2))
  | Prim(index, "MAP", [], _debug) ->
    mic_loc (loc_of_int env index) (MAP)
  | Prim(index, "OR", [], _debug) ->
    mic_loc (loc_of_int env index) (OR)
  | Prim(index, "LAMBDA", [ty1; ty2; expr], _debug) ->
    mic_loc (loc_of_int env index)
      (LAMBDA (convert_type env ty1, convert_type env ty2,
               convert_code env expr))
  | Prim(index, "REDUCE", [], _debug) ->
    mic_loc (loc_of_int env index) (REDUCE)
  | Prim(index, "COMPARE", [], _debug) ->
    mic_loc (loc_of_int env index) (COMPARE)
  | Prim(index, "FAIL", [], _debug) ->
    mic_loc (loc_of_int env index) (FAIL)
  | Prim(index, "UNIT", [], _debug) ->
    mic_loc (loc_of_int env index) (PUSH (Tunit, CUnit))
  | Prim(index, "TRANSFER_TOKENS", [], _debug) ->
    mic_loc (loc_of_int env index) (TRANSFER_TOKENS)
  | Prim(index, "PUSH", [ ty; cst ], _debug) ->
     begin match convert_type env ty, convert_const env cst with
     | Tnat, CInt n ->
        mic_loc (loc_of_int env index) (PUSH (Tnat, CNat n))
     | ty, cst ->
        mic_loc (loc_of_int env index) (PUSH (ty, cst))
     end
  | Prim(index, "H", [], _debug) ->
    mic_loc (loc_of_int env index) (H)
  | Prim(index, "HASH_KEY", [], _debug) ->
    mic_loc (loc_of_int env index) (HASH_KEY)
  | Prim(index, "CHECK_SIGNATURE", [], _debug) ->
    mic_loc (loc_of_int env index) (CHECK_SIGNATURE)
  | Prim(index, "CONCAT", [], _debug) ->
    mic_loc (loc_of_int env index) (CONCAT)
  | Prim(index, "EDIV", [], _debug) ->
    mic_loc (loc_of_int env index) (EDIV)
  | Prim(index, "EXEC", [], _debug) ->
    mic_loc (loc_of_int env index) (EXEC)
  | Prim(index, "MOD", [], _debug) ->
    mic_loc (loc_of_int env index) (MOD)
  | Prim(index, "DIV", [], _debug) ->
    mic_loc (loc_of_int env index) (DIV)
  | Prim(index, "AMOUNT", [], _debug) ->
    mic_loc (loc_of_int env index) (AMOUNT)
  | Prim(index, "NIL", [ty], _debug) ->
    mic_loc (loc_of_int env index)
      (PUSH (Tlist (convert_type env ty), CList []))
  | Prim(index, "EMPTY_SET", [ty], _debug) ->
    mic_loc (loc_of_int env index)
      (PUSH (Tset (convert_type env ty), CSet []))
  | Prim(index, "EMPTY_MAP", [ty1; ty2], _debug) ->
    mic_loc (loc_of_int env index)
      (PUSH (Tmap (convert_type env ty1, convert_type env ty2), CMap []))
  | Prim(index, "NONE", [ty], _debug) ->
    mic_loc (loc_of_int env index)
      (PUSH (Toption (convert_type env ty), CNone))
  | Prim(index, "LEFT", [ty], _debug) ->
    mic_loc (loc_of_int env index)
      (LEFT (convert_type env ty))
  | Prim(index, "CONS", [], _debug) ->
    mic_loc (loc_of_int env index) (CONS)
  | Prim(index, "LOOP", [loop], _debug) ->
    mic_loc (loc_of_int env index)
      (LOOP (convert_code env loop))
  | Prim(index, "ITER", [body], _debug) ->
    mic_loc (loc_of_int env index)
      (ITER (convert_code env body))
  | Prim(index, "RIGHT", [ty], _debug) ->
    mic_loc (loc_of_int env index)
      (RIGHT (convert_type env ty))
  | Prim(index, "INT", [], _debug) ->
    mic_loc (loc_of_int env index) (INT)
  | Prim(index, "SIZE", [], _debug) ->
    mic_loc (loc_of_int env index) (SIZE)
  | Prim(index, "AND", [], _debug) ->
    mic_loc (loc_of_int env index) (AND)
  | Prim(index, "XOR", [], _debug) ->
    mic_loc (loc_of_int env index) (XOR)
  | Prim(index, "ABS", [], _debug) ->
    mic_loc (loc_of_int env index) (ABS)
  | Prim(index, "NOT", [], _debug) ->
    mic_loc (loc_of_int env index) (NOT)
  | Prim(index, "STEPS_TO_QUOTA", [], _debug) ->
    mic_loc (loc_of_int env index) (STEPS_TO_QUOTA)
  | Prim(index, "CREATE_ACCOUNT", [], _debug) ->
    mic_loc (loc_of_int env index) (CREATE_ACCOUNT)
  | Prim(index, "CREATE_CONTRACT", [], _debug) ->
    mic_loc (loc_of_int env index) (CREATE_CONTRACT)
  | Prim(index, "DEFAULT_ACCOUNT", [], _debug) ->
    mic_loc (loc_of_int env index) (DEFAULT_ACCOUNT)

  | _ -> unknown_expr env "convert_code" expr

let rec find nodes name =
  match nodes with
  | [] -> raise (Missing_program_field name)
  | Prim(_, name_maybe, [ v ], _) :: nodes ->
     if name_maybe = name then v
     else find nodes name
  | _ -> raise (Missing_program_field name)

let rec expand expr =
  match Michelson_macros.expand expr with
  | Seq (loc, items, annot) ->
     Seq (loc, List.map expand items, annot)
  | Prim (loc, name, args, annot) ->
     Prim (loc, name, List.map expand args, annot)
  | Int _ | String _ as atom -> atom


let convert_contract env c =
  let c =
    List.map (fun c ->
        let c = Micheline.inject_locations (fun i -> i) c in
      expand c) c in
  let return = convert_type env (find c "return") in
  let parameter = convert_type env (find c "parameter") in
  let storage = convert_type env (find c "storage") in
  let code = convert_code env (find c "code") in
  { code; storage; return; parameter }, env.annoted


let convert_loc_table f loc_tables =
  List.fold_left (
    List.fold_left (fun acc (i, p) ->
        IntMap.add i (liquid_loc_of_script_loc f p) acc
      )
  ) IntMap.empty loc_tables

let contract_of_string filename s =
  let tokens, errors = Micheline_parser.tokenize s in
  match errors with
  | error :: _ ->
    raise (LiquidError (error_to_liqerror filename error))
  | [] ->
     let nodes, errors = Micheline_parser.parse_toplevel tokens in
     match errors with
     | error :: _ ->
       raise (LiquidError (error_to_liqerror filename error))
     | [] ->
        let nodes = List.map Micheline.extract_locations nodes in
        let (nodes, loc_tables) = List.split nodes in
        let env = { filename;
                    loc_table = convert_loc_table filename loc_tables;
                    annoted = false;
                  } in
        Some (nodes, env)
