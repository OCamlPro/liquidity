(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes
open Tezos_context

type tezos_code = Script_repr.code

open Client_proto_programs
(*
type expr =
  | Int of location * string
  | String of location * string
  | Prim of location * string * expr list
  | Seq of location * expr list
 *)

let unknown_expr f_name expr =
  LiquidLoc.raise_error ~loc:(LiquidLoc.loc_in_file f_name) "%s"
    (match expr with
     | Script_repr.Seq (_loc, _exprs, _debug) -> "Seq (_)"
     | Script_repr.String (_loc, s) ->
       Printf.sprintf "String %S" s
     | Script_repr.Int (_loc, s) ->
       Printf.sprintf "Int %s" s
     | Script_repr.Prim(_, s, args, _debug) ->
       Printf.sprintf "Prim(%S,[%d])" s (List.length args)
    )

let rec convert_const expr =
  match expr with
  | Script_repr.Int (_loc, n) -> CInt (LiquidPrinter.integer_of_mic n)
  | Script_repr.String (_loc, s) -> CString s
  | Script_repr.Prim(_, "Unit", [], _debug) -> CUnit
  | Script_repr.Prim(_, "True", [], _debug) -> CBool true
  | Script_repr.Prim(_, "False", [], _debug) -> CBool false
  | Script_repr.Prim(_, "None", [], _debug) -> CNone

  | Script_repr.Prim(_, "Some", [x], _debug) -> CSome (convert_const x)
  | Script_repr.Prim(_, "Left", [x], _debug) -> CLeft (convert_const x)
  | Script_repr.Prim(_, "Right", [x], _debug) -> CRight (convert_const x)
  | Script_repr.Prim(_, "Pair", [x;y], _debug) -> CTuple [convert_const x;
                                                  convert_const y]
  | Script_repr.Prim(_, "List", args, _debug) -> CList (List.map convert_const args)
  | Script_repr.Prim(_, "Map", args, _debug) ->
     CMap (List.map (function
                     | Script_repr.Prim(_, "Item", [x;y], _debug) ->
                        convert_const x, convert_const y
                     | expr ->
                        unknown_expr "convert_const_item" expr) args)
  | Script_repr.Prim(_, "Set", args, _debug) -> CSet (List.map convert_const args)
  | _ -> unknown_expr "convert_const" expr

let rec convert_type expr =
  match expr with
  | Script_repr.Prim(_, "unit", [], _debug) -> Tunit
  | Script_repr.Prim(_, "timestamp", [], _debug) -> Ttimestamp
  | Script_repr.Prim(_, "tez", [], _debug) -> Ttez
  | Script_repr.Prim(_, "int", [], _debug) -> Tint
  | Script_repr.Prim(_, "nat", [], _debug) -> Tnat
  | Script_repr.Prim(_, "bool", [], _debug) -> Tbool
  | Script_repr.Prim(_, "key", [], _debug) -> Tkey
  | Script_repr.Prim(_, "key_hash", [], _debug) -> Tkey_hash
  | Script_repr.Prim(_, "signature", [], _debug) -> Tsignature
  | Script_repr.Prim(_, "string", [], _debug) -> Tstring
  | Script_repr.Prim(_, "pair", [x;y], _debug) -> Ttuple [convert_type x;
                                                  convert_type y]
  | Script_repr.Prim(_, "or", [x;y], _debug) -> Tor (convert_type x,
                                                  convert_type y)
  | Script_repr.Prim(_, "contract", [x;y], _debug) -> Tcontract
                                                (convert_type x,
                                                 convert_type y)
  | Script_repr.Prim(_, "lambda", [x;y], _debug) -> Tlambda
                                                (convert_type x,
                                                 convert_type y)
  | Script_repr.Prim(_, "map", [x;y], _debug) -> Tmap
                                           (convert_type x,
                                            convert_type y)
  | Script_repr.Prim(_, "set", [x], _debug) -> Tset (convert_type x)
  | Script_repr.Prim(_, "list", [x], _debug) -> Tlist (convert_type x)
  | Script_repr.Prim(_, "option", [x], _debug) -> Toption (convert_type x)
  | _ -> unknown_expr "convert_type" expr

let loc_of_int loc_table index =
  try List.assoc index loc_table
  with Not_found -> LiquidLoc.noloc

let rec convert_code loc_table expr =
  match expr with
  | Script_repr.Seq (index, exprs, _debug) ->
    mic_loc (loc_of_int loc_table index)
      (SEQ (List.map (convert_code loc_table) exprs))
  | Script_repr.Prim(index, "DUP", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (DUP 1)
  | Script_repr.Prim(index, "DROP", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (DROP)
  | Script_repr.Prim(index, "DIP", [ arg ], _debug) ->
    mic_loc (loc_of_int loc_table index) (DIP (1, convert_code loc_table arg))
  | Script_repr.Prim(index, "CAR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CAR)
  | Script_repr.Prim(index, "CDR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CDR)
  | Script_repr.Prim(index, "SWAP", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (SWAP)
  | Script_repr.Prim(index, "IF", [x;y], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (IF (convert_code loc_table x, convert_code loc_table y))
  | Script_repr.Prim(index, "IF_NONE", [x;y], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (IF_NONE (convert_code loc_table x, convert_code loc_table y))
  | Script_repr.Prim(index, "IF_LEFT", [x;y], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (IF_LEFT (convert_code loc_table x, convert_code loc_table y))
  | Script_repr.Prim(index, "IF_CONS", [x;y], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (IF_CONS (convert_code loc_table x, convert_code loc_table y))
  | Script_repr.Prim(index, "NOW", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (NOW)
  | Script_repr.Prim(index, "PAIR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (PAIR)
  | Script_repr.Prim(index, "BALANCE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (BALANCE)
  | Script_repr.Prim(index, "SUB", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (SUB)
  | Script_repr.Prim(index, "ADD", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (ADD)
  | Script_repr.Prim(index, "MUL", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MUL)
  | Script_repr.Prim(index, "NEQ", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (NEQ)
  | Script_repr.Prim(index, "EQ", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (EQ)
  | Script_repr.Prim(index, "LT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (LT)
  | Script_repr.Prim(index, "LE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (LE)
  | Script_repr.Prim(index, "GT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (GT)
  | Script_repr.Prim(index, "GE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (GE)
  | Script_repr.Prim(index, "GET", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (GET)
  | Script_repr.Prim(index, "UPDATE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (UPDATE)
  | Script_repr.Prim(index, "MEM", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MEM)
  | Script_repr.Prim(index, "SOME", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (SOME)
  | Script_repr.Prim(index, "MANAGER", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MANAGER)
  | Script_repr.Prim(index, "SOURCE", [ty1; ty2], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (SOURCE (convert_type ty1, convert_type ty2))
  | Script_repr.Prim(index, "MAP", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MAP)
  | Script_repr.Prim(index, "OR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (OR)
  | Script_repr.Prim(index, "LAMBDA", [ty1; ty2; expr], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (LAMBDA (convert_type ty1, convert_type ty2, convert_code loc_table expr))
  | Script_repr.Prim(index, "REDUCE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (REDUCE)
  | Script_repr.Prim(index, "COMPARE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (COMPARE)
  | Script_repr.Prim(index, "FAIL", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (FAIL)
  | Script_repr.Prim(index, "UNIT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (PUSH (Tunit, CUnit))
  | Script_repr.Prim(index, "TRANSFER_TOKENS", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (TRANSFER_TOKENS)
  | Script_repr.Prim(index, "PUSH", [ ty; cst ], _debug) ->
     begin match convert_type ty, convert_const cst with
     | Tnat, CInt n ->
        mic_loc (loc_of_int loc_table index) (PUSH (Tnat, CNat n))
     | ty, cst ->
        mic_loc (loc_of_int loc_table index) (PUSH (ty, cst))
     end
  | Script_repr.Prim(index, "H", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (H)
  | Script_repr.Prim(index, "HASH_KEY", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (HASH_KEY)
  | Script_repr.Prim(index, "CHECK_SIGNATURE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CHECK_SIGNATURE)
  | Script_repr.Prim(index, "CONCAT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CONCAT)
  | Script_repr.Prim(index, "EDIV", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (EDIV)
  | Script_repr.Prim(index, "EXEC", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (EXEC)
  | Script_repr.Prim(index, "MOD", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MOD)
  | Script_repr.Prim(index, "DIV", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (DIV)
  | Script_repr.Prim(index, "AMOUNT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (AMOUNT)
  | Script_repr.Prim(index, "NIL", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (PUSH (Tlist (convert_type ty), CList []))
  | Script_repr.Prim(index, "EMPTY_SET", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (PUSH (Tset (convert_type ty), CSet []))
  | Script_repr.Prim(index, "EMPTY_MAP", [ty1; ty2], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (PUSH (Tmap (convert_type ty1, convert_type ty2), CMap []))
  | Script_repr.Prim(index, "NONE", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (PUSH (Toption (convert_type ty), CNone))
  | Script_repr.Prim(index, "LEFT", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (LEFT (convert_type ty))
  | Script_repr.Prim(index, "CONS", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CONS)
  | Script_repr.Prim(index, "LOOP", [loop], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (LOOP (convert_code loc_table loop))
  | Script_repr.Prim(index, "RIGHT", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (RIGHT (convert_type ty))
  | Script_repr.Prim(index, "INT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (INT)
  | Script_repr.Prim(index, "SIZE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (SIZE)
  | Script_repr.Prim(index, "AND", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (AND)
  | Script_repr.Prim(index, "XOR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (XOR)
  | Script_repr.Prim(index, "ABS", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (ABS)
  | Script_repr.Prim(index, "NOT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (NOT)
  | Script_repr.Prim(index, "STEPS_TO_QUOTA", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (STEPS_TO_QUOTA)
  | Script_repr.Prim(index, "CREATE_ACCOUNT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CREATE_ACCOUNT)
  | Script_repr.Prim(index, "CREATE_CONTRACT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CREATE_CONTRACT)
  | Script_repr.Prim(index, "DEFAULT_ACCOUNT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (DEFAULT_ACCOUNT)

  | _ -> unknown_expr "convert_code" expr

let convert_contract loc_table c =
  let return = convert_type c.Script_repr.ret_type in
  let parameter = convert_type c.Script_repr.arg_type in
  let storage = convert_type c.Script_repr.storage_type in
  let code = convert_code loc_table c.Script_repr.code in
  { code; storage; return; parameter }

let liquid_loc_of_script_loc f { Script_located_ir.start; stop } =
  { loc_file = f;
    loc_pos = Some (
        (start.Script_located_ir.line, start.Script_located_ir.column),
        (stop.Script_located_ir.line, stop.Script_located_ir.column))
  }

let convert_loc_table f loc_table =
  let loc_table = List.assoc "code" loc_table in
  List.map (fun (i, p) -> (i, liquid_loc_of_script_loc f p)) loc_table

let contract_of_string filename s =
  match Client_proto_programs.parse_program s with
  | Ok parsed -> Some (parsed.ast, convert_loc_table filename parsed.loc_table)
  | Error _ -> None

let data_of_string s =
  match Client_proto_programs.parse_data s with
  | Ok data -> Some data.ast
  | Error _ -> None

                 (*
let pp = Tezos_context.pp

module JSON = struct

  open OcpJson.TYPES

  let rec convert json =
    match json with
    | `Bool bool -> B bool
    | `Float f -> F f
    | `Null -> Z
    | `String s -> S s
    | `A list -> L (List.map convert list)
    | `O list -> O (List.map (fun (s,e) -> (s, convert e)) list)

  let to_string j =
    OcpJson.to_string ~minify:false (convert j)

  let () =
    Error_monad.json_to_string := to_string

end
                  *)
