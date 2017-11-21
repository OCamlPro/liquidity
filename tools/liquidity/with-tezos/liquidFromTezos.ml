(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

open Micheline;;

let unknown_expr f_name expr =
  LiquidLoc.raise_error ~loc:(LiquidLoc.loc_in_file f_name) "%s"
    (match expr with
     | Micheline.Seq (_loc, _exprs, _debug) -> "Seq (_)"
     | Micheline.String (_loc, s) ->
       Printf.sprintf "String %S" s
     | Micheline.Int (_loc, s) ->
       Printf.sprintf "Int %s" s
     | Micheline.Prim(_, s, args, _debug) ->
       Printf.sprintf "Prim(%S,[%d])" s (List.length args)
    )

let rec convert_const expr =
  match expr with
  | Micheline.Int (_loc, n) -> CInt (LiquidPrinter.integer_of_mic n)
  | Micheline.String (_loc, s) -> CString s
  | Micheline.Prim(_, "Unit", [], _debug) -> CUnit
  | Micheline.Prim(_, "True", [], _debug) -> CBool true
  | Micheline.Prim(_, "False", [], _debug) -> CBool false
  | Micheline.Prim(_, "None", [], _debug) -> CNone

  | Micheline.Prim(_, "Some", [x], _debug) -> CSome (convert_const x)
  | Micheline.Prim(_, "Left", [x], _debug) -> CLeft (convert_const x)
  | Micheline.Prim(_, "Right", [x], _debug) -> CRight (convert_const x)
  | Micheline.Prim(_, "Pair", [x;y], _debug) -> CTuple [convert_const x;
                                                  convert_const y]
  | Micheline.Prim(_, "List", args, _debug) -> CList (List.map convert_const args)
  | Micheline.Prim(_, "Map", args, _debug) ->
     CMap (List.map (function
                     | Micheline.Prim(_, "Item", [x;y], _debug) ->
                        convert_const x, convert_const y
                     | expr ->
                        unknown_expr "convert_const_item" expr) args)
  | Micheline.Prim(_, "Set", args, _debug) -> CSet (List.map convert_const args)
  | _ -> unknown_expr "convert_const" expr

let rec convert_type expr =
  match expr with
  | Micheline.Prim(_, "unit", [], _debug) -> Tunit
  | Micheline.Prim(_, "timestamp", [], _debug) -> Ttimestamp
  | Micheline.Prim(_, "tez", [], _debug) -> Ttez
  | Micheline.Prim(_, "int", [], _debug) -> Tint
  | Micheline.Prim(_, "nat", [], _debug) -> Tnat
  | Micheline.Prim(_, "bool", [], _debug) -> Tbool
  | Micheline.Prim(_, "key", [], _debug) -> Tkey
  | Micheline.Prim(_, "key_hash", [], _debug) -> Tkey_hash
  | Micheline.Prim(_, "signature", [], _debug) -> Tsignature
  | Micheline.Prim(_, "string", [], _debug) -> Tstring
  | Micheline.Prim(_, "pair", [x;y], _debug) -> Ttuple [convert_type x;
                                                  convert_type y]
  | Micheline.Prim(_, "or", [x;y], _debug) -> Tor (convert_type x,
                                                  convert_type y)
  | Micheline.Prim(_, "contract", [x;y], _debug) -> Tcontract
                                                (convert_type x,
                                                 convert_type y)
  | Micheline.Prim(_, "lambda", [x;y], _debug) -> Tlambda
                                                (convert_type x,
                                                 convert_type y)
  | Micheline.Prim(_, "map", [x;y], _debug) -> Tmap
                                           (convert_type x,
                                            convert_type y)
  | Micheline.Prim(_, "set", [x], _debug) -> Tset (convert_type x)
  | Micheline.Prim(_, "list", [x], _debug) -> Tlist (convert_type x)
  | Micheline.Prim(_, "option", [x], _debug) -> Toption (convert_type x)
  | _ -> unknown_expr "convert_type" expr

let loc_of_int loc_table index =
  try List.assoc index loc_table
  with Not_found -> LiquidLoc.noloc

let rec convert_code loc_table expr =
  match expr with
  | Micheline.Seq (index, exprs, _debug) ->
    mic_loc (loc_of_int loc_table index)
      (SEQ (List.map (convert_code loc_table) exprs))
  | Micheline.Prim(index, "DUP", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (DUP 1)
  | Micheline.Prim(index, "DROP", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (DROP)
  | Micheline.Prim(index, "DIP", [ arg ], _debug) ->
    mic_loc (loc_of_int loc_table index) (DIP (1, convert_code loc_table arg))
  | Micheline.Prim(index, "CAR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CAR)
  | Micheline.Prim(index, "CDR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CDR)
  | Micheline.Prim(index, "SWAP", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (SWAP)
  | Micheline.Prim(index, "IF", [x;y], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (IF (convert_code loc_table x, convert_code loc_table y))
  | Micheline.Prim(index, "IF_NONE", [x;y], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (IF_NONE (convert_code loc_table x, convert_code loc_table y))
  | Micheline.Prim(index, "IF_LEFT", [x;y], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (IF_LEFT (convert_code loc_table x, convert_code loc_table y))
  | Micheline.Prim(index, "IF_CONS", [x;y], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (IF_CONS (convert_code loc_table x, convert_code loc_table y))
  | Micheline.Prim(index, "NOW", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (NOW)
  | Micheline.Prim(index, "PAIR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (PAIR)
  | Micheline.Prim(index, "BALANCE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (BALANCE)
  | Micheline.Prim(index, "SUB", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (SUB)
  | Micheline.Prim(index, "ADD", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (ADD)
  | Micheline.Prim(index, "MUL", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MUL)
  | Micheline.Prim(index, "NEQ", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (NEQ)
  | Micheline.Prim(index, "EQ", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (EQ)
  | Micheline.Prim(index, "LT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (LT)
  | Micheline.Prim(index, "LE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (LE)
  | Micheline.Prim(index, "GT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (GT)
  | Micheline.Prim(index, "GE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (GE)
  | Micheline.Prim(index, "GET", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (GET)
  | Micheline.Prim(index, "UPDATE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (UPDATE)
  | Micheline.Prim(index, "MEM", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MEM)
  | Micheline.Prim(index, "SOME", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (SOME)
  | Micheline.Prim(index, "MANAGER", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MANAGER)
  | Micheline.Prim(index, "SOURCE", [ty1; ty2], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (SOURCE (convert_type ty1, convert_type ty2))
  | Micheline.Prim(index, "MAP", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MAP)
  | Micheline.Prim(index, "OR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (OR)
  | Micheline.Prim(index, "LAMBDA", [ty1; ty2; expr], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (LAMBDA (convert_type ty1, convert_type ty2, convert_code loc_table expr))
  | Micheline.Prim(index, "REDUCE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (REDUCE)
  | Micheline.Prim(index, "COMPARE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (COMPARE)
  | Micheline.Prim(index, "FAIL", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (FAIL)
  | Micheline.Prim(index, "UNIT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (PUSH (Tunit, CUnit))
  | Micheline.Prim(index, "TRANSFER_TOKENS", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (TRANSFER_TOKENS)
  | Micheline.Prim(index, "PUSH", [ ty; cst ], _debug) ->
     begin match convert_type ty, convert_const cst with
     | Tnat, CInt n ->
        mic_loc (loc_of_int loc_table index) (PUSH (Tnat, CNat n))
     | ty, cst ->
        mic_loc (loc_of_int loc_table index) (PUSH (ty, cst))
     end
  | Micheline.Prim(index, "H", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (H)
  | Micheline.Prim(index, "HASH_KEY", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (HASH_KEY)
  | Micheline.Prim(index, "CHECK_SIGNATURE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CHECK_SIGNATURE)
  | Micheline.Prim(index, "CONCAT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CONCAT)
  | Micheline.Prim(index, "EDIV", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (EDIV)
  | Micheline.Prim(index, "EXEC", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (EXEC)
  | Micheline.Prim(index, "MOD", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (MOD)
  | Micheline.Prim(index, "DIV", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (DIV)
  | Micheline.Prim(index, "AMOUNT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (AMOUNT)
  | Micheline.Prim(index, "NIL", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (PUSH (Tlist (convert_type ty), CList []))
  | Micheline.Prim(index, "EMPTY_SET", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (PUSH (Tset (convert_type ty), CSet []))
  | Micheline.Prim(index, "EMPTY_MAP", [ty1; ty2], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (PUSH (Tmap (convert_type ty1, convert_type ty2), CMap []))
  | Micheline.Prim(index, "NONE", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (PUSH (Toption (convert_type ty), CNone))
  | Micheline.Prim(index, "LEFT", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (LEFT (convert_type ty))
  | Micheline.Prim(index, "CONS", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CONS)
  | Micheline.Prim(index, "LOOP", [loop], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (LOOP (convert_code loc_table loop))
  | Micheline.Prim(index, "RIGHT", [ty], _debug) ->
    mic_loc (loc_of_int loc_table index)
      (RIGHT (convert_type ty))
  | Micheline.Prim(index, "INT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (INT)
  | Micheline.Prim(index, "SIZE", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (SIZE)
  | Micheline.Prim(index, "AND", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (AND)
  | Micheline.Prim(index, "XOR", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (XOR)
  | Micheline.Prim(index, "ABS", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (ABS)
  | Micheline.Prim(index, "NOT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (NOT)
  | Micheline.Prim(index, "STEPS_TO_QUOTA", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (STEPS_TO_QUOTA)
  | Micheline.Prim(index, "CREATE_ACCOUNT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CREATE_ACCOUNT)
  | Micheline.Prim(index, "CREATE_CONTRACT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (CREATE_CONTRACT)
  | Micheline.Prim(index, "DEFAULT_ACCOUNT", [], _debug) ->
    mic_loc (loc_of_int loc_table index) (DEFAULT_ACCOUNT)

  | _ -> unknown_expr "convert_code" expr

let rec find nodes name =
  match nodes with
  | [] -> Printf.kprintf failwith "LiquidFromTezos.find: field %S not found" name
  | Micheline.Prim(_, name_maybe, [ v ], _) :: nodes ->
     if name_maybe = name then v
     else find nodes name
  | _ -> failwith "LiquidFromTezos.find: invalid format"


let convert_contract loc_table c =
  let c =
    List.map (Micheline.inject_locations (fun _ -> 0)) c in
  let return = convert_type (find c "return") in
  let parameter = convert_type (find c "parameter") in
  let storage = convert_type (find c "storage") in
  let code = convert_code loc_table (find c "code") in
  { code; storage; return; parameter }




    (*
let liquid_loc_of_script_loc f { Script_located_ir.start; stop } =
  { loc_file = f;
    loc_pos = Some (
        (start.Script_located_ir.line, start.Script_located_ir.column),
        (stop.Script_located_ir.line, stop.Script_located_ir.column))
  }

let convert_loc_table f loc_table =
  let loc_table = List.assoc "code" loc_table in
  List.map (fun (i, p) -> (i, liquid_loc_of_script_loc f p)) loc_table
     *)


let contract_of_string filename s =
  let tokens, errors = Micheline_parser.tokenize s in
  match errors with
  | _ :: _ -> None
  | [] ->
     let nodes, errors = Micheline_parser.parse_toplevel tokens in
     match errors with
     | _ :: _ -> None
     | [] ->
        let nodes = List.map Micheline.extract_locations nodes in
        let (nodes, loc_table) = List.split nodes in
        Some (nodes, [])

             (*  convert_loc_table filename parsed.loc_table) *)



                 (*
let data_of_string s =
  match Client_proto_programs.parse_data s with
  | Ok data -> Some data.ast
  | Error _ -> None

 *)

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
