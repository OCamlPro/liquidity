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
  Printf.eprintf "Error in %s: %s\n%!" f_name
                 (match expr with
                  | Script_repr.Seq (_loc, _exprs, _debug) -> "Seq (_)"
                  | Script_repr.String (_loc, s) ->
                     Printf.sprintf "String %S" s
                  | Script_repr.Int (_loc, s) ->
                     Printf.sprintf "Int %s" s
                  | Script_repr.Prim(_, s, args, _debug) ->
                     Printf.sprintf "Prim(%S,[%d])" s (List.length args)
                 );
  raise Error

let rec convert_const expr =
  match expr with
  | Script_repr.Int (_loc, n) -> CInt n
  | Script_repr.String (_loc, s) -> CString s
  | Script_repr.Prim(_, "Unit", [], _debug) -> CUnit
  | Script_repr.Prim(_, "True", [], _debug) -> CBool true
  | Script_repr.Prim(_, "False", [], _debug) -> CBool false
  | Script_repr.Prim(_, "None", [], _debug) -> CNone

  | Script_repr.Prim(_, "Some", [x], _debug) -> CSome (convert_const x)
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

let rec convert_code expr =
  match expr with
  | Script_repr.Seq (_loc, exprs, _debug) ->
     SEQ (List.map convert_code exprs)
  | Script_repr.Prim(_, "DUP", [], _debug) -> DUP 1
  | Script_repr.Prim(_, "DROP", [], _debug) -> DROP
  | Script_repr.Prim(_, "DIP", [ arg ], _debug) -> DIP (1, convert_code arg)
  | Script_repr.Prim(_, "CAR", [], _debug) -> CAR
  | Script_repr.Prim(_, "CDR", [], _debug) -> CDR
  | Script_repr.Prim(_, "SWAP", [], _debug) -> SWAP
  | Script_repr.Prim(_, "IF", [x;y], _debug) ->
     IF (convert_code x, convert_code y)
  | Script_repr.Prim(_, "IF_NONE", [x;y], _debug) ->
     IF_NONE (convert_code x, convert_code y)
  | Script_repr.Prim(_, "IF_LEFT", [x;y], _debug) ->
     IF_LEFT (convert_code x, convert_code y)
  | Script_repr.Prim(_, "IF_CONS", [x;y], _debug) ->
     IF_CONS (convert_code x, convert_code y)
  | Script_repr.Prim(_, "NOW", [], _debug) -> NOW
  | Script_repr.Prim(_, "PAIR", [], _debug) -> PAIR
  | Script_repr.Prim(_, "BALANCE", [], _debug) -> BALANCE
  | Script_repr.Prim(_, "SUB", [], _debug) -> SUB
  | Script_repr.Prim(_, "ADD", [], _debug) -> ADD
  | Script_repr.Prim(_, "MUL", [], _debug) -> MUL
  | Script_repr.Prim(_, "NEQ", [], _debug) -> NEQ
  | Script_repr.Prim(_, "EQ", [], _debug) -> EQ
  | Script_repr.Prim(_, "LT", [], _debug) -> LT
  | Script_repr.Prim(_, "LE", [], _debug) -> LE
  | Script_repr.Prim(_, "GT", [], _debug) -> GT
  | Script_repr.Prim(_, "GE", [], _debug) -> GE
  | Script_repr.Prim(_, "GET", [], _debug) -> GET
  | Script_repr.Prim(_, "UPDATE", [], _debug) -> UPDATE
  | Script_repr.Prim(_, "MEM", [], _debug) -> MEM
  | Script_repr.Prim(_, "SOME", [], _debug) -> SOME
  | Script_repr.Prim(_, "MANAGER", [], _debug) -> MANAGER
  | Script_repr.Prim(_, "SOURCE", [ty1; ty2], _debug) ->
     SOURCE (convert_type ty1, convert_type ty2)
  | Script_repr.Prim(_, "MAP", [], _debug) -> MAP
  | Script_repr.Prim(_, "OR", [], _debug) -> OR
  | Script_repr.Prim(_, "LAMBDA", [ty1; ty2; expr], _debug) ->
     LAMBDA (convert_type ty1, convert_type ty2, convert_code expr)
  | Script_repr.Prim(_, "REDUCE", [], _debug) -> REDUCE
  | Script_repr.Prim(_, "COMPARE", [], _debug) -> COMPARE
  | Script_repr.Prim(_, "FAIL", [], _debug) -> FAIL
  | Script_repr.Prim(_, "UNIT", [], _debug) -> PUSH (Tunit, CUnit)
  | Script_repr.Prim(_, "TRANSFER_TOKENS", [], _debug) -> TRANSFER_TOKENS
  | Script_repr.Prim(_, "PUSH", [ ty; cst ], _debug) ->
     begin match convert_type ty, convert_const cst with
     | Tnat, CInt n ->
        PUSH (Tnat, CNat n)
     | ty, cst ->
        PUSH (ty, cst)
     end
  | Script_repr.Prim(_, "H", [], _debug) -> H
  | Script_repr.Prim(_, "CHECK_SIGNATURE", [], _debug) -> CHECK_SIGNATURE
  | Script_repr.Prim(_, "CONCAT", [], _debug) -> CONCAT
  | Script_repr.Prim(_, "EDIV", [], _debug) -> EDIV
  | Script_repr.Prim(_, "EXEC", [], _debug) -> EXEC
  | Script_repr.Prim(_, "MOD", [], _debug) -> MOD
  | Script_repr.Prim(_, "DIV", [], _debug) -> DIV
  | Script_repr.Prim(_, "AMOUNT", [], _debug) -> AMOUNT
  | Script_repr.Prim(_, "NIL", [ty], _debug) ->
     PUSH (Tlist (convert_type ty), CList [])
  | Script_repr.Prim(_, "EMPTY_SET", [ty], _debug) ->
     PUSH (Tset (convert_type ty), CSet [])
  | Script_repr.Prim(_, "EMPTY_MAP", [ty1; ty2], _debug) ->
     PUSH (Tmap (convert_type ty1, convert_type ty2), CMap [])
  | Script_repr.Prim(_, "NONE", [ty], _debug) ->
     PUSH (Toption (convert_type ty), CNone)
  | Script_repr.Prim(_, "LEFT", [ty], _debug) ->
     LEFT (convert_type ty)
  | Script_repr.Prim(_, "CONS", [], _debug) -> CONS
  | Script_repr.Prim(_, "LOOP", [loop], _debug) -> LOOP (convert_code loop)
  | Script_repr.Prim(_, "RIGHT", [ty], _debug) ->
     RIGHT (convert_type ty)
  | Script_repr.Prim(_, "INT", [], _debug) -> INT
  | Script_repr.Prim(_, "SIZE", [], _debug) -> SIZE
  | Script_repr.Prim(_, "AND", [], _debug) -> AND
  | Script_repr.Prim(_, "XOR", [], _debug) -> XOR
  | Script_repr.Prim(_, "ABS", [], _debug) -> ABS
  | Script_repr.Prim(_, "NOT", [], _debug) -> NOT
  | Script_repr.Prim(_, "STEPS_TO_QUOTA", [], _debug) -> STEPS_TO_QUOTA
  | Script_repr.Prim(_, "CREATE_ACCOUNT", [], _debug) -> CREATE_ACCOUNT
  | Script_repr.Prim(_, "CREATE_CONTRACT", [], _debug) -> CREATE_CONTRACT
  | Script_repr.Prim(_, "DEFAULT_ACCOUNT", [], _debug) -> DEFAULT_ACCOUNT


  | _ -> unknown_expr "convert_code" expr

let convert_contract c =
  let return = convert_type c.Script_repr.ret_type in
  let parameter = convert_type c.Script_repr.arg_type in
  let storage = convert_type c.Script_repr.storage_type in
  let code = convert_code c.Script_repr.code in
  { code;
    storage; return; parameter }

let contract_of_string s =
  match Client_proto_programs.parse_program s with
  | Ok parsed -> Some parsed.ast
  | Error _ -> None

let data_of_string s =
  match Client_proto_programs.parse_data s with
  | Ok data -> Some data.ast
  | Error _ -> None

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
