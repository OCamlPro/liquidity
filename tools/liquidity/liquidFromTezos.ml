(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

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
                  | Script_repr.Seq (_loc, _exprs) -> "Seq (_)"
                  | Script_repr.String (_loc, s) ->
                     Printf.sprintf "String %S" s
                  | Script_repr.Int (_loc, s) ->
                     Printf.sprintf "Int %s" s
                  | Script_repr.Prim(_, s, args) ->
                     Printf.sprintf "Prim(%S,[%d])" s (List.length args)
                 );
  raise Error

let rec convert_const expr =
  match expr with
  | Script_repr.Int (_loc, n) -> CInt n
  | Script_repr.String (_loc, s) -> CString s
  | Script_repr.Prim(_, "Unit", []) -> CUnit
  | Script_repr.Prim(_, "True", []) -> CBool true
  | Script_repr.Prim(_, "False", []) -> CBool false
  | Script_repr.Prim(_, "None", []) -> CNone

  | Script_repr.Prim(_, "Some", [x]) -> CSome (convert_const x)
  | Script_repr.Prim(_, "Pair", [x;y]) -> CTuple [convert_const x;
                                                  convert_const y]
  | Script_repr.Prim(_, "List", args) -> CList (List.map convert_const args)
  | Script_repr.Prim(_, "Map", args) ->
     CMap (List.map (function
                     | Script_repr.Prim(_, "Item", [x;y]) ->
                        convert_const x, convert_const y
                     | expr ->
                        unknown_expr "convert_const_item" expr) args)
  | Script_repr.Prim(_, "Set", args) -> CSet (List.map convert_const args)
  | _ -> unknown_expr "convert_const" expr

let rec convert_type expr =
  match expr with
  | Script_repr.Prim(_, "unit", []) -> Tunit
  | Script_repr.Prim(_, "timestamp", []) -> Ttimestamp
  | Script_repr.Prim(_, "tez", []) -> Ttez
  | Script_repr.Prim(_, "int", []) -> Tint
  | Script_repr.Prim(_, "nat", []) -> Tnat
  | Script_repr.Prim(_, "bool", []) -> Tbool
  | Script_repr.Prim(_, "key", []) -> Tkey
  | Script_repr.Prim(_, "signature", []) -> Tsignature
  | Script_repr.Prim(_, "string", []) -> Tstring
  | Script_repr.Prim(_, "pair", [x;y]) -> Ttuple [convert_type x;
                                                  convert_type y]
  | Script_repr.Prim(_, "or", [x;y]) -> Tor (convert_type x,
                                                  convert_type y)
  | Script_repr.Prim(_, "contract", [x;y]) -> Tcontract
                                                (convert_type x,
                                                 convert_type y)
  | Script_repr.Prim(_, "lambda", [x;y]) -> Tlambda
                                                (convert_type x,
                                                 convert_type y)
  | Script_repr.Prim(_, "map", [x;y]) -> Tmap
                                           (convert_type x,
                                            convert_type y)
  | Script_repr.Prim(_, "set", [x]) -> Tset (convert_type x)
  | Script_repr.Prim(_, "list", [x]) -> Tlist (convert_type x)
  | Script_repr.Prim(_, "option", [x]) -> Toption (convert_type x)
  | _ -> unknown_expr "convert_type" expr

let rec convert_code expr =
  match expr with
  | Script_repr.Seq (_loc, exprs) ->
     SEQ (List.map convert_code exprs)
  | Script_repr.Prim(_, "DUP", []) -> DUP 1
  | Script_repr.Prim(_, "DROP", []) -> DROP
  | Script_repr.Prim(_, "DIP", [ arg ]) -> DIP (1, convert_code arg)
  | Script_repr.Prim(_, "CAR", []) -> CAR
  | Script_repr.Prim(_, "CDR", []) -> CDR
  | Script_repr.Prim(_, "SWAP", []) -> SWAP
  | Script_repr.Prim(_, "IF", [x;y]) ->
     IF (convert_code x, convert_code y)
  | Script_repr.Prim(_, "IF_NONE", [x;y]) ->
     IF_NONE (convert_code x, convert_code y)
  | Script_repr.Prim(_, "IF_LEFT", [x;y]) ->
     IF_LEFT (convert_code x, convert_code y)
  | Script_repr.Prim(_, "IF_CONS", [x;y]) ->
     IF_CONS (convert_code x, convert_code y)
  | Script_repr.Prim(_, "NOW", []) -> NOW
  | Script_repr.Prim(_, "PAIR", []) -> PAIR
  | Script_repr.Prim(_, "BALANCE", []) -> BALANCE
  | Script_repr.Prim(_, "SUB", []) -> SUB
  | Script_repr.Prim(_, "ADD", []) -> ADD
  | Script_repr.Prim(_, "MUL", []) -> MUL
  | Script_repr.Prim(_, "NEQ", []) -> NEQ
  | Script_repr.Prim(_, "EQ", []) -> EQ
  | Script_repr.Prim(_, "LT", []) -> LT
  | Script_repr.Prim(_, "LE", []) -> LE
  | Script_repr.Prim(_, "GT", []) -> GT
  | Script_repr.Prim(_, "GE", []) -> GE
  | Script_repr.Prim(_, "GET", []) -> GET
  | Script_repr.Prim(_, "UPDATE", []) -> UPDATE
  | Script_repr.Prim(_, "MEM", []) -> MEM
  | Script_repr.Prim(_, "SOME", []) -> SOME
  | Script_repr.Prim(_, "MANAGER", []) -> MANAGER
  | Script_repr.Prim(_, "SOURCE", [ty1; ty2]) ->
     SOURCE (convert_type ty1, convert_type ty2)
  | Script_repr.Prim(_, "MAP", []) -> MAP
  | Script_repr.Prim(_, "OR", []) -> OR
  | Script_repr.Prim(_, "LAMBDA", [ty1; ty2; expr]) ->
     LAMBDA (convert_type ty1, convert_type ty2, convert_code expr)
  | Script_repr.Prim(_, "REDUCE", []) -> REDUCE
  | Script_repr.Prim(_, "COMPARE", []) -> COMPARE
  | Script_repr.Prim(_, "FAIL", []) -> FAIL
  | Script_repr.Prim(_, "UNIT", []) -> PUSH (Tunit, CUnit)
  | Script_repr.Prim(_, "TRANSFER_TOKENS", []) -> TRANSFER_TOKENS
  | Script_repr.Prim(_, "PUSH", [ ty; cst ]) ->
     PUSH (convert_type ty, convert_const cst)

  | Script_repr.Prim(_, "H", []) -> H
  | Script_repr.Prim(_, "CHECK_SIGNATURE", []) -> CHECK_SIGNATURE
  | Script_repr.Prim(_, "CONCAT", []) -> CONCAT
  | Script_repr.Prim(_, "EDIV", []) -> EDIV
  | Script_repr.Prim(_, "EXEC", []) -> EXEC
  | Script_repr.Prim(_, "MOD", []) -> MOD
  | Script_repr.Prim(_, "DIV", []) -> DIV
  | Script_repr.Prim(_, "AMOUNT", []) -> AMOUNT
  | Script_repr.Prim(_, "EMPTY_MAP", [ty1; ty2]) ->
     PUSH (Tmap (convert_type ty1, convert_type ty2), CMap [])
  | Script_repr.Prim(_, "NONE", [ty]) ->
     PUSH (Toption (convert_type ty), CNone)
  | Script_repr.Prim(_, "LEFT", [ty]) ->
     LEFT (convert_type ty)
  | Script_repr.Prim(_, "CONS", []) -> CONS
  | Script_repr.Prim(_, "LOOP", [loop]) -> LOOP (convert_code loop)
  | Script_repr.Prim(_, "RIGHT", [ty]) ->
     RIGHT (convert_type ty)
  | Script_repr.Prim(_, "INT", []) -> INT
  | Script_repr.Prim(_, "ABS", []) -> ABS

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
