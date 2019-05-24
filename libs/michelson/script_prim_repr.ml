(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

         (*

(*
   If you want to add or remove a primitive, you need to modify:
   * the type specific to the primitive
   * "of_string"
   * "to_string"
   * the lists of instructions
 *)


module TYPES = struct
  type namespace = Type_namespace | Constant_namespace | Instr_namespace

  type typ_prim =
    | T_UNKNOWN of string
    | T_unit
    | T_nat
    | T_int
    | T_string
    | T_tez
    | T_bool
    | T_key
    | T_timestamp
    | T_signature
    | T_contract
    | T_pair
    | T_or
    | T_lambda
    | T_option
    | T_list
    | T_set
    | T_map

  type cst_prim =
    | C_UNKNOWN of string
    | C_Unit
    | C_True
    | C_False
    | C_Pair
    | C_Left
    | C_Right
    | C_None
    | C_Some
    | C_List
    | C_Set
    | C_Item
    | C_Map

  type ins_prim =
    | I_UNKNOWN of string
    | I_DROP
    | I_DUP
    | I_SWAP
    | I_SOME
    | I_UNIT
    | I_PAIR
    | I_CAR
    | I_CDR
    | I_CONS
    | I_MEM
    | I_UPDATE
    | I_MAP
    | I_REDUCE
    | I_GET
    | I_EXEC
    | I_FAIL
    | I_NOP
    | I_CONCAT
    | I_ADD
    | I_SUB
    | I_MUL
    | I_EDIV
    | I_OR
    | I_AND
    | I_XOR
    | I_NOT
    | I_ABS
    | I_INT
    | I_NEG
    | I_LSL
    | I_LSR
    | I_COMPARE
    | I_EQ
    | I_NEQ
    | I_LT
    | I_GT
    | I_LE
    | I_GE
    | I_MANAGER
    | I_TRANSFER_TOKENS
    | I_CREATE_ACCOUNT
    | I_CREATE_CONTRACT
    | I_NOW
    | I_AMOUNT
    | I_BALANCE
    | I_CHECK_SIGNATURE
    | I_H
    | I_STEPS_TO_QUOTA
    | I_PUSH
    | I_NONE
    | I_LEFT
    | I_RIGHT
    | I_NIL
    | I_EMPTY_SET
    | I_DIP
    | I_LOOP
    | I_IF_NONE
    | I_IF_LEFT
    | I_IF_CONS
    | I_EMPTY_MAP
    | I_IF
    | I_SOURCE
    | I_LAMBDA

  type t =
    | Ptyp of typ_prim
    | Pcst of cst_prim
    | Pins of ins_prim
    | Punk of string

  type primitive = t
end

open TYPES

let from_namespace name =
  let is_lower = function  '_' | 'a'..'z' -> true | _ -> false in
  let is_upper = function  '_' | 'A'..'Z' -> true | _ -> false in
  let rec for_all a b f =
    Compare.Int.(a > b) || f a && for_all (a + 1) b f in
  let len = String.length name in
  if Compare.Int.(len = 0)
  || Compare.Char.(String.get name 0 = '_') then
    Punk name
  else if is_upper (String.get name 0)
  && for_all 1 (len - 1) (fun i -> is_upper (String.get name i)) then
    Pins (I_UNKNOWN name)
  else if is_upper (String.get name 0)
       && for_all 1 (len - 1) (fun i -> is_lower (String.get name i)) then
    Pcst (C_UNKNOWN name)
  else if is_lower (String.get name 0)
       && for_all 1 (len - 1) (fun i -> is_lower (String.get name i)) then
    Ptyp (T_UNKNOWN name)
  else
    Punk name


let namespace = function
  | Ptyp _ -> Some Type_namespace
  | Pins _ -> Some Instr_namespace
  | Pcst _ -> Some Constant_namespace
  | Punk _ -> None

let to_string = function
  | Punk s -> s
  | Pins ins_prim ->
     begin
       match ins_prim with
       | I_DROP -> "DROP"
       | I_DUP -> "DUP"
       | I_SWAP -> "SWAP"
       | I_SOME -> "SOME"
       | I_UNIT -> "UNIT"
       | I_PAIR -> "PAIR"
       | I_CAR -> "CAR"
       | I_CDR -> "CDR"
       | I_CONS -> "CONS"
       | I_MEM -> "MEM"
       | I_UPDATE -> "UPDATE"
       | I_MAP -> "MAP"
       | I_REDUCE -> "REDUCE"
       | I_GET -> "GET"
       | I_EXEC -> "EXEC"
       | I_FAIL -> "FAIL"
       | I_NOP -> "NOP"
       | I_CONCAT -> "CONCAT"
       | I_ADD -> "ADD"
       | I_SUB -> "SUB"
       | I_MUL -> "MUL"
       | I_EDIV -> "EDIV"
       | I_OR -> "OR"
       | I_AND -> "AND"
       | I_XOR -> "XOR"
       | I_NOT -> "NOT"
       | I_ABS -> "ABS"
       | I_INT -> "INT"
       | I_NEG -> "NEG"
       | I_LSL -> "LSL"
       | I_LSR -> "LSR"
       | I_COMPARE -> "COMPARE"
       | I_EQ -> "EQ"
       | I_NEQ -> "NEQ"
       | I_LT -> "LT"
       | I_GT -> "GT"
       | I_LE -> "LE"
       | I_GE -> "GE"
       | I_MANAGER -> "MANAGER"
       | I_TRANSFER_TOKENS -> "TRANSFER_TOKENS"
       | I_CREATE_ACCOUNT -> "CREATE_ACCOUNT"
       | I_CREATE_CONTRACT -> "CREATE_CONTRACT"
       | I_NOW -> "NOW"
       | I_AMOUNT -> "AMOUNT"
       | I_BALANCE -> "BALANCE"
       | I_CHECK_SIGNATURE -> "CHECK_SIGNATURE"
       | I_H -> "H"
       | I_STEPS_TO_QUOTA -> "STEPS_TO_QUOTA"
       | I_PUSH -> "PUSH"
       | I_NONE -> "NONE"
       | I_LEFT -> "LEFT"
       | I_RIGHT -> "RIGHT"
       | I_NIL -> "NIL"
       | I_EMPTY_SET -> "EMPTY_SET"
       | I_DIP -> "DIP"
       | I_LOOP -> "LOOP"
       | I_IF_NONE -> "IF_NONE"
       | I_IF_LEFT -> "IF_LEFT"
       | I_IF_CONS -> "IF_CONS"
       | I_EMPTY_MAP -> "EMPTY_MAP"
       | I_IF -> "IF"
       | I_SOURCE -> "SOURCE"
       | I_LAMBDA -> "LAMBDA"
       | I_UNKNOWN s -> s
     end
  | Pcst cst_prim ->
     begin
       match cst_prim with
       | C_UNKNOWN s -> s
       | C_Unit -> "Unit"
       | C_True -> "True"
       | C_False -> "False"
       | C_Pair -> "Pair"
       | C_Left -> "Left"
       | C_Right -> "Right"
       | C_None -> "None"
       | C_Some -> "Some"
       | C_List -> "List"
       | C_Set -> "Set"
       | C_Item -> "Item"
       | C_Map -> "Map"

     end
  | Ptyp typ_prim ->
     begin
       match typ_prim with
       | T_UNKNOWN s -> s
       | T_unit -> "unit"
       | T_nat -> "nat"
       | T_int -> "int"
       | T_string -> "string"
       | T_tez -> "tez"
       | T_bool -> "bool"
       | T_key -> "key"
       | T_timestamp -> "timestamp"
       | T_signature -> "signature"
       | T_contract -> "contract"
       | T_pair -> "pair"
       | T_or -> "or"
       | T_lambda -> "lambda"
       | T_option -> "option"
       | T_list -> "list"
       | T_set -> "set"
       | T_map -> "map"
     end

let cst_prims = [
    C_Unit;
    C_True;
    C_False;
    C_Pair;
    C_Left;
    C_Right;
    C_None;
    C_Some;
    C_List;
    C_Set;
    C_Item;
    C_Map;
  ]

let comparable_prims = [
    T_int ; T_nat ;
    T_string ; T_tez ; T_bool ;
    T_key ; T_timestamp
  ]

let uncomparable_prims = [
    T_pair ; T_or ; T_set ; T_map ;
    T_list ; T_option  ; T_lambda ;
    T_unit ; T_signature  ; T_contract ;
  ]

let typ_prims = comparable_prims @ uncomparable_prims
let ins_prims = [
      I_DROP ; I_DUP ; I_SWAP ; I_SOME ; I_UNIT ;
      I_PAIR ; I_CAR ; I_CDR ; I_CONS ;
      I_MEM ; I_UPDATE ; I_MAP ; I_REDUCE ;
      I_GET ; I_EXEC ; I_FAIL ; I_NOP ;
      I_CONCAT ; I_ADD ; I_SUB ;
      I_MUL ; I_EDIV ; I_OR ; I_AND ; I_XOR ;
      I_NOT ;
      I_ABS ; I_INT; I_NEG ; I_LSL ; I_LSR ;
      I_COMPARE ; I_EQ ; I_NEQ ;
      I_LT ; I_GT ; I_LE ; I_GE ;
      I_MANAGER ; I_TRANSFER_TOKENS ; I_CREATE_ACCOUNT ;
      I_CREATE_CONTRACT ; I_NOW ; I_AMOUNT ; I_BALANCE ;
      I_CHECK_SIGNATURE ; I_H ; I_STEPS_TO_QUOTA ;
      I_PUSH ; I_NONE ; I_LEFT ; I_RIGHT ; I_NIL ;
      I_EMPTY_SET ; I_DIP ; I_LOOP ;
      I_IF_NONE ; I_IF_LEFT ; I_IF_CONS ;
      I_EMPTY_MAP ; I_IF ; I_SOURCE ; I_LAMBDA
  ]

let of_string = function
  | "DROP" -> Pins I_DROP
  | "DUP" -> Pins I_DUP
  | "SWAP" -> Pins I_SWAP
  | "SOME" -> Pins I_SOME
  | "UNIT" -> Pins I_UNIT
  | "PAIR" -> Pins I_PAIR
  | "CAR" -> Pins I_CAR
  | "CDR" -> Pins I_CDR
  | "CONS" -> Pins I_CONS
  | "MEM" -> Pins I_MEM
  | "UPDATE" -> Pins I_UPDATE
  | "MAP" -> Pins I_MAP
  | "REDUCE" -> Pins I_REDUCE
  | "GET" -> Pins I_GET
  | "EXEC" -> Pins I_EXEC
  | "FAIL" -> Pins I_FAIL
  | "NOP" -> Pins I_NOP
  | "CONCAT" -> Pins I_CONCAT
  | "ADD" -> Pins I_ADD
  | "SUB" -> Pins I_SUB
  | "MUL" -> Pins I_MUL
  | "EDIV" -> Pins I_EDIV
  | "OR" -> Pins I_OR
  | "AND" -> Pins I_AND
  | "XOR" -> Pins I_XOR
  | "NOT" -> Pins I_NOT
  | "ABS" -> Pins I_ABS
  | "INT" -> Pins I_INT
  | "NEG" -> Pins I_NEG
  | "LSL" -> Pins I_LSL
  | "LSR" -> Pins I_LSR
  | "COMPARE" -> Pins I_COMPARE
  | "EQ" -> Pins I_EQ
  | "NEQ" -> Pins I_NEQ
  | "LT" -> Pins I_LT
  | "GT" -> Pins I_GT
  | "LE" -> Pins I_LE
  | "GE" -> Pins I_GE
  | "MANAGER" -> Pins I_MANAGER
  | "TRANSFER_TOKENS" -> Pins I_TRANSFER_TOKENS
  | "CREATE_ACCOUNT" -> Pins I_CREATE_ACCOUNT
  | "CREATE_CONTRACT" -> Pins I_CREATE_CONTRACT
  | "NOW" -> Pins I_NOW
  | "AMOUNT" -> Pins I_AMOUNT
  | "BALANCE" -> Pins I_BALANCE
  | "CHECK_SIGNATURE" -> Pins I_CHECK_SIGNATURE
  | "H" -> Pins I_H
  | "STEPS_TO_QUOTA" -> Pins I_STEPS_TO_QUOTA
  | "PUSH" -> Pins I_PUSH
  | "NONE" -> Pins I_NONE
  | "LEFT" -> Pins I_LEFT
  | "RIGHT" -> Pins I_RIGHT
  | "NIL" -> Pins I_NIL
  | "EMPTY_SET" -> Pins I_EMPTY_SET
  | "DIP" -> Pins I_DIP
  | "LOOP" -> Pins I_LOOP
  | "IF_NONE" -> Pins I_IF_NONE
  | "IF_LEFT" -> Pins I_IF_LEFT
  | "IF_CONS" -> Pins I_IF_CONS
  | "EMPTY_MAP" -> Pins I_EMPTY_MAP
  | "IF" -> Pins I_IF
  | "SOURCE" -> Pins I_SOURCE
  | "LAMBDA" -> Pins I_LAMBDA

  | "unit" -> Ptyp T_unit
  | "nat" -> Ptyp T_nat
  | "int" -> Ptyp T_int
  | "string" -> Ptyp T_string
  | "tez" -> Ptyp T_tez
  | "bool" -> Ptyp T_bool
  | "key" -> Ptyp T_key
  | "timestamp" -> Ptyp T_timestamp
  | "signature" -> Ptyp T_signature
  | "contract" -> Ptyp T_contract
  | "pair" -> Ptyp T_pair
  | "or" -> Ptyp T_or
  | "lambda" -> Ptyp T_lambda
  | "option" -> Ptyp T_option
  | "list" -> Ptyp T_list
  | "set" -> Ptyp T_set
  | "map" -> Ptyp T_map

  | "Unit" -> Pcst C_Unit
  | "True" -> Pcst C_True
  | "False" -> Pcst C_False
  | "Pair" -> Pcst C_Pair
  | "Left" -> Pcst C_Left
  | "Right" -> Pcst C_Right
  | "None" -> Pcst C_None
  | "Some" -> Pcst C_Some
  | "List" -> Pcst C_List
  | "Set" -> Pcst C_Set
  | "Item" -> Pcst C_Item
  | "Map" -> Pcst C_Map

  | s -> from_namespace s

(* Check that of_string (to_string p) = p for all primitives *)

module StringSet = Set.Make(String)
external compare_prim : t -> t -> int = "%compare"

let () =
  let set = ref StringSet.empty in
  let check f prims =
    List.iter (fun p ->
        let p = f p in
        let s = to_string p in
        if StringSet.mem s !set then
          failwith ("Script_prim_repr.to_string : "^ s ^ " generated twice");
        set := StringSet.add s !set;
        let p' = of_string s in
        if Compare.Int.(compare_prim p p' <> 0) then
          failwith ("Script_prim_repr.of_string : "^ s ^ " not idempotent");
      ) prims
  in

  check (fun p -> Pcst p) cst_prims;
  check (fun p -> Ptyp p) typ_prims;
  check (fun p -> Pins p) ins_prims;
  ()
          *)
