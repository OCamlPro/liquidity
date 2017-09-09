(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module TYPES : sig
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

val to_string : TYPES.t -> string

(* of_string never fails: instead, when it does not recognize a primitive,
  it will check its namespace, and return one of P_UNKNOWN, I_UNKNOWN
  or C_UNKNOWN, or Punk if no namespace matched. *)
val of_string : string -> TYPES.t

val namespace : TYPES.t -> TYPES.namespace option

(* lists of primitives *)
val comparable_prims : TYPES.typ_prim list
val typ_prims : TYPES.typ_prim list
val cst_prims : TYPES.cst_prim list
val ins_prims : TYPES.ins_prim list
