(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Parse/convert Michelson contracts and constants. *)

exception Missing_program_field of string

(** Convert a Micheline contant as a typed Liquidity constant.

    The type is used to recover an actual Liquidity constant, e.g.,
    {[ (0, (1, 2)) ]} with type {[ t = { x:int; y:int; z:int } ]} is
    converted to {[ { x = 0; y = 1; z = 2 } ]}. *)
val convert_const_type :
  LiquidTezosTypes.env ->
  LiquidTezosTypes.expr ->
  LiquidTypes.datatype ->
  LiquidTypes.loc_michelson LiquidTypes.const * LiquidTypes.location

(** Convert a Micheline contant as Liquidity constant. *)
val convert_const_notype :
  LiquidTezosTypes.env ->
  LiquidTezosTypes.expr ->
  LiquidTypes.loc_michelson LiquidTypes.const * LiquidTypes.location

(** Parse a Micheline contract as an intermediate Michelson contract.  *)
val convert_contract :
  LiquidTezosTypes.env ->
  LiquidTezosTypes.contract ->
  LiquidTypes.loc_michelson_contract

(** Parse a string as a Micheline contract. *)
val contract_of_string :
  string -> (* maybe filename *)
  string -> (* content *)
  (LiquidTezosTypes.contract * LiquidTezosTypes.env) option

(** Parse a string as a Micheline constant. *)
val const_of_string :
  string -> (* maybe filename *)
  string -> (* content *)
  (LiquidTezosTypes.expr * LiquidTezosTypes.env) option

val convert_env : LiquidTezosTypes.env -> LiquidTypes.env

(** Extract usefule information from environement for decompiling
    phase. *)
val infos_env :
  LiquidTezosTypes.env ->
  bool (* true if tz annoted *)
  * (LiquidTypes.datatype, string) Hashtbl.t
  * (string * LiquidTypes.datatype) list
