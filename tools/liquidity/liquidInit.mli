(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

(** Initializer *)
type init =
  | Init_constant of LiquidTypes.encoded_const (** constant initializer*)
  | Init_code of (LiquidTypes.encoded_contract *
                  LiquidTypes.loc_michelson_contract)
  (** initializer compiled to michelson contract (to be evaluated by
      node) *)

val compile_liquid_init :
  env -> full_contract_sig -> encoded_exp LiquidTypes.init -> init
