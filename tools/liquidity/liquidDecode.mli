(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2018       .                                          *)
(*    OCamlPro SAS <contract@ocamlpro.com>                                *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

(** Decode a contract. Recover Liquidity constructs from their encoded
    version (with {!LiquidEncode.encode_contract} ). *)
val decode_contract : encoded_contract -> typed_contract

val decode_const : encoded_const -> typed_const
