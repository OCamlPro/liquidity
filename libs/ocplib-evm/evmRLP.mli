(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open EvmTypes

val encode : rlp -> string
val decode : string -> rlp
val decode_partial : string -> int -> int -> rlp * int

(* int64 <-> minimal string *)
val encodeInt64 : int64 -> string
val decodeInt64 : string -> int64
val decodeZ : string -> Z.t
