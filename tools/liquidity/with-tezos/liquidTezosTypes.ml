(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type contract = string Micheline.canonical list

type loc_table = (int * LiquidTypes.location) list

type hash = Hash.Operation_hash.t

exception ParseError of Error_monad.error
