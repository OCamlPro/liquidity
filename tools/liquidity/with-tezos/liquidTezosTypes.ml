(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)
open LiquidTypes

type contract = string Micheline.canonical list

type env = {
  filename : string;
  loc_table : location IntMap.t;
  mutable annoted : bool;
}

type hash = Hash.Operation_hash.t
