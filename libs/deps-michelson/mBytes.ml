(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = bytes

let create = String.create
let length = String.length

include EndianString.BigEndian

let to_string s = s
let of_string s = s
let blit = String.blit
let blit_from_string = String.blit
let blit_to_bytes = String.blit
let copy = String.copy
let sub = String.sub
let substring = String.sub
