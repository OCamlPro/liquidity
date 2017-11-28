(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let peephole = ref true
let keepon = ref false
let typeonly = ref false
let parseonly = ref false
let singleline = ref false
let annotmic = ref true
let annotafter = ref true (* XXX: Disable when Tezos is fixed *)
