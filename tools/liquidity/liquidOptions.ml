(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017 - OCamlPro SAS                                   *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


let verbosity =
  ref
    (try int_of_string (Sys.getenv "LIQUID_VERBOSITY")
     with
     | Not_found -> 0
     | _ -> 1 (* LIQUID_DEBUG not a number *)
    )

let peephole = ref true
let keepon = ref false
let typeonly = ref false
let parseonly = ref false
let singleline = ref false
let annotmic = ref true
let annotafter = ref false (* XXX: Disable when Tezos is fixed *)
let json = ref false

let tezos_node = ref "127.0.0.1:8732"

let source = ref (None : string option)
let amount = ref "0"
let fee = ref "50000"
let delegatable = ref false
let spendable = ref false
let gas_limit = ref "400000"
let storage_limit = ref "60000"

let private_key = ref (None : string option)

let main = ref (None : string option)
let output = ref (None : string option)
