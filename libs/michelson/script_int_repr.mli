(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


type repr


type nat = Natural
type z = Integer
type 't int_val = Int of repr

val zero : _ int_val
val compare : _ int_val -> _ int_val -> int
val to_string : _ int_val -> string
val of_string : string -> _ int_val

val to_int64 : _ int_val -> int64 option
val of_int64 : int64 -> _ int_val

val add : _ int_val -> _ int_val -> _ int_val
val sub : _ int_val -> _ int_val -> _ int_val
val mul : _ int_val -> _ int_val -> _ int_val
val ediv:  'a int_val -> 'b int_val -> ('c int_val * nat int_val) option

val abs : _ int_val -> nat int_val
val neg : _ int_val -> z int_val

val shift_left : 'a int_val -> nat int_val -> 'c int_val
val shift_right : 'a int_val -> nat int_val -> 'c int_val

val logor : nat int_val -> nat int_val -> nat int_val
val logand : nat int_val -> nat int_val -> nat int_val
val logxor : nat int_val -> nat int_val -> nat int_val
val lognot : _ int_val -> z int_val

val int : _ int_val -> _ int_val
