(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type repr = Z.t


type nat = Natural
type z = Integer
type 't int_val = Int of repr

let compare (Int x) (Int y) = Z.compare x y

let zero = Int Z.zero
let to_string (Int x) = Z.to_string x
let of_string s = Int (Z.of_string s)

let to_int64 (Int x) = try Some (Z.to_int64 x) with _ -> None
let of_int64 n = Int (Z.of_int64 n)

let add (Int x) (Int y) = Int (Z.add x y)
let sub (Int x) (Int y) = Int (Z.sub x y)
let mul (Int x) (Int y) = Int (Z.mul x y)
let ediv (Int x) (Int y) =
  try
    let (q,r) = Z.ediv_rem x y in
    Some (Int q,Int r)
  with _ -> None

let abs (Int x) = Int (Z.abs x)
let neg (Int x) = Int (Z.neg x)
let shift_left (Int x) (Int y) = Int (Z.shift_left x (Z.to_int y))
let shift_right (Int x) (Int y) = Int (Z.shift_right x (Z.to_int y))

let logor (Int x) (Int y) = Int (Z.logor x y)
let logxor (Int x) (Int y) = Int (Z.logxor x y)
let logand (Int x) (Int y) = Int (Z.logand x y)
let lognot (Int x) = Int (Z.lognot x)

let int (Int x) = (Int x)
