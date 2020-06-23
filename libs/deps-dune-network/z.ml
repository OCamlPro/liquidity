(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

type t = Num.num

let zero = Num.num_of_int 0

let of_int = Num.num_of_int
let to_int = Num.int_of_num
let of_int64 x = Num.num_of_big_int (Big_int.big_int_of_int64 x)
let to_int64 x = Big_int.int64_of_big_int (Num.big_int_of_num x)
let add = Num.add_num
let mul = Num.mult_num
let pred = Num.pred_num

let div_rem a b =
  Num.quo_num a b, Num.mod_num a b

let of_bits b =
  let n = ref (Num.num_of_int 0) in
  let p = ref (Num.num_of_int 1) in
  let f = Num.num_of_int 256 in
  for i = 0 to String.length b - 1 do
    let c = Char.code (String.get b i) in
    n := Num.add_num !n (Num.mult_num !p (Num.num_of_int c));
    p := Num.mult_num !p f;
  done;
  !n

let to_bits n =
  let n = ref (Num.abs_num n) in
  let l = ref [] in
  let f = Num.num_of_int 256 in
  while Num.gt_num !n zero do
    let d, r = div_rem !n f in
    l := Char.unsafe_chr (to_int r) :: !l;
    n := d;
  done;
  let b = Bytes.create (List.length !l) in
  List.iteri (fun i c ->
      Bytes.set b i c
    ) (List.rev !l);
  Bytes.unsafe_to_string b

let compare = Num.compare_num

let numbits x = assert false

let sign = Num.sign_num

let neg = Num.minus_num

let equal = Num.eq_num

let extract _ _ _  =  assert false

let abs = Num.abs_num

let to_string = Num.string_of_num

let of_string = Num.num_of_string

let div x y = fst (div_rem x y)

let sub x y = Num.sub_num x y
