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

(* Miscellaneous functions *)

(* Create a list of length n, where the i-th element is f i *)
let list_init n f =
  let rec list_init i n f =
    if i = n then [] else
      (f i) :: (list_init (i+1) n f)
  in
  list_init 0 n f

(* Create a list of length n with x everywhere *)
let list_make n x =
  list_init n (fun _ -> x)

(* Remove the nth element of a list *)
let rec list_remove n list =
  if n > 0 then
    match list with
    | [] -> failwith "list_remove"
    | _ :: tail ->
      list_remove (n-1) tail
  else list

(* Replace a character with another one in a string. Returns a new
   string. *)
let string_replace s c1 c2 =
  let rec look acc i =
    try
      let index = String.index_from s i c1 in
      look (index :: acc) (index + 1)
    with Not_found | Invalid_argument _ -> List.rev acc
  in
  let indexes = look [] 0 in
  if indexes = [] then s
  else
    let b = Bytes.of_string s in
    List.iter (fun i -> Bytes.set b i c2) indexes;
    Bytes.to_string b

let has_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  n >= x && String.sub s 0 x = prefix

let remove_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  if n >= x && String.sub s 0 x = prefix then
    Some (String.sub s x (n - x))
  else
    None
