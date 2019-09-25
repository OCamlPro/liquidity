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

let unopt ~default = function
  | None -> default
  | Some x -> x

let map_option ~f = function
  | None -> None
  | Some x -> Some (f x)
let apply_option ~f = function
  | None -> None
  | Some x -> f x

let may_cons xs x = match x with None -> xs | Some x -> x :: xs

let filter_map f l =
  List.rev @@ List.fold_left (fun acc x -> may_cons acc (f x)) [] l

let remove_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  if n >= x && String.sub s 0 x = prefix then
    Some (String.sub s x (n - x))
  else
    None

let common_prefix s1 s2 =
  let last = min (String.length s1) (String.length s2) in
  let rec loop i =
    if last <= i then last
    else if s1.[i] = s2.[i] then
      loop (i+1)
    else
      i in
  loop 0

let has_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  n >= x && String.sub s 0 x = prefix

let rec list_last_exn = function
  | [] -> raise Not_found
  | [x] -> x
  | _ :: xs -> list_last_exn xs

let rec remove_elem_from_list nb = function
  | [] -> []
  | l when nb <= 0 -> l
  | _ :: tl -> remove_elem_from_list (nb - 1) tl

let remove_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  if n >= x && String.sub s 0 x = prefix then
    Some (String.sub s x (n - x))
  else
    None

let split delim ?(limit = max_int) path =
  let l = String.length path in
  let rec do_slashes acc limit i =
    if (i >= l) then
      List.rev acc
    else if (String.get path i = delim) then
      do_slashes acc limit (i + 1)
    else
      do_split acc limit i
  and do_split acc limit i =
    if (limit <= 0) then
      if (i = l) then
        List.rev acc
      else
        List.rev (String.sub path i (l - i) :: acc)
    else
      do_component acc (pred limit) i i
  and do_component acc limit i j =
    if (j >= l) then
      if (i = j) then
        List.rev acc
      else
        List.rev (String.sub path i (j - i) :: acc)
    else if (String.get path j = delim) then
      do_slashes (String.sub path i (j - i) :: acc) limit j
    else
      do_component acc limit i (j + 1) in
  if (limit > 0) then
    do_slashes [] limit 0
  else
    [ path ]

module StringMap = Map.Make (String)
