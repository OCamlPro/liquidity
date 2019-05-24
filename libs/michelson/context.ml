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

(* Liquidity: an implementation of contexts with simple maps *)

module StringMap = Map.Make(String)

type key = string list
type value = MBytes.t

type t = {
     value : MBytes.t option;
     map : t StringMap.t;
   }
type context = t

let empty = { value = None; map = StringMap.empty }

let fold = assert false
let keys = assert false
let fold_keys = assert false

let rec get t key =
  match key with
  | [] -> t.value
  | head :: tail ->
     match StringMap.find head t.map with
     | exception Not_found -> None
     | t -> get t tail

let rec set t key v =
  match key with
  | [] -> { t with value = Some v }
  | head :: tail ->
     let node = try
         StringMap.find head t.map
       with Not_found -> empty
     in
     let node = set node tail v in
     let map = StringMap.add head node t.map in
     { t with map }

let rec mem t key =
  match key with
  | [] -> t.value != None
  | head :: tail ->
     match StringMap.find head t.map with
     | exception Not_found -> false
     | t -> mem t tail

(* Whether the directory (even empty) exists *)
let rec dir_mem t key =
  match key with
  | [] -> true
  | head :: tail ->
     match StringMap.find head t.map with
     | exception Not_found -> false
     | t -> dir_mem t tail

let rec del t key =
  match key with
  | [] -> { t with value = None }
  | head :: tail ->
     let node = try
         StringMap.find head t.map
       with Not_found -> empty
     in
     let node = del node tail in
     let map = StringMap.add head node t.map in
     { t with map }

let rec unprefix t key =
  match key with
  | [] -> Some t
  | head :: tail ->
     match StringMap.find head t.map with
     | exception Not_found -> None
     | t -> unprefix t tail

let rec iter (f : Persist.key -> Persist.value -> unit) key t =
  begin match t.value with
  | None -> ()
  | Some v -> f key v
  end;
  StringMap.iter (fun s t -> iter f (key @ [s]) t) t.map

(* List all the keys with prefix within [keys] *)
let list t keys =
  let list = ref ([] : Persist.key list) in
  List.iter (fun key ->
      match unprefix t key with
      | None -> ()
      | Some t ->
         iter (fun k v -> list := (key @ k) :: !list) key t
    ) keys;
  (!list : Persist.key list)

let rec remove_rec t key =
  match key with
  | [] -> empty
  | head :: tail ->
     let node = try
         StringMap.find head t.map
       with Not_found -> empty
     in
     let node = remove_rec node tail in
     let map = StringMap.add head node t.map in
     { t with map }

module Base58 = struct
  include Base58
  include Make(struct type context = t end)
  end

let register_resolver enc f =
  Base58.register_resolver enc (fun s -> f s)
