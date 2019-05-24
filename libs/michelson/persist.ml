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

open Error_monad
open Hash
open Utils
open Tezos_data

       (*
#include "../../tezos/src/node/db/persist.ml"
        *)


(*-- Signatures --------------------------------------------------------------*)

type key = string list
type value = MBytes.t

module type STORE = sig
  type t
  val mem: t -> key -> bool Lwt.t
  val dir_mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val set: t -> key -> value -> t Lwt.t
  val del: t -> key -> t Lwt.t
  val list: t -> key list -> key list Lwt.t
  val remove_rec: t -> key -> t Lwt.t
end

module type KEY = sig
  type t
  val prefix: key
  val length: int
  val to_path: t -> key
  val of_path: key -> t
  val compare: t -> t -> int
end

module type VALUE = sig
  type t
  val of_bytes: value -> t option
  val to_bytes: t -> value
end

(*-- Utils -------------------------------------------------------------------*)

let prefix prf key =
  prf @ key

let unprefix prf key =
  let rec eat = function
    | k :: key, p :: prefix ->
        assert (k = p) ;
        eat (key, prefix)
    | key, [] -> key
    | _ -> assert false in
  eat (key, prf)


module RawValue = struct
  type t = value
  let to_bytes b = b
  let of_bytes b = Some b
end

(*-- Map Builders ------------------------------------------------------------*)

module MakePersistentMap
    (S : STORE) (K : KEY) (C : VALUE) = struct

  let to_path k =
    let suffix = K.to_path k in
    assert (List.length suffix = K.length) ;
    prefix K.prefix suffix

  let of_path p = K.of_path (unprefix K.prefix p)

  let empty =
    MBytes.of_string ""

  let inited_key =
    prefix K.prefix [ "inited" ]

  let mem c k =
    S.mem c (to_path k)

  let get c k =
    S.get c (to_path k) >|= function
    | None -> None
    | Some b -> C.of_bytes b

  let set c k b =
    S.set c inited_key empty >>= fun c ->
    S.set c (to_path k) (C.to_bytes b)

  let del c k =
    S.del c (to_path k)

  let clear c =
    S.remove_rec c K.prefix

  let fold c x ~f =
    let rec dig i root acc =
      if root = inited_key then
        Lwt.return acc
      else if i <= 0 then
        S.get c root >>= function
        | None -> Lwt.return acc
        | Some b ->
            match C.of_bytes b with
            | None -> Lwt.return acc
            | Some v -> f (of_path root) v acc
      else
        S.list c [root] >>= fun roots ->
        Lwt_list.fold_right_s (dig (i - 1)) roots acc in
    S.mem c inited_key >>= function
    | true -> dig K.length K.prefix x
    | false -> Lwt.return x

  let iter c ~f = fold c () ~f:(fun k v () -> f k v)
  let bindings c = fold c [] ~f:(fun k v acc -> Lwt.return ((k, v) :: acc))

end








module MakeHashResolver
    (Store : sig
       type t
       val dir_mem: t -> string list -> bool Lwt.t
       val list: t -> string list list -> string list list Lwt.t
       val prefix: string list
     end)
    (H: HASH) = struct
  let plen = List.length Store.prefix
  let build path =
    H.of_path_exn @@
    Utils.remove_elem_from_list plen path
  let resolve t p =
    let rec loop prefix = function
      | [] ->
          Lwt.return [build prefix]
      | "" :: ds ->
          Store.list t [prefix] >>= fun prefixes ->
          Lwt_list.map_p (fun prefix -> loop prefix ds) prefixes
          >|= List.flatten
      | [d] ->
          Store.list t [prefix] >>= fun prefixes ->
          Lwt_list.filter_map_p (fun prefix ->
              match remove_prefix d (List.hd (List.rev prefix)) with
              | None -> Lwt.return_none
              | Some _ -> Lwt.return (Some (build prefix))
            ) prefixes
      | d :: ds ->
          Store.dir_mem t (prefix @ [d]) >>= function
          | true -> loop (prefix @ [d]) ds
          | false -> Lwt.return_nil in
    loop Store.prefix (H.prefix_path p)
end
