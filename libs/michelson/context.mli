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

(** View over the context store, restricted to types, access and
    functional manipulation of an existing context. *)

open Hash

type t
type context = t
type key = string list
type value = MBytes.t

val dir_mem: context -> key -> bool Lwt.t
val mem: context -> key -> bool Lwt.t
val get: context -> key -> value option Lwt.t
val set: context -> key -> value -> t Lwt.t
val del: context -> key -> t Lwt.t
val remove_rec: context -> key -> t Lwt.t

val fold:
  context -> key -> init:'a ->
  f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
  'a Lwt.t
val keys: context -> key -> key list Lwt.t
val fold_keys:
  context -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t


val register_resolver:
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit
