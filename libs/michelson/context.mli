(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
