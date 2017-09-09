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

include Persist.STORE

val register_resolver:
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit

                                                            (* for Liquidity
val complete: t -> string -> string list Lwt.t
                                                             *)

val empty : t
