(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module TYPES : sig
  type t =
    Z
  | B of bool
  | F of float
  | S of string
  | L of t list
  | O of (string * t) list
end
open TYPES

val of_string : string -> t
val print : t -> unit

val to_string : ?minify:bool -> t -> string
val to_buffer : ?minify:bool -> Buffer.t -> t -> unit
