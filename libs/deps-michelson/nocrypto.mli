(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Hash : sig

  val digest :
    [< `SHA256 | `SHA3_KEC | `SHA3_KEC512 ] -> string -> string

end

type blake2b_ctx
val blake2b_init : ?size:int -> unit -> blake2b_ctx * string
val blake2b_update : blake2b_ctx * string -> string -> unit
val blake2b_final : blake2b_ctx * string -> string
val blake2b : string -> string
