(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, INRIA & OCamlPro SAS <fabrice@lefessant.net>    *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type create_flag = VERIFY | SIGN
type serial_flag = EC_COMPRESSED | EC_UNCOMPRESSED

module Context : sig
  type t

  val create : create_flag list -> t
end

type seckey = string

module Pubkey : sig
  type t
  val parse : Context.t -> string(*[65]*) -> t option
  val serialize : Context.t -> t -> serial_flag list -> string
end

module Signature : sig
  type t

  val parse_compact : Context.t -> string(*[64]*) -> t option
  val parse_der : Context.t -> string -> t option
  val serialize_compact : Context.t -> t -> string(*[64]*)
  val serialize_der : Context.t -> t -> string

end

val verify : Context.t -> Signature.t -> string(*[32]*) -> Pubkey.t -> bool
val sign : Context.t -> string(*[32]*) -> seckey -> Signature.t

module ECDSA_recoverable :
  sig

    type signature

    val signature_parse_compact :
      Context.t -> string(*[64]*) -> int -> signature option

    val signature_serialize_compact :
      Context.t -> signature -> string(*[64]*) * int

    val signature_convert :
      Context.t -> signature -> Signature.t

    val sign :
      Context.t -> string(*[32]*) -> seckey -> signature option

    val recover :
      Context.t -> signature -> string(*[32]*) -> Pubkey.t option

  end
