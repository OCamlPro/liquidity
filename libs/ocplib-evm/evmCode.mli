(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val code_of_instruction : EvmTypes.instruction -> int
val instruction_of_code : int -> EvmTypes.instruction

val disassemble : string -> EvmTypes.code
val assemble : EvmTypes.code -> string
