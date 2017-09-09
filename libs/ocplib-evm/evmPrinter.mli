(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val string_of_instruction : EvmTypes.instruction -> string
val instruction_of_string : string -> EvmTypes.instruction

val raise_invalid_instruction_argument : EvmTypes.instruction -> 'a

val string_of_code : EvmTypes.code -> string

val string_of_block_hash : EvmTypes.block_hash -> string
val string_of_block_header : EvmTypes.block_header -> string
val string_of_transaction : EvmTypes.transaction -> string
val string_of_account_state : EvmTypes.account_state -> string

val string_of_rlp : EvmTypes.rlp -> string
