(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val block_header_of_rlp : EvmTypes.rlp -> EvmTypes.block_header
val genesis_hash : EvmTypes.block_hash

val transactions_of_body_rlp :
  EvmTypes.rlp -> EvmTypes.rlp list
val transaction_of_rlp : EvmTypes.rlp -> EvmTypes.transaction

val account_state_of_rlp : EvmTypes.rlp -> EvmTypes.account_state
