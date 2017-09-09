(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type db_key = string
  type trie_key = string
  type trie_value = string

  val iter :
    LevelDB.db
    -> db_key
    -> (trie_key -> trie_value -> unit)
    -> unit
  val lookup :
    LevelDB.db
    -> db_key
    -> trie_key
    -> trie_value option
