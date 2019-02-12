(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val implementation :
  Lexing.lexbuf -> Parsetree.structure * (string * Location.t) list
val expression : Lexing.lexbuf -> Parsetree.expression
val core_type :  Lexing.lexbuf -> Parsetree.core_type
