(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val initial_env : string -> env
val translate : string -> Parsetree.structure -> syntax_exp contract * env
val read_file : string -> Parsetree.structure

val translate_expression :
  string -> Parsetree.expression -> LiquidTypes.syntax_exp


val structure_of_string : string -> Parsetree.structure
val expression_of_string : string -> Parsetree.expression
