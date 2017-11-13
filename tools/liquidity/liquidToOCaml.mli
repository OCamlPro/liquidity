(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val output_version : string

val structure_of_contract : syntax_exp contract -> Parsetree.structure
val string_of_structure : Parsetree.structure -> string

val translate_expression : syntax_exp -> Parsetree.expression
val string_of_expression : Parsetree.expression -> string
