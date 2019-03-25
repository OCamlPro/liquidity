(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Parsing from OCaml AST to Liquidity AST *)

open LiquidTypes

val initial_env : string -> env
val mk_inner_env : env -> string -> env

val translate :
  filename:string -> Parsetree.structure -> syntax_contract

val translate_multi :
  (string * Parsetree.structure) list -> syntax_contract

val read_file : string -> Parsetree.structure

val translate_expression : env -> Parsetree.expression -> syntax_exp

val translate_type : env -> Parsetree.core_type -> datatype

val structure_of_string : ?filename:string -> string -> Parsetree.structure

val expression_of_string : ?filename:string -> string -> Parsetree.expression

val type_of_string : ?filename:string -> string -> Parsetree.core_type
