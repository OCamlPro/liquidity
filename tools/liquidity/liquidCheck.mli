(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

(** Raise a typecheching error *)
val error :
  location ->
  ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a

(** Typecheck a contract, returna a contract with type information.
    @param warnings flag to indicate if warnings whould be produced
    @param decompiling flag to indicate if we are typechecking an AST
    constructed by the decompiler, in this case typing is more
    permissive *)
val typecheck_contract :
  warnings:bool -> decompiling:bool -> syntax_contract -> typed_contract

(** Typecheck a single entry point *)
val typecheck_entry :
  typecheck_env -> syntax_exp entry -> typed_exp entry

(** Typecheck an expression. If the paramater [expected_ty] is
    present, fails if the type of the expression is not the expected
    one. *)
val typecheck_code :
  typecheck_env ->
  ?expected_ty:LiquidTypes.datatype ->
  syntax_exp ->
  typed_exp
