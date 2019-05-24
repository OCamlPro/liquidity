(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

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

val typecheck_const :
  typecheck_env ->
  ?loc:LiquidTypes.location ->
  ?expected_ty:LiquidTypes.datatype ->
  syntax_const ->
  typed_const
