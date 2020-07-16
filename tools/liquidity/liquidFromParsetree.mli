(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2020 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                             Steven De Oliveira                           *)
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

(** Parsing from OCaml AST to Liquidity AST *)

open LiquidTypes

val initial_env : string -> env
val mk_inner_env : env -> is_module:bool -> string -> env

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
