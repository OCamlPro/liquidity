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

(** Convert Liquidity code and constants as OCaml AST objects for neat
    pretty printing. *)

open LiquidTypes

val output_version : string

val structure_of_contract :
  ?abbrev:bool ->
  ?type_annots: (datatype, string) Hashtbl.t ->
  ?types: (string * datatype) list ->
  (datatype, 'a) exp contract -> Parsetree.structure
val convert_const : ?abbrev:bool -> (datatype, 'a) exp const -> Parsetree.expression
val convert_code : ?abbrev:bool -> (datatype, 'a) exp -> Parsetree.expression
val convert_type : ?abbrev:bool -> datatype -> Parsetree.core_type
