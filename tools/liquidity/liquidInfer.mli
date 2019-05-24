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

val mk_tvar : string -> datatype
val fresh_tvar : unit -> datatype
val wrap_tvar : datatype -> datatype
val has_tvar : datatype -> bool
val make_subst : string list -> datatype list -> (string * datatype) list

val instantiate_to : (string * datatype) list -> datatype -> datatype
val instantiate : StringSet.t * datatype -> datatype

val unify : location -> datatype -> datatype -> unit
val generalize : datatype -> datatype -> unit

val find_variant_type :
  loc:location -> env ->
  (pattern * 'a) list -> datatype option

val make_type_eqn :
  location -> (datatype list * datatype) list -> datatype list -> datatype

val mono_contract : typed_contract -> typed_contract
