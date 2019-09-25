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

(** Convert Michelson contracts/constants to Micheline
    contracts/constants for pretty-printing or use by the client. *)

type loc_table = (int * (LiquidTypes.location * string option)) list

(** Convert a Michelson contract to Micheline one. Also returns a
    table mapping Micheling locations to Liquidity source
    locations. This table is used to produce localized error messages
    and to localize errors returned by the Dune node. *)
val convert_contract :
  expand:bool -> LiquidTypes.loc_michelson_contract ->
  LiquidMichelineTypes.contract * loc_table

(** Convert a Michelson constant to Micheline one *)
val convert_const :
  expand:bool -> LiquidTypes.loc_michelson LiquidTypes.const -> LiquidMichelineTypes.expr

(** {2 Pretty printing Micheline } *)

(** {3 Pretty printing Micheline to string } *)

val line_of_contract : LiquidMichelineTypes.contract -> string
val string_of_contract : LiquidMichelineTypes.contract -> string
val json_of_contract : LiquidMichelineTypes.contract -> string

(** {3 Pretty printing/parsing Micheline to/from Json } *)

val contract_of_json : string -> LiquidMichelineTypes.contract
val contract_of_ezjson : LiquidMichelineTypes.json -> LiquidMichelineTypes.contract

val line_of_const : LiquidMichelineTypes.expr -> string
val string_of_const : LiquidMichelineTypes.expr -> string
val json_of_const : LiquidMichelineTypes.expr -> string

val const_of_json : string -> LiquidMichelineTypes.expr
val const_of_ezjson : LiquidMichelineTypes.json -> LiquidMichelineTypes.expr


val read_micheline_file : string -> LiquidMichelineTypes.contract * LiquidMichelineTypes.env
val read_micheline_json : string -> LiquidMichelineTypes.contract * LiquidMichelineTypes.env

val arg_list : bool ref ->
  (Arg.key * Arg.spec * Arg.doc) list
