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
open Michelson_Tezos

type expr = string Micheline.canonical
type contract = expr list

type env = {
  filename : string;
  loc_table : location IntMap.t;
  type_annots : (datatype, string) Hashtbl.t;
  mutable types : (datatype list * int) StringMap.t;
  mutable contract_types : (string * contract_sig) list;
  mutable annoted : bool;
  mutable generalize_types : bool;
}

let empty_env filename = {
  filename;
  loc_table = IntMap.empty;
  type_annots = Hashtbl.create 17;
  types = StringMap.empty;
  contract_types = ["UnitContract", unit_contract_sig];
  annoted = false;
  generalize_types = false;
}

type json = Data_encoding.json

let set_generalize_types env b =
  env.generalize_types <- b
