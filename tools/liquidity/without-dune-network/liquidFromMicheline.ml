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

exception Missing_program_field of string

let convert_const_type env string ty =
  failwith "mini version cannot decompile"
let convert_const_notype env string =
  failwith "mini version cannot decompile"
let convert_contract env string =
  failwith "mini version cannot decompile"
let contract_of_string filename string =
  failwith "mini version cannot decompile"
let const_of_string filename string =
  failwith "mini version cannot decompile"
let convert_env _ =
  failwith "mini version cannot decompile"
let infos_env env =
  failwith "mini version cannot decompile"
