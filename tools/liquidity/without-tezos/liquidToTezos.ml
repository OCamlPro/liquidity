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

type loc_table = (int * (LiquidTypes.location * string option)) list

let string_of_contract (c : michelson_contract) =
  LiquidPrinter.Michelson.string_of_contract c

let line_of_contract (c : michelson_contract) =
  LiquidPrinter.Michelson.line_of_contract c

let convert_contract ~expand (c : loc_michelson_contract) =
  LiquidEmit.emit_contract ~expand c,
  [] (* TODO : loc_table *)

let string_of_const c =
  LiquidPrinter.Michelson.string_of_const c

let line_of_const c =
  LiquidPrinter.Michelson.line_of_const c

let convert_const ~expand c =
  LiquidEmit.emit_const ~expand c

let json_of_contract (c : michelson_contract) =
  failwith "mini version cannot generate json contract"

let contract_of_json _ =
  failwith "mini version cannot parse json contract"

let contract_of_ezjson _ =
  failwith "mini version cannot parse ezjson contract"

let json_of_const _ =
  failwith "mini version cannot generate json constant"

let const_of_json _ =
  failwith "mini version cannot parse json constant"

let const_of_ezjson _ =
  failwith "mini version cannot parse ezjson constant"

let read_tezos_file (_filename : string) =
  failwith "mini version cannot decompile"

let read_tezos_json (_filename : string) =
  failwith "mini version cannot decompile"

let arg_list work_done = []
