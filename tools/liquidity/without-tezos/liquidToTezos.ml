(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

type loc_table = (int * (LiquidTypes.location * string option)) list

let string_of_contract (c : michelson_contract) =
  LiquidPrinter.Michelson.string_of_contract c

let line_of_contract (c : michelson_contract) =
  LiquidPrinter.Michelson.line_of_contract c

let convert_contract ~expand (c : loc_michelson_contract) =
  LiquidEmit.emit_contract ~expand c,
  [] (* TODO : loc_table *)

let convert_const (c : const) =
  failwith "mini version cannot convert const"

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
