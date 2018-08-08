(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
