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

let convert_const_type loc_table string ty =
    failwith "mini version cannot decompile"
let convert_contract loc_table string =
    failwith "mini version cannot decompile"
let contract_of_string filename string =
    failwith "mini version cannot decompile"
let const_of_string filename string =
    failwith "mini version cannot decompile"
