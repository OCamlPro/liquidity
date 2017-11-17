(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let string_of_contract (c : michelson_exp contract) =
  LiquidPrinter.Michelson.string_of_contract c

let line_of_contract (c : michelson_exp contract) =
  LiquidPrinter.Michelson.line_of_contract c

let convert_contract (c : noloc_michelson contract) =
  LiquidEmit.emit_contract c

let read_tezos_file (_filename : string) = assert false

let arg_list work_done = []
