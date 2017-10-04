(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

val warning_printer : (location -> warning -> unit) ref

val warn : location -> warning -> unit

val report_error : error -> unit

val raise_error :
  ?loc:location -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val loc_in_file : string -> location

val noloc : location
