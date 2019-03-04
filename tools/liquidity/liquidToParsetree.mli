(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Convert Liquidity code and constants as OCaml AST objects for neat
    pretty printing. *)

open LiquidTypes

val output_version : string

val structure_of_contract :
  ?abbrev:bool ->
  ?type_annots: (datatype, string) Hashtbl.t ->
  ?types: (string * datatype) list ->
  (datatype, 'a) exp contract -> Parsetree.structure
val convert_const : ?abbrev:bool -> (datatype, 'a) exp const -> Parsetree.expression
val convert_code : ?abbrev:bool -> (datatype, 'a) exp -> Parsetree.expression
val convert_type : ?abbrev:bool -> datatype -> Parsetree.core_type
