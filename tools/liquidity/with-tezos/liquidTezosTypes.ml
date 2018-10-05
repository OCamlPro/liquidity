(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)
open LiquidTypes
open Michelson_Tezos

type expr = string Micheline.canonical
type contract = expr list

type env = {
  filename : string;
  loc_table : location IntMap.t;
  type_annots : (datatype, string) Hashtbl.t;
  mutable types : (string * datatype) list;
  mutable contract_types : (string * contract_sig) list;
  mutable annoted : bool;
}

let empty_env filename = {
  filename;
  loc_table = IntMap.empty;
  type_annots = Hashtbl.create 17;
  types = [];
  contract_types = ["UnitContract", unit_contract_sig];
  annoted = false;
}

type json = Data_encoding.json
