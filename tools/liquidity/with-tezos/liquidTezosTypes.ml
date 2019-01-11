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
