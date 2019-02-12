(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module OCAML = LiquidOCamlParse
module RE = LiquidReasonParse

let implementation buf =
  if !LiquidOptions.ocaml_syntax then
    OCAML.implementation buf
  else
    RE.implementation buf

let core_type buf =
  if !LiquidOptions.ocaml_syntax then
    OCAML.core_type buf
  else
    RE.core_type buf

let expression buf =
  if !LiquidOptions.ocaml_syntax then
    OCAML.expression buf
  else
    RE.expression buf
