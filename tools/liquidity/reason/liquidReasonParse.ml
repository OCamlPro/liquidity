(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Reason_toolchain

let implementation lexbuf =
  try
    let str, comments = RE.implementation_with_comments lexbuf in
    let comments = List.map (fun {Reason_comment.text; location; _ } ->
        (text, location)
      ) comments in
    To_current.copy_structure str, comments
  with
  | Reason_syntax_util.Error (loc, Syntax_error err) ->
    raise (Syntaxerr.Error(Syntaxerr.Other loc))

let core_type lexbuf =
  To_current.copy_core_type (RE.core_type lexbuf)

let expression lexbuf =
  To_current.copy_expression (RE.expression lexbuf)
