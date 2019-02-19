(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Reason_toolchain

let wrap f lexbuf =
  try
    f lexbuf
  with
  | Reason_syntax_util.Error (loc, _)
  | Reason_lexer.Error(_, loc) ->
    raise (Syntaxerr.Error(Syntaxerr.Other loc))
  | e -> raise e

let implementation lexbuf =
  let str, comments = RE.implementation_with_comments lexbuf in
  let comments = List.map (fun {Reason_comment.text; location; _ } ->
      (text, location)
    ) comments in
  To_current.copy_structure str, comments

let core_type lexbuf =
  To_current.copy_core_type (RE.core_type lexbuf)

let expression lexbuf =
  To_current.copy_expression (RE.expression lexbuf)


let implementation = wrap implementation
let core_type = wrap core_type
let expression = wrap expression
