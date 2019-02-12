(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module OCAML = struct
  module Parser = LiquidOCamlParser
  module Lexer = LiquidOCamlLexer

  (* Entry points in the parser *)

  (* Skip tokens to the end of the phrase *)

  let rec skip_phrase lexbuf =
    try
      match Lexer.token lexbuf with
        Parser.SEMISEMI | Parser.EOF -> ()
      | _ -> skip_phrase lexbuf
    with
    | Lexer.Error (Lexer.Unterminated_comment _, _)
    | Lexer.Error (Lexer.Unterminated_string, _)
    | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
    | Lexer.Error (Lexer.Illegal_character _, _) -> skip_phrase lexbuf
  ;;

  let maybe_skip_phrase lexbuf =
    if Parsing.is_current_lookahead Parser.SEMISEMI
    || Parsing.is_current_lookahead Parser.EOF
    then ()
    else skip_phrase lexbuf

  let wrap parsing_fun lexbuf =
    try
      Docstrings.init ();
      Lexer.init ();
      let ast = parsing_fun Lexer.token lexbuf in
      Parsing.clear_parser();
      Docstrings.warn_bad_docstrings ();
      ast
    with
    | Lexer.Error(Lexer.Illegal_character _, _) as err
      when !Location.input_name = "//toplevel//"->
      skip_phrase lexbuf;
      raise err
    | Syntaxerr.Error _ as err
      when !Location.input_name = "//toplevel//" ->
      maybe_skip_phrase lexbuf;
      raise err
    | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))

  let implementation = wrap Parser.implementation
  and interface = wrap Parser.interface
  and toplevel_phrase = wrap Parser.toplevel_phrase
  and use_file = wrap Parser.use_file
  and core_type = wrap Parser.parse_core_type
  and expression = wrap Parser.parse_expression
  and pattern = wrap Parser.parse_pattern

  let comments () = LiquidOCamlLexer.comments ()
  let implementation_with_comments buf =
    let str = implementation buf in
    str, comments ()
end

module RE = struct

  open Reason_toolchain

  let implementation lexbuf =
    try
      let str, comments = RE.implementation_with_comments lexbuf in
      let comments = List.map (fun {Reason_comment.text; location; _ } ->
          (text, location)
        ) comments in
      To_current.copy_structure str, comments
    with
    | Reason_syntax_util.Error (loc, err) ->
      raise (Syntaxerr.Error(Syntaxerr.Other loc))

  let core_type lexbuf =
    To_current.copy_core_type (RE.core_type lexbuf)


end


let implementation buf =
  if  !LiquidOptions.ocaml_syntax then
    OCAML.implementation_with_comments buf
  else
    RE.implementation buf

let core_type buf =
  if  !LiquidOptions.ocaml_syntax then
    OCAML.core_type buf
  else
    RE.core_type buf

let expression = OCAML.expression
