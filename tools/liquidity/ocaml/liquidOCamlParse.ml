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
let implementation buf =
  let str = implementation buf in
  str, comments ()


(* redefined keywords of the modified OCaml lexer *)
let liquidity_keywords = Parser.[
  "and", AND;
  "as", AS;
  "assert", ASSERT;
  "begin", BEGIN;
  (*    "class", CLASS; *)
  (*    "constraint", CONSTRAINT; *)
  "do", DO;
  "done", DONE;
  "downto", DOWNTO;
  "else", ELSE;
  "end", END;
  (* "exception", EXCEPTION; *)
  "external", EXTERNAL;
  "false", FALSE;
  "for", FOR;
  "fun", FUN;
  "function", FUNCTION;
  (* "functor", FUNCTOR; *)
  "if", IF;
  "in", IN;
  (* "include", INCLUDE; *)
  (* "inherit", INHERIT; *)
  (* "initializer", INITIALIZER; *)
  (* "lazy", LAZY; *)
  "let", LET;
  "match", MATCH;
  (* "entry", METHOD; *)
  "contract", MODULE;
  (* "mutable", MUTABLE; *)
  (* "new", NEW; *)
  (* "nonrec", NONREC; *)
  (* "object", OBJECT; *)
  "of", OF;
  (* "open", OPEN; *)
  "or", OR;
  (*  "parser", PARSER; *)
  (* "private", PRIVATE; *)
  "rec", REC;
  "sig", SIG;
  "struct", STRUCT;
  "then", THEN;
  "to", TO;
  "true", TRUE;
  (* "try", TRY; *)
  "type", TYPE;
  "val", VAL;
  (* "virtual", VIRTUAL; *)

  (* "when", WHEN; *)
  "while", WHILE;
  "with", WITH;

  "lor", INFIXOP3("lor"); (* Should be INFIXOP2 *)
  "lxor", INFIXOP3("lxor"); (* Should be INFIXOP2 *)
  "mod", INFIXOP3("mod");
  "land", INFIXOP3("land");
  "lsl", INFIXOP4("lsl");
  "lsr", INFIXOP4("lsr");
  "xor", INFIXOP3("xor"); (* Should be INFIXOP2 *)
  "asr", INFIXOP4("asr")
]

let () =
  Lexer.define_keywords liquidity_keywords

let stage_module_replace = ref 0

(* replace "module" with "contract%module" *)
let preprocess token lexbuf =
  let open Parser in
  match !stage_module_replace with
  | 0 ->
    begin
      match token lexbuf with
      | LIDENT "module" ->
        stage_module_replace := 1;
        MODULE
      | t -> t
    end
  | 1 ->
    stage_module_replace := 2;
    PERCENT
  | 2 ->
    stage_module_replace := 0;
    LIDENT "module"
  | _ -> assert false

let () =
  Lexer.set_preprocessor
    (fun () -> stage_module_replace := 0)
    preprocess
