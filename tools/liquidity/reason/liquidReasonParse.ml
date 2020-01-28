(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

module Parser = Reason_parser
module Lexer = Reason_lexer

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

let liquidity_keywords = Parser.[
    "and", AND;
    "as", AS;
    "assert", ASSERT;
    "begin", BEGIN;
    "class", CLASS;
    "constraint", CONSTRAINT;
    "do", DO;
    "done", DONE;
    "downto", DOWNTO;
    "else", ELSE;
    "end", END;
    "exception", EXCEPTION;
    "external", EXTERNAL;
    "false", FALSE;
    "for", FOR;
    "fun", FUN;
    "esfun", ES6_FUN;
    "function", FUNCTION;
    "functor", FUNCTOR;
    "if", IF;
    "in", IN;
    "include", INCLUDE;
    "inherit", INHERIT;
    "initializer", INITIALIZER;
    "lazy", LAZY;
    "let", LET;
    "switch", SWITCH;
    "contract", MODULE; (* LIQUIDITY *)
    "pub", PUB;
    "mutable", MUTABLE;
    "new", NEW;
    "nonrec", NONREC;
    "object", OBJECT;
    "of", OF;
    "open", OPEN;
    "or", OR;
    (*  "parser", PARSER; *)
    "pri", PRI;
    "rec", REC;
    "sig", SIG;
    "struct", STRUCT;
    "then", THEN;
    "to", TO;
    "true", TRUE;
    "try", TRY;
    "type", TYPE;
    "val", VAL;
    "virtual", VIRTUAL;
    "when", WHEN;
    "while", WHILE;
    "with", WITH;

    "mod", INFIXOP3("mod");
    "land", INFIXOP3("land");
    "lor", INFIXOP3("lor");
    "lxor", INFIXOP3("lxor");
    "lsl", INFIXOP4("lsl");
    "lsr", INFIXOP4("lsr");
    "asr", INFIXOP4("asr")
  ]

let () =
  Lexer.define_keywords liquidity_keywords

let stage_module_replace = ref 0
let prev_percent = ref false

(* replace "module" with "%module contract" *)
let preprocess token lexbuf =
  let open Parser in
  match !stage_module_replace with
  | 0 ->
    begin
      match token lexbuf with
      | LIDENT "module" when not !prev_percent ->
        stage_module_replace := 1;
        PERCENT
      | EOL ->
        (* preprocessor in reason lexer is given EOLs also *)
        token lexbuf
      | PERCENT ->
        prev_percent := true;
        PERCENT
      | t ->
        prev_percent := false;
        t
    end
  | 1 ->
    stage_module_replace := 2;
    LIDENT "module"
  | 2 ->
    stage_module_replace := 0;
    prev_percent := false;
    MODULE
  | _ -> assert false

let () =
  Lexer.set_preprocessor
    (fun () ->
       stage_module_replace := 0;
       prev_percent := false;
    )
    preprocess
