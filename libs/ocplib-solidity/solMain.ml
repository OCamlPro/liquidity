(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open SolTypes

let parse_sol_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  SolLexer.reset ();
  Location.init lexbuf filename;
  try
    let c = SolParser.contracts SolLexer.token lexbuf in
    SolLexer.reset ();
    close_in ic;
    c
  with
  | e ->
     SolLexer.dump_history ();
    SolLexer.reset ();
     let loc = Location.curr lexbuf in
     Location.print_loc Format.err_formatter loc;
     Format.fprintf Format.err_formatter
                    "@.Error: syntax error@.";
     close_in ic;
     let lnum = loc.Location.loc_start.Lexing.pos_lnum  in
     let offset = loc.Location.loc_start.Lexing.pos_cnum -
                 loc.Location.loc_start.Lexing.pos_bol  in
     let lnum_min = max (lnum - 2) 1 in
     let lnum_max = lnum + 2 in
     let ic = open_in filename in
     let rec iter i =
       let line = input_line ic in
       if i < lnum_min then
         iter (i+1)
       else begin
           Printf.eprintf "%d%c %s\n" i
                          (if i = lnum then '>' else ' ') line;
           if i = lnum then
             Printf.eprintf "%d--%s^\n" i
                            (String.make offset '-');
           if i < lnum_max then iter (i+1)
         end
     in
     (try iter 1 with End_of_file -> ());
     close_in ic;
     raise e

let sol_file filename =
  Printf.eprintf "Parsing %S\n%!" filename;
  let _c = parse_sol_file filename in
  ()

let () =
  Arg.parse [] sol_file "ocp-solidity FILE.sol"
