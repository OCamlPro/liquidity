(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes


let string_of_pre pre =
  LiquidPrinter.Michelson.string_of_code (LiquidEmit.emit_code (SEQ pre))

let rec clean_code code =
  let code =
    match code with
    | SEQ expr -> SEQ (clean_seq expr)
    | IF (e1, e2) -> IF (clean_code e1, clean_code e2)
    | IF_NONE (e1, e2) -> IF_NONE (clean_code e1, clean_code e2)
    | IF_LEFT (e1, e2) -> IF_LEFT (clean_code e1, clean_code e2)
    | IF_CONS (e1, e2) -> IF_CONS (clean_code e1, clean_code e2)
    | DIP (n, e) -> DIP (n, clean_code e)
    | LOOP e -> LOOP (clean_code e)
    | LAMBDA (arg_type, res_type, e) ->
       LAMBDA (arg_type, res_type, clean_code e)
    | _ -> code
  in
  match code with
  | DIP (_, SEQ [FAIL]) | DIP (_, FAIL)
    | LOOP (SEQ [FAIL]) | LOOP FAIL
    -> FAIL
  | _ -> code

and clean_seq exprs =
  match exprs with
  | [] -> []
  | e :: exprs ->
     let e = clean_code e in
     if e = FAIL then [FAIL]
     else
       let exprs =  clean_seq exprs in
       match e, exprs with
       | _, FAIL :: _ -> [FAIL]
       | _ -> e :: exprs

let clean_contract contract =
  { contract with code = clean_code contract.code }
