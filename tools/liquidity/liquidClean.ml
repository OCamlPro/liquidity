(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes


let rec clean_code code =
  let ins =
    match code.ins with
    | SEQ expr -> SEQ (clean_seq expr)
    | IF (e1, e2) -> IF (clean_code e1, clean_code e2)
    | IF_NONE (e1, e2) -> IF_NONE (clean_code e1, clean_code e2)
    | IF_LEFT (e1, e2) -> IF_LEFT (clean_code e1, clean_code e2)
    | IF_CONS (e1, e2) -> IF_CONS (clean_code e1, clean_code e2)
    | DIP (n, e) -> DIP (n, clean_code e)
    | LOOP e -> LOOP (clean_code e)
    | ITER e -> ITER (clean_code e)
    | LAMBDA (arg_type, res_type, e) ->
      LAMBDA (arg_type, res_type, clean_code e)
    | ins -> ins
  in
  match ins with
  | DIP (_, {ins=SEQ [{ins=FAILWITH}]}) | DIP (_, {ins=FAILWITH})
  | LOOP ({ins=SEQ [{ins=FAILWITH}]}) | LOOP {ins=FAILWITH}
    -> { code with ins = FAILWITH}
  | _ -> code

and clean_seq exprs =
  match exprs with
  | [] -> []
  | e :: exprs ->
    let e = clean_code e in
    match e.ins with
    | FAILWITH -> [e]
    | _ ->
      let exprs =  clean_seq exprs in
      match e, exprs with
      | _, ({ins=FAILWITH} as fail) :: _ -> [fail]
      | _ -> e :: exprs

let clean_contract contract =
  { contract with mic_code = clean_code contract.mic_code }
