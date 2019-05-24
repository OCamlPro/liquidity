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
