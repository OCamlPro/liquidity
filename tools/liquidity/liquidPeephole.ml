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


(* let string_of_pre pre = *)
(*   LiquidPrinter.Michelson.string_of_code (LiquidEmit.emit_code (SEQ pre)) *)

(* Try to simplify Michelson with peepholes optims: mostly, move
   DIP_DROPs backwards to decrease the size of the stack. *)

let ii ~loc ins = { ins; loc; loc_name = None }
let lii = ii

let drop ~loc n = ii ~loc (DROP n)

let dip_drop ~loc (a,b)=
  if a = 0 then drop ~loc b else ii ~loc (DIP_DROP(a,b))

let rec simplify_pre ({ ins } as e) =
  { e with
    ins =
      match ins with
      | SEQ expr -> SEQ (simplify_seq expr)
      | IF (e1, e2) -> IF (simplify_pre e1, simplify_pre e2)
      | IF_NONE (e1, e2) -> IF_NONE (simplify_pre e1, simplify_pre e2)
      | IF_LEFT (e1, e2) -> IF_LEFT (simplify_pre e1, simplify_pre e2)
      | IF_CONS (e1, e2) -> IF_CONS (simplify_pre e1, simplify_pre e2)
      | DIP (n, e) -> DIP (n, simplify_pre e)
      | LOOP e -> LOOP (simplify_pre e)
      | ITER e -> ITER (simplify_pre e)
      | MAP e -> MAP (simplify_pre e)
      | LAMBDA (arg_type, res_type, e) ->
        LAMBDA (arg_type, res_type, simplify_pre e)
      | CREATE_CONTRACT c -> CREATE_CONTRACT (simplify c)

      | _ -> ins
  }

and simplify_seq exprs =
  match exprs with
  | [] -> []
  | e :: exprs ->
    let e = simplify_pre e in
    match e.ins with
    | FAILWITH -> [e]
    | SEQ e_exprs -> simplify_seq (e_exprs @ exprs)
    | _ ->
      let exprs = simplify_seq exprs in
      simplify_step e exprs

and simplify_step e exprs =
  let ii = ii ~loc:e.loc in
  match e.ins, exprs with

  | SEQ e, exprs -> simplify_steps e exprs
  | DIP_DROP(n, 0), exprs -> exprs
  | DIP_DROP(0, n), exprs -> simplify_step { e with ins = DROP n } exprs
  | DIP (0, e), exprs -> simplify_step e exprs
  | DROP 0, exprs -> exprs
  | DUP _, {ins=DROP 1} :: exprs -> exprs
  | DUP _, ({ins=DROP n} as e) :: exprs -> { e with ins = DROP (n-1) } :: exprs
  | DUP 1, {ins=SWAP} :: {ins=DROP 1} :: exprs -> exprs
  | SWAP, {ins=SWAP} :: exprs -> exprs
  | SWAP, ({ins= (ADD|MUL|AND|OR|XOR)} as e) :: exprs -> simplify_step e exprs
  | EQ, { ins = NOT } :: exprs -> simplify_step { e with ins = NEQ } exprs
  | NEQ, { ins = NOT } :: exprs -> simplify_step { e with ins = EQ } exprs
  | GT, { ins = NOT }:: exprs -> simplify_step { e with ins = LE } exprs
  | LT, { ins = NOT }:: exprs -> simplify_step { e with ins = GE } exprs
  | LE, { ins = NOT }:: exprs -> simplify_step { e with ins = GT } exprs
  | GE, { ins = NOT }:: exprs -> simplify_step { e with ins = LT } exprs

(* TODO: maybe ?
  | CAR _, { ins = PAIR } :: _ -> simplify_step { e with ins = DUP 1 } exprs
  | CDR _, { ins = PAIR } :: _ -> simplify_step { e with ins = DUP 2 } exprs
*)

  | FAILWITH, _ -> [e]

  | IF(i1,i2), exprs ->
    begin
      match i1.ins, i2.ins, exprs with
      | SEQ ({ins=DROP n1} :: e1), SEQ ({ins=DROP n2} :: e2), exprs
        when n1 = n2 ->
        simplify_stepi ~loc:e.loc (DIP_DROP(1, n1))
          (simplify_stepi ~loc:e.loc (IF ( lii ~loc:i1.loc @@ SEQ e1,
                                           lii ~loc:i2.loc @@ SEQ e2 )) exprs)

      | SEQ ({ ins=DIP_DROP(n,m)} :: e1),
        SEQ ({ ins=DIP_DROP(n',m')} :: e2),
        exprs when n=n'
        ->
        let min_m = min m m' in
        simplify_stepi ~loc:e.loc (DIP_DROP(n+1,min_m))
          (simplify_stepi ~loc:e.loc
             (IF
                (lii ~loc:i1.loc @@ SEQ (simplify_stepi ~loc:i1.loc (DIP_DROP(n,m-min_m)) e1),
                 lii ~loc:i2.loc @@ SEQ (simplify_stepi ~loc:i2.loc (DIP_DROP(n,m'-min_m)) e2)
                )) exprs)

      | _ -> e :: exprs
    end

  (* takes nothing, add one item on stack : 0 -> 1 *)
  | (PUSH _ | NOW | BALANCE | SELF _ | SOURCE | SENDER | AMOUNT | STEPS_TO_QUOTA
    | LAMBDA _
    ),
    {ins=DIP_DROP (n,m); loc} :: exprs ->
    if n > 0 then
      dip_drop ~loc (n-1, m) :: simplify_step e exprs
    else
    if m = 1 then
      exprs
    else
      lii ~loc (DIP_DROP (n,m-1)) :: exprs

  | (PUSH _ | NOW | BALANCE | SELF _ | SOURCE | SENDER | AMOUNT | STEPS_TO_QUOTA
    | LAMBDA _
    ), ({ins=DROP n} as e) :: exprs when n > 0 ->
    if n = 1 then exprs else { e with ins = DROP (n-1) } :: exprs


  (* takes one item on stack, creates one :  1 -> 1 *)
  | (CAR _| CDR _ | CDAR (_, _) | CDDR (_, _)
    | LE | LT | GE | GT | NEQ | EQ | SOME
    | ADDRESS | NOT | ABS | INT | NEG | LEFT _ | RIGHT _
    | SET_DELEGATE | SIZE | CONTRACT _
    | IMPLICIT_ACCOUNT | HASH_KEY
    | BLAKE2B | SHA256 | SHA512
    | CONCAT | RENAME _ | PACK | UNPACK _
    ),
    {ins=DIP_DROP (n,m); loc} :: exprs when n > 0 ->
    simplify_stepi ~loc (DIP_DROP (n,m))
      (simplify_step e exprs)

  | (CAR _ | CDR _ | CDAR (_, _) | CDDR (_, _)
    | LE | LT | GE | GT | NEQ | EQ | SOME
    | ADDRESS | NOT | ABS | INT | NEG | LEFT _ | RIGHT _
    | SET_DELEGATE | SIZE | CONTRACT _
    | IMPLICIT_ACCOUNT | HASH_KEY
    | BLAKE2B | SHA256 | SHA512
    | CONCAT | RENAME _ | PACK | UNPACK _
    ),
    {ins = DROP n; loc} :: exprs -> lii ~loc (DROP n) :: exprs


  (* takes two items on stack, creates one : 2 -> 1 *)
  | (PAIR | RECORD (_, _)
    | ADD | SUB | COMPARE | GET | MEM
    | CONS | EXEC
    | OR | AND | XOR | MUL | EDIV | LSL | LSR ),
    {ins=DIP_DROP (n,m); loc} :: exprs when n > 0 ->
    simplify_stepi ~loc (DIP_DROP (n+1,m))
      (simplify_step e exprs)

  (* takes three items on stack, creates one *)
  | (UPDATE | TRANSFER_TOKENS | CHECK_SIGNATURE | SLICE),
    {ins=DIP_DROP (n,m); loc} :: exprs when n > 0 ->
    simplify_stepi ~loc (DIP_DROP (n+2,m))
      (simplify_step e exprs)

  (* takes four items on stack, creates one : 4 -> 1 *)
  (* FIXME takes 4 -> 2 *)
  (* | (CREATE_ACCOUNT),
   *   {ins=DIP_DROP (n,m); loc} :: exprs when n > 0 ->
   *    simplify_stepi ~loc (DIP_DROP (n+3,m))
   *      (simplify_step e exprs) *)

  (* takes two items on stack, creates two : 2 -> 2 *)
  | SWAP,
    {ins=DIP_DROP (n,m); loc} :: exprs when n > 1 ->
    simplify_stepi ~loc (DIP_DROP (n,m))
      (simplify_step e exprs)


  | DIP (n,e), {ins = DROP m; loc} :: exprs when n >= m ->
    ii (DROP m) :: simplify_stepi ~loc (DIP(n-m,e)) exprs


  | DIP_DROP (n,m), {ins=DIP_DROP (n',m')} :: exprs when n = n' ->
    ii (DIP_DROP (n, m+m')) :: exprs

  | PUSH (ty', CList tail), {ins=PUSH (ty, head)} :: {ins=CONS; loc} :: exprs
    when ty' = Tlist ty ->
    let loc = LiquidLoc.merge e.loc loc in
    simplify_stepi ~loc (PUSH (ty', CList (head :: tail))) exprs

  | DUP 1, {ins=DIP_DROP (1,1)} :: exprs -> exprs
  | DUP 1, {ins=DIP_DROP (1,m)} :: exprs when m > 1 ->
    simplify_stepi ~loc:e.loc (DIP_DROP (1, m-1)) exprs

  | DUP n, {ins=DIP_DROP (1, m); loc} :: exprs when m = n - 1 && n > 1 ->
    simplify_stepi ~loc (DROP m) (simplify_stepi ~loc:e.loc (DUP 1) exprs)

  | DUP n, {ins=DIP_DROP (1, m); loc} :: exprs when m >= n && n > 1 ->
    simplify_stepi ~loc (DROP (m - n)) exprs

  | DUP 3, {ins=DIP_DROP (2,2); loc} :: exprs ->
    simplify_stepi ~loc:e.loc SWAP
      (simplify_stepi ~loc (DROP 1)
         (simplify_stepi ~loc:e.loc SWAP exprs))

  | DUP 2, {ins=SWAP} :: {ins=DROP 1; loc} :: exprs ->
    simplify_stepi ~loc (DROP 1)
      (simplify_stepi ~loc:e.loc (DUP 1) exprs)

  | DUP 2, {ins=DIP_DROP (2,1)} :: exprs ->
    simplify_stepi ~loc:e.loc SWAP exprs

  | DUP n, {ins=DIP_DROP(m,p); loc} :: exprs ->
    let after =
      if n<m then
        if m =1 then
          drop ~loc p :: simplify_stepi ~loc:e.loc (DUP n) exprs
        else
          simplify_stepi ~loc (DIP_DROP(m-1,p))
            (simplify_stepi ~loc:e.loc (DUP n) exprs)
      else
      if n >= m+p then
        if m = 1 then
          drop ~loc p :: simplify_stepi ~loc:e.loc (DUP (n-p)) exprs
        else
          simplify_stepi ~loc (DIP_DROP (m-1,p))
            (simplify_stepi ~loc:e.loc (DUP (n-p)) exprs)
      else
      if p = 1 then
        {e with ins=DUP n} :: lii ~loc (DIP_DROP(m,p)) :: exprs
      else
        let x = n-m in
        let y = p -x - 1 in
        let code =
          simplify_stepi ~loc:e.loc (DUP(n-x))
            (simplify_stepi ~loc (DIP_DROP(m,1)) exprs)
        in
        let code =
          if y > 0 then
            simplify_stepi ~loc (DIP_DROP(m,y)) code
          else code
        in
        let code =
          if x > 0 then
            dip_drop ~loc (m-1, x) :: code
          else code
        in
        code

    in
    (* let diff_before, diff_after =
     *   let rec aux bef aft = match bef, aft with
     *     | be :: bef, af :: aft when be.ins = af.ins -> aux bef aft
     *     | _ -> List.rev bef, List.rev aft in
     *   aux (List.rev (e :: dd :: exprs)) (List.rev after) in
     * let before_s = LiquidPrinter.Michelson.string_of_loc_michelson (ii @@ SEQ diff_before) in
     * let after_s = LiquidPrinter.Michelson.string_of_loc_michelson (ii @@ SEQ diff_after) in
     * Printf.eprintf "\nBEFORE:\n%s\nAFTER:\n%s\n\n" before_s after_s; *)
    after
  | _ -> e :: exprs

and simplify_stepi ~loc i code = simplify_step (ii ~loc i) code

and simplify_steps list tail =
  let rec iter list_rev tail =
    match list_rev with
      [] -> tail
    | e :: list ->
      iter list (simplify_step e tail)
  in
  iter (List.rev list) tail

and simplify contract =
  { contract with mic_code = simplify_pre contract.mic_code }

let simplify contract =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Peephole optimization of Michelson code@.";
  simplify contract
