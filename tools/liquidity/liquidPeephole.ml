(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes


(* let string_of_pre pre = *)
(*   LiquidPrinter.Michelson.string_of_code (LiquidEmit.emit_code (SEQ pre)) *)

(* Try to simplify Michelson with peepholes optims: mostly, move
   DIP_DROPs backwards to decrease the size of the stack. *)

let ii i = { i; noloc_name = None }

let drops n = LiquidMisc.list_init n (fun _ -> ii DROP)

let dip_drop (a,b)=
  if a = 0 then drops b else [ii (DIP_DROP(a,b))]

let rec simplify_pre ({ i } as e) =
  { e with
    i =
      match i with
      | SEQ expr -> SEQ (simplify_seq expr)
      | IF (e1, e2) -> IF (simplify_pre e1, simplify_pre e2)
      | IF_NONE (e1, e2) -> IF_NONE (simplify_pre e1, simplify_pre e2)
      | IF_LEFT (e1, e2) -> IF_LEFT (simplify_pre e1, simplify_pre e2)
      | IF_CONS (e1, e2) -> IF_CONS (simplify_pre e1, simplify_pre e2)
      | DIP (n, e) -> DIP (n, simplify_pre e)
      | LOOP e -> LOOP (simplify_pre e)
      | LAMBDA (arg_type, res_type, e) ->
        LAMBDA (arg_type, res_type, simplify_pre e)
      | _ -> i
  }

and simplify_seq exprs =
  match exprs with
  | [] -> []
  | e :: exprs ->
     let e = simplify_pre e in
     if e.i = FAIL then [ii FAIL]
     else
       let exprs =  simplify_seq exprs in
       simplify_step e exprs

and simplify_step e exprs =
  match e.i, exprs with

  | SEQ e, exprs -> simplify_steps e exprs
  | DIP_DROP(n,0), exprs -> exprs
  | DIP (0, e), exprs -> simplify_step e exprs
  | DUP _, {i=DROP} :: exprs -> exprs
  | PUSH _, {i=FAIL} :: _
    | FAIL, _ -> [ii FAIL]

  | IF(i1,i2), exprs ->
     begin
       match i1.i, i2.i,exprs with
       | SEQ ({i=DROP} :: e1), SEQ ({i=DROP} :: e2), exprs ->
         simplify_stepi (DIP_DROP(1,1))
           (simplify_stepi (IF ( ii @@ SEQ e1,
                                 ii @@ SEQ e2 )) exprs)

       | SEQ ({ i=DIP_DROP(n,m)} :: e1),
         SEQ ({ i=DIP_DROP(n',m')} :: e2),
         exprs when n=n'
         ->
          let min_m = min m m' in
          simplify_stepi (DIP_DROP(n,min_m))
            (simplify_stepi
               (IF
                  (ii @@ SEQ (simplify_stepi (DIP_DROP(n,m-min_m)) e1),
                   ii @@ SEQ (simplify_stepi (DIP_DROP(n,m'-min_m)) e2)
                  )) exprs)

       | SEQ [{i=FAIL}],
         SEQ [{i=PUSH _}],
         {i=DROP} :: exprs ->
          simplify_step (ii @@ IF (ii @@ SEQ [ii FAIL], ii @@ SEQ [])) exprs

       | SEQ [{i=FAIL}],
         SEQ [],
         {i=DROP} :: exprs ->
          simplify_stepi (DIP_DROP(1,1))
                        (simplify_stepi
                           (IF (ii @@ SEQ [ii FAIL],
                                ii @@ SEQ [])) exprs)

       | SEQ [{i=FAIL}],
         SEQ [],
         {i=DIP_DROP(n,m)} :: exprs ->
          simplify_stepi (DIP_DROP(n+1,m))
                        (simplify_stepi
                           (IF (ii @@ SEQ [ii FAIL],
                                ii @@ SEQ [])) exprs)

       | _ -> e :: exprs
     end

  (* takes nothing, add one item on stack : 0 -> 1 *)
  | (PUSH _ | NOW | BALANCE | SELF | SOURCE _ | AMOUNT | STEPS_TO_QUOTA
     | LAMBDA _
    ),
    {i=DIP_DROP (n,m)} :: exprs ->
     if n > 0 then
       dip_drop (n-1,m) @ simplify_step e exprs
     else
       if m = 1 then
         exprs
       else
         ii (DIP_DROP (n,m-1)) :: exprs

  | (PUSH _ | NOW | BALANCE | SELF | SOURCE _ | AMOUNT | STEPS_TO_QUOTA
     | LAMBDA _
    ), {i=DROP} :: exprs -> exprs


  (* takes one item on stack, creates one :  1 -> 1 *)
  | (CAR | CDR | CDAR _ | CDDR _
     | LE | LT | GE | GT | NEQ | EQ | SOME
     | MANAGER | H | NOT | ABS | INT | NEG | LEFT _ | RIGHT _
     | EDIV | LSL | LSR
    ),
    {i=DIP_DROP (n,m)} :: exprs when n > 0 ->
     simplify_stepi (DIP_DROP (n,m))
                   (simplify_step e exprs)

  | (CAR | CDR | CDAR _ | CDDR _
     | LE | LT | GE | GT | NEQ | EQ | SOME
     | MANAGER | H | NOT | ABS | INT | NEG | LEFT _ | RIGHT _
     | EDIV | LSL | LSR
    ),
    {i=DROP} :: exprs -> ii DROP :: exprs


  (* takes two items on stack, creates one : 2 -> 1 *)
  | (PAIR | ADD | SUB | COMPARE | GET | CONCAT | MEM
     | CONS | CHECK_SIGNATURE | EXEC | MAP
     | OR | AND | XOR | MUL),
    {i=DIP_DROP (n,m)} :: exprs when n > 0 ->
     simplify_stepi (DIP_DROP (n+1,m))
                   (simplify_step e exprs)

  (* takes three items on stack, creates one *)
  | (UPDATE | REDUCE),
    {i=DIP_DROP (n,m)} :: exprs when n > 0 ->
     simplify_stepi (DIP_DROP (n+2,m))
                   (simplify_step e exprs)

  (* takes four items on stack, creates one : 4 -> 1 *)
  | (CREATE_ACCOUNT),
    {i=DIP_DROP (n,m)} :: exprs when n > 0 ->
     simplify_stepi (DIP_DROP (n+3,m))
                   (simplify_step e exprs)

  (* takes two items on stack, creates two : 2 -> 2 *)
  | SWAP,
    {i=DIP_DROP (n,m)} :: exprs when n > 1 ->
     simplify_stepi (DIP_DROP (n,m))
                   (simplify_step e exprs)


  | DIP (n,e), {i=DROP} :: exprs when n > 0 ->
     ii DROP :: simplify_stepi (DIP(n-1,e)) exprs


  | DIP_DROP (n,m), {i=DIP_DROP (n',m')} :: exprs when n = n' ->
     ii (DIP_DROP (n, m+m')) :: exprs

  | PUSH (ty', CList tail), {i=PUSH (ty, head)} :: {i=CONS} :: exprs
       when ty' = Tlist ty ->
     simplify_stepi (PUSH (ty', CList (head :: tail))) exprs

  | DUP 1, {i=DIP_DROP (1,1)} :: exprs -> exprs
  | DUP 1, {i=DIP_DROP (1,m)} :: exprs when m > 1 ->
     simplify_stepi (DIP_DROP (1, m-1)) exprs

  | DUP 2, {i=DIP_DROP (1,1)} :: exprs ->
     simplify_stepi DROP (simplify_stepi (DUP 1) exprs)
  | DUP 2, {i=DIP_DROP (1,2)} :: exprs ->
     simplify_stepi DROP exprs

  | DUP 3, {i=DIP_DROP (2,2)} :: exprs ->
     simplify_stepi SWAP (simplify_stepi DROP (simplify_stepi SWAP exprs))
  | DUP 2, {i=SWAP} :: {i=DROP} :: exprs ->
     simplify_stepi DROP
                   (simplify_stepi (DUP 1) exprs)
  | DUP 2, {i=DIP_DROP (2,1)} :: exprs ->
     simplify_stepi SWAP exprs
  | DUP n, {i=DIP_DROP(m,p)} :: exprs ->
     (* let before = DUP n :: DIP_DROP(m,p) :: exprs in *)
     let after =
       if n<m then
         if m =1 then
           drops p @ (simplify_stepi (DUP n) exprs)
         else
           simplify_stepi (DIP_DROP(m-1,p)) (simplify_stepi (DUP n) exprs)
       else
         if n >= m+p then
           if m = 1 then
             drops p @ (simplify_stepi (DUP (n-p)) exprs)
           else
             simplify_stepi (DIP_DROP (m-1,p))
                           (simplify_stepi (DUP (n-p)) exprs)
         else
           if p = 1 then
             {e with i=DUP n} :: ii (DIP_DROP(m,p)) :: exprs
           else
             let x = n-m in
             let y = p -x - 1 in
             let code =
               simplify_stepi (DUP(n-x))
                             (simplify_stepi (DIP_DROP(m,1)) exprs)
             in
             let code =
               if y > 0 then
                 simplify_stepi (DIP_DROP(m,y)) code
               else code
             in
             let code =
               if x > 0 then
                 dip_drop (m-1, x) @ code
               else code
             in
             code

     in
     (*
     let before_s = string_of_pre before in
     let after_s = string_of_pre after in
     Printf.eprintf "BEFORE:\n%s\nAFTER:\n%s\n" before_s after_s;
      *)
     after
  | _ -> e :: exprs

and simplify_stepi i code = simplify_step (ii i) code

and simplify_steps list tail =
  let rec iter list_rev tail =
    match list_rev with
      [] -> tail
    | e :: list ->
       iter list (simplify_step e tail)
  in
  iter (List.rev list) tail

let simplify contract =
  { contract with code = simplify_pre contract.code }
