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
  { contract with code = clean_code contract.code }


(* let rec remove_loc_code (code : loc_michelson) : noloc_michelson =
 *   let i =
 *     match code.ins with
 *     | SEQ expr -> SEQ (List.map remove_loc_code expr)
 *     | IF (e1, e2) -> IF (remove_loc_code e1, remove_loc_code e2)
 *     | IF_NONE (e1, e2) -> IF_NONE (remove_loc_code e1, remove_loc_code e2)
 *     | IF_LEFT (e1, e2) -> IF_LEFT (remove_loc_code e1, remove_loc_code e2)
 *     | IF_CONS (e1, e2) -> IF_CONS (remove_loc_code e1, remove_loc_code e2)
 *     | DIP (n, e) -> DIP (n, remove_loc_code e)
 *     | LOOP e -> LOOP (remove_loc_code e)
 *     | ITER e -> LOOP (remove_loc_code e)
 *     | LAMBDA (arg_type, res_type, e) ->
 *       LAMBDA (arg_type, res_type, remove_loc_code e)
 *     (\* same *\)
 *     | ANNOT n -> ANNOT n
 *     | EXEC -> EXEC
 *     | DUP n -> DUP n
 *     | DIP_DROP(n,p) -> DIP_DROP(n,p)
 *     | DROP -> DROP
 *     | CAR -> CAR
 *     | CDR -> CDR
 *     | CDAR n -> CDAR n
 *     | CDDR n -> CDDR n
 *     | PUSH (n,ty) -> PUSH (n,ty)
 *     | PAIR -> PAIR
 *     | COMPARE -> COMPARE
 *     | LE -> LE
 *     | LT -> LT
 *     | GE -> GE
 *     | GT -> GT
 *     | NEQ -> NEQ
 *     | EQ -> EQ
 *     | FAIL -> FAIL
 *     | NOW -> NOW
 *     | TRANSFER_TOKENS -> TRANSFER_TOKENS
 *     | ADD -> ADD
 *     | SUB -> SUB
 *     | BALANCE -> BALANCE
 *     | SWAP -> SWAP
 *     | GET -> GET
 *     | UPDATE -> UPDATE
 *     | SOME -> SOME
 *     | CONCAT -> CONCAT
 *     | MEM -> MEM
 *     | MAP -> MAP
 *     | REDUCE -> REDUCE
 *     | SELF -> SELF
 *     | AMOUNT -> AMOUNT
 *     | STEPS_TO_QUOTA -> STEPS_TO_QUOTA
 *     | MANAGER -> MANAGER
 *     | CREATE_ACCOUNT -> CREATE_ACCOUNT
 *     | CREATE_CONTRACT -> CREATE_CONTRACT
 *     | H -> H
 *     | HASH_KEY -> HASH_KEY
 *     | CHECK_SIGNATURE -> CHECK_SIGNATURE
 *     | CONS -> CONS
 *     | OR -> OR
 *     | XOR -> XOR
 *     | AND -> AND
 *     | NOT -> NOT
 *     | INT -> INT
 *     | ABS -> ABS
 *     | NEG -> NEG
 *     | MUL -> MUL
 *     | LEFT ty -> LEFT ty
 *     | RIGHT ty -> RIGHT ty
 *     | EDIV -> EDIV
 *     | LSL -> LSL
 *     | LSR -> LSR
 *     | SOURCE (ty1, ty2) -> SOURCE (ty1, ty2)
 *     | SIZE -> SIZE
 *     | DEFAULT_ACCOUNT -> DEFAULT_ACCOUNT
 *     | MOD -> MOD
 *     | DIV -> DIV
 *   in
 *   { i; noloc_name = code.loc_name }
 *
 * let remove_loc_contract contract =
 *   { contract with code = remove_loc_code contract.code } *)
