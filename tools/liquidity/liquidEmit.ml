(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let i ~loc exp = {
  ins = exp;
  loc;
  loc_name = None;
}

let rec emit_code ~expand code =
  let name = code.loc_name in
  let i = i ~loc:code.loc in
  match code.ins with
  | ANNOT s -> M_INS_ANNOT s
  | SEQ exprs -> M_INS_EXP ("SEQ", [], List.map (emit_code ~expand) exprs, name)
  | IF (ifthen, ifelse) ->
    M_INS_EXP ("IF", [],
               [emit_code ~expand ifthen; emit_code ~expand ifelse], name)
  | IF_NONE (ifthen, ifelse) ->
    M_INS_EXP ("IF_NONE", [],
               [emit_code ~expand ifthen; emit_code ~expand ifelse], name)
  | IF_CONS (ifcons, ifnil) ->
    M_INS_EXP ("IF_CONS", [],
               [emit_code ~expand ifcons; emit_code ~expand ifnil], name)
  | IF_LEFT (left, right) ->
    M_INS_EXP ("IF_LEFT", [],
               [emit_code ~expand left; emit_code ~expand right], name)
  | LOOP loop ->
     M_INS_EXP ("LOOP", [], [emit_code ~expand loop], name)
  | ITER body ->
     M_INS_EXP ("ITER", [], [emit_code ~expand body], name)
  | LAMBDA (arg_type, res_type, body) ->
     M_INS_EXP ("LAMBDA",
                [arg_type; res_type],
                [emit_code ~expand body], name)
  | LEFT ty -> M_INS_EXP ("LEFT", [ty], [], name)
  | RIGHT ty -> M_INS_EXP ("RIGHT", [ty], [], name)

  (* Special case for empty map/set  TODO check if necessary *)
  (* | PUSH (ty, CMap []) -> M_INS_EXP ("EMPTY_MAP", [ty], [], name)
   * | PUSH (ty, CSet []) -> M_INS_EXP ("EMPTY_SET", [ty], [], name) *)

  | PUSH (Tunit, CUnit) -> M_INS ("UNIT", name)
  | PUSH (Tlist ty, CList []) -> M_INS_EXP ("NIL", [ty], [], name)
  | PUSH (ty, cst) -> M_INS_CST ("PUSH", ty, cst, name)
  | DIP (0, exp) -> assert false
  | DIP (1, exp) -> M_INS_EXP ("DIP", [], [emit_code ~expand exp], name)
  | DIP (n, exp) ->
    if expand then
      M_INS_EXP ("DIP", [],
                 [emit_code ~expand @@ i @@
                  SEQ [{ code with ins = DIP(n-1, exp) }]], None)
    else
      M_INS_EXP (Printf.sprintf "D%sP" (String.make n 'I'), [],
                 [emit_code ~expand exp], name)
  | DUP 0 -> assert false
  | DUP 1 -> M_INS ("DUP", name)
  | DUP n ->
    if expand then
      emit_code ~expand @@ i @@
      SEQ [
        i @@ DIP(1, i @@ SEQ [i @@ DUP(n-1)]);
        {ins = SWAP; loc = code.loc; loc_name = name }
      ]
    else M_INS (Printf.sprintf "D%sP" (String.make n 'U'), name)

  | CDAR 0 -> emit_code expand { code with ins = CAR }
  | CDDR 0 -> emit_code expand { code with ins = CDR }
  | CDAR n ->
    if expand then
      emit_code ~expand @@ i @@
      SEQ (LiquidMisc.list_init n (fun _ -> i CDR) @ [{ code with ins = CAR }])
    else M_INS (Printf.sprintf "C%sAR" (String.make n 'D'), name)
  | CDDR n ->
    if expand then
      emit_code ~expand @@ i @@
      SEQ (LiquidMisc.list_init n (fun _ -> i CDR) @ [{ code with ins = CDR }])
    else M_INS (Printf.sprintf "C%sDR" (String.make n 'D'), name)
  | DROP -> M_INS ("DROP", name)
  | CAR -> M_INS ("CAR", name)
  | CDR -> M_INS ("CDR", name)
  | PAIR -> M_INS ("PAIR", name)
  | COMPARE -> M_INS ("COMPARE", name)
  | LE -> M_INS ("LE", name)
  | LT -> M_INS ("LT", name)
  | GE -> M_INS ("GE", name)
  | GT -> M_INS ("GT", name)
  | NEQ -> M_INS ("NEQ", name)
  | EQ -> M_INS ("EQ", name)
  | FAIL _ -> M_INS ("FAIL", name)
  | NOW -> M_INS ("NOW", name)
  | TRANSFER_TOKENS -> M_INS ("TRANSFER_TOKENS", name)
  | ADD -> M_INS ("ADD", name)
  | SUB -> M_INS ("SUB", name)
  | BALANCE -> M_INS ("BALANCE", name)
  | SWAP -> M_INS ("SWAP", name)
  | DIP_DROP (n,m) ->
    emit_code ~expand @@
    i @@ DIP (n, i @@ SEQ (LiquidMisc.list_init m (fun _ -> i DROP)))
  | SOME -> M_INS ("SOME", name)
  | GET -> M_INS ("GET", name)
  | UPDATE -> M_INS ("UPDATE", name)
  | CONCAT -> M_INS ("CONCAT", name)
  | MEM -> M_INS ("MEM", name)
  | SELF -> M_INS ("SELF", name)
  (*  | SOURCE -> M_INS "SOURCE" *)
  | AMOUNT -> M_INS ("AMOUNT", name)
  | STEPS_TO_QUOTA -> M_INS ("STEPS_TO_QUOTA", name)
  | MANAGER -> M_INS ("MANAGER", name)
  | ADDRESS -> M_INS ("ADDRESS", name)
  | CREATE_ACCOUNT -> M_INS ("CREATE_ACCOUNT", name)
  | H -> M_INS ("H", name)
  | HASH_KEY -> M_INS ("HASH_KEY", name)
  | CHECK_SIGNATURE -> M_INS ("CHECK_SIGNATURE", name)
  | SIZE -> M_INS ("SIZE", name)
  | DEFAULT_ACCOUNT -> M_INS ("DEFAULT_ACCOUNT", name)
  | SET_DELEGATE -> M_INS ("SET_ACCOUNT", name)
  | CONS -> M_INS ("CONS", name)
  | OR -> M_INS ("OR", name)
  | XOR -> M_INS ("XOR", name)
  | AND -> M_INS ("AND", name)
  | NOT -> M_INS ("NOT", name)
  | INT -> M_INS ("INT", name)
  | ABS -> M_INS ("ABS", name)
  | NEG -> M_INS ("NEG", name)
  | MUL -> M_INS ("MUL", name)
  | EXEC -> M_INS ("EXEC", name)
  | REDUCE -> M_INS ("REDUCE", name)
  | MAP -> M_INS ("MAP", name)
  | EDIV -> M_INS ("EDIV", name)
  | LSL -> M_INS ("LSL", name)
  | LSR -> M_INS ("LSR", name)
  | SOURCE -> M_INS ("SOURCE", name)
  | MOD -> M_INS ("MOD", name)
  | DIV -> M_INS ("DIV", name)
  | CREATE_CONTRACT -> M_INS ("CREATE_CONTRACT", name)

let emit_contract ~expand contract =
  { contract with code = emit_code ~expand contract.code }
