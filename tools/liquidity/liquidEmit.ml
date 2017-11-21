(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes


let rec emit_code code =
  match code.i with
  | ANNOT s -> M_INS_ANNOT s
  | SEQ exprs -> M_INS_EXP ("SEQ", [], List.map emit_code exprs)
  | IF (ifthen, ifelse) ->
     M_INS_EXP ("IF", [], [emit_code ifthen; emit_code ifelse])
  | IF_NONE (ifthen, ifelse) ->
     M_INS_EXP ("IF_NONE", [], [emit_code ifthen; emit_code ifelse])
  | IF_CONS (ifcons, ifnil) ->
     M_INS_EXP ("IF_CONS", [], [emit_code ifcons; emit_code ifnil])
  | IF_LEFT (left, right) ->
     M_INS_EXP ("IF_LEFT", [], [emit_code left; emit_code right])
  | LOOP loop ->
     M_INS_EXP ("LOOP", [], [emit_code loop])
  | LAMBDA (arg_type, res_type, body) ->
     M_INS_EXP ("LAMBDA",
                [arg_type; res_type],
                [emit_code body])
  | LEFT ty -> M_INS_EXP ("LEFT", [ty], [])
  | RIGHT ty -> M_INS_EXP ("RIGHT", [ty], [])


  | PUSH (ty, cst) -> M_INS_CST ("PUSH", ty, cst)
  | DIP (n, exp) -> M_INS_EXP (Printf.sprintf "D%sP" (String.make n 'I'),
                               [],
                               [emit_code exp])
  | DUP n -> M_INS (Printf.sprintf "D%sP" (String.make n 'U'))
  | CDAR n -> M_INS (Printf.sprintf "C%sAR" (String.make n 'D'))
  | CDDR n -> M_INS (Printf.sprintf "C%sDR" (String.make n 'D'))
  | DROP -> M_INS "DROP"
  | CAR -> M_INS "CAR"
  | CDR -> M_INS "CDR"
  | PAIR -> M_INS "PAIR"
  | COMPARE -> M_INS "COMPARE"
  | LE -> M_INS "LE"
  | LT -> M_INS "LT"
  | GE -> M_INS "GE"
  | GT -> M_INS "GT"
  | NEQ -> M_INS "NEQ"
  | EQ -> M_INS "EQ"
  | FAIL -> M_INS "FAIL"
  | NOW -> M_INS "NOW"
  | TRANSFER_TOKENS -> M_INS "TRANSFER_TOKENS"
  | ADD -> M_INS "ADD"
  | SUB -> M_INS "SUB"
  | BALANCE -> M_INS "BALANCE"
  | SWAP -> M_INS "SWAP"
  | DIP_DROP (n,m) ->
     emit_code {i= DIP (n,
                        { i = SEQ (LiquidMisc.list_init m
                                                        (fun _ -> {i=DROP}))})}
  | SOME -> M_INS "SOME"
  | GET -> M_INS "GET"
  | UPDATE -> M_INS "UPDATE"
  | CONCAT -> M_INS "CONCAT"
  | MEM -> M_INS "MEM"
  | SELF -> M_INS "SELF"
  (*  | SOURCE -> M_INS "SOURCE" *)
  | AMOUNT -> M_INS "AMOUNT"
  | STEPS_TO_QUOTA -> M_INS "STEPS_TO_QUOTA"
  | MANAGER -> M_INS "MANAGER"
  | CREATE_ACCOUNT -> M_INS "CREATE_ACCOUNT"
  | H -> M_INS "H"
  | HASH_KEY -> M_INS "HASH_KEY"
  | CHECK_SIGNATURE -> M_INS "CHECK_SIGNATURE"
  | SIZE -> M_INS "SIZE"
  | DEFAULT_ACCOUNT -> M_INS "DEFAULT_ACCOUNT"
  | CONS -> M_INS "CONS"
  | OR -> M_INS "OR"
  | XOR -> M_INS "XOR"
  | AND -> M_INS "AND"
  | NOT -> M_INS "NOT"
  | INT -> M_INS "INT"
  | ABS -> M_INS "ABS"
  | NEG -> M_INS "NEG"
  | MUL -> M_INS "MUL"
  | EXEC -> M_INS "EXEC"
  | REDUCE -> M_INS "REDUCE"
  | MAP -> M_INS "MAP"
  | EDIV -> M_INS "EDIV"
  | LSL -> M_INS "LSL"
  | LSR -> M_INS "LSR"
  | SOURCE (arg_ty, res_ty) -> M_INS_EXP ("SOURCE", [arg_ty; res_ty], [])
  | MOD -> M_INS "MOD"
  | DIV -> M_INS "DIV"
  | CREATE_CONTRACT -> M_INS "CREATE_CONTRACT"

let emit_contract contract = { contract with code = emit_code contract.code }
