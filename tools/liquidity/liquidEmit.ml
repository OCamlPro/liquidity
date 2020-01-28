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

let i ~loc exp = {
  ins = exp;
  loc;
  loc_name = None;
}

let var_annot = function
  | None -> []
  | Some name -> ["@" ^ name]

let field_annot = function
  | None -> []
  | Some field -> ["%" ^ field]

let rec emit_code ~expand code =
  let name = code.loc_name in
  let i = i ~loc:code.loc in
  match code.ins with
  | RENAME name -> M_INS_EXP ("RENAME", [], [], var_annot name)
  | SEQ exprs -> M_INS_EXP ("SEQ", [], List.map (emit_code ~expand) exprs, [])
  | IF (ifthen, ifelse) ->
    M_INS_EXP ("IF", [],
               [emit_code ~expand ifthen; emit_code ~expand ifelse], var_annot name)
  | IF_NONE (ifthen, ifelse) ->
    M_INS_EXP ("IF_NONE", [],
               [emit_code ~expand ifthen; emit_code ~expand ifelse], var_annot name)
  | IF_CONS (ifcons, ifnil) ->
    M_INS_EXP ("IF_CONS", [],
               [emit_code ~expand ifcons; emit_code ~expand ifnil], var_annot name)
  | IF_LEFT (left, right) ->
    M_INS_EXP ("IF_LEFT", [],
               [emit_code ~expand left; emit_code ~expand right], var_annot name)
  | LOOP loop ->
    M_INS_EXP ("LOOP", [], [emit_code ~expand loop], var_annot name)
  | LOOP_LEFT loop ->
    M_INS_EXP ("LOOP_LEFT", [], [emit_code ~expand loop], var_annot name)
  | ITER body ->
    M_INS_EXP ("ITER", [], [emit_code ~expand body], var_annot name)
  | MAP body ->
    M_INS_EXP ("MAP", [], [emit_code ~expand body], var_annot name)
  | LAMBDA (arg_type, res_type, body) ->
    M_INS_EXP ("LAMBDA",
               [arg_type; res_type],
               [emit_code ~expand body], var_annot name)
  | LEFT (ty, constr) ->
    M_INS_EXP ("LEFT", [ty], [],
               var_annot name @ field_annot constr @ field_annot (Some ""))
  | RIGHT (ty, constr) ->
    M_INS_EXP ("RIGHT", [ty], [],
               var_annot name @ field_annot (Some "") @ field_annot constr)
  | CONTRACT ty -> M_INS_EXP ("CONTRACT", [ty], [], var_annot name)

  | UNPACK ty -> M_INS_EXP ("UNPACK", [ty], [], var_annot name)

  (* Special case for empty map/set  TODO check if necessary *)
  (* | PUSH (ty, CMap []) -> M_INS_EXP ("EMPTY_MAP", [ty], [], var_annot name)
   * | PUSH (ty, CSet []) -> M_INS_EXP ("EMPTY_SET", [ty], [], var_annot name) *)

  | PUSH (Tunit, CUnit) -> M_INS ("UNIT", var_annot name)
  | PUSH (Tlist ty, CList []) -> M_INS_EXP ("NIL", [ty], [], var_annot name)
  | PUSH (ty, cst) ->
    let cst = emit_const ~expand cst in
    M_INS_CST ("PUSH", ty, cst, var_annot name)

  | DIP (0, exp) -> assert false
  | DIP (1, exp) -> M_INS_EXP ("DIP", [], [emit_code ~expand exp], var_annot name)
  | DIP (n, exp) ->
    if expand then
      M_INS_EXP ("DIP", [],
                 [emit_code ~expand @@ i @@
                  SEQ [{ code with ins = DIP(n-1, exp) }]], [])
    else
      M_INS_EXP (Printf.sprintf "D%sP" (String.make n 'I'), [],
                 [emit_code ~expand exp], var_annot name)
  | DUP 0 -> assert false
  | DUP 1 -> M_INS ("DUP", var_annot name)
  | DUP n ->
    if expand then
      emit_code ~expand @@ i @@
      SEQ [
        i @@ DIP(1, i @@ SEQ [i @@ DUP(n-1)]);
        {ins = SWAP; loc = code.loc; loc_name = name }
      ]
    else M_INS (Printf.sprintf "D%sP" (String.make n 'U'), var_annot name)

  | CDAR (0, field) -> emit_code expand { code with ins = CAR field }
  | CDDR (0, field) -> emit_code expand { code with ins = CDR field }
  | CDAR (n, field) ->
    if expand then
      emit_code ~expand @@ i @@
      SEQ (LiquidMisc.list_init n
             (fun _ -> i @@ CDR None) @ [{ code with ins = CAR field }])
    else M_INS (Printf.sprintf "C%sAR" (String.make n 'D'),
                var_annot name @ field_annot field)
  | CDDR (n, field) ->
    if expand then
      emit_code ~expand @@ i @@
      SEQ (LiquidMisc.list_init n
             (fun _ -> i @@ CDR None) @ [{ code with ins = CDR field }])
    else M_INS (Printf.sprintf "C%sDR" (String.make n 'D'),
                var_annot name @ field_annot field)
  | DROP -> M_INS ("DROP", var_annot name)
  | CAR field -> M_INS ("CAR", var_annot name @ field_annot field)
  | CDR field -> M_INS ("CDR", var_annot name @ field_annot field)
  | PAIR -> M_INS ("PAIR", var_annot name)
  | RECORD (label1, label2) ->
    M_INS ("PAIR",
           var_annot name @ field_annot (Some label1) @ field_annot label2)
  | COMPARE -> M_INS ("COMPARE", var_annot name)
  | LE -> M_INS ("LE", var_annot name)
  | LT -> M_INS ("LT", var_annot name)
  | GE -> M_INS ("GE", var_annot name)
  | GT -> M_INS ("GT", var_annot name)
  | NEQ -> M_INS ("NEQ", var_annot name)
  | EQ -> M_INS ("EQ", var_annot name)
  | FAILWITH -> M_INS ("FAILWITH", var_annot name)
  | NOW -> M_INS ("NOW", var_annot name)
  | TRANSFER_TOKENS -> M_INS ("TRANSFER_TOKENS", var_annot name)
  | ADD -> M_INS ("ADD", var_annot name)
  | SUB -> M_INS ("SUB", var_annot name)
  | BALANCE -> M_INS ("BALANCE", var_annot name)
  | SWAP -> M_INS ("SWAP", var_annot name)
  | DIP_DROP (n,m) ->
    emit_code ~expand @@
    i @@ DIP (n, i @@ SEQ (LiquidMisc.list_init m (fun _ -> i DROP)))
  | SOME -> M_INS ("SOME", var_annot name)
  | GET -> M_INS ("GET", var_annot name)
  | UPDATE -> M_INS ("UPDATE", var_annot name)
  | CONCAT -> M_INS ("CONCAT", var_annot name)
  | SLICE -> M_INS ("SLICE", var_annot name)
  | MEM -> M_INS ("MEM", var_annot name)
  | SELF -> M_INS ("SELF", var_annot name)
  (*  | SOURCE -> M_INS "SOURCE" *)
  | AMOUNT -> M_INS ("AMOUNT", var_annot name)
  | STEPS_TO_QUOTA -> M_INS ("STEPS_TO_QUOTA", var_annot name)
  | ADDRESS -> M_INS ("ADDRESS", var_annot name)
  | CREATE_ACCOUNT -> M_INS ("CREATE_ACCOUNT", var_annot name)
  | PACK -> M_INS ("PACK", var_annot name)
  | BLAKE2B -> M_INS ("BLAKE2B", var_annot name)
  | SHA256 -> M_INS ("SHA256", var_annot name)
  | SHA512 -> M_INS ("SHA512", var_annot name)
  | HASH_KEY -> M_INS ("HASH_KEY", var_annot name)
  | CHECK_SIGNATURE -> M_INS ("CHECK_SIGNATURE", var_annot name)
  | SIZE -> M_INS ("SIZE", var_annot name)
  | IMPLICIT_ACCOUNT -> M_INS ("IMPLICIT_ACCOUNT", var_annot name)
  | SET_DELEGATE -> M_INS ("SET_DELEGATE", var_annot name)
  | CONS -> M_INS ("CONS", var_annot name)
  | OR -> M_INS ("OR", var_annot name)
  | XOR -> M_INS ("XOR", var_annot name)
  | AND -> M_INS ("AND", var_annot name)
  | NOT -> M_INS ("NOT", var_annot name)
  | INT -> M_INS ("INT", var_annot name)
  | ISNAT -> M_INS ("ISNAT", var_annot name)
  | ABS -> M_INS ("ABS", var_annot name)
  | NEG -> M_INS ("NEG", var_annot name)
  | MUL -> M_INS ("MUL", var_annot name)
  | EXEC -> M_INS ("EXEC", var_annot name)
  | EDIV -> M_INS ("EDIV", var_annot name)
  | LSL -> M_INS ("LSL", var_annot name)
  | LSR -> M_INS ("LSR", var_annot name)
  | SOURCE -> M_INS ("SOURCE", var_annot name)
  | SENDER -> M_INS ("SENDER", var_annot name)
  | MOD -> M_INS ("MOD", var_annot name)
  | DIV -> M_INS ("DIV", var_annot name)
  | CREATE_CONTRACT contract ->
    M_INS_EXP ("CREATE_CONTRACT", [], [
        M_INS_EXP ("SEQ", [], emit_contract ~expand contract , [])
      ], var_annot name)
  | EXTENSION (minst, tys) -> M_INS_EXP (minst, tys, [], var_annot name)
  | GET_BALANCE -> M_INS ("GET_BALANCE", var_annot name)
  | IS_IMPLICIT -> M_INS ("IS_IMPLICIT", var_annot name)
  | BLOCK_LEVEL -> M_INS ("BLOCK_LEVEL", var_annot name)
  | COLLECT_CALL -> M_INS ("COLLECT_CALL", var_annot name)

and emit_const ~expand cst = match cst with
  | ( CUnit
    | CBool _
    | CInt _
    | CNat _
    | CTez _
    | CTimestamp _
    | CString _
    | CBytes _
    | CKey _
    | CSignature _
    | CNone
    | CKey_hash _
    | CContract _
    | CAddress _ ) as cst -> cst
  | CLambda l ->
    CLambda { l with body = emit_code ~expand l.body }
  | CTuple l ->
    CTuple (List.map (emit_const ~expand) l)
  | CSome c ->
    CSome (emit_const ~expand c)
  | CList l ->
    CList (List.map (emit_const ~expand) l)
  | CSet l ->
    CSet (List.map (emit_const ~expand) l)
  | CMap l ->
    CMap (List.map (fun (k, v) ->
        (emit_const ~expand k,
         emit_const ~expand v)) l)
  | CBigMap l ->
    CBigMap (List.map (fun (k, v) ->
        (emit_const ~expand k,
         emit_const ~expand v)) l)
  | CLeft c ->
    CLeft (emit_const ~expand c)
  | CRight c ->
    CRight (emit_const ~expand c)
  | CRecord l ->
    CRecord (List.map (fun (f, c) -> f, emit_const ~expand c) l)
  | CConstr (s, c) ->
    CConstr (s, emit_const ~expand c)


and emit_contract ~expand (contract : loc_michelson_contract) =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Emit Michelson for contract@.";
  [
    M_INS_EXP ("parameter", [contract.mic_parameter], [], []);
    M_INS_EXP ("storage", [contract.mic_storage], [], []);
    M_INS_EXP ("code", [], [emit_code ~expand contract.mic_code], []);
  ] @ match contract.mic_fee_code with
  | None -> []
  | Some mic_fee_code ->
    [ M_INS_EXP ("code", [], [emit_code ~expand mic_fee_code], ["@fee"]) ]
