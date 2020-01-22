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

let mk ?name ~loc (desc: (datatype, typed) exp_desc) ty = mk ?name ~loc desc ty

let base_of_var arg =
  try
    let pos = String.index arg '/' in
    String.sub arg 0 pos
  with Not_found ->
    raise (Invalid_argument ("LiquidDecode.base_of_var: "^arg))

let rec decode_const (c : encoded_const) : typed_const = match c with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CSignature _ | CNone  | CKey_hash _
    | CAddress _ ) as c -> c
  | CSome x -> CSome (decode_const x)
  | CLeft x -> CLeft (decode_const x)
  | CRight x -> CRight (decode_const x)
  | CTuple xs -> CTuple (List.map (decode_const) xs)
  | CList xs -> CList (List.map (decode_const) xs)
  | CSet xs -> CSet (List.map (decode_const) xs)
  | CMap l ->
    CMap (List.map (fun (x,y) -> decode_const x, decode_const y) l)
  | CBigMap BMList l ->
    CBigMap (BMList (List.map (fun (x,y) -> decode_const x, decode_const y) l))
  | CBigMap BMId _ as c -> c
  | CRecord labels ->
    CRecord (List.map (fun (f, x) -> f, decode_const x) labels)
  | CConstr (constr, x) ->
    CConstr (constr, decode_const x)
  | CLambda { arg_name; arg_ty; body; ret_ty; recursive } ->
    let body = decode body in
    CLambda { arg_name; arg_ty; body; ret_ty; recursive }

and decode ( exp : encoded_exp ) : typed_exp =
  let loc = exp.loc in
  match exp.desc with
  | Const { ty; const } ->
    let const = decode_const const in
    mk ?name:exp.name ~loc (Const { ty; const }) exp.ty

  | Let { bnd_var; inline; bnd_val; body } ->
    let bnd_val = decode bnd_val in
    let body = decode body in
    mk ?name:exp.name ~loc (Let { bnd_var; inline; bnd_val; body }) body.ty

  | Var v ->
    mk ?name:exp.name ~loc (Var v) exp.ty

  | Project { field; record } ->
    let record = decode record in
    mk ?name:exp.name ~loc (Project { field; record }) exp.ty

  | SetField { record; field; set_val } ->
    let record = decode record in
    let set_val = decode set_val in
    mk ?name:exp.name ~loc (SetField { record; field; set_val }) exp.ty

  | Seq (exp1, exp2) ->
    let exp1 = decode exp1 in
    let exp2 = decode exp2 in
    mk ?name:exp.name ~loc (Seq (exp1, exp2)) exp.ty

  | If { cond; ifthen; ifelse } ->
    let cond = decode cond in
    let ifthen = decode ifthen in
    let ifelse = decode ifelse in
    mk ?name:exp.name ~loc (If { cond; ifthen; ifelse }) exp.ty

  | Transfer { dest; amount } ->
    let dest = decode dest in
    let amount = decode amount in
    let desc = Transfer { dest; amount } in
    mk ?name:exp.name ~loc desc exp.ty

  | Call { amount; entry = _; arg;
           contract = { desc = MatchOption {
               arg = { desc = ContractAt { arg = addr; entry; entry_param }};
               ifnone = { desc = Failwith _ };
               ifsome = { desc = Var x }; some_name }  }
         } when some_name.nname = x ->
    let amount = decode amount in
    let contract = decode addr in
    let arg = decode arg in
    let desc = Call { contract; amount; entry; arg } in
    mk ?name:exp.name ~loc desc exp.ty

  | Call { contract; amount; entry; arg } ->
    let amount = decode amount in
    let contract = decode contract in
    let entry = match contract.ty with
      | Tcontract (e, _ ) -> e
      | _ -> entry in
    let arg = decode arg in
    let desc = Call { contract; amount; entry; arg } in
    mk ?name:exp.name ~loc desc exp.ty

  | SelfCall { amount; entry; arg } ->
    let amount = decode amount in
    let arg = decode arg in
    let desc = SelfCall { amount; entry; arg } in
    mk ?name:exp.name ~loc desc exp.ty

  | Failwith arg ->
    let arg = decode arg in
    mk ~loc (Failwith arg) Tfail

  (* TODO *)
  (* List.rev -> List.reduce (::) *)

  (* String.concat [x1; x2] ==> x1 @ x2 *)
  | Apply { prim = Prim_concat | Prim_string_concat | Prim_bytes_concat;
            args = [{
                desc = Apply {
                    prim = Prim_Cons;
                    args = [
                      s1;
                      { desc = Apply {
                            prim = Prim_Cons;
                            args = [
                              s2;
                              { desc = Const { const = CList [] } }
                            ]}}
                    ]} }]} ->
    let s1, s2 = decode s1, decode s2 in
    mk ?name:exp.name ~loc
      (Apply { prim = Prim_concat_two; args = [s1; s2] }) exp.ty

  | Apply { prim; args } ->
    let args = List.map decode args in
    mk ?name:exp.name ~loc (Apply { prim; args }) exp.ty

  | MatchOption { arg; ifnone; some_name; ifsome } ->
    let arg = decode arg in
    let ifnone = decode ifnone in
    let ifsome = decode ifsome in
    mk ?name:exp.name ~loc
      (MatchOption { arg; ifnone; some_name; ifsome }) exp.ty

  | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
    let arg = decode arg in
    let ifplus = decode ifplus in
    let ifminus = decode ifminus in
    mk ?name:exp.name ~loc
      (MatchNat { arg; plus_name; ifplus; minus_name; ifminus }) exp.ty

  | Loop { arg_name; body; arg } ->
    let arg = decode arg in
    let body = decode body in
    mk ?name:exp.name ~loc (Loop { arg_name; body; arg }) exp.ty

  | LoopLeft { arg_name; body; arg; acc } ->
    let arg = decode arg in
    let acc = match acc with None -> None | Some acc -> Some (decode acc) in
    let body = decode body in
    mk ?name:exp.name ~loc (LoopLeft { arg_name; body; arg; acc }) exp.ty

  | Fold { prim; arg_name; body; arg; acc } ->
    let arg = decode arg in
    let acc = decode acc in
    let body = decode body in
    mk ?name:exp.name ~loc (Fold { prim; arg_name; body; arg; acc }) exp.ty

  | Map { prim; arg_name; body; arg } ->
    let arg = decode arg in
    let body = decode body in
    mk ?name:exp.name ~loc (Map { prim; arg_name; body; arg }) exp.ty

  | MapFold { prim; arg_name; body; arg; acc } ->
    let arg = decode arg in
    let acc = decode acc in
    let body = decode body in
    mk ?name:exp.name ~loc (MapFold { prim; arg_name; body; arg; acc }) exp.ty

  | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
    let arg = decode arg in
    let ifcons = decode ifcons in
    let ifnil = decode ifnil in
    mk ?name:exp.name ~loc
      (MatchList { arg; head_name; tail_name; ifcons; ifnil }) exp.ty

  | Lambda { arg_name; arg_ty; body; ret_ty; recursive } ->
    let body = decode body in
    mk ?name:exp.name ~loc
      (Lambda { arg_name; arg_ty; body; ret_ty; recursive }) exp.ty

  | Closure { arg_name; arg_ty; call_env; body; ret_ty } ->
    let call_env = List.map (fun (v, e) -> v, decode e) call_env in
    let body = decode body in
    mk ?name:exp.name ~loc
      (Closure { arg_name; arg_ty; call_env; body; ret_ty }) exp.ty

  | Record fields ->
    let fields = List.map (fun (label, exp) ->
        label, decode exp
      ) fields in
    mk ?name:exp.name ~loc (Record fields) exp.ty

  | Constructor { constr; arg } ->
    let arg = decode arg in
    mk ?name:exp.name ~loc (Constructor { constr; arg }) exp.ty

  | MatchVariant { arg; cases } ->
    let arg = decode arg in
    let cases = List.map (fun (pat, e) -> pat, decode e) cases in
    mk ?name:exp.name ~loc (MatchVariant { arg; cases }) exp.ty

  | ContractAt { arg; entry; entry_param } ->
    let arg = decode arg in
    mk ?name:exp.name ~loc (ContractAt { arg; entry; entry_param }) exp.ty

  | Unpack { arg; ty } ->
    let arg = decode arg in
    mk ?name:exp.name ~loc (Unpack { arg; ty }) exp.ty

  | CreateContract { args; contract } ->
    let args = List.map decode args in
    let contract = decode_contract contract in
    mk ?name:exp.name ~loc (CreateContract { args; contract }) exp.ty

  | TypeAnnot { e; ty } ->
    let e = decode e in
    mk ?name:exp.name ~loc (TypeAnnot { e; ty }) exp.ty

  | Type ty ->
    mk ?name:exp.name ~loc (Type ty) exp.ty

(* Recover entry point from a pattern matchin branch *)
and entry_of_case param_constrs top_storage (pat, body, fee_body) =
  let extract_storage_name body = match body.desc with
    | Let { bnd_var = { nname = storage_name };
            bnd_val = { desc = Var var_storage };
            body }
      when var_storage = top_storage ->
      storage_name, body
    | _ -> top_storage, body
  in
  match pat with
  | PConstr (s, [parameter_name]) when is_entry_case ~allow_capital:true s ->
    let storage_name, body = extract_storage_name body in
    let entry_name = entry_name_of_case ~allow_capital:true s in
    let parameter = List.assoc s param_constrs in
    let fee_code = match fee_body with
      | None -> None
      | Some fee_body ->
        let fee_storage_name, fee_body = extract_storage_name fee_body in
        if base_of_var fee_storage_name <> base_of_var storage_name then
          (Format.eprintf "Mismatch between fee code storage name and \
                           entry points. Fee code cannot be decoded.@.";
           None)
        else
          Some (decode fee_body)
    in
    {
      entry_sig = {
        entry_name;
        parameter;
        parameter_name;
        storage_name;
      };
      code = decode body;
      fee_code;
    }
  | _ -> raise Exit

(* Recover entries points from a top-level pattern matching, also move
   top-level (at the level of the matching branch) local definitions
   to global definitions *)
and decode_entries param_constrs top_parameter top_storage values exp fee_exp =
  match exp.desc with
  | MatchVariant { arg = { desc = Var var_parameter}; cases }
    when var_parameter = top_parameter &&
         List.for_all (function
             | PConstr (s, _), _ -> is_entry_case s
             | _ -> false) cases
    ->
    let fee_cases = match fee_exp with
      | None -> None
      | Some { desc =
                 MatchVariant { arg = { desc = Var var_parameter};
                                cases = fee_cases } }
        when var_parameter = top_parameter &&
             try
               List.for_all2 (fun (pat_e, _) (pat_fee, b_fee) ->
                   match pat_e, pat_fee with
                   | PConstr (s_e, v_e), PConstr (s_fee, v_fee) ->
                     let ok =
                       s_e = s_fee &&
                       (List.for_all2 (fun v_e v_fee ->
                            base_of_var v_e = base_of_var v_fee
                          ) v_e v_fee
                        ||
                        let bv_fee = LiquidBoundVariables.bv b_fee in
                               List.for_all (fun v_fee ->
                            not @@ StringSet.mem v_fee bv_fee) v_fee
                       ) in
                     if not ok then
                       Format.eprintf
                         "Mismatch between fee code for entry point \
                          %s : %a and %s : %a@."
                         s_e Format.(pp_print_list pp_print_string) v_e
                         s_fee Format.(pp_print_list pp_print_string) v_fee;
                     ok
                   | _ -> false
                 ) cases fee_cases
             with Invalid_argument _ -> false
        ->
        Some fee_cases
      | Some e ->
        Format.eprintf "Mismatch between fee code and entry points. \
                        Fee code cannot be decoded.@.";
        None
    in
    let cases = match fee_cases with
      | None -> List.map (fun (pat, c) -> pat, c, None) cases
      | Some fee_cases ->
        List.map2 (fun (pat, c) (_, fc) -> pat, c, Some fc) cases fee_cases
    in
    List.rev values,
    List.map (entry_of_case param_constrs top_storage) cases

  | Let { bnd_var; inline; bnd_val; body } ->
    let bv_val = LiquidBoundVariables.bv bnd_val in
    if StringSet.mem top_parameter bv_val ||
       StringSet.mem top_storage bv_val then raise Exit;
    decode_entries param_constrs top_parameter top_storage
      ({ val_name = bnd_var.nname;
         val_private = false;
         inline;
         val_exp = decode bnd_val } :: values) body fee_exp

  | Apply { prim = Prim_tuple; args = [ { desc = Const { const = CList [] } } as le;
                                        exp_sto ] } ->
    let values, entries =
      decode_entries param_constrs top_parameter top_storage values exp_sto fee_exp in
    let le = decode le in
    let entries = List.map (fun e ->
        { e with
          code = { e.code with
                   desc = Apply { prim = Prim_tuple; args = [le; e.code] } }
        }
      ) entries in
    values, entries

  | Apply { prim = Prim_tuple; args = [ { desc = MatchVariant _ } as exp_ops ;
                                        sto ] } ->
    let values, entries =
      decode_entries param_constrs top_parameter top_storage values exp_ops fee_exp in
    let sto = decode sto in
    let entries = List.map (fun e ->
        { e with
          code = { e.code with
                   desc = Apply { prim = Prim_tuple; args = [e.code; sto] } }
        }
      ) entries in
    values, entries

  | _ -> raise Exit

and move_outer_lets parameter storage values exp =
  match exp.desc with
  | Let { bnd_var; inline; bnd_val; body }
    when let bv = LiquidBoundVariables.bv bnd_val in
      not @@ StringSet.mem parameter bv &&
      not @@ StringSet.mem storage bv ->
    move_outer_lets parameter storage
      ({ val_name = bnd_var.nname;
         inline;
         val_private = false;
         val_exp = decode bnd_val } :: values) body
  | _ ->
    (* kept in reverse order because used as an accumulator to decode_entries *)
    values, exp

(* Recover multiple-entry points contract from a contract in
   single-entry point form *)
and decode_contract contract =
  let subs = List.map decode_contract contract.subs in
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Decode contract %s@."
      (LiquidNamespace.qual_contract_name contract);
  let c_init = match contract.c_init with
    | None -> None
    | Some i -> Some { i with init_body = decode i.init_body } in
  try match contract.entries with
    | [{ entry_sig = { entry_name = _;
                       parameter = Tsum (_, param_constrs);
                       parameter_name;
                       storage_name;
                     };
         code;
         fee_code;
       }] ->
      let values, code =
        move_outer_lets parameter_name storage_name [] code in
      let values, fee_code = match fee_code with
        | None -> values, None
        | Some fee_code ->
          let values, fee_code =
            move_outer_lets parameter_name storage_name values fee_code in
          values, Some fee_code in
      let values, entries =
        decode_entries param_constrs parameter_name storage_name
          values code fee_code in
      { contract with values ; entries; c_init; subs }
    | [({ entry_sig = { parameter;
                        parameter_name;
                        storage_name;
                      };
          code;
          fee_code;
        } as e)] ->
      (* sinle entry point contract *)
      let values, code =
        move_outer_lets parameter_name storage_name [] code in
      { contract with
        values = List.rev values;
        entries = [ { e with
                      code = decode code;
                      fee_code = match fee_code with
                        | None -> None
                        | Some fee_code -> Some (decode fee_code) } ];
        c_init;
        subs;
      }
    | _ -> raise Exit
  with Exit ->
    (* decode contract without decoding entries *)
    { contract with
      values =
        List.map (fun v -> { v with val_exp = decode v.val_exp })
          contract.values;
      entries =
        List.map (fun e -> { e with
                             code = decode e.code;
                             fee_code = match e.fee_code with
                               | None -> None
                               | Some fee_code -> Some (decode fee_code) }
                 ) contract.entries;
      c_init;
      subs;
    }
