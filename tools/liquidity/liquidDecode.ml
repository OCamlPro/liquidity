(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2018       .                                          *)
(*    OCamlPro SAS <contract@ocamlpro.com>                                *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let mk ?name ~loc (desc: (datatype, typed) exp_desc) ty = mk ?name ~loc desc ty

let rec decode_const (c : encoded_const) : typed_const = match c with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CContract _ | CSignature _ | CNone  | CKey_hash _
    | CAddress _ ) as c -> c
  | CSome x -> CSome (decode_const x)
  | CLeft x -> CLeft (decode_const x)
  | CRight x -> CRight (decode_const x)
  | CTuple xs -> CTuple (List.map (decode_const) xs)
  | CList xs -> CList (List.map (decode_const) xs)
  | CSet xs -> CSet (List.map (decode_const) xs)
  | CMap l ->
    CMap (List.map (fun (x,y) -> decode_const x, decode_const y) l)
  | CBigMap l ->
    CBigMap (List.map (fun (x,y) -> decode_const x, decode_const y) l)
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

  | Call { contract; amount; entry; arg } ->
    let amount = decode amount in
    let contract = decode contract in
    let desc = match entry, arg.desc with
      | None, Constructor { constr = Constr c; arg } when is_entry_case c ->
        let entry = Some (entry_name_of_case c) in
        let arg = decode arg in
        Call { contract; amount; entry; arg }
      | _, _ ->
        let arg = decode arg in
        Call { contract; amount; entry; arg }
    in
    mk ?name:exp.name ~loc desc exp.ty

  | Failwith arg ->
    let arg = decode arg in
    mk ~loc (Failwith arg) Tfail

  | Apply { prim = Prim_unknown } -> assert false

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

  | ContractAt { arg; c_sig } ->
    let arg = decode arg in
    mk ?name:exp.name ~loc (ContractAt { arg; c_sig }) exp.ty

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
and entry_of_case param_constrs top_storage (pat, body) =
  match pat, body.desc with
  | PConstr (s, [parameter_name]),
    Let { bnd_var = { nname = storage_name };
          bnd_val = { desc = Var var_storage };
          body = code }
    when is_entry_case s && var_storage = top_storage ->
    let entry_name = entry_name_of_case s in
    let parameter = List.assoc s param_constrs in
    {
      entry_sig = {
        entry_name;
        parameter;
        parameter_name;
        storage_name;
      };
      code = decode code;
    }
  | PConstr (s, [parameter_name]), _
    when is_entry_case s ->
    let entry_name = entry_name_of_case s in
    let parameter = List.assoc s param_constrs in
    {
      entry_sig = {
        entry_name;
        parameter;
        parameter_name;
        storage_name = top_storage ;
      };
      code = decode body;
    }
  | _ -> raise Exit

(* Recover entries points from a top-level pattern matching, also move
   top-level (at the level of the matching branch) local definitions
   to global definitions *)
and decode_entries param_constrs top_parameter top_storage values exp =
  match exp.desc with
  | MatchVariant { arg = { desc = Var var_parameter}; cases }
    when var_parameter = top_parameter &&
         List.for_all (function
             | PConstr (s, _), _ -> is_entry_case s
             | _ -> false) cases
    ->
    List.rev values, List.map (entry_of_case param_constrs top_storage) cases
  | Let { bnd_var; inline; bnd_val; body } ->
    decode_entries param_constrs top_parameter top_storage
      ({ val_name = bnd_var.nname;
         val_private = false;
         inline;
         val_exp = decode bnd_val } :: values) body
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
  let c_init = match contract.c_init with
    | None -> None
    | Some i -> Some { i with init_body = decode i.init_body } in
  try match contract.entries with
    | [{ entry_sig = { entry_name = "main";
                       parameter = Tsum (_, param_constrs);
                       parameter_name;
                       storage_name;
                     };
         code;
       }] ->
      (* multi-entry points encoded contract *)
      let values, code =
        move_outer_lets parameter_name storage_name [] code in
      let values, entries =
        decode_entries param_constrs parameter_name storage_name values code in
      { contract with values ; entries; c_init; subs }
    | [({ entry_sig = { parameter;
                        parameter_name;
                        storage_name;
                      };
          code;
        } as e)] ->
      (* sinle entry point contract *)
      let values, code =
        move_outer_lets parameter_name storage_name [] code in
      { contract with
        values = List.rev values;
        entries = [ { e with code = decode code } ];
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
        List.map (fun e -> { e with code = decode e.code }) contract.entries;
      c_init;
      subs;
    }
