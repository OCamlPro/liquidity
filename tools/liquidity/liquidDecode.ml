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

let rec decode ( exp : encoded_exp ) : typed_exp =
  let loc = exp.loc in
  match exp.desc with
  | Const c ->
    mk ?name:exp.name ~loc (Const c) exp.ty

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

  | Transfer { contract; amount; entry; arg } ->
    let amount = decode amount in
    let contract = decode contract in
    let desc = match entry, arg.desc with
      | None, Constructor { constr = Constr c; arg } when is_entry_case c ->
        let entry = Some (entry_name_of_case c) in
        let arg = decode arg in
        Transfer { contract; amount; entry; arg }
      | _, _ ->
        let arg = decode arg in
        Transfer { contract; amount; entry; arg }
    in
    mk ?name:exp.name ~loc desc exp.ty

  | Failwith arg ->
    let arg = decode arg in
    mk ~loc (Failwith arg) Tfail

  | Apply { prim = Prim_unknown } -> assert false

  (* TODO *)
  (* List.rev -> List.reduce (::) *)

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

  | Lambda { arg_name; arg_ty; body; ret_ty } ->
    let body = decode body in
    mk ?name:exp.name ~loc
      (Lambda { arg_name; arg_ty; body; ret_ty }) exp.ty

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

and entry_of_case param_constrs top_storage (pat, body) =
  match pat, body.desc with
  | CConstr (s, [parameter_name]),
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
  | CConstr (s, [parameter_name]), _
    when is_entry_case s ->
    let entry_name = entry_name_of_case s in
    let parameter = List.assoc s param_constrs in
    {
      entry_sig = {
        entry_name;
        parameter;
        parameter_name;
        storage_name = "storage/1";
      };
      code = decode body;
    }
  | _ -> raise Exit


and decode_entries param_constrs top_parameter top_storage values exp =
  match exp.desc with
  | MatchVariant { arg = { desc = Var var_parameter}; cases }
    when var_parameter = top_parameter &&
         List.for_all (function
             | CConstr (s, _), _ -> is_entry_case s
             | _ -> false) cases
    ->
    List.rev values, List.map (entry_of_case param_constrs top_storage) cases
  | Let { bnd_var; inline; bnd_val; body } ->
    decode_entries param_constrs top_parameter top_storage
      ((bnd_var.nname, inline, decode bnd_val) :: values) body
  | _ -> raise Exit

and decode_contract contract =
  try match contract.entries with
    | [{ entry_sig = { entry_name = "main";
                       parameter = Tsum (_, param_constrs);
                       parameter_name;
                       storage_name;
                     };
         code;
       }] ->
      (* raise Exit; *)
      let values, entries =
        decode_entries param_constrs parameter_name storage_name [] code in
      { contract with values ; entries }
    | _ -> raise Exit
  with Exit ->
    (* decode contract without decoding entries *)
    { contract with
      values = List.map (fun (v, i, e) -> (v, i, decode e)) contract.values;
      entries =
        List.map (fun e -> { e with code = decode e.code }) contract.entries;
    }
