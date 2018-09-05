(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2018       .                                          *)
(*    OCamlPro SAS <contract@ocamlpro.com>                                *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let mk ?name (desc: (datatype, typed) exp_desc) ty = mk ?name desc ty

let rec decode ( exp : encoded_exp ) : typed_exp =
  match exp.desc with
  | Const c ->
    mk ?name:exp.name (Const c) exp.ty

  | Let { bnd_var; inline; loc; bnd_val; body } ->
     let bnd_val = decode bnd_val in
     let body = decode body in
     mk ?name:exp.name (Let { bnd_var; inline; loc; bnd_val; body }) body.ty

  | Var v ->
    mk ?name:exp.name (Var v) exp.ty

  | Project { loc; field; record } ->
    let record = decode record in
    mk ?name:exp.name (Project { loc; field; record }) exp.ty

  | SetField { record; loc; field; set_val } ->
    let record = decode record in
    let set_val = decode set_val in
    mk ?name:exp.name (SetField { record; loc; field; set_val }) exp.ty

  | Seq (exp1, exp2) ->
    let exp1 = decode exp1 in
    let exp2 = decode exp2 in
    mk ?name:exp.name (Seq (exp1, exp2)) exp.ty

  | If { cond; ifthen; ifelse } ->
    let cond = decode cond in
    let ifthen = decode ifthen in
    let ifelse = decode ifelse in
    mk ?name:exp.name (If { cond; ifthen; ifelse }) exp.ty

  | Transfer { loc; contract; amount; entry; arg } ->
    let amount = decode amount in
    let contract = decode contract in
    let desc = match entry, arg.desc with
      | None, Constructor { constr = Constr c; arg } when is_entry_case c ->
        let entry = Some (entry_name_of_case c) in
        let arg = decode arg in
        Transfer { loc; contract; amount; entry; arg }
      | _, _ ->
        let arg = decode arg in
        Transfer { loc; contract; amount; entry; arg }
    in
    mk ?name:exp.name desc exp.ty

  | Failwith { arg; loc } ->
    let arg = decode arg in
    mk (Failwith { arg; loc }) Tfail

  | Apply { prim = Prim_unknown } -> assert false

  (* TODO *)
  (* List.rev -> List.reduce (::) *)

  | Apply { prim = Prim_concat | Prim_string_concat | Prim_bytes_concat;
            loc;
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
    mk ?name:exp.name
      (Apply { prim = Prim_concat_two; loc; args = [s1; s2] }) exp.ty

  | Apply { prim; loc; args } ->
    let args = List.map decode args in
    mk ?name:exp.name (Apply { prim; loc; args }) exp.ty

  | MatchOption { arg; loc; ifnone; some_name; ifsome } ->
    let arg = decode arg in
    let ifnone = decode ifnone in
    let ifsome = decode ifsome in
    mk ?name:exp.name
      (MatchOption { arg; loc; ifnone; some_name; ifsome }) exp.ty

  | MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus } ->
    let arg = decode arg in
    let ifplus = decode ifplus in
    let ifminus = decode ifminus in
    mk ?name:exp.name
      (MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus }) exp.ty

  | Loop { arg_name; loc; body; arg } ->
     let arg = decode arg in
     let body = decode body in
     mk ?name:exp.name (Loop { arg_name; loc; body; arg }) exp.ty

  | Fold { prim; arg_name; loc; body; arg; acc } ->
    let arg = decode arg in
    let acc = decode acc in
    let body = decode body in
    mk ?name:exp.name (Fold { prim; arg_name; loc; body; arg; acc }) exp.ty

  | Map { prim; arg_name; loc; body; arg } ->
    let arg = decode arg in
    let body = decode body in
    mk ?name:exp.name (Map { prim; arg_name; loc; body; arg }) exp.ty

  | MapFold { prim; arg_name; loc; body; arg; acc } ->
    let arg = decode arg in
    let acc = decode acc in
    let body = decode body in
    mk ?name:exp.name (MapFold { prim; arg_name; loc; body; arg; acc }) exp.ty

  | MatchList { arg; loc; head_name; tail_name; ifcons; ifnil } ->
    let arg = decode arg in
    let ifcons = decode ifcons in
    let ifnil = decode ifnil in
    mk ?name:exp.name
      (MatchList { arg; loc; head_name; tail_name; ifcons; ifnil }) exp.ty

  | Lambda { arg_name; arg_ty; loc; body; ret_ty } ->
    let body = decode body in
    mk ?name:exp.name
      (Lambda { arg_name; arg_ty; loc; body; ret_ty }) exp.ty

  | Closure { arg_name; arg_ty; loc; call_env; body; ret_ty } ->
    let call_env = List.map (fun (v, e) -> v, decode e) call_env in
    let body = decode body in
    mk ?name:exp.name
      (Closure { arg_name; arg_ty; loc; call_env; body; ret_ty }) exp.ty

  | Record { loc; fields } ->
    let fields = List.map (fun (label, exp) ->
        label, decode exp
      ) fields in
    mk ?name:exp.name (Record { loc; fields }) exp.ty

  | Constructor { loc; constr; arg } ->
    let arg = decode arg in
    mk ?name:exp.name (Constructor { loc; constr; arg }) exp.ty

  | MatchVariant { arg; loc; cases } ->
    let arg = decode arg in
    let cases = List.map (fun (pat, e) -> pat, decode e) cases in
    mk ?name:exp.name (MatchVariant { arg; loc; cases }) exp.ty

  | ContractAt { loc; arg; c_sig } ->
    let arg = decode arg in
    mk ?name:exp.name (ContractAt { loc; arg; c_sig }) exp.ty

  | Unpack { loc; arg; ty } ->
    let arg = decode arg in
    mk ?name:exp.name (Unpack { loc; arg; ty }) exp.ty

  | CreateContract { loc; args; contract } ->
    let args = List.map decode args in
    let contract = decode_contract contract in
    mk ?name:exp.name (CreateContract { loc; args; contract }) exp.ty

and entry_of_case param_constrs top_storage (pat, body) =
  match pat, body.desc with
  | CConstr (s, [parameter_name]),
    Let { bnd_var = storage_name;
          bnd_val = { desc = Var { name = var_storage }};
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
  | MatchVariant { arg = { desc = Var { name = var_parameter}}; cases }
    when var_parameter = top_parameter &&
         List.for_all (function
             | CConstr (s, _), _ -> is_entry_case s
             | _ -> false) cases
    ->
    List.rev values, List.map (entry_of_case param_constrs top_storage) cases
  | Let { bnd_var; inline; bnd_val; body } ->
    decode_entries param_constrs top_parameter top_storage
      ((bnd_var, inline, decode bnd_val) :: values) body
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
