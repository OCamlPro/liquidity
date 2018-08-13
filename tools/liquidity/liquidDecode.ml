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
  | Const (loc, ty, cst) ->
    mk ?name:exp.name (Const (loc, ty, cst)) exp.ty

  | Let (name, inline, loc, e, body) ->
     let e = decode e in
     let body = decode body in
     mk ?name:exp.name (Let (name, inline, loc, e, body)) body.ty

  | Var (name, loc) ->
    mk ?name:exp.name (Var (name, loc)) exp.ty

  | Project (loc, label, arg) ->
    let arg = decode arg in
    mk ?name:exp.name (Project (loc, label, arg)) exp.ty

  | SetField (arg, loc, label, e) ->
    let arg = decode arg in
    let e = decode e in
    mk ?name:exp.name (SetField (arg, loc, label, e)) exp.ty

  | Seq (exp1, exp2) ->
    let exp1 = decode exp1 in
    let exp2 = decode exp2 in
    mk ?name:exp.name (Seq (exp1, exp2)) exp.ty

  | If (cond, ifthen, ifelse) ->
    let cond = decode cond in
    let ifthen = decode ifthen in
    let ifelse = decode ifelse in
    mk ?name:exp.name (If (cond, ifthen, ifelse)) exp.ty

  | Transfer (loc, contract_exp, tez_exp, entry, arg_exp) ->
    let tez_exp = decode tez_exp in
    let contract_exp = decode contract_exp in
    let desc = match entry, arg_exp.desc with
      | None, Constructor (loc, Constr c, arg_exp) when is_entry_case c ->
        let entry = entry_name_of_case c in
        let arg_exp = decode arg_exp in
        Transfer (loc, contract_exp, tez_exp, Some entry, arg_exp)
      | _, _ ->
        let arg_exp = decode arg_exp in
        Transfer (loc, contract_exp, tez_exp, entry, arg_exp)
    in
    mk ?name:exp.name desc exp.ty

  | Failwith (err, loc) ->
    let err = decode err in
    mk (Failwith (err, loc)) Tfail

  | Apply (Prim_unknown, _, _) -> assert false

  (* TODO *)
  (* List.rev -> List.reduce (::) *)
  (* concat x y => concat [x; y] *)

  | Apply (prim, loc, args) ->
    let args = List.map decode args in
    mk ?name:exp.name (Apply (prim, loc, args)) exp.ty

  | MatchOption (arg, loc, ifnone, name, ifsome) ->
    let arg = decode arg in
    let ifnone = decode ifnone in
    let ifsome = decode ifsome in
    mk ?name:exp.name
      (MatchOption (arg, loc, ifnone, name, ifsome)) exp.ty

  | MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus) ->
    let arg = decode arg in
    let ifplus = decode ifplus in
    let ifminus = decode ifminus in
    mk ?name:exp.name
      (MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus)) exp.ty

  | Loop (name, loc, body, arg) ->
     let arg = decode arg in
     let body = decode body in
     mk ?name:exp.name (Loop (name, loc, body, arg)) exp.ty

  | Fold (prim, name, loc, body, arg, acc) ->
    let arg = decode arg in
    let acc = decode acc in
    let body = decode body in
    mk ?name:exp.name (Fold (prim, name, loc, body, arg, acc)) exp.ty

  | Map (prim, name, loc, body, arg) ->
    let arg = decode arg in
    let body = decode body in
    mk ?name:exp.name (Map (prim, name, loc, body, arg)) exp.ty

  | MapFold (prim, name, loc, body, arg, acc) ->
    let arg = decode arg in
    let acc = decode acc in
    let body = decode body in
    mk ?name:exp.name (MapFold (prim, name, loc, body, arg, acc)) exp.ty

  | MatchList (arg, loc, head_name, tail_name, ifcons, ifnil) ->
    let arg = decode arg in
    let ifcons = decode ifcons in
    let ifnil = decode ifnil in
    mk ?name:exp.name
      (MatchList (arg, loc, head_name, tail_name, ifcons, ifnil)) exp.ty

  | Lambda (arg_name, arg_type, loc, body, ret_ty) ->
    let body = decode body in
    mk ?name:exp.name
      (Lambda (arg_name, arg_type, loc, body, ret_ty)) exp.ty

  | Closure (arg_name, arg_type, loc, clos_env, body, ret_ty) ->
    let clos_env = List.map (fun (v, e) -> v, decode e) clos_env in
    let body = decode body in
    mk ?name:exp.name
      (Closure (arg_name, arg_type, loc, clos_env, body, ret_ty)) exp.ty

  | Record (loc, fields) ->
    let fields = List.map (fun (label, exp) ->
        label, decode exp
      ) fields in
    mk ?name:exp.name (Record(loc, fields)) exp.ty

  | Constructor(loc, c, arg) ->
    let arg = decode arg in
    mk ?name:exp.name (Constructor(loc, c, arg)) exp.ty

  | MatchVariant (arg, loc, cases) ->
    let arg = decode arg in
    let cases = List.map (fun (pat, e) -> pat, decode e) cases in
    mk ?name:exp.name (MatchVariant (arg, loc, cases)) exp.ty

  | ContractAt (loc, addr, ty) ->
    let addr = decode addr in
    mk ?name:exp.name (ContractAt (loc, addr, ty)) exp.ty

  | Unpack (loc, e, ty) ->
    let e = decode e in
    mk ?name:exp.name (Unpack (loc, e, ty)) exp.ty

  | CreateContract (loc, args, contract) ->
    let args = List.map decode args in
    let contract = decode_contract contract in
    mk ?name:exp.name (CreateContract (loc, args, contract)) exp.ty

and entry_of_case param_constrs top_storage (pat, body) =
  match pat, body.desc with
  | CConstr (s, [parameter_name]),
    Let (storage_name, _, _, { desc = Var (var_storage, _)}, code)
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
  | MatchVariant ({ desc = Var (var_parameter, _)} , loc, cases)
    when var_parameter = top_parameter &&
         List.for_all (function
             | CConstr (s, _), _ -> is_entry_case s
             | _ -> false) cases
    ->
    List.rev values, List.map (entry_of_case param_constrs top_storage) cases
  | Let (v, inline, _loc, exp, body) ->
    decode_entries param_constrs top_parameter top_storage
      ((v, inline, decode exp) :: values) body
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
