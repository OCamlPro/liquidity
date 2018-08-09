(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* untype:
   * rename variables STRING/NUM into valid OCaml identifiers.
 *)

open LiquidTypes

let mk desc ty = mk desc ty

type env = {
    env_map : string StringMap.t;
    env_revmap : string StringMap.t;
  }

let empty_env () = {
      env_map = StringMap.empty;
      env_revmap = StringMap.empty;
    }

let new_binding tyvar var env =
  {
    env_map = StringMap.add tyvar var env.env_map;
    env_revmap = StringMap.add var tyvar env.env_revmap;
  }

let find_name env name =
  try
    StringMap.find name env.env_map
  with Not_found ->
       name

let base_of_var arg =
  try
    let pos = String.index arg '/' in
    String.sub arg 0 pos
  with Not_found ->
    raise (Invalid_argument ("base_of_var: "^arg))

let escape_var arg =
  try
    let pos = String.index arg '/' in
    String.sub arg 0 pos ^ "_" ^
      String.sub arg (pos+1) (String.length arg - pos - 1)
  with Not_found -> assert false

let find_free env var_arg bv =
  let var_arg' = base_of_var var_arg in
  if not (StringSet.mem var_arg bv) then
    ("_" ^ var_arg', env)
  else
  let rec iter n var_arg =
    match
      StringMap.find var_arg env.env_revmap
    with
    | var_arg'' ->
       if StringSet.mem var_arg'' bv then
         let var_arg' = Printf.sprintf "%s%d" var_arg' n in
         iter (n+1) var_arg'
      else var_arg
    | exception Not_found -> var_arg
  in
  let var_arg' = iter 0 var_arg' in
  let env' = new_binding var_arg var_arg' env in
  (var_arg', env')

(* To improve the naming of variables, we compute bound-variables for their
scopes. Unfortunately, without hash-consing, this can be quite expensive.
 *)

let rec untype (env : env) (code : (datatype, 'a) exp) : (datatype, 'b) exp =
  let desc =
    match code.desc with
    | If (cond, ifthen, ifelse) ->
       If (untype env cond, untype env ifthen, untype env ifelse)
    | Seq (x, y) -> Seq (untype env x, untype env y)
    | Const (loc, ty, cst) ->  Const (loc, ty, cst)
    | Failwith (err, loc) -> Failwith (untype env err, loc)

    | Apply(Prim_Left, loc, [arg; unused]) ->
       Constructor(loc, Left unused.ty, untype env arg)
    | Apply(Prim_Right, loc, [arg; unused]) ->
       Constructor(loc, Right unused.ty, untype env arg)
    | Apply (prim, loc, args) ->
      Apply(prim, loc, List.map (untype env) args)

    | Lambda (arg_name, arg_type, loc, body, res_type) ->
       let base = base_of_var arg_name in
       let env = empty_env () in
       let env = new_binding arg_name base env in
       Lambda (base, arg_type, loc, untype env body, Tunit)

    | Closure (arg_name, arg_type, loc, call_env, body, res_type) ->
       let call_env = List.map (fun (name, t) -> name, untype env t) call_env in
       let base = base_of_var arg_name in
       let env = empty_env () in
       let env = new_binding arg_name base env in
       Closure (base, arg_type, loc, call_env, untype env body, Tunit)

    | Var (name, loc) ->
       let name = find_name env name in
       Var (name, loc)

    | Project (loc, field, arg) ->
      Project (loc, field, untype env arg)

    | SetField (arg, loc, field, exp) ->
      SetField (untype env arg, loc, field, untype env exp)

    | Loop (var_arg, loc, body_exp, arg_exp) ->
       let arg_exp = untype env arg_exp in
       let bv = body_exp.bv in
       let (var_arg', env') = find_free env var_arg bv in
       Loop (var_arg', loc, untype env' body_exp, arg_exp)

    | Fold (prim, var_arg, loc, body_exp, arg_exp, acc_exp) ->
       let arg_exp = untype env arg_exp in
       let acc_exp = untype env acc_exp in
       let bv = body_exp.bv in
       let (var_arg', env') = find_free env var_arg bv in
       Fold (prim, var_arg', loc, untype env' body_exp, arg_exp, acc_exp)

    | Map (prim, var_arg, loc, body_exp, arg_exp) ->
       let arg_exp = untype env arg_exp in
       let bv = body_exp.bv in
       let (var_arg', env') = find_free env var_arg bv in
       Map (prim, var_arg', loc, untype env' body_exp, arg_exp)

    | MapFold (prim, var_arg, loc, body_exp, arg_exp, acc_exp) ->
       let arg_exp = untype env arg_exp in
       let acc_exp = untype env acc_exp in
       let bv = body_exp.bv in
       let (var_arg', env') = find_free env var_arg bv in
       MapFold (prim, var_arg', loc, untype env' body_exp, arg_exp, acc_exp)

    | Let (var_arg, inline, loc, arg_exp, body_exp) ->
       let arg_exp = untype env arg_exp in
       let bv = body_exp.bv in
       let (var_arg', env') = find_free env var_arg bv in
       Let (var_arg', inline, loc, arg_exp, untype env' body_exp)

    | MatchOption (exp, loc, ifnone, some_pat, ifsome) ->
       let bv = ifsome.bv in
       let (some_pat', env') = find_free env some_pat bv in
       MatchOption (untype env exp, loc,
                    untype env ifnone,
                    some_pat', untype env' ifsome)

    | MatchNat (exp, loc, p, ifplus, m, ifminus) ->
       let (p, env') = find_free env p ifplus.bv in
       let (m, env'') = find_free env m ifminus.bv in
       MatchNat (untype env exp, loc,
                 p, untype env' ifplus,
                 m, untype env'' ifminus)

    | MatchList (exp, loc, head_pat, tail_pat, ifcons, ifnil) ->
       let bv = ifcons.bv in
       let (head_pat, env') = find_free env head_pat bv in
       let (tail_pat, env'') = find_free env' tail_pat bv in
       MatchList (untype env exp, loc,
                  head_pat, tail_pat, untype env'' ifcons,
                  untype env ifnil)

    | Transfer (loc, contract_exp, amount_exp, entry, arg_exp) ->
      Transfer (loc,
                untype env contract_exp,
                untype env amount_exp,
                entry,
                untype env arg_exp)

    | MatchVariant (arg, loc, cases) ->
      let arg = untype env arg in
      let cases = List.map (function
          | CConstr (c, vars), carg ->
            let vars, carg = untype_case env vars carg in
            CConstr (c, vars), carg
          | _ -> assert false
        ) cases in
       MatchVariant (arg, loc, cases)

    | Constructor (loc, c, exp) -> Constructor (loc, c, untype env exp)

(*
    | MatchVariant (arg, loc,
                    [
                      CConstr ("Left", [left_var]), left_arg;
                      CConstr ("Right", [right_var]), right_arg;
                   ]) ->
       let arg = untype env arg in
       let left_var, left_arg = untype_case env left_var left_arg in
       let right_var, right_arg = untype_case env right_var right_arg in
       MatchVariant (arg, loc,
                    [
                      CConstr ("Left", [left_var]), left_arg;
                      CConstr ("Right", [right_var]), right_arg;
                    ])
*)

    | CreateContract (loc, args, contract) ->
      CreateContract (loc, List.map (untype env) args, untype_contract contract)

    | ContractAt (loc, addr, ty) ->
      ContractAt (loc, untype env addr, ty)

    | Unpack (loc, e, ty) ->
      Unpack (loc, untype env e, ty)

    | Record (loc, fields) ->
      Record (loc, List.map (fun (l, e) -> l, untype env e) fields)

    (* | _ ->
     *
     *    LiquidLoc.raise_error
     *      "untype: unimplemented code:\n%s%!"
     *      (LiquidPrinter.Liquid.string_of_code code) *)

  in
  mk desc code.ty

and untype_case env vars arg =
  let bv = arg.bv in
  let vars', env' = List.fold_left (fun (vars', env) var ->
      let (var', env') = find_free env var bv in
      var' :: vars', env'
    ) ([], env) vars in
  let arg' = untype env' arg in
  (List.rev vars', arg')

and untype_entry (entry : (datatype, 'a) exp entry) =
  let env = empty_env () in
  let base_parameter = base_of_var entry.entry_sig.parameter_name in
  let base_storage = base_of_var entry.entry_sig.storage_name in
  let env = new_binding entry.entry_sig.parameter_name base_parameter env in
  let env = new_binding entry.entry_sig.storage_name base_storage env in
  { entry_sig = { entry.entry_sig with
                  parameter_name = base_parameter;
                  storage_name = base_storage;
                };
    code = untype env entry.code }

and untype_contract contract =
  let contract = LiquidBoundVariables.bound_contract contract in
  { contract with
    values =
      List.map (fun (v, i, e) -> (v, i, untype (empty_env ()) e))
        contract.values;
    entries = List.map untype_entry contract.entries }

let untype_code code = untype (empty_env ()) code
