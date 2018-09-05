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
    if var_arg' <> "_" then
      ("_" ^ var_arg', env)
    else
      (var_arg', env)
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
    | If { cond; ifthen; ifelse } ->
      If { cond = untype env cond;
           ifthen = untype env ifthen;
           ifelse = untype env ifelse }
    | Seq (x, y) -> Seq (untype env x, untype env y)
    | Const c -> Const c
    | Failwith { arg; loc } -> Failwith { arg = untype env arg; loc }

    | Apply { prim = Prim_Left; loc; args =  [arg; unused] } ->
       Constructor { loc; constr = Left unused.ty; arg = untype env arg }
    | Apply { prim = Prim_Right; loc; args = [arg; unused] } ->
       Constructor { loc; constr = Right unused.ty; arg =  untype env arg }
    | Apply { prim; loc; args } ->
      Apply { prim; loc; args = List.map (untype env) args }

    | Lambda { arg_name; arg_ty; loc; body } ->
       let base = base_of_var arg_name in
       let env = empty_env () in
       let env = new_binding arg_name base env in
       Lambda { arg_name = base; arg_ty; loc;
                body = untype env body; ret_ty = Tunit }

    | Closure { arg_name; arg_ty; loc; call_env; body } ->
       let call_env = List.map (fun (name, t) -> name, untype env t) call_env in
       let base = base_of_var arg_name in
       let env = empty_env () in
       let env = new_binding arg_name base env in
       Closure { arg_name = base; arg_ty; loc; call_env;
                 body = untype env body; ret_ty = Tunit }

    | Var { name; loc } ->
       let name = find_name env name in
       Var { name; loc }

    | Project { loc; field; record } ->
      Project { loc; field; record = untype env record }

    | SetField { record; loc; field; set_val } ->
      SetField { record = untype env record;
                 loc;
                 field;
                 set_val = untype env set_val }

    | Loop { arg_name; loc; body; arg } ->
       let arg = untype env arg in
       let (arg_name, env) = find_free env arg_name body.bv in
       let body = untype env body in
       Loop { arg_name; loc; body; arg }

    | Fold { prim; arg_name; loc; body; arg; acc } ->
       let arg = untype env arg in
       let acc = untype env acc in
       let (arg_name, env) = find_free env arg_name body.bv in
       let body = untype env body in
       Fold { prim; arg_name; loc; body; arg; acc }

    | Map { prim; arg_name; loc; body; arg } ->
       let arg = untype env arg in
       let (arg_name, env) = find_free env arg_name body.bv in
       let body = untype env body in
       Map { prim; arg_name; loc; body; arg }

    | MapFold { prim; arg_name; loc; body; arg; acc } ->
       let arg = untype env arg in
       let acc = untype env acc in
       let (arg_name, env) = find_free env arg_name body.bv in
       let body = untype env body in
       MapFold { prim; arg_name; loc; body; arg; acc }

    | Let { bnd_var; inline; loc; bnd_val; body } ->
       let bnd_val = untype env bnd_val in
       let (bnd_var, env) = find_free env bnd_var body.bv in
       let body = untype env body in
       Let { bnd_var; inline; loc; bnd_val; body }

    | MatchOption { arg; loc; ifnone; some_name; ifsome } ->
      let arg = untype env arg in
      let ifnone = untype env ifnone in
      let (some_name, env) = find_free env some_name ifsome.bv in
      let ifsome = untype env ifsome in
      MatchOption { arg; loc; ifnone; some_name; ifsome }

    | MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus } ->
      let (plus_name, env') = find_free env plus_name ifplus.bv in
      let (minus_name, env'') = find_free env minus_name ifminus.bv in
      let arg = untype env arg in
      let ifplus = untype env' ifplus in
      let ifminus = untype env'' ifminus in
      MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus }

    | MatchList { arg; loc; head_name; tail_name; ifcons; ifnil } ->
      let arg = untype env arg in
      let ifnil = untype env ifnil in
      let bv = ifcons.bv in
      let (head_name, env) = find_free env head_name bv in
      let (tail_name, env) = find_free env tail_name bv in
      let ifcons = untype env ifcons in
      MatchList { arg; loc; head_name; tail_name; ifcons; ifnil }

    | Transfer { loc; contract; amount; entry; arg } ->
      Transfer { loc;
                 contract = untype env contract;
                 amount = untype env amount;
                 entry;
                 arg = untype env arg }

    | MatchVariant { arg; loc; cases } ->
      let arg = untype env arg in
      let cases = List.map (function
          | CConstr (c, vars), body ->
            let vars, body = untype_case env vars body in
            CConstr (c, vars), body
          | CAny, body -> CAny, untype env body
        ) cases in
      MatchVariant { arg; loc; cases }

    | Constructor { loc; constr; arg } ->
      Constructor { loc; constr; arg = untype env arg }

    | CreateContract { loc; args; contract } ->
      CreateContract { loc;
                       args = List.map (untype env) args;
                       contract = untype_contract contract }

    | ContractAt { loc; arg; c_sig } ->
      ContractAt { loc; arg = untype env arg; c_sig }

    | Unpack { loc; arg; ty } ->
      Unpack { loc; arg = untype env arg; ty }

    | Record { loc; fields } ->
      Record { loc; fields = List.map (fun (l, e) -> l, untype env e) fields }

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

and untype_entry env (entry : (datatype, 'a) exp entry) =
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
  let values, env =
    List.fold_left (fun (acc, env) (v, i, e) ->
        let v' = base_of_var v in
        let env = new_binding v v' env in
        let value = (v', i, untype env e) in
        value :: acc, env) ([], empty_env ()) contract.values in
  let values = List.rev values in
  let entries = List.map (untype_entry env) contract.entries in
  { contract with values; entries }

let untype_code code = untype (empty_env ()) code
