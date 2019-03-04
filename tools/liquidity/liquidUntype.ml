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

let mk ~loc desc ty = mk ~loc desc ty

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

let new_lbinding tyvar var env =
  new_binding tyvar.nname var.nname env

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

let base_of_lvar arg =
  { arg with nname = base_of_var arg.nname }

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
      ("_" (* ^ var_arg' *), env)
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

let find_lfree env v bv =
  let (nv, env) = find_free env v.nname bv in
  { v with nname = nv}, env

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
    | Const { ty; const } -> Const { ty; const = untype_const const }
    | Failwith arg -> Failwith (untype env arg)

    | Apply { prim = Prim_Left; args =  [arg; unused] } ->
      Constructor { constr = Left unused.ty; arg = untype env arg }
    | Apply { prim = Prim_Right; args = [arg; unused] } ->
      Constructor { constr = Right unused.ty; arg =  untype env arg }
    | Apply { prim; args } ->
      Apply { prim; args = List.map (untype env) args }

    | Lambda lam ->
      Lambda (untype_lambda lam)

    | Closure { arg_name; arg_ty; call_env; body } ->
      let call_env = List.map (fun (name, t) -> name, untype env t) call_env in
      let base = base_of_lvar arg_name in
      let env = empty_env () in
      let env = new_lbinding arg_name base env in
      Closure { arg_name = base; arg_ty; call_env;
                body = untype env body; ret_ty = Tunit }

    | Var name ->
      let name = find_name env name in
      Var name

    | Project { field; record } ->
      Project { field; record = untype env record }

    | SetField { record; field; set_val } ->
      SetField { record = untype env record;
                 field;
                 set_val = untype env set_val }

    | Loop { arg_name; body; arg } ->
      let arg = untype env arg in
      let (arg_name, env) = find_lfree env arg_name body.bv in
      let body = untype env body in
      Loop { arg_name; body; arg }

    | LoopLeft { arg_name; body; arg; acc } ->
      let arg = untype env arg in
      let acc =
        match acc with None -> None | Some acc -> Some (untype env acc) in
      let (arg_name, env) = find_lfree env arg_name body.bv in
      let body = untype env body in
      LoopLeft { arg_name; body; arg; acc }

    | Fold { prim; arg_name; body; arg; acc } ->
      let arg = untype env arg in
      let acc = untype env acc in
      let (arg_name, env) = find_lfree env arg_name body.bv in
      let body = untype env body in
      Fold { prim; arg_name; body; arg; acc }

    | Map { prim; arg_name; body; arg } ->
      let arg = untype env arg in
      let (arg_name, env) = find_lfree env arg_name body.bv in
      let body = untype env body in
      Map { prim; arg_name; body; arg }

    | MapFold { prim; arg_name; body; arg; acc } ->
      let arg = untype env arg in
      let acc = untype env acc in
      let (arg_name, env) = find_lfree env arg_name body.bv in
      let body = untype env body in
      MapFold { prim; arg_name; body; arg; acc }

    | Let { bnd_var; inline; bnd_val; body } ->
      let bnd_val = untype env bnd_val in
      let (bnd_var, env) = find_lfree env bnd_var body.bv in
      let body = untype env body in
      Let { bnd_var; inline; bnd_val; body }

    | MatchOption { arg; ifnone; some_name; ifsome } ->
      let arg = untype env arg in
      let ifnone = untype env ifnone in
      let (some_name, env) = find_lfree env some_name ifsome.bv in
      let ifsome = untype env ifsome in
      MatchOption { arg; ifnone; some_name; ifsome }

    | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
      let (plus_name, env') = find_lfree env plus_name ifplus.bv in
      let (minus_name, env'') = find_lfree env minus_name ifminus.bv in
      let arg = untype env arg in
      let ifplus = untype env' ifplus in
      let ifminus = untype env'' ifminus in
      MatchNat { arg; plus_name; ifplus; minus_name; ifminus }

    | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
      let arg = untype env arg in
      let ifnil = untype env ifnil in
      let bv = ifcons.bv in
      let (head_name, env) = find_lfree env head_name bv in
      let (tail_name, env) = find_lfree env tail_name bv in
      let ifcons = untype env ifcons in
      MatchList { arg; head_name; tail_name; ifcons; ifnil }

    | Transfer { dest; amount } ->
      Transfer { dest = untype env dest;
                 amount = untype env amount }

    | Call { contract; amount; entry; arg } ->
      Call { contract = untype env contract;
             amount = untype env amount;
             entry;
             arg = untype env arg }

    | MatchVariant { arg; cases } ->
      let arg = untype env arg in
      let cases = List.map (function
          | PConstr (c, vars), body ->
            let vars, body = untype_case env vars body in
            PConstr (c, vars), body
          | PAny, body -> PAny, untype env body
        ) cases in
      MatchVariant { arg; cases }

    | Constructor { constr; arg } ->
      Constructor { constr; arg = untype env arg }

    | CreateContract { args; contract } ->
      CreateContract { args = List.map (untype env) args;
                       contract = untype_contract contract }

    | ContractAt { arg; c_sig } ->
      ContractAt { arg = untype env arg; c_sig }

    | Unpack { arg; ty } ->
      Unpack { arg = untype env arg; ty }

    | Record fields ->
      Record (List.map (fun (l, e) -> l, untype env e) fields)

    | TypeAnnot { e; ty } ->
      TypeAnnot { e = untype env e; ty }

    | Type ty -> Type ty
    (* | _ ->
     *
     *    LiquidLoc.raise_error
     *      "untype: unimplemented code:\n%s%!"
     *      (LiquidPrinter.Liquid.string_of_code code) *)

  in
  mk ~loc:code.loc desc code.ty

and untype_lambda { arg_name; arg_ty; body; ret_ty; recursive } =
  let base = base_of_lvar arg_name in
  let env = empty_env () in
  let env = new_lbinding arg_name base env in
  let recursive, env, ret_ty = match recursive with
    | None -> recursive, env, Tunit
    | Some f ->
      let f_base = base_of_var f in
      let env = new_binding f f_base env in
      (Some f_base, env, ret_ty)
  in
  { arg_name = base; arg_ty;
    body = untype env body; ret_ty;
    recursive }

and untype_const c = match c with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CContract _ | CSignature _ | CNone  | CKey_hash _
    | CAddress _ ) as c -> c

  | CSome x -> CSome (untype_const x)
  | CLeft x -> CLeft (untype_const x)
  | CRight x -> CRight (untype_const x)

  | CTuple xs -> CTuple (List.map (untype_const) xs)
  | CList xs -> CList (List.map (untype_const) xs)
  | CSet xs -> CSet (List.map (untype_const) xs)

  | CMap l ->
    CMap (List.map (fun (x,y) -> untype_const x, untype_const y) l)

  | CBigMap l ->
    CBigMap (List.map (fun (x,y) -> untype_const x, untype_const y) l)

  | CRecord labels ->
    CRecord (List.map (fun (f, x) -> f, untype_const x) labels)

  | CConstr (constr, x) ->
    CConstr (constr, untype_const x)

  | CLambda lam ->
    CLambda (untype_lambda lam)

and untype_case env vars arg =
  let bv = arg.bv in
  let vars', env' = List.fold_left (fun (vars', env) var ->
      let (var', env') = find_free env var bv in
      var' :: vars', env'
    ) ([], env) vars in
  let arg' = untype env' arg in
  (List.rev vars', arg')

and untype_entry env (entry : (datatype, 'a) exp entry) =
  let bv = entry.code.bv in
  let base_parameter, env = find_free env entry.entry_sig.parameter_name bv in
  let base_storage, env = find_free env entry.entry_sig.storage_name bv in
  let env = new_binding entry.entry_sig.parameter_name base_parameter env in
  let env = new_binding entry.entry_sig.storage_name base_storage env in
  { entry_sig = { entry.entry_sig with
                  parameter_name = base_parameter;
                  storage_name = base_storage;
                };
    code = untype env entry.code }

and untype_contract contract =
  let subs = List.map untype_contract contract.subs in
  let contract = LiquidBoundVariables.bound_contract contract in
  let values, env =
    List.fold_left (fun (acc, env) v ->
        let val_name = base_of_var v.val_name in
        let env = new_binding v.val_name val_name env in
        let value = { v with val_name; val_exp = untype env v.val_exp } in
        value :: acc, env) ([], empty_env ()) contract.values in
  let values = List.rev values in
  let entries = List.map (untype_entry env) contract.entries in
  let c_init = match contract.c_init with
    | None -> None
    | Some i -> Some { i with init_body = untype env i.init_body } in
  { contract with values; entries; c_init; subs }

let untype_code code = untype (empty_env ()) code
