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

(* TODO: we don't handle correctly all the occurrences of Tfail, i.e.
   when an error occurs in a sub-part of a type and another type is expected,
   we should probably unify.
*)

(*
  Typecheck an AST.
 *)

open LiquidTypes
open LiquidNamespace
open LiquidInfer
open LiquidPrinter.Liquid

let noloc env = LiquidLoc.loc_in_file env.env.filename

let error loc msg =
  LiquidLoc.raise_error ~loc ("Type error:  " ^^ msg ^^ "%!")

(* Two types are comparable if they are equal and of a comparable type *)
let check_comparable loc prim ty1 ty2 =
  if not (comparable_type ty1 && eq_types ty1 ty2) then
    error loc "arguments of %s not comparable: %s\nwith\n%s\n"
      (LiquidTypes.string_of_primitive prim)
      (string_of_type ty1)
      (string_of_type ty2)

let new_binding env name ?effect ?(gen=false) ty =
  let effect = match effect with
    | Some e -> e
    | None -> LiquidTypes.may_contain_arrow_type ty in
  let count = ref 0 in
  let ftvars = free_tvars ty in
  (* if gen then begin
   *   Printf.printf "Generalizing %s with : " name;
   *   StringSet.iter (fun v ->
   *       Printf.printf "%s " v
   *     ) (StringSet.diff ftvars env.ftvars);
   *   Printf.printf "\n"
   *   end; *)
  let tys, ftvars =
    if gen then (StringSet.diff ftvars env.ftvars, ty), env.ftvars
    else (StringSet.empty, ty), StringSet.union env.ftvars ftvars in
  let env = { env with
              vars = StringMap.add name (name, tys, effect) env.vars;
              vars_counts = StringMap.add name count env.vars_counts;
              ftvars;
            } in
  (env, count)

let check_used env name count =
  if env.warnings && !count = 0 && name.nname.[0] <> '_' then begin
    LiquidLoc.warn name.nloc (Unused name.nname)
  end

let check_used_in_env env name =
  try
    let count = StringMap.find name.nname env.vars_counts in
    check_used env name count;
  with Not_found ->
  match env.clos_env with
  | None -> check_used env name (ref 0)
  | Some ce ->
    try
      let _, (count, _) = StringMap.find name.nname ce.env_bindings in
      check_used env name count;
    with Not_found ->
      check_used env name (ref 0)

(* Find variable name in either the global environment *)
let find_var ?(count_used=true) env loc name =
  try
    let (name, tys, effect) = StringMap.find name env.vars in
    let count = StringMap.find name env.vars_counts in
    if count_used then incr count;
    { (mk (Var name) ~loc (instantiate tys)) with effect }
  with Not_found ->
  try
    let v = lookup_global_value ~loc name env in
    let v_ftvars = free_tvars v.val_exp.ty in
    let ty = instantiate (v_ftvars, v.val_exp.ty) in
    let effect = v.val_exp.effect in
    { (mk (Var name) ~loc ty) with effect }
  with Not_found ->
    error loc "unbound variable %S" name

let eq_exp env (e1 : typed_exp) (e2 : typed_exp) =
  let eq_var v1 v2 =
    let get v =
      try let (v, _, _) = StringMap.find v1 env.vars in v
      with Not_found -> error (noloc env) "unbound variable %S" v in
    get v1 = get v2 in
  eq_typed_exp eq_var e1 e2

let type_error loc msg actual expected =
  error loc "%s.\nExpected type:\n  %s\nActual type:\n  %s"
    msg
    (string_of_type expected)
    (string_of_type actual)

let fail_neq ?info ?more ~loc ~expected_ty ty =
  if not @@ eq_types ty expected_ty then
    let msg = match info with
      | None -> "Unexpected type"
      | Some info -> "Unexpected type " ^ info in
    let msg = match more with
      | None -> msg
      | Some more -> Printf.sprintf "%s (%s)" msg more in
    type_error loc msg ty expected_ty

let error_prim loc prim args expected_args =
  let prim = LiquidTypes.string_of_primitive prim in
  let nargs = List.length args in
  let nexpected = List.length expected_args in
  if nargs <> nexpected then
    error loc "Prim %S: %d args provided, %d args expected"
      prim nargs nexpected
  else
    let args = List.map (fun { ty } -> ty) args in
    List.iteri (fun i (arg, expected_ty) ->
        let info = Printf.sprintf "Primitive %s, argument %d" prim (i+1) in
        fail_neq ~info ~loc ~expected_ty arg
      ) (List.combine args expected_args);
    Printf.eprintf "Fatal error on typechecking primitive %S\n%!" prim;
    assert false


(* Extract signature of contract, use previous name if the same
   signature was generated before otherwise use the same name as the
   contract for its signature *)
let sig_of_contract contract =
  let c_sig = sig_of_contract contract in
  let sig_name = StringMap.fold (fun name c_sig' -> function
      | Some _ as acc -> acc
      | None -> if eq_signature c_sig' c_sig then Some name else None
    ) contract.ty_env.contract_types None in
  let sig_name = match sig_name with
    | None -> Some contract.contract_name
    | Some _ -> sig_name in
  let c_sig = { c_sig with sig_name } in
  begin match sig_name with
    | None -> assert false
    | Some n ->
      contract.ty_env.contract_types <- StringMap.add n c_sig
          contract.ty_env.contract_types
  end;
  { f_sig_name = c_sig.sig_name;
    f_storage = contract.storage;
    f_entries_sig = c_sig.entries_sig }

(* Merge nested matches to recover encoding for pattern matching over
   sum type *)
let rec merge_matches acc loc cases constrs =
  match cases, constrs with
  | [ PConstr ("Left", l), case_l; PConstr ("Right", r), case_r ],
    [ c1, ty1; c2, ty2 ] ->
    List.rev @@ (PConstr (c2, r), case_r) ::
                (PConstr (c1, l), case_l) :: acc

  | [ PConstr ("Left", l), case_l;
      PConstr ("Right", [x]), { desc = Let { bnd_var = v; bnd_val = case_r;
                                             body = { desc = Var v' }}}],
    _ :: _
    when v.nname = v'
    ->
    merge_matches acc loc [ PConstr ("Left", l), case_l;
                            PConstr ("Right", [x]), case_r ] constrs

  | [ PConstr ("Left", l), case_l;
      PConstr ("Right", [x]), { desc =  Let { bnd_var = v; bnd_val = case_r;
                                              body = { desc = Const { const = CUnit } }}}],
    _ :: _ ->
    merge_matches acc loc [ PConstr ("Left", l), case_l;
                            PConstr ("Right", [x]), case_r ] constrs

  | [ PConstr ("Left", l), case_l; PConstr ("Right", [x]), case_r ],
    (c1, ty1) :: constrs ->
    begin match case_r.desc with
      | MatchVariant { arg = { desc = Var x' }; cases }
        when x = x' ->
        (* match arg with
           | Left l -> case_l
           | Right x -> match x with
                        | Left -> ...*)

        merge_matches ((PConstr (c1, l), case_l) :: acc) loc cases constrs
      | _ ->
        (* ==> match | C1 l -> case_l | _ -> case_r *)
        List.rev @@ (PAny, case_r) :: (PConstr (c1, l), case_l) :: acc
    end
  | _ -> raise Exit



let rec type_of_const ~loc env = function
  | CUnit -> Tunit
  | CBool _ -> Tbool
  | CInt _ -> Tint
  | CNat _ -> Tnat
  | CTez _ -> Ttez
  | CTimestamp _ -> Ttimestamp
  | CString _ -> Tstring
  | CBytes _ -> Tbytes
  | CKey _ -> Tkey
  | CSignature _ -> Tsignature
  | CAddress _ -> Taddress
  | CTuple l ->
    Ttuple (List.map (type_of_const ~loc env) l)
  | CNone -> Toption (fresh_tvar ())
  | CSome c -> Toption (type_of_const ~loc env c)
  | CMap [] -> Tmap (fresh_tvar (), fresh_tvar ())
  | CMap ((k,e) :: _) -> Tmap (type_of_const ~loc env k, type_of_const ~loc env e)

  | CBigMap (BMList [] | BMId _)-> Tbigmap (fresh_tvar (), fresh_tvar ())
  | CBigMap BMList ((k,e) :: _) -> Tbigmap (type_of_const ~loc env k, type_of_const ~loc env e)

  | CList [] -> Tlist (fresh_tvar ())
  | CList (e :: _) -> Tlist (type_of_const ~loc env e)

  | CSet [] -> Tset (fresh_tvar ())
  | CSet (e :: _) -> Tset (type_of_const ~loc env e)

  | CLeft c -> Tor (type_of_const ~loc env c, fresh_tvar ())
  | CRight c -> Tor (fresh_tvar (), type_of_const ~loc env c)

  | CKey_hash _ -> Tkey_hash

  | CRecord [] -> assert false
  | CRecord ((label, _) :: _ as fields) ->
    let ty, _ = find_label ~loc label env.env in
    begin match ty with
      | Trecord (n, l) ->
        let l = List.map2 (fun (lab_c, c) (lab_t, t) ->
            lab_t, type_of_const ~loc env c
          ) fields l in
        Trecord (n, l)
      | _ -> assert false
    end
  | CConstr (constr, c) ->
    let ty, (constr, _, i) = find_constr ~loc constr env.env in
    begin match ty with
      | Tsum (n, l) ->
        let l = List.mapi (fun j ((constr, t) as ct) ->
          if i = j then (constr, type_of_const ~loc env c)
          else ct
          ) l in
        Tsum (n, l)
      | _ -> assert false
    end
  | CLambda { arg_ty; ret_ty } -> Tlambda (arg_ty, ret_ty, default_uncurry ())

(* helper function to set uncurry flag of type of [f] in application [f x] *)
let rec set_uncurry exp =
  match exp.desc with
  | Apply { prim = Prim_exec _;
            args = [ { ty = Tlambda (_, _, uncurry)
                          | Tclosure (_, _, uncurry) } as f; _ ] } ->
    (* Application is total if returned type is not a lambda of if
       the expression is a totally applied lambda *)
    let total = match exp.ty with
      | Tlambda (_, _, uncurry) | Tclosure (_, _, uncurry) ->
        begin match !(!uncurry) with
          | None ->
            (* Warn partial applications *)
            LiquidLoc.warn exp.loc Partial_application;
            Some false
          | Some u -> Some u
        end
      | _ -> Some true in
    (* Don't uncurry a previously curried function *)
    if !(!uncurry) <> Some false then
      !uncurry := total;
    (* Recursively set (un)curry flags in function part *)
    set_uncurry f
  | _ -> ()

let rec typecheck_const ~loc env (cst : syntax_const) ty : datatype * typed_const =
  match ty, cst with
  (* No implicit conversions *)
  | ( Tunit, CUnit
    | Tbool, CBool _
    | Tint, CInt _
    | Tstring, CString _
    | Tbytes, CBytes _
    | Tnat, CNat _
    | Ttez, CTez _
    | Tkey, CKey _
    | Tkey_hash, CKey_hash _
    | Taddress, CAddress _
    | Ttimestamp, CTimestamp _
    | Tsignature, CSignature _
    | Toption _, CNone) as ty_cst
    -> ty_cst

  (* Implicit conversions *)
  | Tint, CNat s -> ty, CInt s
  | Tnat, CInt s -> ty, CNat s
  | Tkey, CBytes s -> ty, CKey s
  | Tkey_hash, CBytes s -> ty, CKey_hash s
  | Taddress, CKey_hash s -> ty, CKey_hash s
  | Taddress, CBytes s -> ty, CAddress s
  | Tsignature, CBytes s -> ty, CSignature s

  | Ttimestamp, CString s -> ty, CTimestamp (ISO8601.of_string s)
  | Ttez, CString s -> ty, CTez (LiquidNumber.tez_of_liq s)
  | Tkey_hash, CString s -> ty, CKey_hash s
  | Tkey, CString s -> ty, CKey s
  | Tsignature, CString s -> ty, CSignature s

  | Tchainid, CString s -> ty, CString s
  | Tchainid, CBytes s -> ty, CBytes s

  (* Structures *)
  | Ttuple tys, CTuple csts ->
    begin
      try
        let ltys = List.map2 (typecheck_const ~loc env) csts tys in
        let tys, csts = List.split ltys in
        Ttuple tys, CTuple csts
      with Invalid_argument _ ->
        error loc "constant type mismatch (tuple length differs from type)"
    end

  | Toption ty, CSome cst ->
    let ty, cst = typecheck_const ~loc env cst ty in
    Toption ty, CSome cst

  | Tor (left_ty, right_ty), CLeft cst ->
    let left_ty, cst = typecheck_const ~loc env cst left_ty in
    Tor (left_ty, right_ty), CLeft cst

  | Tor (left_ty, right_ty), CRight cst ->
    let right_ty, cst = typecheck_const ~loc env cst right_ty in
    Tor (left_ty, right_ty), CRight cst

  | Tmap (ty1, ty2), CMap csts ->
    let (ty1, ty2), csts = List.fold_left (fun ((ty1, ty2), acc) (cst1, cst2) ->
        let ty1, cst1 = typecheck_const ~loc env cst1 ty1 in
        let ty2, cst2 = typecheck_const ~loc env cst2 ty2 in
        (ty1, ty2), (cst1, cst2) :: acc
      ) ((ty1, ty2), []) csts in
    let csts = List.rev csts in
    if not @@ comparable_type ty1 then
      error loc "Keys of map are of a non comparable type %s"
        (string_of_type ty);
    Tmap (ty1, ty2), CMap csts

  | Tbigmap (ty1, ty2), (CMap csts | CBigMap BMList csts) -> (* allow map *)
    let (ty1, ty2), csts = List.fold_left (fun ((ty1, ty2), acc) (cst1, cst2) ->
        let ty1, cst1 = typecheck_const ~loc env cst1 ty1 in
        let ty2, cst2 = typecheck_const ~loc env cst2 ty2 in
        (ty1, ty2), (cst1, cst2) :: acc
      ) ((ty1, ty2), []) csts in
    let csts = List.rev csts in
    if not @@ comparable_type ty1 then
      error loc "Keys of big map are of a non comparable type %s"
        (string_of_type ty);
    Tbigmap (ty1, ty2), CBigMap (BMList csts)
  | Tbigmap (ty1, ty2), CBigMap BMId id -> (* allow map *)
    if not @@ comparable_type ty1 then
      error loc "Keys of big map are of a non comparable type %s"
        (string_of_type ty);
    Tbigmap (ty1, ty2), CBigMap (BMId id)

  | Tlist ty, CList csts ->
    let ty, csts = List.fold_left (fun (ty, acc) cst ->
        let ty, cst = typecheck_const ~loc env cst ty in
        ty, cst :: acc
      ) (ty, []) csts in
    let csts = List.rev csts in
    Tlist ty, CList csts

  | Tset ty, CSet csts ->
    let ty, csts = List.fold_left (fun (ty, acc) cst ->
        let ty, cst = typecheck_const ~loc env cst ty in
        ty, cst :: acc
      ) (ty, []) csts in
    if not @@ comparable_type ty then
      error loc "Elements of set are of a non comparable type %s"
        (string_of_type ty);
    let csts = List.rev csts in
    Tset ty, CSet csts

  | Trecord (rname, labels), CRecord fields ->
    (* order record fields wrt type *)
    List.iter (fun (f, _) ->
        if not @@ List.mem_assoc f labels then
          error loc "Record field %s is not in type %s" f rname
      ) fields;
    let fields, labels = List.map (fun (f, ty) ->
        try
          let cst = List.assoc f fields in
          let ty, cst = typecheck_const ~loc env cst ty in
          (f, cst), (f, ty)
        with Not_found ->
          error loc "Record field %s is missing" f
      ) labels |> List.split in
    Trecord (rname, labels), CRecord fields

  | Trecord (rname, labels), CTuple csts when env.decompiling ->
    (* Allow pairs when decompiling *)
    let rec mk labels csts = match labels, csts with
      | (f, ty) :: labels, cst :: csts ->
        let ty, cst = typecheck_const ~loc env cst ty in
        ((f, ty), (f, cst)) :: mk labels csts
      | [], _ -> []
      | (f, _) :: _ , [] -> error loc "Record field %s is missing" f
    in
    let labels, fields = List.split (mk labels csts) in
    Trecord (rname, labels), CRecord fields

  | Tsum (None, constrs), CConstr (c, cst) ->
    let constrs, cst = try
      let ty = List.assoc c constrs in
      let ty, cst = typecheck_const ~loc env cst ty in
      let constrs =
        List.map (fun (c, t) -> if eq_types t ty then (c, ty) else (c, t)) constrs in
      constrs, cst
      with Not_found ->
        let ty, cst =
          typecheck_const ~loc env cst (type_of_const ~loc env cst) in
        let constrs = constrs @ [c, ty] in
        constrs, cst
    in
    let ty = Tsum (None, constrs) in
    let c = CConstr (c, cst) in
    (ty, c)

  | Tsum (Some sname, constrs), CConstr (c, cst) ->
    let ty =
      try List.assoc c constrs
      with Not_found ->
        error loc "Constructor %s does not belong to type %s" c sname in
    let ty, cst = typecheck_const ~loc env cst ty in
    let constrs =
      List.map (fun (c, t) -> if eq_types t ty then (c, ty) else (c, t)) constrs in
    let ty = Tsum (Some sname, constrs) in
    let c = CConstr (c, cst) in
    (ty, c)

  | Tsum (sname, constrs), (CLeft _ | CRight _) when env.decompiling ->
    (* Allow Left/Right constants for sum type when decompiling *)
    let rec seek constrs cst = match constrs, cst with
      | (c, ty) :: _, CLeft cst ->
        let ty, cst = typecheck_const ~loc env cst ty in
        (c, ty, cst)
      | _ :: constrs, CRight cst -> seek constrs cst
      | _, _ ->
        error loc "Constant cannot be converted to type %s" (string_of_type ty)
    in
    let c, ty, cst = seek constrs cst in
    let constrs =
      List.map (fun (c, t) -> if t = ty then (c, ty) else (c, t)) constrs in
    Tsum (sname, constrs), CConstr (c, cst)

  | Tlambda (arg_ty, ret_ty, _), CLambda lam ->
    let lam, ty = typecheck_lambda ~loc env lam in
    ty, CLambda lam

  | Tvar tv, c ->
    unify loc ty (type_of_const ~loc env c);
    let ty = match (Ref.get tv).tyo with
      | None -> ty
      | Some ty -> ty in
    typecheck_const ~loc env c ty

  | _ ->
    error loc "constant type mismatch, expected %s, got %s"
      (string_of_type ty)
      (string_of_type (type_of_const ~loc env cst))

and typecheck_lambda ~loc env
    ({ arg_name; arg_ty; body; ret_ty; recursive } as origlam) =
  let ty =
    (* Pre-compute env type if will be tranformed to closure *)
    let bvs = LiquidBoundVariables.bv (mk ~loc (Lambda origlam) ()) in
    if StringSet.is_empty bvs then
      Tlambda (arg_ty, ret_ty, default_uncurry ())
    else
      let call_env_args = List.map (fun v ->
          (find_var env loc v).ty
        ) (StringSet.elements bvs) in
      let call_env_ty = match call_env_args with
        | [ty] -> ty
        | _ -> Ttuple call_env_args in
      Tclosure ((arg_ty, call_env_ty), ret_ty, default_uncurry ())
  in
  let (env, check_rec_calls) =
    match recursive with
    | None -> env, (fun _ -> ())
    | Some f ->
      let (env, f_count) = new_binding env f ty in
      env,
      (fun env -> check_used env { nname = f; nloc = loc} f_count)
  in
  let (env, arg_count) = new_binding env arg_name.nname arg_ty in
  let body = typecheck_expected "function body" env ret_ty body in
  check_used env arg_name arg_count;
  check_rec_calls env;
  let lam = { arg_name; arg_ty; body; ret_ty = body.ty; recursive } in
  (lam, ty)


(* Typecheck an expression. Returns a typed expression *)
and typecheck env ( exp : syntax_exp ) : typed_exp =
  let loc = exp.loc in
  let unify = unify loc in
  match exp.desc with

  | Const { ty; const } ->
    let ty, const = typecheck_const ~loc env const ty in
    mk ?name:exp.name ~loc (Const { ty; const }) (ty:datatype)

  | Let { bnd_var; inline; bnd_val; body } ->
    let bnd_val = typecheck env bnd_val in
    if bnd_val.ty = Tfail then
      LiquidLoc.warn bnd_val.loc AlwaysFails;
    let gen = match expand bnd_val.ty with
      | Tlambda _ | Tclosure _ -> true
      | _ -> false in
    let (env, count) =
      new_binding env bnd_var.nname ~effect:exp.effect ~gen bnd_val.ty in
    let body = typecheck env body in
    let desc = Let { bnd_var; inline; bnd_val; body } in
    check_used env bnd_var count;
    mk ?name:exp.name ~loc desc body.ty

  | Var v -> find_var env loc v

  | Project { field; record } ->
    let record = typecheck env record in
    let record_ty, (field, ty, _) =
      try
        find_label ~loc field env.env
      with Not_found -> error loc "unbound record field %S" field
    in
    unify record_ty record.ty;
    mk ?name:exp.name ~loc (Project { field; record }) ty

  | SetField { record; field; set_val } ->
    let record_ty, (field, exp_ty, _) =
      try find_label ~loc field env.env
      with Not_found -> error loc "unbound record field %S" field
    in
    let record = typecheck_expected "record update" env record_ty record in
    let set_val = typecheck_expected "field update value" env exp_ty set_val in
    mk ?name:exp.name ~loc (SetField { record; field; set_val }) record.ty

  | Seq (exp1, exp2) ->
    let exp1 = typecheck_expected "sequence" env Tunit exp1 in
    if exp1.ty = Tfail then LiquidLoc.warn exp1.loc AlwaysFails;
    let exp2 = typecheck env exp2 in
    let desc = Seq (exp1, exp2) in
    (* TODO: if not fail1 then remove exp1 *)
    mk ?name:exp.name ~loc desc exp2.ty

  | If { cond; ifthen; ifelse } ->
    let cond =
      typecheck_expected "if condition" env Tbool cond in
    let ifthen = typecheck env ifthen in
    let ifelse, ty =
      if ifthen.ty = Tfail then
        let ifelse = typecheck env ifelse in
        ifelse, ifelse.ty
      else
        let ifelse =
          typecheck_expected "else branch" env ifthen.ty ifelse in
        ifelse, ifthen.ty
    in
    let desc = If { cond; ifthen; ifelse } in
    mk ?name:exp.name ~loc desc ty

  | Transfer { dest; amount } ->
    let amount = typecheck_expected "transfer amount" env Ttez amount in
    let dest = typecheck_expected "transfer destination" env Tkey_hash dest in
    let desc = Transfer { dest; amount } in
    mk ?name:exp.name ~loc desc Toperation

  | SelfCall { amount; entry; arg } ->
    let amount = typecheck_expected "call amount" env Ttez amount in
    let self_entries = env.t_contract_sig.f_entries_sig in
    let arg =
      try
        let { parameter = arg_ty } =
          List.find (fun { entry_name } -> entry_name = entry)
            self_entries in
        typecheck_expected "call argument" env arg_ty arg
      with Not_found ->
        (* if env.decompiling then
         *   typecheck env arg
         * else *)
        error loc
          "contract has no entry point %s (available entry points: %a)"
          entry
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt e -> Format.pp_print_string fmt e.entry_name))
          self_entries;
    in
    let desc = SelfCall { amount; entry; arg } in
    mk ?name:exp.name ~loc desc Toperation

  | Call { contract; amount; entry; arg } ->
    let amount = typecheck_expected "call amount" env Ttez amount in
    let contract = typecheck env contract in
    let arg, entry =
      match expand contract.ty, entry with
      | Tcontract (Some c_entry, arg_ty), Some entry
        when not env.decompiling && entry <> c_entry ->
        error loc
          "contract handle is for entry point %s, \
           but is called with entry point %s"
          c_entry entry
      | Tcontract (e, arg_ty), entry ->
        let entry = match e, entry with
          | None, None -> None
          | Some e, _ | _, Some e -> Some e in
        typecheck_expected "call argument" env arg_ty arg, entry
      | Taddress, Some _ -> typecheck env arg, entry
      | Tvar _, Some _ ->
        let arg = typecheck env arg in
        unify contract.ty Taddress;
        arg, entry
      | (Taddress | Tvar _), None ->
        error loc "contract call on address must specify an entry point"
      | Tpartial (Pcont pe), _ ->
        let entry = match pe, entry with
          | None, None -> None
          | _, Some e | Some (e, _), _ -> Some e in
        let arg = typecheck env arg in
        unify contract.ty (Tcontract (entry, arg.ty));
        arg, entry
      | ty, _ ->
        error contract.loc
          "Bad contract type.\nAllowed types:\n  \
           - Contract handle%s\n  \
           - address\n\
           Actual type:\n  %s"
          (match entry with
           | None -> ""
           | Some e -> " for entry point " ^ e)
          (string_of_type ty)
    in
    let desc = Call { contract; amount; entry; arg } in
    mk ?name:exp.name ~loc desc Toperation

  (* contract.entry_name (param) amount *)
  | Apply { prim = Prim_exec _;
            args = { desc = Project { field = entry; record = contract }} ::
                   [param; amount] }
    when match (typecheck env contract).ty with
      | Tcontract _ | Taddress -> true
      | _ -> false
    ->
    typecheck env
      (mk (Call { contract; amount; entry = Some entry; arg = param })
         ~loc ())

  | Apply { prim = Prim_exec _;
            args = { desc = Var "Contract.call" } :: args } ->
    let nb_args = List.length args in
    if nb_args <> 3 || nb_args <> 4  then
      error loc
        "Contract.call expects 3 or 4 arguments, it was given %d arguments."
        nb_args
    else
      error loc "Bad syntax for Contract.call."

  (* Extended primitives *)
  | Apply { prim = Prim_extension (ename, effect, targs, nb_arg, nb_ret, minst);
            args } ->
    let eprim = find_extprim ~loc ename env.env in
    let args = List.map (typecheck env) args in
    let rec mk_inst acc tvs args = match tvs, args with
      | [], [] -> List.rev acc
      | v :: tvs, [] -> mk_inst ((v, fresh_tvar ()) :: acc) tvs []
      | v :: tvs, t :: args -> mk_inst ((v, t) :: acc) tvs args
      | [], _ :: _ ->
        error loc "Expecting %d type arguments, got %d\n"
          (List.length eprim.tvs) (List.length targs) in
    let tv_inst = mk_inst [] eprim.tvs targs in
    let targs = List.map snd tv_inst in
    let instantiate_ext ty =
      let subst = StringSet.fold (fun a acc ->
          if List.mem_assoc a tv_inst then acc
          else (a, fresh_tvar ()) :: acc
        ) (free_tvars ty) (List.rev tv_inst) |> List.rev in
      instantiate_to subst ty in
    let atys = List.map instantiate_ext eprim.atys in
    let rty = instantiate_ext eprim.rty in
    let prim = Prim_extension (ename, effect, targs, nb_arg, nb_ret, minst) in
    begin
      try List.iter2 (fun a aty -> unify a.ty aty) args atys
      with Invalid_argument _ ->
        error loc "Primitive %S expects %d arguments, was given %d"
          (LiquidTypes.string_of_primitive prim)
          (List.length eprim.atys) (List.length args)
    end;
    mk ?name:exp.name ~loc (Apply { prim; args }) rty

  (* f x1 x2 [x3 ...] -> ((f x1) x2) x3) *)
  | Apply { prim = Prim_exec true; args = f :: ((_ :: _ :: _) as r) } ->
    let exp = List.fold_left (fun f x ->
        { exp with desc = Apply { prim = Prim_exec false; args =  [f; x] }}
      ) f r
    in
    (* let exp = match exp.desc with
     *   | Apply { prim = Prim_exec false; args } ->
     *     { exp with desc = Apply { prim = Prim_exec true; args } }
     *   | _ -> assert false in *)
    let exp = typecheck env exp in
    (* Set uncurry flags in expression if needed *)
    set_uncurry exp;
    exp

  | Apply { prim = Prim_exec true; args = [_; _] as args } ->
    let exp =
      typecheck_apply ?name:exp.name ~loc env (Prim_exec true) loc args in
    (* Set uncurry flags in expression if needed *)
    set_uncurry exp;
    exp

  | Apply { prim; args } ->
    typecheck_apply ?name:exp.name ~loc env prim loc args

  | Failwith arg ->
    let arg = typecheck env arg in
    mk (Failwith arg) ~loc Tfail (* no name *)

  | MatchOption { arg; ifnone; some_name; ifsome } ->
    let arg = typecheck env arg in
    let arg_ty = match expand arg.ty with
      | Tfail -> error loc "cannot match failure"
      | Toption ty -> ty
      | Tvar _ | Tpartial _ ->
        let ty = fresh_tvar () in
        unify arg.ty (Toption ty); ty
      | ty -> error loc "not an option type : %s" (string_of_type ty)
    in
    let ifnone = typecheck env ifnone in
    let (env, count) = new_binding env some_name.nname arg_ty in
    let ifsome = typecheck env ifsome in
    check_used env some_name count;
    let desc = MatchOption { arg; ifnone; some_name; ifsome } in
    let ty =
      match ifnone.ty, ifsome.ty with
      | ty, Tfail | Tfail, ty -> ty
      | ty1, ty2 when has_tvar ty1 || has_tvar ty2 ->
        unify ty1 ty2; ty1
      | ty1, ty2 ->
        fail_neq ~loc:ifsome.loc ~expected_ty:ty1 ty2
          ~info:"in branches of match";
        ty1
    in
    mk ?name:exp.name ~loc desc ty

  | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
    let arg = typecheck_expected "match%nat" env Tint arg in
    let (env2, count_p) = new_binding env plus_name.nname Tnat in
    let ifplus = typecheck env2 ifplus in
    let (env3, count_m) = new_binding env minus_name.nname Tnat in
    let ifminus = typecheck env3 ifminus in
    check_used env plus_name count_p;
    check_used env minus_name count_m;
    let desc = MatchNat { arg; plus_name; ifplus; minus_name; ifminus } in
    let ty =
      match ifplus.ty, ifminus.ty with
      | ty, Tfail | Tfail, ty -> ty
      | ty1, ty2 when has_tvar ty1 || has_tvar ty2 ->
        unify ty1 ty2; ty1
      | ty1, ty2 ->
        fail_neq ~loc:ifminus.loc ~expected_ty:ty1 ty2
          ~info:"in branches of match%nat";
        ty1
    in
    mk ?name:exp.name ~loc desc ty

  | Loop { arg_name; body; arg } ->
    let arg = typecheck env arg in
    if arg.ty = Tfail then error loc "Loop.loop arg is a failure";
    let (env, count) = new_binding env arg_name.nname arg.ty in
    let body =
      typecheck_expected "Loop.loop body" env (Ttuple [Tbool; arg.ty]) body in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (Loop { arg_name; body; arg }) arg.ty

  | LoopLeft { arg_name; body; arg; acc = Some acc } ->
    let arg = typecheck env arg in
    let acc = typecheck env acc in
    let arg_ty = Ttuple [arg.ty; acc.ty] in
    let (env, count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck env body in
    let res_ty = match body.ty with
      | ty when has_tvar ty || has_tvar arg_ty ->
        let left_ty = fresh_tvar () in
        let right_ty = fresh_tvar () in
        unify arg.ty left_ty;
        unify body.ty (Ttuple [Tor (left_ty, right_ty); acc.ty]);
        right_ty
      | Ttuple [Tor (left_ty, right_ty); acc_ty] ->
        fail_neq ~loc:acc.loc ~expected_ty:acc_ty acc.ty
          ~info:"in Loop.left accumulator";
        fail_neq ~loc:arg.loc ~expected_ty:left_ty arg.ty
          ~info:"in Loop.left argument";
        right_ty
      | _ ->
        error loc
          "Loop.left body must be of type (('a, 'b) variant * 'c), \
           got %s instead" (string_of_type body.ty) in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (LoopLeft { arg_name; body; arg; acc = Some acc })
      (Ttuple [res_ty; acc.ty])

  | LoopLeft { arg_name; body; arg; acc = None } ->
    let arg = typecheck env arg in
    let (env, count) = new_binding env arg_name.nname arg.ty in
    let body = typecheck env body in
    let res_ty = match body.ty with
      | ty when has_tvar ty || has_tvar arg.ty ->
        let left_ty = fresh_tvar () in
        let right_ty = fresh_tvar () in
        unify arg.ty left_ty;
        unify body.ty (Tor (left_ty, right_ty));
        right_ty
      | Tor (left_ty, right_ty) ->
        fail_neq ~loc:arg.loc ~expected_ty:left_ty arg.ty
          ~info:"in Loop.left argument";
        right_ty
      | _ ->
        error loc
          "Loop.left body must be of type ('a, 'b) variant, \
           got %s instead" (string_of_type body.ty) in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (LoopLeft { arg_name; body; arg; acc = None }) res_ty

  (* For collections, replace generic primitives with their typed ones *)

  | Fold { prim; arg_name; body; arg; acc } ->
    let arg = typecheck env arg in
    let acc = typecheck env acc in
    let prim, arg_ty = match prim, arg.ty, acc.ty with

      | Prim_map_iter, ty, Tunit when has_tvar ty ->
        let k_ty = fresh_tvar () in
        let v_ty = fresh_tvar () in
        unify ty (Tmap (k_ty, v_ty));
        Prim_map_iter, Ttuple [k_ty; v_ty]
      | Prim_set_iter, ty, Tunit when has_tvar ty ->
        let elt_ty = fresh_tvar () in
        unify ty (Tset elt_ty);
        Prim_set_iter, elt_ty
      | Prim_list_iter, ty, Tunit when has_tvar ty ->
        let elt_ty = fresh_tvar () in
        unify ty (Tlist elt_ty);
        Prim_list_iter, elt_ty

      | Prim_map_fold, ty, acc_ty when (has_tvar ty || has_tvar acc_ty) ->
        let k_ty = fresh_tvar () in
        let v_ty = fresh_tvar () in
        let a_ty = fresh_tvar () in
        unify ty (Tmap (k_ty, v_ty));
        unify a_ty acc_ty;
        Prim_map_fold, Ttuple[Ttuple [k_ty; v_ty]; acc_ty]
      | Prim_set_fold, ty, acc_ty when (has_tvar ty || has_tvar acc_ty) ->
        let elt_ty = fresh_tvar () in
        let a_ty = fresh_tvar () in
        unify ty (Tset elt_ty);
        unify a_ty acc_ty;
        Prim_set_fold, Ttuple[elt_ty; acc_ty]
      | Prim_list_fold, ty, acc_ty when (has_tvar ty || has_tvar acc_ty) ->
        let elt_ty = fresh_tvar () in
        let a_ty = fresh_tvar () in
        unify ty (Tlist elt_ty);
        unify a_ty acc_ty;
        Prim_list_fold, Ttuple[elt_ty; acc_ty]

      | (Prim_coll_iter|Prim_map_iter), Tmap (k_ty, v_ty), Tunit ->
        Prim_map_iter, Ttuple [k_ty; v_ty]
      | (Prim_coll_iter|Prim_set_iter), Tset elt_ty, Tunit ->
        Prim_set_iter, elt_ty
      | (Prim_coll_iter|Prim_list_iter), Tlist elt_ty, Tunit ->
        Prim_list_iter, elt_ty

      | (Prim_map_fold|Prim_coll_fold), Tmap (k_ty, v_ty), acc_ty ->
        Prim_map_fold, Ttuple[Ttuple [k_ty; v_ty]; acc_ty]
      | (Prim_set_fold|Prim_coll_fold), Tset elt_ty, acc_ty ->
        Prim_set_fold, Ttuple[elt_ty; acc_ty]
      | (Prim_list_fold|Prim_coll_fold), Tlist elt_ty, acc_ty ->
        Prim_list_fold, Ttuple[elt_ty; acc_ty]

      | _ ->
        error arg.loc "%s expects a collection, got %s"
          (LiquidTypes.string_of_fold_primitive prim)
          (string_of_type arg.ty)
    in
    let (env, count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck_expected
        (LiquidTypes.string_of_fold_primitive prim ^" body") env acc.ty body in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (Fold { prim; arg_name; body; arg; acc }) acc.ty

  | Map { prim; arg_name; body; arg } ->
    let arg = typecheck env arg in
    let prim, arg_ty, k_ty = match prim, arg.ty with

      | Prim_map_map, ty when has_tvar ty ->
        let k_ty = fresh_tvar () in
        let v_ty = fresh_tvar () in
        unify ty (Tmap (k_ty, v_ty));
        Prim_map_map, Ttuple [k_ty; v_ty], Some k_ty
      | Prim_list_map, ty when has_tvar ty ->
        let elt_ty = fresh_tvar () in
        unify ty (Tlist elt_ty);
        Prim_list_map, elt_ty, None

      | (Prim_map_map|Prim_coll_map), Tmap (k_ty, v_ty) ->
        Prim_map_map, Ttuple [k_ty; v_ty], Some k_ty
      | (Prim_list_map|Prim_coll_map), Tlist elt_ty ->
        Prim_list_map, elt_ty, None
      | _ ->
        error arg.loc "%s expects a collection, got %s"
          (LiquidTypes.string_of_map_primitive prim)
          (string_of_type arg.ty)
    in
    let (env, count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck env body in
    let ret_ty = match expand arg.ty with
      | Tmap (k_ty, _) -> Tmap (k_ty, body.ty)
      | Tset _ -> Tset body.ty
      | Tlist _ -> Tlist body.ty
      | Tvar _ | Tpartial _ ->
        begin match prim, k_ty with
          | Prim_map_map, Some k_ty -> Tmap (k_ty, body.ty)
          | Prim_list_map, None -> Tlist body.ty
          | _ -> assert false
        end
      | _ -> assert false
    in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (Map { prim; arg_name; body; arg }) ret_ty

  | MapFold { prim; arg_name; body; arg; acc } ->
    let arg = typecheck env arg in
    let acc = typecheck env acc in
    let prim, arg_ty, k_ty = match prim, arg.ty, acc.ty with

      | Prim_map_map_fold, ty, acc_ty when has_tvar ty ->
        let k_ty = fresh_tvar () in
        let v_ty = fresh_tvar () in
        unify ty (Tmap (k_ty, v_ty));
        Prim_map_map_fold, Ttuple[Ttuple [k_ty; v_ty]; acc_ty], Some k_ty
      | Prim_list_map_fold, ty, acc_ty when has_tvar ty ->
        let elt_ty = fresh_tvar () in
        unify ty (Tlist elt_ty);
        Prim_list_map_fold, Ttuple[elt_ty; acc_ty], None

      | (Prim_map_map_fold|Prim_coll_map_fold), Tmap (k_ty, v_ty), acc_ty ->
        Prim_map_map_fold, Ttuple[Ttuple [k_ty; v_ty]; acc_ty], Some k_ty
      | (Prim_list_map_fold|Prim_coll_map_fold), Tlist elt_ty, acc_ty ->
        Prim_list_map_fold, Ttuple[elt_ty; acc_ty], None
      | _ ->
        error arg.loc "%s expects a collection, got %s"
          (LiquidTypes.string_of_map_fold_primitive prim)
          (string_of_type arg.ty)
    in
    let (env, count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck env body in
    let body_r = match body.ty with
      | Ttuple [r; baccty] when has_tvar baccty || has_tvar acc.ty ->
        unify baccty acc.ty; r
      | Ttuple [r; baccty] when eq_types baccty acc.ty -> r
      | _ ->
        error body.loc
          "body of %s must be of type 'a * %s, but has type %s"
          (LiquidTypes.string_of_map_fold_primitive prim)
          (string_of_type acc.ty)
          (string_of_type body.ty)
    in
    let ret_ty = match expand arg.ty with
      | Tmap (k_ty, _) -> Tmap (k_ty, body_r)
      | Tset _ -> Tset body_r
      | Tlist _ -> Tlist body_r
      | Tvar _ | Tpartial _ ->
        begin match prim, k_ty with
          | Prim_map_map_fold, Some k_ty -> Tmap (k_ty, body_r)
          | Prim_list_map_fold, None -> Tlist body_r
          | _ -> assert false
        end
      | _ -> assert false
    in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (MapFold { prim; arg_name; body; arg; acc })
      (Ttuple [ret_ty; acc.ty])

  | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
    let arg = typecheck env arg in
    let arg_ty = match expand arg.ty with
      | Tfail -> error loc "cannot match failure"
      | Tlist ty -> ty
      | Tvar _ | Tpartial _ ->
        let ty = fresh_tvar () in
        unify arg.ty (Tlist ty); ty
      | _ -> error loc "not a list type"
    in
    let ifnil = typecheck env ifnil in
    let (env, count_head) = new_binding env head_name.nname arg_ty in
    let (env, count_tail) = new_binding env tail_name.nname (Tlist arg_ty) in
    let ifcons = typecheck env ifcons in
    check_used env head_name count_head;
    check_used env tail_name count_tail;
    let desc = MatchList { arg; head_name; tail_name; ifcons; ifnil } in
    let ty =
      match ifnil.ty, ifcons.ty with
      | ty, Tfail | Tfail, ty -> ty
      | ty1, ty2 when has_tvar ty1 || has_tvar ty2 ->
        unify ty1 ty2; ty1
      | ty1, ty2 ->
        fail_neq ~loc:ifcons.loc ~expected_ty:ty1 ty2
          ~info:"in branches of match";
        ty1
    in
    mk ?name:exp.name ~loc desc ty

  | Lambda lam ->
    let lam, ty = typecheck_lambda ~loc env lam in
    mk ?name:exp.name ~loc (Lambda lam) ty

  (* This cannot be produced by parsing *)
  | Closure _ -> assert false

  (* Records with zero elements cannot be parsed *)
  | Record [] -> assert false

  | Record (( (label, _) :: _ ) as lab_x_exp_list) ->
    let record_ty, _ =
      try find_label ~loc label env.env
      with Not_found -> error loc "unbound record label %S" label
    in
    let rname, labels = match record_ty with
      | Trecord (rname, rtys) -> rname, rtys
      | _ -> assert false in
    let fields = List.map (fun (label, exp) ->
        let _, (label, ty', _) = try
            find_label ~loc label env.env
          with Not_found -> error loc "unbound label %S" label
        in
        let ty =
          try List.assoc label labels
          with Not_found ->
            error loc "label %s does not belong to type %s" label rname in
        unify ty ty';
        let exp = typecheck_expected ("label "^ label) env ty' exp in
        (label, exp)
      ) lab_x_exp_list in
    (* order record fields wrt type *)
    let fields = List.map (fun (l, _) ->
        try List.find (fun (l', _) -> l = l') fields
        with Not_found -> error loc "label %s is not defined" l;
      ) labels in
    mk ?name:exp.name ~loc (Record fields) record_ty

  | Constructor { constr = Constr constr; arg } ->
    begin try
        let constr_ty, (constr, arg_ty, _) = find_constr ~loc constr env.env in
        let arg = typecheck_expected "construtor argument" env arg_ty arg in
        mk ?name:exp.name ~loc (Constructor { constr = Constr constr; arg })
          constr_ty
      with Not_found ->
        error loc "unbound constructor %S" constr
    end

  | Constructor { constr = Left right_ty; arg } ->
    let arg = typecheck env arg in
    let ty = Tor (arg.ty, right_ty) in
    mk ?name:exp.name ~loc (Constructor { constr = Left right_ty; arg }) ty

  | Constructor { constr = Right left_ty; arg } ->
    let arg = typecheck env arg in
    let ty = Tor (left_ty, arg.ty) in
    mk ?name:exp.name ~loc (Constructor { constr = Right left_ty; arg }) ty

  (* Typecheck and normalize pattern matching.
     - When decompiling, try to merge nested patterns
     - Order cases based on constructor order in type declaration
     - Merge wildcard patterns if they are compatible (keep at most one)  *)
  | MatchVariant { arg; cases } ->
    let untyped_arg = arg in
    let arg = typecheck env arg in
    let decoded = match arg.ty, env.decompiling with
      | Tsum (_, constrs), true ->
        (* allow loose typing when decompiling *)
        begin try
            let cases = merge_matches [] loc cases constrs in
            Some (typecheck env
                    { exp with
                      desc = MatchVariant { arg = untyped_arg; cases } })
          with Exit -> None
        end
      | _ -> None in
    begin match decoded with
      | Some exp -> exp
      | None ->
        let constrs =
          try
            match expand arg.ty with
            | Tfail ->
              error loc "cannot match failure"
            | Tsum (_, constrs) -> constrs
            | Tor (left_ty, right_ty) ->
              (* Left, Right pattern matching *)
              ["Left", left_ty; "Right", right_ty]
            | Tvar _ (* | Tpartial _ *) ->
              begin match find_variant_type ~loc env.env cases with
                | Some (Tsum (_, constrs) as ty) ->
                  unify arg.ty ty;
                  constrs
                | Some (Tor (left_ty, right_ty) as ty) ->
                  unify arg.ty ty;
                  ["Left", left_ty; "Right", right_ty ]
                | _ -> error loc "not a variant type: %s"
                         (string_of_type arg.ty)
              end
            | _ -> raise Not_found
          with Not_found ->
            error loc "not a variant type: %s"
              (string_of_type arg.ty)
        in
        let expected_type = ref None in
        let cases_extra_constrs =
          List.fold_left (fun acc -> function
              | PAny, _ -> acc
              | PConstr (c, _), _ -> StringSet.add c acc
            ) StringSet.empty cases
          |> ref
        in
        (* Normalize cases:
           - match cases in order
           - one (at most) wildcard at the end *)
        let cases = List.map (fun (constr, _) ->
            let pat, body = find_case loc env constr cases in
            cases_extra_constrs := StringSet.remove constr !cases_extra_constrs;
            constr, pat, body
          ) constrs in
        let are_unbound vars body =
          let body_vars = LiquidBoundVariables.bv body in
          not (List.exists (fun v -> StringSet.mem v body_vars) vars) in
        let rec normalize (acc : (pattern * syntax_exp) list) rev_cases = match rev_cases, acc with
          | [], _ -> acc
          | (_, PAny, body1) :: rev_cases, [PAny, body2]
            when eq_syntax_exp body1 body2 ->
            normalize [PAny, body1] rev_cases
          | (_, PAny, body1) :: rev_cases, [PConstr (_, vars2), body2]
            when are_unbound vars2 body2 && eq_syntax_exp body1 body2 ->
            normalize [PAny, body1] rev_cases
          | (_, PConstr (_, vars1) , body1) :: rev_cases, [PAny, body2]
            when are_unbound vars1 body1 && eq_syntax_exp body1 body2 ->
            normalize [PAny, body1] rev_cases
          | (_, PConstr (_, vars1) , body1) :: rev_cases,
            [PConstr (_, vars2), body2]
            when are_unbound vars1 body1 && are_unbound vars2 body2 &&
                 eq_syntax_exp body1 body2 ->
            normalize [PAny, body1] rev_cases
          | (c1, PAny, body1) :: rev_cases, _ ->
            (* body1 <> body2 *)
            normalize ((PConstr (c1, []), body1) :: acc)  rev_cases
          | (_, PConstr (c1, vars1), body1) :: rev_cases, _ ->
            normalize ((PConstr (c1, vars1), body1) :: acc)  rev_cases
        in
        let cases = normalize [] (List.rev cases) in

        if not (StringSet.is_empty !cases_extra_constrs) then
          error loc "constructors %s do not belong to type %s"
            (String.concat ", " (StringSet.elements !cases_extra_constrs))
            (string_of_type arg.ty);

        let cases = List.map (fun (pat, e) ->
            let add_vars_env vars var_ty =
              match vars with
              | [] -> env, None
              | [ var ] ->
                let (env, count) = new_binding env var var_ty in
                env, Some count
              | _ ->
                error loc "cannot deconstruct constructor args"
            in
            let env, count_opt =
              match pat with
              | PConstr ("Left", vars) ->
                let var_ty = match constrs with
                  | ["Left", left_ty; _] -> left_ty
                  | _ -> error loc "expected variant, got %s"
                           (string_of_type arg.ty) in
                add_vars_env vars var_ty
              | PConstr ("Right", vars) ->
                let var_ty = match constrs with
                  | [_; "Right", right_ty] -> right_ty
                  | _ -> error loc "expected variant, got %s"
                           (string_of_type arg.ty) in
                add_vars_env vars var_ty
              | PConstr (constr, vars) ->
                let var_ty =
                  try List.assoc constr constrs
                  with Not_found -> error loc "unknown constructor %S" constr
                in
                add_vars_env vars var_ty
              | PAny -> env, None
            in
            let e =
              match !expected_type with
              | Some expected_type ->
                typecheck_expected "pattern matching branch" env expected_type e
              | None ->
                let e = typecheck env e in
                begin match e.ty with
                  | Tfail -> ()
                  | _ -> expected_type := Some e.ty
                end;
                e
            in
            begin match pat, count_opt with
              | PConstr (_, [var]), Some count ->
                check_used env { nname = var; nloc = loc} count
              | _ -> ()
            end;
            (pat, e)
          ) cases
        in

        let desc = MatchVariant { arg; cases } in
        let ty = match !expected_type with
          | None -> Tfail
          | Some ty -> ty
        in
        mk ?name:exp.name ~loc desc ty
    end

  | Unpack { arg; ty } ->
    let arg = typecheck_expected "Bytes.unpack argument" env Tbytes arg in
    let desc = Unpack { arg; ty } in
    mk ?name:exp.name ~loc desc (Toption ty)

  | ContractAt { arg; entry; entry_param } ->
    let arg = typecheck_expected "[%%handle ...] argument" env Taddress arg in
    let desc = ContractAt { arg; entry; entry_param } in
    mk ?name:exp.name ~loc desc (Toption (Tcontract (Some entry, entry_param)))

  | CreateContract { args; contract } ->
    let contract = typecheck_contract ~warnings:env.warnings
        ~others:env.visible_contracts
        ~decompiling:env.decompiling contract in
    begin match args with
      | [delegate; init_balance; init_storage] ->
        let delegate =
          typecheck_expected "delegate" env (Toption Tkey_hash) delegate in
        let init_balance =
          typecheck_expected "initial balance" env Ttez init_balance in
        let contract_storage_type =
          normalize_type ~from_env:env.env ~in_env:contract.ty_env contract.storage in
        let init_storage = typecheck_expected "initial storage"
            env contract_storage_type init_storage in
        let desc = CreateContract {
            args = [delegate; init_balance; init_storage];
            contract } in
        mk ?name:exp.name ~loc desc (Ttuple [Toperation; Taddress])
      | _ ->
        error loc "Contract.create expects 3 arguments, was given %d"
          (List.length args)
    end

  | TypeAnnot { e; ty } ->
    typecheck_expected "annotated expression" env ty e

  | Type ty -> assert false (* Not supposed to be typechecked *)

and find_case loc env constr cases =
  match List.find_all (function
      | PAny, _ -> true
      | PConstr (cname, _), _ -> cname = constr
    ) cases
  with
  | [] ->
    error loc "non-exhaustive pattern. Constructor %s is not matched." constr
  | matched_case :: unused ->
    List.iter (function
        | PAny, _ -> ()
        | (PConstr _, (e : syntax_exp)) ->
          LiquidLoc.warn e.loc (UnusedMatched constr)
      ) unused;
    matched_case

and typecheck_prim1 env prim loc args =
  match prim, args with
  | Prim_tuple_get, [{ ty = tuple_ty };
                     { desc = Const { const = CInt n | CNat n }}] ->
    begin match expand tuple_ty with
      | Tpartial (Ptup _) | Tvar _ ->
        let ty = fresh_tvar () in
        let n = LiquidNumber.int_of_integer n in
        unify loc tuple_ty (Tpartial (Ptup [(n, ty)]));
        prim, ty
      | _ ->
        let tuple = match expand tuple_ty with
          | Ttuple tuple -> tuple
          | Trecord (_, rtys) -> List.map snd rtys
          | _ -> error loc "get takes a tuple as first argument, got:\n%s"
                   (string_of_type tuple_ty)
        in
        let n = LiquidNumber.int_of_integer n in
        let size = List.length tuple in
        if size <= n then error loc "get outside tuple";
        let ty = List.nth tuple n in
        prim, ty
    end

  (* | Prim_tuple_get, _ ->
   *   error loc "get in tuple can only be performed with a constant \
   *              integer or natural" *)

  | Prim_tuple_set, [{ ty = tuple_ty ; loc = tuple_loc };
                     { desc = Const { const = CInt n | CNat n }};
                     { ty ; loc = val_loc }] ->
    begin match expand tuple_ty with
      | Tpartial (Ptup _) | Tvar _ ->
        let n = LiquidNumber.int_of_integer n in
        unify loc tuple_ty (Tpartial (Ptup [(n, ty)]));
        prim, tuple_ty
      | _ ->
        let tuple = match expand tuple_ty with
          | Ttuple tuple -> tuple
          | Trecord (_, rtys) -> List.map snd rtys
          | _ -> error loc "set takes a tuple as first argument, got:\n%s"
                   (string_of_type tuple_ty)
        in
        let n = LiquidNumber.int_of_integer n in
        let expected_ty = List.nth tuple n in
        let size = List.length tuple in
        if size <= n then error loc "set outside tuple";
        unify loc ty expected_ty;
        (* if ty <> Tfail then
         *   fail_neq ~loc:val_loc ~expected_ty ty
         *     ~info:"in tuple update"; *)
        let ty = tuple_ty in
        prim, ty
    end

  | Prim_tuple_set, _ ->
    error loc "set in tuple can only be performed with a constant \
               integer or natural"

  | _ ->
    let prim =
      (* Unqualified versions of primitives. They should not be used,
         but they can be generated by decompiling Michelson. *)
      (* No inference needed here (types given by decompilation) *)
      match prim, args with
      | Prim_coll_update, [ _; _; { ty = Tset _ }] -> Prim_set_update
      | Prim_coll_update, [ _; _; { ty = (Tmap _ | Tbigmap _) }] ->
        Prim_map_update
      | Prim_coll_mem, [ _; { ty = Tset _ } ] -> Prim_set_mem
      | Prim_coll_mem, [ _; { ty = (Tmap _ | Tbigmap _) } ] -> Prim_map_mem
      | Prim_coll_find, [ _; { ty = (Tmap _ | Tbigmap _) } ] -> Prim_map_find
      | Prim_coll_size, [{ ty = Tlist _ } ] -> Prim_list_size
      | Prim_coll_size, [{ ty = Tset _ } ] -> Prim_set_size
      | Prim_coll_size, [{ ty = Tmap _ } ] -> Prim_map_size
      | Prim_coll_size, [{ ty = Tstring } ] -> Prim_string_size
      | Prim_coll_size, [{ ty = Tbytes } ] -> Prim_bytes_size
      | Prim_slice, [ _; _; { ty = Tstring } ] -> Prim_string_sub
      | Prim_slice, [ _; _; { ty = Tbytes } ] -> Prim_bytes_sub
      | Prim_concat, [{ ty = Tlist Tstring } ] -> Prim_string_concat
      | Prim_concat, [{ ty = Tlist Tbytes } ] -> Prim_bytes_concat
      | _ -> prim
    in
    prim, typecheck_prim2 env prim loc args

and typecheck_prim2 env prim loc args =
  if List.exists (fun a -> has_tvar a.ty) args then
    typecheck_prim2i env prim loc args
  else
    typecheck_prim2t env prim loc args

and typecheck_prim2i env prim loc args =
  let unify = unify loc in
  match prim, List.map (fun a -> a.ty) args with
  | (Prim_neq | Prim_lt | Prim_gt | Prim_eq | Prim_le | Prim_ge),
    [ ty1; ty2 ] ->
    let overloads = [ ([ Tbool; Tbool ], Tbool) ;
                      ([ Tint; Tint ], Tbool) ;
                      ([ Tnat; Tnat ], Tbool) ;
                      ([ Ttez; Ttez ], Tbool) ;
                      ([ Tstring; Tstring ], Tbool) ;
                      ([ Tbytes; Tbytes ], Tbool) ;
                      ([ Ttimestamp; Ttimestamp ], Tbool) ;
                      ([ Tkey_hash; Tkey_hash ], Tbool) ;
                      ([ Taddress; Taddress ], Tbool) ] in
    unify ty1 ty2;
    make_type_eqn loc overloads [ ty1; ty2 ]

  | Prim_compare, [ ty1; ty2 ] ->
    let overloads = [ ([ Tbool; Tbool ], Tint) ;
                      ([ Tint; Tint ], Tint) ;
                      ([ Tnat; Tnat ], Tint) ;
                      ([ Ttez; Ttez ], Tint) ;
                      ([ Tstring; Tstring ], Tint) ;
                      ([ Tbytes; Tbytes ], Tint) ;
                      ([ Ttimestamp; Ttimestamp ], Tint) ;
                      ([ Tkey_hash; Tkey_hash ], Tint) ;
                      ([ Taddress; Taddress ], Tint) ] in
    unify ty1 ty2;
    make_type_eqn loc overloads [ ty1; ty2 ]

  | Prim_neg, [ ty ] ->
    let overloads = [ ([ Tnat ], Tnat) ;
                      ([ Tint ], Tint)  ] in
    make_type_eqn loc overloads [ ty ]

  | Prim_add, [ ty1; ty2 ] ->
    let overloads = [ ([ Ttez; Ttez ], Ttez) ;
                      ([ Tnat; Tnat ], Tnat) ;
                      ([ Tint; Tint ], Tint) ;
                      ([ Tnat; Tint ], Tint) ;
                      ([ Tint; Tnat ], Tint) ;
                      ([ Tint; Ttimestamp ], Ttimestamp) ;
                      ([ Ttimestamp; Tint ], Ttimestamp) ] in
    make_type_eqn loc overloads [ ty1; ty2 ]

  | Prim_sub, [ ty1; ty2 ] ->
    let overloads = [ ([ Ttez; Ttez ], Ttez) ;
                      ([ Tnat; Tnat ], Tint) ;
                      ([ Tint; Tint ], Tint) ;
                      ([ Tnat; Tint ], Tint) ;
                      ([ Tint; Tnat ], Tint) ;
                      ([ Ttimestamp; Tint ], Ttimestamp) ;
                      ([ Ttimestamp; Ttimestamp ], Ttimestamp) ] in
    make_type_eqn loc overloads [ ty1; ty2 ]

  | Prim_mul, [ ty1; ty2 ] ->
    let overloads = [ ([ Tnat; Ttez ], Ttez) ;
                      ([ Ttez; Tnat ], Ttez) ;
                      ([ Tnat; Tnat ], Tnat) ;
                      ([ Tint; Tint ], Tint) ;
                      ([ Tnat; Tint ], Tint) ;
                      ([ Tint; Tnat ], Tint) ] in
    make_type_eqn loc overloads [ ty1; ty2 ]

  | Prim_ediv, [ ty1; ty2 ] -> (*
     let overloads = [ ([ Ttez; Ttez ], Toption (Ttuple [Tnat; Ttez])) ;
                       ([ Ttez; Tnat ], Toption (Ttuple [Ttez; Ttez])) ;
                       ([ Tnat; Tnat ], Toption (Ttuple [Tnat; Tnat])) ;
                       ([ Tint; Tint ], Toption (Ttuple [Tint; Tnat])) ;
                       ([ Tnat; Tint ], Toption (Ttuple [Tint; Tnat])) ;
                       ([ Tint; Tnat ], Toption (Ttuple [Tint; Tnat])) ] in
     make_type_eqn loc overloads [ ty1; ty2 ] *)
    let overloads = [ ([ Ttez; Ttez ], Tnat) ;
                      ([ Ttez; Tnat ], Ttez) ;
                      ([ Tnat; Tnat ], Tnat) ;
                      ([ Tint; Tint ], Tint) ;
                      ([ Tnat; Tint ], Tint) ;
                      ([ Tint; Tnat ], Tint) ] in
    let t1 = make_type_eqn loc overloads [ ty1; ty2 ] in
    let overloads = [ ([ Ttez; Ttez ], Ttez) ;
                      ([ Ttez; Tnat ], Ttez) ;
                      ([ Tnat; Tnat ], Tnat) ;
                      ([ Tint; Tint ], Tnat) ;
                      ([ Tnat; Tint ], Tnat) ;
                      ([ Tint; Tnat ], Tnat) ] in
    let t2 = make_type_eqn loc overloads [ ty1; ty2 ] in
    Toption (Ttuple [t1; t2])

  | (Prim_xor | Prim_or), [ ty1; ty2] ->
    let overloads = [ ([ Tbool; Tbool ], Tbool) ;
                      ([ Tnat; Tnat ], Tnat) ] in
    make_type_eqn loc overloads [ ty1; ty2 ]

  | Prim_and, [ ty1; ty2 ] ->
    let overloads = [ ([ Tbool; Tbool ], Tbool) ;
                      ([ Tint; Tnat ], Tnat) ;
                      ([ Tnat; Tnat ], Tnat) ] in
    make_type_eqn loc overloads [ ty1; ty2 ]

  | Prim_not, [ ty ] ->
    let overloads = [ ([ Tbool ], Tbool) ;
                      ([ Tint ], Tint) ;
                      ([ Tnat ], Tint) ] in
    make_type_eqn loc overloads [ty]

  | Prim_abs, [ ty ] -> unify ty Tint; Tint
  | Prim_is_nat, [ ty ] -> unify ty Tint; Toption Tnat
  | Prim_int, [ ty ] -> unify ty Tnat; Tint

  | Prim_sub, [ ty ] ->
    let overloads = [ ([ Tint ], Tint) ;
                      ([ Tnat ], Tint) ] in
    make_type_eqn loc overloads [ty]

  | (Prim_lsl | Prim_lsr), [ ty1; ty2 ] -> unify ty1 Tnat; unify ty2 Tnat; Tnat

  | Prim_tuple, ty_args -> Ttuple (List.map (fun e -> e.ty) args)

  | Prim_map_add, [ key_ty; value_ty; map_ty ] ->
    unify map_ty (Tpartial (Pmap (key_ty, value_ty)));
    map_ty

  | Prim_map_update, [ key_ty; value_tyo; map_ty ] ->
    let value_ty = fresh_tvar () in
    unify value_tyo (Toption value_ty);
    unify map_ty (Tpartial (Pmap (key_ty, value_ty)));
    map_ty

  | Prim_map_remove, [ key_ty; map_ty ]
  | Prim_map_find, [ key_ty; map_ty ]
  | Prim_map_mem, [ key_ty; map_ty ] ->
    let value_ty = fresh_tvar () in
    unify map_ty (Tpartial (Pmap (key_ty, value_ty)));
    begin match prim with
      | Prim_map_remove -> map_ty
      | Prim_map_find -> Toption value_ty
      | Prim_map_mem -> Tbool
      | _ -> assert false
    end

  | (Prim_set_add | Prim_set_remove), [ key_ty; set_ty ]
  | Prim_set_update, [ key_ty; Tbool; set_ty ] -> (* should Tbool be unified ?*)
    unify set_ty (Tset key_ty); Tset key_ty

  | Prim_set_mem, [ key_ty; set_ty ] ->
    unify set_ty (Tset key_ty); Tbool

  | Prim_list_size, [ ty ] -> unify ty (Tlist (fresh_tvar ())); Tnat
  | Prim_set_size, [ ty ] -> unify ty (Tset (fresh_tvar ())); Tnat
  | Prim_map_size, [ ty ] ->
    unify ty (Tmap (fresh_tvar (), fresh_tvar ())); Tnat

  | Prim_big_map_create, [ ty ] ->
    unify ty Tunit;
    Tbigmap (fresh_tvar (), fresh_tvar ())

  | Prim_Some, [ ty ] -> Toption ty

  | Prim_self, [ ty ] -> unify ty Tunit; Taddress

  | Prim_now, [ ty ] -> unify ty Tunit; Ttimestamp
  | ( Prim_balance | Prim_amount ), [ ty ] -> unify ty Tunit; Ttez
  | ( Prim_source | Prim_sender ), [ ty ] -> unify ty Tunit; Taddress
  | Prim_gas, [ ty ] -> unify ty Tunit; Tnat
  | Prim_chain_id, [ ty ] -> unify ty Tunit; Tchainid

  | Prim_pack, [ ty ] ->
    Tbytes

  | (Prim_blake2b | Prim_sha256 | Prim_sha512), [ ty ] ->
    unify ty Tbytes; Tbytes

  | Prim_hash_key, [ ty ] -> unify ty Tkey; Tkey_hash

  | Prim_check, [ ty1; ty2; ty3 ] ->
    unify ty1 Tkey; unify ty2 Tsignature; unify ty3 Tbytes; Tbool

  | Prim_address, [ ty ] ->
    unify ty (Tpartial (Pcont None));
    Taddress

  | Prim_default_account, [ ty ] ->
    unify ty Tkey_hash; unit_contract_ty

  | Prim_set_delegate, [ ty ] ->
    unify ty (Toption Tkey_hash); Toperation

  | Prim_exec _,
    [ (Tlambda (from_ty, to_ty, _) | Tclosure ((from_ty, _), to_ty, _)); ty ] ->
    unify ty from_ty; to_ty

  | Prim_exec _, [ lty; aty ] ->
    let to_ty = fresh_tvar () in
    unify lty (Tlambda (aty, to_ty, default_uncurry ())); to_ty

  | Prim_list_rev, [ ty ] -> unify ty (Tlist (fresh_tvar ())); ty

  | Prim_concat_two, [ ty1; ty2 ] ->
    let overloads = [ ([ Tstring; Tstring ], Tstring) ;
                      ([ Tbytes; Tbytes ], Tbytes) ] in
    unify ty1 ty2;
    make_type_eqn loc overloads [ty1;ty2]

  | Prim_string_concat, [ ty ] -> unify ty (Tlist Tstring); Tstring
  | Prim_bytes_concat, [ ty ] -> unify ty (Tlist Tbytes); Tbytes

  | Prim_Cons, [ head_ty; list_ty ] ->
    unify list_ty (Tlist head_ty); Tlist head_ty

  | Prim_string_size, [ ty ] -> unify ty Tstring; Tnat
  | Prim_bytes_size, [ ty ] -> unify ty Tbytes; Tnat

  | Prim_string_sub, [ ty1; ty2; ty3 ] ->
    unify ty1 Tnat; unify ty2 Tnat; unify ty3 Tstring; Toption Tstring

  | Prim_bytes_sub, [ ty1; ty2; ty3 ] ->
    unify ty1 Tnat; unify ty2 Tnat; unify ty3 Tbytes; Toption Tbytes

  | Prim_get_balance, [ ty ] ->
    unify ty (Tpartial (Pcont None));
    Ttez

  | Prim_block_level, [ ty ] ->
    unify ty Tunit;
    Tnat

  | Prim_collect_call, [ ty ] ->
    unify ty Tunit;
    Tbool

  | Prim_is_implicit, [ ty ] ->
    unify ty (Tpartial (Pcont (Some ("default", Tunit))));
    Toption Tkey_hash

  | _ -> failwith ("typecheck_prim2i " ^
                   (LiquidTypes.string_of_primitive prim) ^ " TODO")

and typecheck_prim2t env prim loc args =
  match prim, List.map (fun a -> a.ty) args with
  | ( Prim_neq | Prim_lt | Prim_gt | Prim_eq | Prim_le | Prim_ge ),
    [ ty1; ty2 ] ->
    check_comparable loc prim ty1 ty2;
    Tbool
  | Prim_compare,
    [ ty1; ty2 ] ->
    check_comparable loc prim ty1 ty2;
    Tint

  | Prim_neg, [( Tint | Tnat )] -> Tint

  | (Prim_add | Prim_sub) , [ Ttez; Ttez ] -> Ttez
  | Prim_mul, ([ Tnat; Ttez ] | [ Ttez; Tnat ]) -> Ttez

  | (Prim_add|Prim_mul), [ Tnat; Tnat ] -> Tnat
  | (Prim_add|Prim_sub|Prim_mul), [ (Tint|Tnat);
                                    (Tint|Tnat) ] -> Tint

  | Prim_add, [ Ttimestamp; Tint ] -> Ttimestamp
  | Prim_add, [ Tint; Ttimestamp ] -> Ttimestamp
  | Prim_sub, [ Ttimestamp; Tint ] -> Ttimestamp
  | Prim_sub, [ Ttimestamp; Ttimestamp ] -> Tint

  (* TODO: improve types of ediv in Michelson ! *)
  | Prim_ediv, [ Tnat; Tnat ] ->
    Toption (Ttuple [Tnat; Tnat])
  | Prim_ediv, [ Tint|Tnat; Tint|Tnat ] ->
    Toption (Ttuple [Tint; Tnat])
  | Prim_ediv, [ Ttez; Tnat ] ->
    Toption (Ttuple [Ttez; Ttez])
  | Prim_ediv, [ Ttez; Ttez ] ->
    Toption (Ttuple [Tnat; Ttez])


  | Prim_xor, [ Tbool; Tbool ] -> Tbool
  | Prim_or, [ Tbool; Tbool ] -> Tbool
  | Prim_and, [ Tbool; Tbool ] -> Tbool
  | Prim_not, [ Tbool ] -> Tbool

  | Prim_xor, [ Tnat; Tnat ] -> Tnat
  | Prim_or, [ Tnat; Tnat ] -> Tnat
  | Prim_and, [ Tint|Tnat; Tnat ] -> Tnat
  | Prim_not, [ Tint|Tnat ] -> Tint

  | Prim_abs, [ Tint ] -> Tint
  | Prim_is_nat, [ Tint ] -> Toption Tnat
  | Prim_int, [ Tnat ] -> Tint
  | Prim_sub, [ Tint|Tnat ] -> Tint

  | (Prim_lsl|Prim_lsr), [ Tnat ; Tnat ] -> Tnat

  | Prim_tuple, ty_args -> Ttuple (List.map (fun e -> e.ty) args)

  | Prim_map_find,
    [ key_ty;
      (Tmap (expected_key_ty, value_ty) | Tbigmap (expected_key_ty, value_ty)) ]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Map.find (key)";
    Toption value_ty
  | Prim_map_update,
    [ key_ty;
      Toption value_ty;
      ( Tmap (expected_key_ty, expected_value_ty)
      | Tbigmap (expected_key_ty, expected_value_ty)) as m]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Map.update (key)";
    fail_neq ~loc ~expected_ty:expected_value_ty value_ty
      ~info:"in Map.update (value)";
    begin match m with
      | Tmap _ -> Tmap (key_ty, value_ty)
      | Tbigmap _ -> Tbigmap (key_ty, value_ty)
      |  _ -> assert false
    end
  | Prim_map_add,
    [ key_ty;
      value_ty;
      ( Tmap (expected_key_ty, expected_value_ty)
      | Tbigmap (expected_key_ty, expected_value_ty)) as m]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Map.add (key)";
    fail_neq ~loc ~expected_ty:expected_value_ty value_ty
      ~info:"in Map.add (value)";
    begin match m with
      | Tmap _ -> Tmap (key_ty, value_ty)
      | Tbigmap _ -> Tbigmap (key_ty, value_ty)
      |  _ -> assert false
    end
  | Prim_map_remove,
    [ key_ty;
      ( Tmap (expected_key_ty, value_ty)
      | Tbigmap (expected_key_ty, value_ty)) as m]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Map.remove (key)";
    begin match m with
      | Tmap _ -> Tmap (key_ty, value_ty)
      | Tbigmap _ -> Tbigmap (key_ty, value_ty)
      |  _ -> assert false
    end

  | Prim_map_mem,
    [ key_ty;
      (Tmap (expected_key_ty,_) | Tbigmap (expected_key_ty,_)) ]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Map.mem (key)";
    Tbool

  | Prim_set_mem,[ key_ty; Tset expected_key_ty]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Set.mem";
    Tbool

  | Prim_list_size, [ Tlist _]  ->  Tnat
  | Prim_set_size, [ Tset _]  ->  Tnat
  | Prim_map_size, [ Tmap _]  ->  Tnat

  | Prim_big_map_create, [ Tunit ] ->
    Tbigmap (fresh_tvar (), fresh_tvar ())

  | Prim_set_update, [ key_ty; Tbool; Tset expected_key_ty]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Set.update";
    Tset key_ty
  | Prim_set_add, [ key_ty; Tset expected_key_ty]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Set.add";
    Tset key_ty
  | Prim_set_remove, [ key_ty; Tset expected_key_ty]
    ->
    fail_neq ~loc ~expected_ty:expected_key_ty key_ty
      ~info:"in Set.remove";
    Tset key_ty

  | Prim_Some, [ ty ] -> Toption ty
  | Prim_self, [ Tunit ] -> Taddress
  | Prim_now, [ Tunit ] -> Ttimestamp
  | Prim_balance, [ Tunit ] -> Ttez
  | Prim_source, [ Tunit ] -> Taddress
  | Prim_sender, [ Tunit ] -> Taddress
  | Prim_amount, [ Tunit ] -> Ttez
  | Prim_gas, [ Tunit ] -> Tnat
  | Prim_pack, [ _ ] -> Tbytes
  | Prim_blake2b, [ Tbytes ] -> Tbytes
  | Prim_sha256, [ Tbytes ] -> Tbytes
  | Prim_sha512, [ Tbytes ] -> Tbytes
  | Prim_hash_key, [ Tkey ] -> Tkey_hash
  | Prim_chain_id, [ Tunit ] -> Tchainid

  | Prim_check, [ Tkey; Tsignature; Tbytes ] ->
    Tbool
  | Prim_check, _ ->
    error_prim loc Prim_check args [Tkey; Tsignature; Tbytes]

  | Prim_address, [ Tcontract _ ] ->
    Taddress

  | Prim_default_account, [ Tkey_hash ] -> unit_contract_ty

  | Prim_set_delegate, [ Toption Tkey_hash ] ->
    Toperation

  | Prim_exec _, [ ( Tlambda(from_ty, to_ty, _)
                   | Tclosure((from_ty, _), to_ty, _));
                   ty ] ->

    fail_neq ~loc ~expected_ty:from_ty ty
      ~info:"in argument of function application";
    to_ty

  | Prim_list_rev, [ Tlist ty ] -> Tlist ty

  | Prim_concat_two, [ Tstring; Tstring ] -> Tstring
  | Prim_concat_two, [ Tbytes; Tbytes ] -> Tbytes
  | Prim_string_concat, [ Tlist Tstring ] -> Tstring
  | Prim_bytes_concat, [ Tlist Tbytes ] -> Tbytes

  | Prim_Cons, [ head_ty; Tunit ] ->
    Tlist head_ty
  | Prim_Cons, [ head_ty; Tlist tail_ty ] ->
    fail_neq ~loc ~expected_ty:head_ty tail_ty
      ~info:"in list construction";
    Tlist tail_ty

  | Prim_string_size, [ Tstring ] -> Tnat
  | Prim_bytes_size, [ Tbytes ] -> Tnat

  | Prim_string_sub, [ Tnat; Tnat; Tstring ] -> Toption Tstring
  | Prim_bytes_sub, [ Tnat; Tnat; Tbytes ] -> Toption Tbytes

  | Prim_get_balance, [ Tcontract _ ] -> Ttez
  (* XXX : should be address -> tez option *)

  | Prim_block_level, [ Tunit ] -> Tnat
  | Prim_collect_call, [ Tunit ] -> Tbool
  | Prim_is_implicit, [ Tcontract ((None | Some "default"), Tunit) ] ->
    Toption Tkey_hash

  | prim, args_tys ->
    let nb_args, str_types = expected_prim_types prim in
    if List.length args <> nb_args then
      error loc "Wrong number of arguments for primitive %S: \
                 expected %d, got %d\n"
        (LiquidTypes.string_of_primitive prim)
        nb_args
        (List.length args_tys)
    else
      error loc "Wrong arguments types for for primitive %S.\n\
                 Expected: %s\n\
                 Got:%s\n"
        (LiquidTypes.string_of_primitive prim)
        str_types
        (String.concat ", "
           (List.map string_of_type args_tys))

and expected_prim_types = function
  | Prim_neq | Prim_lt | Prim_gt | Prim_eq | Prim_le | Prim_ge | Prim_compare ->
    2, "'a, 'a"
  | Prim_neg ->
    1, "int | nat"
  | Prim_add | Prim_sub ->
    2, Printf.sprintf
      "(nat | int | %s | timestamp), (nat | int | %s | timestamp)"
      (LiquidOptions.amount_type ()) (LiquidOptions.amount_type ())
  | Prim_mul | Prim_ediv ->
    2, Printf.sprintf
      "(nat | int | %s), (nat | int | %s)"
      (LiquidOptions.amount_type ()) (LiquidOptions.amount_type ())

  | Prim_xor | Prim_or ->
    2, "(bool | nat), (bool | nat)"
  | Prim_and ->
    2, "(bool | nat | int), (bool | nat)"
  | Prim_not ->
    1, "(bool | nat | int)"

  | Prim_abs | Prim_is_nat ->
    1, "int"
  | Prim_int ->
    1, "nat"

  | Prim_lsl | Prim_lsr ->
    2, "nat, nat"

  | Prim_map_update ->
    3, "'k, 'v option, (('k, 'v) map | ('k, 'v) big_map)"
  | Prim_map_add ->
    3, "'k, 'v, (('k, 'v) map | ('k, 'v) big_map)"
  | Prim_map_remove | Prim_map_mem | Prim_map_find ->
    2, "'k, (('k, 'v) map | ('k, 'v) big_map)"

  | Prim_set_mem ->
    2, "'a, 'a set"

  | Prim_list_size ->
    1, "'a list"
  | Prim_set_size ->
    1, "'a set"
  | Prim_map_size ->
    1, "('k, 'v) map"

  | Prim_set_update ->
    3, "'a, bool, 'a set"
  | Prim_set_add | Prim_set_remove ->
    2, "'a, 'a set"

  | Prim_Some | Prim_pack ->
    1, "'a"

  | Prim_self
  | Prim_now
  | Prim_balance
  | Prim_source
  | Prim_sender
  | Prim_amount
  | Prim_gas
  | Prim_chain_id
  | Prim_block_level
  | Prim_collect_call
  | Prim_big_map_create ->
    1, "unit"

  | Prim_blake2b
  | Prim_sha256
  | Prim_sha512 ->
    1, "bytes"

  | Prim_hash_key ->
    1, "key"

  | Prim_check ->
    3, "key, signature, bytes"

  | Prim_address | Prim_get_balance ->
    1, "<Contract handle>"

  | Prim_default_account ->
    1, "key_hash"

  | Prim_set_delegate ->
    1, "key_hash option"

  | Prim_exec _ ->
    2, "('a -> 'b), 'a"

  | Prim_list_rev ->
    1, "'a list"

  | Prim_concat_two ->
    2, "(string, string) | (bytes, bytes)"
  | Prim_string_concat ->
    1, "string list"
  | Prim_bytes_concat ->
    1, "bytes list"

  | Prim_Cons ->
    2, "'a, 'a list"

  | Prim_string_size ->
    1, "string"
  | Prim_bytes_size ->
    1, "bytes"

  | Prim_string_sub ->
    3, "nat, nat, string"
  | Prim_bytes_sub ->
    3, "nat, nat, bytes"

  | Prim_tuple -> assert false (* any number of argument *)

  | Prim_tuple_get ->
    1, "nat|int"
  | Prim_tuple_set ->
    2, "nat|int, 'a"

  | Prim_coll_mem ->
    2, "'a, ('a set | ('a, 'b) map | ('a, 'b) big_map)"
  | Prim_coll_find ->
    2, "'a, ('a set | ('a, 'b) map | ('a, 'b) big_map)"
  | Prim_coll_update ->
    3, "('a, bool, 'a set) | ('a, 'b option, (('a, 'b) map | ('a, 'b) big_map))"

  | Prim_coll_size ->
    1, "'a list | 'a set | ('a, 'b) map | string | bytes"

  | Prim_Left ->
    1, "'a"
  | Prim_Right ->
    1, "'a"
  | Prim_slice ->
    3, "nat, nat, bytes|string"
  | Prim_concat ->
    1, "bytes list | string list"

  | Prim_is_implicit ->
    1, "UnitContract.instance"

  | Prim_extension _
  | Prim_unused _
    -> assert false (* already handled *)


and typecheck_expected info env expected_ty exp =
  let exp = typecheck env exp in
  if exp.ty <> Tfail then
    if has_tvar exp.ty || has_tvar expected_ty then
      unify exp.loc exp.ty expected_ty
    else
      fail_neq ~info:("in "^info) ~loc:exp.loc ~expected_ty exp.ty;
  exp

and typecheck_apply ?name env prim loc args =
  let args = List.map (typecheck env) args in
  let prim, ty = typecheck_prim1 env prim loc args in
  mk ?name (Apply { prim; args }) ty


and typecheck_entry env entry =
  (* let env = { env with clos_env = None } in *)
  (* register storage *)
  let (env, count_storage) =
    new_binding env entry.entry_sig.storage_name env.t_contract_sig.f_storage in
  (* register parameter *)
  let (env, count_param) =
    new_binding env entry.entry_sig.parameter_name entry.entry_sig.parameter in
  let expected_ty = Ttuple [Tlist Toperation; env.t_contract_sig.f_storage] in
  (* Code for entry point must be of type (operation list * storage) *)
  let code =
    typecheck_expected "return value" env expected_ty entry.code in
  let expected_fee_ty = Ttuple [Ttez; Tnat] in
  (* Code for entry point must be of type (operation list * storage) *)
  let fee_code = match entry.fee_code with
    | None -> None
    | Some fee_code ->
      Some (typecheck_expected "fee return value"
              { env with warnings = false } expected_fee_ty fee_code)
  in
  let check_used v c =
    check_used env { nname = v; nloc = noloc env } c in
  check_used entry.entry_sig.parameter_name count_param;
  check_used entry.entry_sig.storage_name count_storage;
  { entry with code; fee_code }

and typecheck_contract ~others ~warnings ~decompiling contract =
  let rothers, rsubs =
    List.fold_left (fun (others, subs) c ->
        let c = typecheck_contract
            ~others ~warnings ~decompiling c in
        c :: others, c :: subs) (others, []) contract.subs in
  let others, subs = List.rev rothers, List.rev rsubs in

  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Typecheck contract %s@." (qual_contract_name contract);

  let t_contract_sig = sig_of_contract contract in
  let t_contract_sig =
    (* when decompiling recover signature of encoded Contract.self *)
    if not decompiling then t_contract_sig
    else match t_contract_sig.f_entries_sig with
      | [e] ->
        begin match expand e.parameter with
          | Tsum (_, l) when List.for_all (fun (e,_) -> is_entry_case e) l ->
            let f_entries_sig = List.map (fun (c, parameter) ->
                { entry_name = entry_name_of_case c;
                  parameter;
                  parameter_name = "parameter";
                  storage_name = "storage";
                }) l in
            { t_contract_sig with f_entries_sig }

          | _ -> t_contract_sig
        end
      | _ -> t_contract_sig in
  let env =
    {
      warnings;
      annot = false;
      decompiling;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      env = contract.ty_env;
      clos_env = None;
      t_contract_sig;
      ftvars = StringSet.empty;
      visible_contracts = others;
    } in

  (* Add bindings to the environment for the global values *)
  let env, values, glob_counts =
    List.fold_left (fun (env, values, counts) v ->
        let val_exp = typecheck env v.val_exp in
        let gen = match expand val_exp.ty with
          | Tlambda _ | Tclosure _ -> true
          | _ -> false in
        let (env, count) =
          new_binding env v.val_name ~effect:val_exp.effect ~gen val_exp.ty in
        let v = { v with val_exp } in
        env, (v :: values), ((v, count) :: counts)
      ) (env, [], []) contract.values in
  (* Typecheck entries *)
  let entries = List.map (typecheck_entry env) contract.entries in
  (* Report unused global values, for modules report only private ones *)
  List.iter (fun (v, count) ->
      if is_only_module contract && not v.val_private then ()
      else
        check_used env { nname = v.val_name; nloc = noloc env } (* TODO *) count
    ) glob_counts;
  let c_init = match contract.c_init with
    | None -> None
    | Some i ->
      let env, counts = List.fold_left (fun (env, counts) (arg, nloc, arg_ty) ->
          let (env, count) = new_binding env arg arg_ty in
          env, ({ nname = arg; nloc}, count) :: counts
        ) (env, []) i.init_args in
      let init_body = typecheck_expected "initial storage" env
          env.t_contract_sig.f_storage i.init_body in
      List.iter (fun (arg, count) ->
          check_used env arg count;
        ) counts;
      Some { i with init_body }
  in
  { contract with
    values = List.rev values;
    entries;
    c_init;
    subs }

let typecheck_contract
    ~warnings
    ~decompiling
    ?(monomorphise=true)
    ?(keep_tvars=false)
    contract =
  let contract =
    typecheck_contract ~others:[] ~warnings ~decompiling contract in
  if monomorphise then mono_contract ~keep_tvars contract else contract


let typecheck_code env ?expected_ty code =
  match expected_ty with
  | Some expected_ty -> typecheck_expected "value" env expected_ty code
  | None -> typecheck env code

let typecheck_const env ?(loc=noloc env) ?expected_ty cst =
  match expected_ty with
  | Some ty -> typecheck_const ~loc env cst ty |> snd
  | None -> typecheck_const ~loc env cst (type_of_const ~loc env cst) |> snd

(* XXX just for printing, do not use *)
let rec type_of_const = function
  | CUnit -> Tunit
  | CBool _ -> Tbool
  | CInt _ -> Tint
  | CNat _ -> Tnat
  | CTez _ -> Ttez
  | CTimestamp _ -> Ttimestamp
  | CString _ -> Tstring
  | CBytes _ -> Tbytes
  | CKey _ -> Tkey
  | CSignature _ -> Tsignature
  | CAddress _ -> Taddress
  | CTuple l ->
    Ttuple (List.map type_of_const l)
  | CNone -> Toption Tunit
  | CSome c -> Toption (type_of_const c)
  | CMap [] -> Tmap (Tint, Tunit)
  | CMap ((k,e) :: _) -> Tmap (type_of_const k, type_of_const e)

  | CBigMap (BMList [] | BMId _) -> Tbigmap (Tint, Tunit)
  | CBigMap BMList ((k,e) :: _) -> Tbigmap (type_of_const k, type_of_const e)

  | CList [] -> Tlist (Tunit)
  | CList (e :: _) -> Tlist (type_of_const e)

  | CSet [] -> Tset (Tint)
  | CSet (e :: _) -> Tset (type_of_const e)

  | CLeft c -> Tor (type_of_const c, fresh_tvar ())
  | CRight c -> Tor (fresh_tvar (), type_of_const c)

  | CKey_hash _ -> Tkey_hash

  | CLambda { arg_ty; ret_ty } -> Tlambda (arg_ty, ret_ty, default_uncurry ())

  (* XXX just for printing *)
  | CRecord _ -> Trecord ("<record>", [])
  | CConstr _ -> Tsum (None, [])
