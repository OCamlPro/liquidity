(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let noloc env = LiquidLoc.loc_in_file env.env.filename

let error loc msg =
  LiquidLoc.raise_error ~loc ("Type error: " ^^ msg ^^ "%!")

let mk_typed ?name (desc: (datatype, typed) exp_desc) ty = mk ?name desc ty
let mk ?name (desc: (datatype, encoded) exp_desc) ty =
  let name = match name with
    | Some n when n.[0] <> '_' -> name
    | _ -> None
  in
  mk ?name desc ty

let mk_typed_nat ~loc i =
  mk_typed
    (Const { loc; ty = Tnat; const = CNat (LiquidPrinter.integer_of_int i) })
    Tnat

let mk_nat ~loc i =
  mk
    (Const { loc; ty = Tnat; const = CNat (LiquidPrinter.integer_of_int i) })
    Tnat

let mk_nil ~loc list_ty =
  mk (Const { loc; ty = list_ty; const = CList [] }) list_ty

let mk_typed_nil ~loc list_ty =
  mk_typed (Const { loc; ty = list_ty; const = CList [] }) list_ty

let mk_tuple loc args =
  let tuple_ty = Ttuple (List.map (fun t -> t.ty) args) in
  mk (Apply { prim = Prim_tuple; loc; args }) tuple_ty

let const_unit ~loc = mk (Const { loc; ty = Tunit; const = CUnit }) Tunit

let const_true ~loc = mk (Const { loc; ty = Tbool; const = CBool true }) Tbool

let const_false ~loc = mk (Const {loc; ty = Tbool; const = CBool false }) Tbool

let unused env ~loc ?constr ty =
  mk (Apply { prim = Prim_unused constr; loc = noloc env;
              args = [const_unit ~loc] }) ty

let uniq_ident env name =
  env.counter := !(env.counter) + 1;
  Printf.sprintf "%s/%d" name !(env.counter)

(* let fresh_tmp = *)
(*   let cpt = ref 0 in *)
(*   fun () -> *)
(*     incr cpt; *)
(*     Printf.sprintf "tmp#%d" !cpt *)

let new_binding env name ?(fail=false) ty =
  let new_name = uniq_ident env name in
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (new_name, ty, fail) env.vars;
              vars_counts = StringMap.add new_name count env.vars_counts;
            } in
  (new_name, env, count)

(* Find variable name in either the global environment or the closure
   environment, returns a corresponding expression *)
let find_var ?(count_used=true) env loc name =
  try
    let vname = name in
    let (name, ty, fail) = StringMap.find name env.vars in
    let count = StringMap.find name env.vars_counts in
    if count_used then incr count;
    let aname =
      if env.annot then Some vname
      else None in
    let exp = mk ?name:aname (Var { name; loc }) ty in
    { exp with fail }
  with Not_found ->
  match env.clos_env with
  | None -> error loc "unbound variable %S" name
  | Some ce ->
    try
      let v, (cpt_in, cpt_out) = StringMap.find name ce.env_bindings in
      if count_used then begin
        incr cpt_in;
        incr cpt_out;
      end;
      v
    with Not_found ->
      error loc "unbound variable %S" name

(* Create environment for closure *)
let env_for_clos env loc bvs arg_name arg_type =
  let _, free_vars = StringSet.fold (fun v (index, free_vars) ->
      let index = index + 1 in
      let free_vars =
        try
          let (bname, btype, _) = StringMap.find v env.vars in
          let cpt_out = StringMap.find bname env.vars_counts in
          StringMap.add v (bname, btype, index, (ref 0, cpt_out)) free_vars
        with Not_found ->
        match env.clos_env with
        | None -> free_vars
        | Some ce ->
          try
            let bname, btype, _, (cpt_in, cpt_out) =
              StringMap.find v ce.env_vars in
            StringMap.add v (bname, btype, index, (cpt_in, cpt_out)) free_vars
          with Not_found -> free_vars
      in
      (index, free_vars)
    ) bvs (0, StringMap.empty)
  in
  let free_vars_l =
    StringMap.bindings free_vars
    |> List.sort (fun (_, (_,_,i1,_)) (_, (_,_,i2,_)) -> compare i1 i2)
  in
  let ext_env = env in
  let env = { env with vars = StringMap.empty } in
  match free_vars_l with
  | [] -> (* no closure environment *)
    let (new_name, env, _) = new_binding env arg_name arg_type in
    env, new_name, arg_type, []
  | _ ->
    let env_arg_name = uniq_ident env "closure_env" in
    let env_arg_type =
      Ttuple (arg_type :: List.map (fun (_, (_,ty,_,_)) -> ty) free_vars_l) in
    let env_arg_var = mk (Var { name = env_arg_name; loc }) env_arg_type in
    let new_name = uniq_ident env arg_name in
    let env_vars =
      StringMap.add arg_name
        (new_name, arg_type, 0, (ref 0, ref 0)) free_vars in
    (* let size = StringMap.cardinal env_vars in *)
    let env_bindings =
      StringMap.map (fun (name, ty, index, count) ->
          let ei = mk_nat ~loc index in
          let exp = mk ~name
              (Apply { prim = Prim_tuple_get; loc; args = [env_arg_var; ei] })
              ty in
          exp, count
        ) env_vars
    in
    let call_bindings = List.map (fun (name, _) ->
        name, find_var ~count_used:false ext_env loc name
      ) free_vars_l
    in
    (* Format.eprintf "--- Closure %s ---@." env_arg_name; *)
    (* StringMap.iter (fun name (e,_) -> *)
    (*     Format.eprintf "%s -> %s@." *)
    (*       name (LiquidPrinter.Liquid.string_of_code e) *)
    (*   ) env_bindings; *)
    let env_closure = {
      env_vars;
      env_bindings;
      call_bindings;
    } in
    let env =
      { env with
        clos_env = Some env_closure
      }
    in
    env, env_arg_name, env_arg_type, call_bindings


let rec encode_type ty =
  (* if env.only_typecheck then ty *)
  (* else *)
  match ty with
  | Ttez | Tunit | Ttimestamp | Tint | Tnat | Tbool | Tkey | Tkey_hash
  | Tsignature | Tstring | Tbytes | Toperation | Taddress | Tfail -> ty
  | Ttuple tys ->
    let tys' = List.map encode_type tys in
    if List.for_all2 (==) tys tys' then ty
    else Ttuple tys'
  | Tset t | Tlist t | Toption t ->
    let t' = encode_type t in
    if t' == t then ty
    else begin match ty with
      | Tset t -> Tset t'
      | Tlist t -> Tlist t'
      | Toption t -> Toption t'
      | _ -> assert false
    end
  | Tor (t1, t2) | Tlambda (t1, t2)
  | Tbigmap (t1, t2) | Tmap (t1, t2) ->
    let t1', t2' = encode_type t1, encode_type t2 in
    if t1 == t1' && t2 == t2' then ty
    else begin match ty with
      | Tor (t1, t2) -> Tor (t1', t2')
      | Tlambda (t1, t2) -> Tlambda (t1', t2')
      | Tmap (t1, t2) -> Tmap (t1', t2')
      | Tbigmap (t1, t2) -> Tbigmap (t1', t2')
      | _ -> assert false
    end
  | Tclosure  ((t1, t2), t3) ->
    let t1' = encode_type t1 in
    let t2' = encode_type t2 in
    let t3' = encode_type t3 in
    if t1 == t1' && t2 == t2' && t3 == t3' then ty
    else Tclosure ((t1', t2'), t3')
  | Trecord (name, labels) ->
    (* encode_record_type labels *)
    Trecord (name, List.map (fun (l, ty) -> l, encode_type ty) labels)
  | Tsum (name, cstys) ->
    (* encode_sum_type cstys *)
    Tsum (name, List.map (fun (c, ty) -> c, encode_type ty) cstys)
  | Tcontract contract_sig ->
    let parameter = encode_type (encode_contract_sig contract_sig) in
    Tcontract { contract_sig with entries_sig = [{
        entry_name = "main";
        parameter_name = "parameter";
        storage_name = "storage";
        parameter;
      }] }

and encode_record_type labels =
  Ttuple (List.map (fun (_, ty) -> encode_type ty) labels)

and encode_sum_type cstys =
  let rec rassoc = function
    | [] -> assert false
    | [_, ty] -> encode_type ty
    | (_, lty) :: rstys ->
      Tor (encode_type lty, rassoc rstys)
  in
  rassoc cstys

and encode_contract_sig csig =
  match csig.entries_sig with
  | [] -> assert false (* ? *)
  | [{ parameter }] -> parameter
  | entries ->
      Tsum ("_entries",
            List.map (fun { entry_name; parameter = t } ->
              (prefix_entry ^ entry_name, t)
              ) entries)

let rec has_big_map = function
  | Tbigmap (_t1, _t2) -> true
  | Ttez | Tunit | Ttimestamp | Tint | Tnat | Tbool | Tkey | Tkey_hash
  | Tsignature | Tstring | Tbytes | Toperation | Taddress | Tfail -> false
  | Ttuple tys ->
    List.exists has_big_map tys
  | Tset t | Tlist t | Toption t -> has_big_map t
  | Tor (t1, t2) | Tlambda (t1, t2)
  | Tmap (t1, t2) ->
    has_big_map t1 || has_big_map t2
  | Tclosure  ((t1, t2), t3) ->
    has_big_map t1 || has_big_map t2 || has_big_map t3
  | Trecord (_, labels) ->
    List.exists (fun (_, ty) -> has_big_map ty) labels
  | Tsum (_, cstys) ->
    List.exists (fun (_, ty) -> has_big_map ty) cstys
  | Tcontract { entries_sig } ->
    List.exists (fun { parameter } -> has_big_map parameter) entries_sig

let encode_storage_type env ty =
  let ty = encode_type ty in
  match ty with
  | Ttuple (Tbigmap (t1, t2) :: r)
    when not @@ List.exists has_big_map (t1 :: t2 :: r) -> ty
  | Trecord (_, ((_, Tbigmap (t1, t2)) :: r))
    when not @@ List.exists has_big_map (t1 :: t2 :: List.map snd r) -> ty
  | _ when not (has_big_map ty) -> ty
  | _ ->
    error (noloc env)
      "only one big map is only allowed as first component of storage \
       (either a tuple or a record)"

let encode_parameter_type env ty =
  let ty = encode_type ty in
  if has_big_map ty then
    error (noloc env) "big maps are not allowed in parameter type";
  ty

let encode_return_type env ty =
  let ty = encode_type ty in
  if has_big_map ty then
    error (noloc env) "big maps are not allowed in return type";
  ty

let rec encode_const env c = match c with
  | CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
  | CBytes _ | CKey _ | CContract _ | CSignature _ | CNone  | CKey_hash _
  | CAddress _ -> c

  | CSome x -> CSome (encode_const env x)
  | CLeft x -> CLeft (encode_const env x)
  | CRight x -> CRight (encode_const env x)

  | CTuple xs -> CTuple (List.map (encode_const env) xs)
  | CList xs -> CList (List.map (encode_const env) xs)
  | CSet xs -> CSet (List.map (encode_const env) xs)

  | CMap l ->
    CMap (List.map (fun (x,y) -> encode_const env x, encode_const env y) l)

  | CBigMap l ->
    CBigMap (List.map (fun (x,y) -> encode_const env x, encode_const env y) l)

  | CRecord labels ->
    CTuple (List.map (fun (_, x) -> encode_const env x) labels)

  | CConstr (constr, x) when env.decompiling ->
    CConstr (constr, encode_const env x)

  | CConstr (constr, x) when is_entry_case constr ->
    let entry_name = entry_name_of_case constr in
    let rec iter entries =
      match entries with
      | [] -> assert false
      | [e] ->
        if e.entry_name <> entry_name then
          error (noloc env)  "unknown entry point %s" entry_name;
        encode_const env x
      | e :: entries ->
        if e.entry_name = entry_name then CLeft (encode_const env x)
        else CRight (iter entries)
    in
    iter env.t_contract_sig.entries_sig

  | CConstr (constr, x) ->
    try
      let ty_name, _ = StringMap.find constr env.env.constrs in
      let constr_ty = StringMap.find ty_name env.env.types in
      match constr_ty with
      | Tsum (_, constrs) ->
        let rec iter constrs =
          match constrs with
          | [] -> assert false
          | [c, _] ->
            assert (c = constr);
            encode_const env x
          | (c, _) :: constrs ->
            if c = constr then CLeft (encode_const env x)
            else CRight (iter constrs)
        in
        iter constrs
      | _ -> raise Not_found
    with Not_found ->
      error (noloc env)  "unknown constructor %s" constr

let rec deconstify loc ty c =
  if not @@ type_contains_nonlambda_operation ty then
    mk (Const { loc; ty; const = c }) ty
  else match c, (encode_type ty) with
    | (CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
      | CBytes _
      | CKey _ | CContract _ | CSignature _ | CNone  | CKey_hash _ | CAddress _),
      _ ->
      mk (Const { loc; ty; const =c }) ty

    | CSome c, Toption ty' ->
      mk (Apply { prim = Prim_Some; loc; args = [deconstify loc ty' c] }) ty

    | CLeft c, Tor (ty', _) ->
      mk (Apply { prim = Prim_Left; loc; args = [deconstify loc ty' c] }) ty
    | CRight c, Tor (_, ty') ->
      mk (Apply { prim = Prim_Right; loc; args = [deconstify loc ty' c] }) ty

    | CTuple cs, Ttuple tys ->
      mk (Apply { prim = Prim_tuple; loc;
                  args = List.map2 (deconstify loc) tys cs }) ty

    | CList cs, Tlist ty' ->
      List.fold_right (fun c acc ->
          mk (Apply { prim = Prim_Cons; loc;
                      args = [deconstify loc ty' c; acc] }) ty
        )
        cs
        (mk (Const { loc; ty; const = CList [] }) ty)

    | CSet cs, Tset ty' ->
      List.fold_right (fun c acc ->
          mk (Apply { prim = Prim_set_add; loc;
                      args = [deconstify loc ty' c; acc] }) ty
        )
        cs
        (mk (Const { loc; ty; const = CSet [] }) ty)

    | CMap cs, Tmap (tk, te) ->
      List.fold_right (fun (k, e) acc ->
          mk (Apply { prim = Prim_map_add; loc;
                      args = [deconstify loc tk k; deconstify loc te e; acc] })
            ty
        )
        cs
        (mk (Const { loc; ty; const = CMap [] }) ty)

    | CBigMap cs, Tbigmap (tk, te) ->
      List.fold_right (fun (k, e) acc ->
          mk (Apply { prim = Prim_map_add; loc;
                      args = [deconstify loc tk k; deconstify loc te e; acc] })
            ty
        )
        cs
        (mk (Const { loc; ty; const = CBigMap [] }) ty)

    (* Removed by encode const *)
    | CRecord _, _
    | CConstr _, _ -> assert false

    | _, _ ->
      Format.eprintf "%s : %s@."
        (LiquidPrinter.Liquid.string_of_const c)
        (LiquidPrinter.Liquid.string_of_type ty);
      assert false

let rec decr_counts_vars env e =
  if e.fail then () else
  match e.desc with
  | Var { name } ->
    begin try
      let count = StringMap.find name env.vars_counts in
      decr count
      with Not_found -> ()
    end

  | Const _ -> ()

  | Failwith { arg = e }
  | Project { record = e }
  | Constructor { arg = e }
  | ContractAt { arg = e }
  | Unpack { arg = e }
  | Lambda { body = e } -> decr_counts_vars env e

  | SetField { record = e1; set_val = e2 }
  | Seq (e1, e2)
  | Let { bnd_val = e1; body = e2 }
  | Loop { body = e1; arg = e2 }
  | Map { body = e1; arg = e2 } ->
    decr_counts_vars env e1;
    decr_counts_vars env e2;

  | If { cond = e1 ; ifthen = e2 ; ifelse = e3 }
  | MatchNat { arg = e1; ifplus = e2; ifminus = e3 }
  | MatchList { arg = e1; ifnil = e2; ifcons = e3 }
  | MatchOption { arg = e1; ifnone = e2; ifsome = e3 }
  | Fold { body = e1; arg = e2; acc = e3 }
  | MapFold { body = e1; arg = e2; acc = e3 }
  | Transfer { contract = e1; amount = e2; arg = e3 } ->
    decr_counts_vars env e1;
    decr_counts_vars env e2;
    decr_counts_vars env e3;

  | Apply { args }
  | CreateContract { args } ->
    List.iter (decr_counts_vars env) args

  | Closure { call_env; body } ->
    decr_counts_vars env body;
    List.iter (fun (_, e) -> decr_counts_vars env e) call_env;

  | Record { fields } ->
    List.iter (fun (_, e) -> decr_counts_vars env e) fields

  | MatchVariant { arg; cases } ->
    decr_counts_vars env arg;
    List.iter (fun (_, e) -> decr_counts_vars env e) cases


let rec encode env ( exp : typed_exp ) : encoded_exp =
  match exp.desc with

  | Const { loc; ty; const } ->
    let const = encode_const env const in
    (* use functions instead of constants if contains operations *)
    let c = deconstify loc ty const in
    mk ?name:exp.name c.desc ty

  | Let { bnd_var; inline; loc; bnd_val; body } ->
     let bnd_val = encode env bnd_val in
     let bnd_val = if env.annot && bnd_val.name = None
       then { bnd_val with name = Some bnd_var }
       else bnd_val
     in
     let (new_name, env, count) =
       new_binding env bnd_var ~fail:bnd_val.fail bnd_val.ty in
     if inline then (* indication for closure encoding *)
       env.force_inline := StringMap.add bnd_var bnd_val !(env.force_inline);
     let body = encode env body in
     (* check_used env name loc count; *)
     if not bnd_val.transfer (* no inlining of values with transfer *) then begin
       match !count with
       | c when c <= 0 ->
         if bnd_val.fail then
           () (* No inling of values with side effects which don't
                 appear later on *)
         else begin
           decr_counts_vars env bnd_val;
           env.to_inline :=
             StringMap.add new_name (const_unit ~loc) !(env.to_inline)
         end
       | c when c = 1 || inline ->
         env.to_inline := StringMap.add new_name bnd_val !(env.to_inline)
       | _ -> ()
     end;
     mk ?name:exp.name
       (Let { bnd_var = new_name; inline; loc; bnd_val; body }) body.ty

  | Var { name; loc } -> find_var env loc name

  | Project { loc; field; record } ->
    let record = encode env record in
    mk ?name:exp.name (Project { loc; field; record }) exp.ty

  | SetField { record; loc; field; set_val } ->
    let record = encode env record in
    let set_val = encode env set_val in
    mk ?name:exp.name (SetField { record; loc; field; set_val }) exp.ty

  | Seq (exp1, exp2) ->
    (* TODO: if not exp.fail then remove exp1 *)
    let exp1 = encode env exp1 in
    let exp2 = encode env exp2 in
    mk ?name:exp.name (Seq (exp1, exp2)) exp.ty

  | If { cond; ifthen; ifelse } ->
    let cond = encode env cond in
    let ifthen = encode env ifthen in
    let ifelse = encode env ifelse in
    mk ?name:exp.name (If { cond; ifthen; ifelse }) exp.ty

  | Transfer { loc; contract; amount; entry; arg } ->
    let amount = encode env amount in
    let contract = encode env contract in
    let arg =  match entry with
      | None -> arg
      | Some _
        when match contract.ty with
          | Tcontract { entries_sig = [_] } -> true
          | _ -> false
        -> arg
      | Some entry ->
        let arg_ty = encode_type contract.ty in
        (* constant type not yet encoded *)
        mk_typed ?name:arg.name
          (Constructor { loc; constr = Constr (prefix_entry ^ entry); arg })
          arg_ty
    in
    let arg = encode env arg in
    mk ?name:exp.name
      (Transfer { loc; contract; amount; entry = None; arg }) Toperation

  | Failwith { arg; loc } ->
    let arg = encode env arg in
    mk (Failwith { arg; loc }) Tfail

  | Apply { prim = Prim_unknown } -> assert false

  (* List.rev -> List.reduce (::) *)
  | Apply { prim = Prim_list_rev; loc; args = [l] } ->
    let l = encode env l in
    let elt_ty = match l.ty with
      | Tlist ty -> ty
      | _ -> assert false
    in
    let list_ty = l.ty in
    let arg_name = uniq_ident env "arg" in
    let arg_ty = Ttuple [elt_ty; list_ty] in
    let arg = mk (Var { name = arg_name; loc }) arg_ty in
    let e =
      mk (Apply { prim = Prim_tuple_get; loc; args = [arg; mk_nat ~loc 0] })
        elt_ty in
    let acc =
      mk (Apply { prim = Prim_tuple_get; loc; args = [arg; mk_nat ~loc 1] })
        list_ty in
    let f_body = mk (Apply { prim = Prim_Cons; loc; args = [e; acc] }) list_ty in
    let empty_acc = mk_nil ~loc list_ty in
    let desc = Fold { prim = Prim_list_fold;
                      arg_name; loc;
                      body = f_body;
                      arg = l;
                      acc = empty_acc } in
    mk ?name:exp.name desc list_ty

  (* concat x y => concat [x; y] *)
  | Apply { prim = Prim_concat_two; loc; args = [x; y] } ->
    let prim = match x.ty with
      | Tstring -> Prim_string_concat
      | Tbytes -> Prim_bytes_concat
      | _ -> assert false in
    let ty = Tlist x.ty in
    let l =
      mk_typed
        (Apply { prim = Prim_Cons; loc;
                 args = [x; mk_typed (
                     Apply { prim = Prim_Cons; loc;
                             args = [y; mk_typed_nil ~loc ty] }) ty] }) ty in
    encode env { exp with desc = Apply { prim; loc; args = [l] } }

  | Apply { prim; loc; args } ->
    encode_apply exp.name env prim loc args exp.ty

  | MatchOption { arg; loc; ifnone; some_name; ifsome } ->
     let arg = encode env arg in
     let name_ty = match arg.ty with
       | Toption ty -> ty
       | _ -> assert false
     in
     let ifnone = encode env ifnone in
     let (some_name, env, count) = new_binding env some_name name_ty in
     let ifsome = encode env ifsome in
     mk ?name:exp.name
       (MatchOption { arg; loc; ifnone; some_name; ifsome }) exp.ty

  | MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus } ->
     let arg = encode env arg in
     let (plus_name, env2, count_p) = new_binding env plus_name Tnat in
     let ifplus = encode env2 ifplus in
     let (minus_name, env3, count_m) = new_binding env minus_name Tnat in
     let ifminus = encode env3 ifminus in
     (* check_used env plus_name loc count_p; *)
     (* check_used env minus_name loc count_m; *)
     mk ?name:exp.name
       (MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus }) exp.ty

  | Loop { arg_name; loc; body; arg } ->
     let arg = encode env arg in
     let (arg_name, env, count) = new_binding env arg_name arg.ty in
     let body = encode env body in
     (* check_used env name loc count; *)
     mk ?name:exp.name (Loop { arg_name; loc; body; arg }) exp.ty

  | Fold { prim; arg_name; loc; body; arg; acc } ->
    let arg = encode env arg in
    let acc = encode env acc in
    let name_ty = match prim, arg.ty with
      | Prim_map_iter, Tmap (k_ty, v_ty) -> Ttuple [k_ty; v_ty]
      | Prim_set_iter, Tset elt_ty -> elt_ty
      | Prim_list_iter, Tlist elt_ty -> elt_ty
      | Prim_map_fold, Tmap (k_ty, v_ty) -> Ttuple [Ttuple [k_ty; v_ty]; acc.ty]
      | Prim_set_fold, Tset elt_ty -> Ttuple [elt_ty; acc.ty]
      | Prim_list_fold, Tlist elt_ty -> Ttuple [elt_ty; acc.ty]
      | _ -> assert false
    in
    let (arg_name, env, count) = new_binding env arg_name name_ty in
    let body = encode env body in
    mk ?name:exp.name (Fold { prim; arg_name; loc; body; arg; acc }) exp.ty

  | Map { prim; arg_name; loc; body; arg } ->
    let arg = encode env arg in
    let name_ty = match prim, arg.ty with
      | Prim_map_map, Tmap (k_ty, v_ty) -> Ttuple [k_ty; v_ty]
      | Prim_set_map, Tset elt_ty -> elt_ty
      | Prim_list_map, Tlist elt_ty -> elt_ty
      | _ -> assert false
    in
    let (arg_name, env, count) = new_binding env arg_name name_ty in
    let body = encode env body in
    mk ?name:exp.name (Map { prim; arg_name; loc; body; arg }) exp.ty

  | MapFold { prim; arg_name; loc; body; arg; acc } ->
    let arg = encode env arg in
    let acc = encode env acc in
    let name_ty = match prim, arg.ty with
      | Prim_map_map_fold, Tmap (k_ty, v_ty) ->
        Ttuple [Ttuple [k_ty; v_ty]; acc.ty]
      | Prim_set_map_fold, Tset elt_ty ->
        Ttuple [elt_ty; acc.ty]
      | Prim_list_map_fold, Tlist elt_ty ->
        Ttuple [elt_ty; acc.ty]
      | _ -> assert false
    in
    let (arg_name, env, count) = new_binding env arg_name name_ty in
    let body = encode env body in
    mk ?name:exp.name (MapFold { prim; arg_name; loc; body; arg; acc }) exp.ty

  | MatchList { arg; loc; head_name; tail_name; ifcons; ifnil } ->
     let arg = encode env arg in
     let elt_ty = match arg.ty with
       | Tlist ty -> ty
       | _ -> assert false
     in
     let ifnil = encode env ifnil in
     let (head_name, env, count) = new_binding env head_name elt_ty in
     let (tail_name, env, count) = new_binding env tail_name arg.ty in
     let ifcons = encode env ifcons in
     (* check_used env head_name loc count; *)
     (* check_used env tail_name loc count; *)
     mk ?name:exp.name
       (MatchList { arg; loc; head_name; tail_name; ifcons; ifnil })
       exp.ty

  | Lambda { arg_name; arg_ty; loc; body } ->
     let env_at_lambda = env in
     let lambda_arg_type = arg_ty in
     let lambda_arg_name = arg_name in
     let lambda_body = body in
     let bvs = LiquidBoundVariables.bv exp in
     if StringSet.is_empty bvs ||
        StringSet.for_all (fun bv -> StringMap.mem bv !(env.force_inline)) bvs
     then
       (* not a closure (or will be pure after inlining),
          create a real lambda *)
       let env = { env_at_lambda with
                   vars = StringSet.fold (fun bv ->
                       StringMap.add bv (StringMap.find bv env.vars)
                     ) bvs StringMap.empty
                 } in
       let (arg_name, env, arg_count) =
         new_binding env lambda_arg_name lambda_arg_type in
       let body = encode env lambda_body in
       (* check_used env lambda_arg_name loc arg_count; *)
       let ty = Tlambda (lambda_arg_type, body.ty) in
       mk ?name:exp.name (Lambda { arg_name; arg_ty = lambda_arg_type; loc;
                                   body; ret_ty = body.ty }) ty
     else
       (* create closure with environment *)
       let env, arg_name, arg_ty, call_env =
         env_for_clos env loc bvs arg_name arg_ty in
       let body = encode env body in
       (* begin match env.clos_env with *)
       (*   | None -> () *)
       (*   | Some clos_env -> *)
       (*     Format.eprintf "--- Closure %s ---@." arg_name; *)
       (*     StringMap.iter (fun name (e, (cpt_in, cpt_out)) -> *)
       (*         Format.eprintf "%s -> %s , (%d, %d)@." *)
       (*           name (LiquidPrinter.Liquid.string_of_code e) !cpt_in !cpt_out *)
       (*       ) clos_env.env_bindings *)
       (* end; *)
       let desc =
         Closure { arg_name; arg_ty; loc; call_env; body; ret_ty = body.ty } in
       let call_env_type = match call_env with
         | [] -> assert false
         | [_, t] -> t.ty
         | _ -> Ttuple (List.map (fun (_, t) -> t.ty) call_env)
       in
       let ty = Tclosure ((lambda_arg_type, call_env_type), body.ty) in
       mk ?name:exp.name desc ty

  (* Closures are created by encoding phase *)
  | Closure _ -> assert false

  | Record { loc; fields } ->
    let fields = List.map (fun (label, exp) ->
        label, encode env exp
      ) fields in
    let desc = Record { loc; fields } in
    mk ?name:exp.name desc exp.ty

  | Constructor { loc; constr = Constr c; arg } when env.decompiling ->
    let arg = encode env arg in
    let desc = Constructor { loc; constr = Constr c; arg } in
    mk ?name:exp.name desc exp.ty

  | Constructor { loc; constr = Constr constr; arg } ->
     let ty_name, arg_ty = StringMap.find constr env.env.constrs in
     let arg = encode env arg in
     let constr_ty = StringMap.find ty_name env.env.types in
     let exp =
       match constr_ty with
       | Tsum (_, constrs) ->
         let rec iter constrs orty =
           match constrs, orty with
           | [], _ -> assert false
           | [c, _], orty ->
             assert (c = constr);
             arg
           | (c, cty) :: constrs, orty ->
             let left_ty, right_ty = match orty with
               | Tor (left_ty, right_ty) -> left_ty, right_ty
               | Tsum (_, [_, left_ty; _, right_ty]) -> left_ty, right_ty
               | Tsum (_, (_, left_ty) :: rcstrs) ->
                 left_ty, Tsum ("", rcstrs)
               | _ -> assert false
             in
             let desc =
               if c = constr then
                 (* We use an unused argument to carry the type to
                    the code generator *)
                 Apply { prim = Prim_Left; loc;
                         args = [arg; unused env ~loc ~constr right_ty] }
               else
                 let arg = iter constrs right_ty in
                 let u = match constrs with
                   | [_] -> unused env ~loc ~constr left_ty
                   | _ ->
                     (* marker for partially contructed values *)
                     unused env ~loc ~constr:"_" left_ty
                 in
                 Apply { prim = Prim_Right; loc; args =  [arg; u] }
             in
             mk desc orty
         in
         iter constrs (encode_type constr_ty)
       | _ -> assert false
     in
     mk ?name:exp.name exp.desc constr_ty

  | Constructor { loc; constr = Left right_ty; arg } ->
     let arg = encode env arg in
     let ty = Tor(arg.ty, right_ty) in
     let desc = Apply { prim = Prim_Left; loc;
                        args = [arg; unused env ~loc right_ty] } in
     mk ?name:exp.name desc ty

  | Constructor { loc; constr = Right left_ty; arg } ->
     let arg = encode env arg in
     let ty = Tor (left_ty, arg.ty) in
     let desc = Apply { prim = Prim_Right; loc;
                        args = [arg; unused env ~loc left_ty] } in
     mk ?name:exp.name desc ty

  | MatchVariant { arg; loc; cases } ->
    let arg = encode env arg in
    let constrs = match arg.ty with
      | Tsum (_, constrs) -> constrs
      | Tor (left_ty, right_ty) ->
        [ "Left", left_ty; "Right", right_ty]
      | _ -> assert false
    in
    let cases = List.map (fun (pat, e) ->
        let pat, env = match pat with
          | CAny | CConstr (_, []) -> pat, env
          | CConstr (c, [var]) ->
            let var_ty = List.assoc c constrs in
            let (var, env, _) = new_binding env var var_ty in
            CConstr (c, [var]), env
          | CConstr _ -> assert false in
        (pat, encode env e)
      ) cases
    in
    mk ?name:exp.name (MatchVariant { arg; loc; cases }) exp.ty

  | ContractAt { loc; arg; c_sig } ->
    let arg = encode env arg in
    mk ?name:exp.name (ContractAt { loc; arg; c_sig }) exp.ty

  | Unpack { loc; arg; ty } ->
    let arg = encode env arg in
    mk ?name:exp.name (Unpack { loc; arg; ty }) exp.ty

  | CreateContract { loc; args; contract } ->
    let args = List.map (encode env) args in
    let contract, c_to_inline =
      encode_contract ~annot:env.annot env.env contract in
    (* Performed inlining and simplifications on subcontract at encoding time *)
    let contract =
      LiquidSimplify.simplify_contract ~decompile_annoted:env.decompiling
        contract c_to_inline in
    mk ?name:exp.name (CreateContract { loc; args; contract }) exp.ty


and encode_apply name env prim loc args ty =
  let args = List.map (encode env) args in
  match prim, args with
  | Prim_exec, [ x; { ty = Tclosure (_, ty) | Tlambda (_,ty) } ] ->
    mk ?name (Apply { prim; loc; args }) ty

  | _ -> mk ?name (Apply { prim; loc; args }) ty


and encode_entry env entry =
  (* "storage/1" *)
  let (storage_name, env, _) =
    new_binding env entry.entry_sig.storage_name env.t_contract_storage in
  (* "parameter/2" *)
  let (parameter_name, env, _) =
    new_binding env entry.entry_sig.parameter_name entry.entry_sig.parameter in
  {
    entry_sig = {
      entry.entry_sig with
      parameter = encode_parameter_type env entry.entry_sig.parameter;
      parameter_name;
      storage_name;
    };
    code = encode env entry.code;
  }


and encode_contract ?(annot=false) ?(decompiling=false) env contract =
  let env =
    {
      warnings=false;
      annot;
      decompiling;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      force_inline = ref StringMap.empty;
      env;
      clos_env = None;
      t_contract_sig = sig_of_contract contract;
      t_contract_storage = contract.storage;
    } in

  let parameter = encode_contract_sig env.t_contract_sig  in
  let loc = LiquidLoc.loc_in_file env.env.filename in
  let rec values_on_top l exp = match l with
    | [] -> exp
    | (bnd_var, inline, bnd_val) :: rest ->
      mk_typed (Let { bnd_var; inline; loc; bnd_val;
                      body = values_on_top rest exp }) exp.ty in
  let code_desc, parameter_name, storage_name = match contract.entries with
    | [e] -> e.code.desc, e.entry_sig.parameter_name, e.entry_sig.storage_name
    | _ ->
      let parameter = mk_typed (Var { name = "parameter"; loc }) parameter in
      MatchVariant {
        arg = parameter;
        loc;
        cases = List.map (fun e ->
            let constr = prefix_entry ^ e.entry_sig.entry_name in
            env.env.constrs <-
              StringMap.add constr ("_entries", e.entry_sig.parameter)
                env.env.constrs;
            let pat =
              CConstr (constr, [e.entry_sig.parameter_name]) in
            let body =
              mk_typed
                (Let { bnd_var = e.entry_sig.storage_name;
                       inline = false; loc;
                       bnd_val =
                         mk_typed
                           (Var { name = "storage"; loc })
                           env.t_contract_storage;
                       body = e.code }) e.code.ty in
            pat, body
          ) contract.entries },
      "parameter",
      "storage"
  in

  (* "storage/1" *)
  let (storage_name, env, _) =
    new_binding env storage_name env.t_contract_storage in
  (* "parameter/2" *)
  let (pname, env, _) = new_binding env parameter_name parameter in

  let code = encode env @@
    values_on_top contract.values @@
    mk_typed code_desc (Ttuple [Tlist Toperation; contract.storage]) in
  let contract = {
    contract_name = contract.contract_name;
    values = [];
    storage = encode_storage_type env contract.storage;
    entries = [{
        entry_sig = {
          entry_name = "main";
          parameter_name = pname;
          storage_name;
          parameter = encode_parameter_type env parameter;
        };
        code;
      }];
  } in
  contract, !(env.to_inline)


let encode_code tenv code =
  encode tenv code


let encode_const env t_contract_sig t_contract_storage const =
  let env =
    {
      warnings = false;
      annot = false;
      decompiling = false;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      force_inline = ref StringMap.empty;
      env = env;
      clos_env = None;
      t_contract_sig;
      t_contract_storage;
    } in

  encode_const env const
