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
let mk ?name (desc: (datatype, encoded) exp_desc) ty = mk ?name desc ty

let mk_typed_nat i =
  mk_typed (Const (Tnat, CNat (LiquidPrinter.integer_of_int i))) Tnat

let mk_nat i =
  mk (Const (Tnat, CNat (LiquidPrinter.integer_of_int i))) Tnat

let mk_nil list_ty =
  mk (Const (list_ty, CList [])) list_ty

let mk_typed_nil list_ty =
  mk_typed (Const (list_ty, CList [])) list_ty

let mk_tuple loc l =
  let tuple_ty = Ttuple (List.map (fun t -> t.ty) l) in
  mk (Apply (Prim_tuple, loc, l)) tuple_ty

let const_unit = mk (Const (Tunit, CUnit)) Tunit

let const_true = mk (Const (Tbool, CBool true)) Tbool

let const_false = mk (Const (Tbool, CBool false)) Tbool

let unused env ty =
  mk (Apply(Prim_unused, noloc env, [const_unit])) ty

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

let maybe_reset_vars env transfer =
  if transfer then
    { env with
      vars = StringMap.empty;
      clos_env = None;
    }
  else env

(* Find variable name in either the global environment or the closure
   environment, returns a corresponding expression *)
let find_var ?(count_used=true) env loc name =
  try
    let vname = name in
    let (name, ty, fail) = StringMap.find name env.vars in
    let count = StringMap.find name env.vars_counts in
    if count_used then incr count;
    let aname =
      if env.annot && name.[0] <> '_' then Some vname
      else None in
    { (mk ?name:aname (Var (name, loc, [])) ty) with fail }
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
    let env_arg_var = mk (Var (env_arg_name, loc, [])) env_arg_type in
    let new_name = uniq_ident env arg_name in
    let env_vars =
      StringMap.add arg_name
        (new_name, arg_type, 0, (ref 0, ref 0)) free_vars in
    (* let size = StringMap.cardinal env_vars in *)
    let env_bindings =
      StringMap.map (fun (_, ty, index, count) ->
          let ei = mk_nat index in
          let exp = mk (Apply(Prim_tuple_get, loc, [env_arg_var; ei])) ty in
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
  | Tsignature | Tstring | Tfail -> ty
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
  | Tor (t1, t2) | Tcontract (t1, t2) | Tlambda (t1, t2) | Tmap (t1, t2) ->
    let t1', t2' = encode_type t1, encode_type t2 in
    if t1 == t1' && t2 == t2' then ty
    else begin match ty with
      | Tor (t1, t2) -> Tor (t1', t2')
      | Tcontract (t1, t2) -> Tcontract (t1', t2')
      | Tlambda (t1, t2) -> Tlambda (t1', t2')
      | Tmap (t1, t2) -> Tmap (t1', t2')
      | _ -> assert false
    end
  | Tclosure  ((t1, t2), t3) ->
    let t1' = encode_type t1 in
    let t2' = encode_type t2 in
    let t3' = encode_type t3 in
    if t1 == t1' && t2 == t2' && t3 == t3' then ty
    else Tclosure ((t1', t2'), t3')
  | Trecord (_, labels) -> encode_record_type labels
  | Tsum (_, cstys) -> encode_sum_type cstys

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

let rec encode_const env c = match c with
  | CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
  | CKey _ | CSignature _ | CNone  | CKey_hash _ -> c

  | CSome x -> CSome (encode_const env x)
  | CLeft x -> CLeft (encode_const env x)
  | CRight x -> CRight (encode_const env x)

  | CTuple xs -> CTuple (List.map (encode_const env) xs)
  | CList xs -> CList (List.map (encode_const env) xs)
  | CSet xs -> CSet (List.map (encode_const env) xs)

  | CMap l ->
    CMap (List.map (fun (x,y) -> encode_const env x, encode_const env y) l)

  | CRecord labels ->
    CTuple (List.map (fun (_, x) -> encode_const env x) labels)

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

let rec decr_counts_vars env e =
  if e.fail || e.transfer then () else
  match e.desc with
  | Var (v, _, []) ->
    begin try
      let count = StringMap.find v env.vars_counts in
      decr count
      with Not_found -> ()
    end

  | Const (_, _) | Var (_, _, _) -> ()

  | SetVar (_, _, _, e)
  | Constructor (_, _, e)
  | Lambda (_, _, _, e, _) -> decr_counts_vars env e

  | Seq (e1, e2)
  | Let (_, _, e1, e2)
  | Loop (_, _, e1, e2) ->
    decr_counts_vars env e1;
    decr_counts_vars env e2;

  | If (e1, e2, e3)
  | MatchOption (e1, _, e2, _, e3)
  | MatchNat (e1, _, _, e2, _, e3)
  | MatchList (e1, _, _, _, e2, e3)
  | Fold (_, _, _, e1, e2, e3) ->
    decr_counts_vars env e1;
    decr_counts_vars env e2;
    decr_counts_vars env e3;

  | Apply (prim, _, l) ->
    List.iter (decr_counts_vars env) l

  | Closure (_, _, _, cenv, e, _) ->
    decr_counts_vars env e;
    List.iter (fun (_, e) -> decr_counts_vars env e) cenv;

  | Record (_, labels) ->
    List.iter (fun (_, e) -> decr_counts_vars env e) labels

  | MatchVariant (e, _, cases) ->
    decr_counts_vars env e;
    List.iter (fun (_, e) -> decr_counts_vars env e) cases;

  | LetTransfer (_, _, _, e1, e2, e3, e4, e5) ->
    decr_counts_vars env e1;
    decr_counts_vars env e2;
    decr_counts_vars env e3;
    decr_counts_vars env e4;
    decr_counts_vars env e5


let rec encode env ( exp : typed_exp ) : encoded_exp =
  match exp.desc with

  | Const (ty, cst) ->
    mk ?name:exp.name (Const (ty, encode_const env cst)) ty

  | Let (name, loc, e, body) ->

     let e = encode env e in
     let e = if env.annot && e.name = None && name.[0] <> '_'
       then { e with name = Some name }
       else e
     in
     let env = maybe_reset_vars env e.transfer in
     let (new_name, env, count) = new_binding env name ~fail:e.fail e.ty in
     let body = encode env body in
     (* check_used env name loc count; *)
     if (not e.transfer) && (not e.fail) then begin
       match !count with
       | c when c <= 0 ->
         decr_counts_vars env e;
         env.to_inline := StringMap.add new_name (const_unit) !(env.to_inline)
       | 1 ->
         env.to_inline := StringMap.add new_name e !(env.to_inline)
       | _ -> ()
     end;
     mk ?name:exp.name (Let (new_name, loc, e, body)) body.ty

  | Var (name, loc, labels) ->
    let e = find_var env loc name in
    List.fold_left
      (fun e label ->
         (* let ty = match first_alias e.ty with *)
         (*   | Some (ty_name, (Trecord _ as ty)) -> ty_name, ty *)
         (*   | _ -> error loc "not a record" *)
         (* in *)
         let arg1 = mk e.desc e.ty in
         let n, label_ty =
           try
             let (ty_name', n, label_ty) =
               StringMap.find label env.env.labels in
             n, label_ty
           with Not_found ->
             error loc "bad label"
         in
         mk (Apply(Prim_tuple_get, loc, [arg1; mk_nat n])) label_ty
      ) e labels

  | SetVar (name, loc, [], e) -> encode env e

  | SetVar (name, loc, label :: labels, arg) ->
     let arg1 = find_var env loc name in
     let label_types = match arg1.ty with
       | Trecord (_, label_types) -> label_types
       | _ -> assert false
     in
     let exception Return of (int * datatype) in
     let n, lty =
       try
         List.iteri (fun n (l, lty) ->
             if l = label then raise (Return (n, lty))
           ) label_types;
         error loc "bad label"
       with Return n -> n
     in
     let arg =
       match labels with
       | [] ->
         let arg = encode env arg in
         if arg.transfer then error loc "transfer within set-field";
         arg
       | _::_ ->
         let get_exp = mk (Apply(Prim_tuple_get, loc, [arg1; mk_nat n])) lty in
         let tmp_name = uniq_ident env "_tmp#" in
         let (new_name, env, count) = new_binding env tmp_name lty in
         let body =
           encode env
             (mk_typed (SetVar (tmp_name, loc, labels, arg)) exp.ty)
         in
         mk (Let (new_name, loc, get_exp, body)) body.ty
     in
     mk ?name:exp.name (Apply(Prim_tuple_set, loc, [arg1; mk_nat n; arg])) arg1.ty

  | Seq (exp1, exp2) ->
    (* TODO: if not exp.fail then remove exp1 *)
    let exp1 = encode env exp1 in
    let exp2 = encode env exp2 in
    mk ?name:exp.name (Seq (exp1, exp2)) exp.ty

  | If (cond, ifthen, ifelse) ->
    let cond = encode env cond in
    let ifthen = encode env ifthen in
    let ifelse = encode env ifelse in
    mk ?name:exp.name (If (cond, ifthen, ifelse)) exp.ty

  | LetTransfer (storage_name, result_name,
                 loc,
                 contract_exp, tez_exp,
                 storage_exp, arg_exp, body) ->
    let tez_exp = encode env tez_exp in
    let contract_exp = encode env contract_exp in
    let arg_exp = encode env arg_exp in
    let storage_exp = encode env storage_exp in
    let return_ty = match contract_exp.ty with
      | Tcontract (_, return_ty) -> return_ty
      | _ -> assert false
    in
    let (new_storage, env, storage_count) =
      new_binding env storage_name env.contract.storage in
    let (new_result, env, result_count) =
      new_binding env result_name return_ty in
    let body = encode env body in
    (* check_used env storage_name loc storage_count; *)
    (* check_used env result_name loc result_count; *)
    mk ?name:exp.name (LetTransfer(new_storage, new_result,
                    loc,
                    contract_exp, tez_exp,
                    storage_exp, arg_exp, body)) body.ty

  | Apply (Prim_unknown, _, _) -> assert false


  (* List.rev -> List.reduce (::) *)
  | Apply (Prim_list_rev, loc, [l]) ->
    let l = encode env l in
    let elt_ty = match l.ty with
      | Tlist ty -> ty
      | _ -> assert false
    in
    let list_ty = l.ty in
    let arg_name = uniq_ident env "arg" in
    let arg_ty = Ttuple [elt_ty; list_ty] in
    let arg = mk (Var (arg_name, loc, [])) arg_ty in
    let e = mk (Apply(Prim_tuple_get, loc, [arg; mk_nat 0])) elt_ty in
    let acc =
      mk (Apply(Prim_tuple_get, loc, [arg; mk_nat 1])) list_ty in
    let f_body = mk (Apply (Prim_Cons, loc, [e; acc])) list_ty in
    let f_desc = Lambda (arg_name, arg_ty, loc, f_body, list_ty) in
    let f = mk f_desc (Tlambda (arg_ty, list_ty)) in
    let empty_acc = mk_nil list_ty in
    let desc = Apply (Prim_list_reduce, loc, [f; l; empty_acc]) in
    mk ?name:exp.name desc list_ty

  (* List.reduce (closure) -> Loop.loop *)
  | Apply (Prim_list_reduce, loc, [f; l; acc]) ->
    let f = encode env f in
    let l = encode env l in
    let acc = encode env acc in
    let args = [f; l; acc] in
    begin match f.ty with
      | Tclosure ((arg_ty, env_ty), acc_ty) ->
        let elt_ty = match l.ty with
          | Tlist ty -> ty
          | _ -> assert false
        in
        let loop_arg_name = uniq_ident env "arg" in
        let head_name = uniq_ident env "head" in
        let tail_name = uniq_ident env "tail" in
        (* let loop_arg_ty = arg_ty in *)
        let loop_body_ty = Ttuple [Tbool; arg_ty] in
        let list_ty = Tlist elt_ty in
        let arg = mk (Var (loop_arg_name, loc, [])) arg_ty in
        let head = mk (Var (head_name, loc, [])) elt_ty in
        let tail = mk (Var (tail_name, loc, [])) list_ty in
        let l' =
          mk (Apply(Prim_tuple_get, loc, [arg; mk_nat 0])) list_ty in
        let acc' =
          mk (Apply(Prim_tuple_get, loc, [arg; mk_nat 1])) acc_ty in
        let nil_case = mk_tuple loc [
            const_false ;
            mk_tuple loc [mk_nil list_ty; acc']
          ] in
        let cons_case =
          mk_tuple loc [
            const_true;
            mk_tuple loc [
              tail;
              mk (Apply (Prim_exec, loc, [
                  mk_tuple loc [head; acc'];
                  f
                ])) acc_ty
            ]
          ]
        in
        let loop_body = mk
            (MatchList (l', loc, head_name, tail_name, cons_case, nil_case))
            loop_body_ty
        in
        let loop = mk
            (Loop (loop_arg_name, loc, loop_body,
                   mk_tuple loc [l; acc]))
            (Ttuple [list_ty; acc_ty])
        in
        mk ?name:exp.name (Apply (Prim_tuple_get, loc, [loop; mk_nat 1]))
          acc_ty
      | _ ->
        mk ?name:exp.name (Apply (Prim_list_reduce, loc, args)) acc.ty
    end

  (* List.map (closure) -> {List.rev(List.reduce (closure)} *)
  | Apply (Prim_list_map, loc, [f; l]) ->
    let f' = encode env f in
    begin match f'.ty with
      | Tlambda _ ->
        let l = encode env l in
        mk ?name:exp.name (Apply (Prim_list_map, loc, [f'; l])) exp.ty
      | Tclosure ((arg_ty, _), ty_ret) ->
        let arg_name = uniq_ident env "arg" in
        let acc_ty = Tlist ty_ret in
        let rarg_ty = Ttuple [arg_ty; Tlist ty_ret] in
        let arg = mk_typed (Var (arg_name, loc, [])) rarg_ty in
        let x =
          mk_typed (Apply(Prim_tuple_get, loc, [arg; mk_typed_nat 0])) arg_ty in
        let acc =
          mk_typed (Apply(Prim_tuple_get, loc, [arg; mk_typed_nat 1])) acc_ty in
        let f_body = mk_typed (Apply(Prim_Cons, loc, [
            mk_typed (Apply(Prim_exec, loc, [x; f])) ty_ret;
            acc
          ])) acc_ty in
        let f_red =
          mk_typed (Lambda (arg_name, rarg_ty, loc, f_body, acc_ty))
            (Tlambda (rarg_ty, acc_ty)) in
        let red =
          mk_typed (Apply (Prim_list_reduce, loc,
                     [f_red; l; mk_typed_nil acc_ty])) acc_ty in
        let rev_red = mk_typed ?name:exp.name (Apply (Prim_list_rev, loc, [red])) acc_ty in
        encode env rev_red
      | _ -> assert false
    end

  (* Map.reduce (closure) -> {Map.reduce (::) |> List.rev |> List.reduce} *)
  | Apply (Prim_map_reduce, loc, [f; m; acc]) ->
    let f' = encode env f in
    begin match f'.ty with
      | Tlambda _ ->
        let m = encode env m in
        let acc = encode env acc in
        mk ?name:exp.name (Apply (Prim_map_reduce, loc, [f'; m; acc])) exp.ty
      | Tclosure ((Ttuple [kv_ty; acc_ty], _), ty_ret) ->
        let arg_name = uniq_ident env "arg" in
        let elts_ty = Tlist kv_ty in
        let arg_ty = Ttuple [kv_ty; elts_ty] in
        let arg = mk_typed (Var (arg_name, loc, [])) elts_ty in
        let kv =
          mk_typed (Apply(Prim_tuple_get, loc, [arg; mk_typed_nat 0])) kv_ty in
        let acc_elts =
          mk_typed (Apply(Prim_tuple_get, loc, [arg; mk_typed_nat 1])) elts_ty in
        let gather_body =
          mk_typed (Apply(Prim_Cons, loc, [kv; acc_elts])) elts_ty in
        let gather_fun =
          mk_typed (Lambda (arg_name, arg_ty, loc, gather_body, elts_ty))
            (Tlambda (arg_ty, elts_ty))
        in
        let rev_elts =
          mk_typed (Apply(Prim_map_reduce, loc,
                          [gather_fun; m; mk_typed_nil elts_ty]))
            elts_ty
        in
        let elts = mk_typed (Apply(Prim_list_rev, loc, [rev_elts])) elts_ty in
        let red = mk_typed ?name:exp.name
            (Apply(Prim_list_reduce, loc, [f; elts; acc])) acc_ty in
        encode env red
      | _ -> assert false
    end

  (* Map.map (closure) -> {Map.reduce (Map.update)} *)
  | Apply (Prim_map_map, loc, [f; m]) ->
    let f' = encode env f in
    begin match f'.ty with
      | Tlambda _ ->
        let m = encode env m in
        mk ?name:exp.name (Apply (Prim_map_map, loc, [f'; m])) exp.ty
      | Tclosure ((Ttuple [k_ty; v_ty] as kv_ty, _), ty_ret) ->
        let arg_name = uniq_ident env "arg" in
        let acc_ty = Tmap (k_ty, ty_ret) in
        let arg_ty = Ttuple [kv_ty; acc_ty] in
        let arg = mk_typed (Var (arg_name, loc, [])) arg_ty in
        let kv =
          mk_typed (Apply(Prim_tuple_get, loc, [arg; mk_typed_nat 0])) kv_ty in
        let acc =
          mk_typed (Apply(Prim_tuple_get, loc, [arg; mk_typed_nat 1])) acc_ty in
        let k =
          mk_typed (Apply(Prim_tuple_get, loc, [kv; mk_typed_nat 0])) k_ty in
        let acc_ty = Tmap (k_ty, ty_ret) in
        let update_body =
          mk_typed (Apply(Prim_map_update, loc, [
              k;
              mk_typed (Apply(Prim_Some, loc, [
                  mk_typed (Apply(Prim_exec, loc, [kv; f])) ty_ret
                ])) (Toption ty_ret);
              acc
            ])) acc_ty in
        let update_fun =
          mk_typed (Lambda (arg_name, arg_ty, loc, update_body, acc_ty))
            (Tlambda (arg_ty, acc_ty))
        in
        let red =
          mk_typed ?name:exp.name (Apply (Prim_map_reduce, loc, [
              update_fun; m;
              mk_typed (Const (acc_ty, CMap [])) acc_ty
            ])) acc_ty in
        encode env red
      | _ -> assert false
    end

  (* Set.reduce (closure) -> {Set.reduce (::) |> List.rev |> List.reduce} *)
  | Apply (Prim_set_reduce, loc, [f; s; acc]) ->
    let f' = encode env f in
    begin match f'.ty with
      | Tlambda _ ->
        let s = encode env s in
        let acc = encode env acc in
        mk ?name:exp.name (Apply (Prim_set_reduce, loc, [f'; s; acc])) exp.ty
      | Tclosure ((Ttuple [elt_ty; acc_ty], _), ty_ret) ->
        let arg_name = uniq_ident env "arg" in
        let elts_ty = Tlist elt_ty in
        let arg_ty = Ttuple [elt_ty; elts_ty] in
        let arg = mk_typed (Var (arg_name, loc, [])) arg_ty in
        let elt =
          mk_typed (Apply(Prim_tuple_get, loc, [arg; mk_typed_nat 0])) elt_ty in
        let acc_elts =
          mk_typed
            (Apply(Prim_tuple_get, loc, [arg; mk_typed_nat 1])) elts_ty in
        let gather_body =
          mk_typed (Apply(Prim_Cons, loc, [elt; acc_elts])) elts_ty in
        let gather_fun =
          mk_typed (Lambda (arg_name, arg_ty, loc, gather_body, elts_ty))
            (Tlambda (arg_ty, elts_ty))
        in
        let rev_elts =
          mk_typed (Apply(Prim_set_reduce, loc,
                          [gather_fun; s; mk_typed_nil elts_ty]))
            elts_ty
        in
        let elts = mk_typed (Apply(Prim_list_rev, loc, [rev_elts])) elts_ty in
        let red = mk_typed ?name:exp.name
            (Apply(Prim_list_reduce, loc, [f; elts; acc])) acc_ty in
        encode env red
      | _ -> assert false
    end

  | Apply (prim, loc, args) ->
    encode_apply exp.name env prim loc args exp.ty

  | MatchOption (arg, loc, ifnone, name, ifsome) ->
     let arg = encode env arg in
     let name_ty = match arg.ty with
       | Toption ty -> ty
       | _ -> assert false
     in
     let env = maybe_reset_vars env arg.transfer in
     let ifnone = encode env ifnone in
     let (new_name, env, count) = new_binding env name name_ty in
     let ifsome = encode env ifsome in
     mk ?name:exp.name (MatchOption (arg, loc, ifnone, new_name, ifsome)) exp.ty

  | MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus) ->
     let arg = encode env arg in
     let env = maybe_reset_vars env arg.transfer in
     let (plus_name, env2, count_p) = new_binding env plus_name Tnat in
     let ifplus = encode env2 ifplus in
     let (minus_name, env3, count_m) = new_binding env minus_name Tnat in
     let ifminus = encode env3 ifminus in
     (* check_used env plus_name loc count_p; *)
     (* check_used env minus_name loc count_m; *)
     mk ?name:exp.name (MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus)) exp.ty

  | Loop (name, loc, body, arg) ->
     let arg = encode env arg in
     let env = maybe_reset_vars env arg.transfer in
     let (new_name, env, count) = new_binding env name arg.ty in
     let body = encode env body in
     (* check_used env name loc count; *)
     mk ?name:exp.name (Loop (new_name, loc, body, arg)) exp.ty

  | Fold (prim, name, loc, body, arg, acc) ->
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
    let env = maybe_reset_vars env arg.transfer in
    let (new_name, env, count) = new_binding env name name_ty in
    let body = encode env body in
    mk ?name:exp.name (Fold (prim, new_name, loc, body, arg, acc)) exp.ty

  | MatchList (arg, loc, head_name, tail_name, ifcons, ifnil) ->
     let arg = encode env arg in
     let elt_ty = match arg.ty with
       | Tlist ty -> ty
       | _ -> assert false
     in
     let env = maybe_reset_vars env arg.transfer in
     let ifnil = encode env ifnil in
     let (new_head_name, env, count) = new_binding env head_name elt_ty in
     let (new_tail_name, env, count) = new_binding env tail_name arg.ty in
     let ifcons = encode env ifcons in
     (* check_used env head_name loc count; *)
     (* check_used env tail_name loc count; *)
     mk ?name:exp.name
       (MatchList (arg, loc, new_head_name, new_tail_name, ifcons, ifnil))
       exp.ty

  | Lambda (arg_name, arg_type, loc, body, _) ->
     let env_at_lambda = env in
     let lambda_arg_type = arg_type in
     let lambda_arg_name = arg_name in
     let lambda_body = body in
     let bvs = LiquidBoundVariables.bv exp in
     if StringSet.is_empty bvs then
       (* not a closure, create a real lambda *)
       let env = { env_at_lambda with vars = StringMap.empty } in
       let (new_arg_name, env, arg_count) =
         new_binding env lambda_arg_name lambda_arg_type in
       let body = encode env lambda_body in
       (* check_used env lambda_arg_name loc arg_count; *)
       let ty = Tlambda (lambda_arg_type, body.ty) in
       mk ?name:exp.name (Lambda (new_arg_name, lambda_arg_type, loc, body, body.ty)) ty
     else
       (* create closure with environment *)
       let env, arg_name, arg_type, call_env =
         env_for_clos env loc bvs arg_name arg_type in
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
         Closure (arg_name, arg_type, loc, call_env, body, body.ty) in
       let call_env_type = match call_env with
         | [] -> assert false
         | [_, t] -> t.ty
         | _ -> Ttuple (List.map (fun (_, t) -> t.ty) call_env)
       in
       let ty = Tclosure ((lambda_arg_type, call_env_type), body.ty) in
       mk ?name:exp.name desc ty

  | Closure _ -> assert false

  | Record (_loc, []) -> assert false
  | Record (loc, (( (label, _) :: _ ) as lab_x_exp_list)) ->
     let ty_name, _, _ =
       try
         StringMap.find label env.env.labels
       with Not_found ->
         error loc "unbound label %S" label
     in
     let record_ty = StringMap.find ty_name env.env.types in
     let len = List.length (match record_ty with
         | Trecord (_, rtys) -> rtys
         | _ -> assert false) in
     let t = Array.make len const_unit in
     List.iteri (fun i (label, exp) ->
         let ty_name', label_pos, ty = StringMap.find label env.env.labels in
         let exp = encode env exp in
         t.(label_pos) <- exp;
       ) lab_x_exp_list;
     let args = Array.to_list t in
     let desc = Apply(Prim_tuple, loc, args) in
     mk ?name:exp.name desc record_ty

  | Constructor(loc, Constr constr, arg) ->
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
           (* | (c, ty, left_ty, right_ty) :: constrs -> *)
           | (c, cty) :: constrs, orty ->
             let left_ty, right_ty = match orty with
               | Tor (left_ty, right_ty) -> left_ty, right_ty
               | _ -> assert false
             in
             let desc =
               if c = constr then
                 (* We use an unused argument to carry the type to
                    the code generator *)
                 Apply(Prim_Left, loc, [arg; unused env right_ty])
               else
                 let arg = iter constrs right_ty in
                 Apply(Prim_Right, loc, [arg; unused env left_ty])
             in
             mk desc orty
         in
         iter constrs (encode_type constr_ty)
       | _ -> assert false
     in
     mk ?name:exp.name exp.desc constr_ty

  | Constructor(loc, Left right_ty, arg) ->
     let arg = encode env arg in
     let ty = Tor(arg.ty, right_ty) in
     let desc = Apply(Prim_Left,loc,[arg; unused env right_ty]) in
     mk ?name:exp.name desc ty

  | Constructor(loc, Right left_ty, arg) ->
     let arg = encode env arg in
     let ty = Tor(left_ty, arg.ty) in
     let desc = Apply(Prim_Right,loc,[arg; unused env left_ty]) in
     mk ?name:exp.name desc ty

  | Constructor(loc, Source (from_ty, to_ty), _arg) ->
     let ty = Tcontract(from_ty, to_ty) in
     let desc = Apply(Prim_Source,loc,[unused env from_ty; unused env to_ty]) in
     mk ?name:exp.name desc ty

  | MatchVariant (arg, loc, cases) ->
    let arg = encode env arg in
    let constrs = match arg.ty with
      | Tsum (_, constrs) -> constrs
      | Tor (left_ty, right_ty) ->
        [ "Left", left_ty; "Right", right_ty]
      | _ -> assert false
    in
    let cases = List.map (fun case ->
       let c, var, e = match case with
        | CConstr (c, []), e -> c, "_", e
        | CConstr (c, [var]), e -> c, var, e
        | CAny, _ | CConstr _, _ -> assert false
       in
       let var_ty = List.assoc c constrs in
       let (var, env, _) = new_binding env var var_ty in
       let e = encode env e in
       (CConstr (c, [var]), e)
      ) cases
    in
    mk ?name:exp.name (MatchVariant (arg, loc, cases)) exp.ty


and encode_apply name env prim loc args ty =
  let args = List.map (encode env) args in
  match prim, args with
  | Prim_exec, [ x; { ty = Tclosure (_, ty) | Tlambda (_,ty) } ] ->
    mk ?name (Apply (prim, loc, args)) ty

  | _ -> mk ?name (Apply (prim, loc, args)) ty


let encode_contract ?(annot=false) env contract =
  let env =
    {
      warnings=false;
      annot;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      env = env;
      clos_env = None;
      contract;
    } in

  (* "storage/1" *)
  let (_ , env, _) = new_binding env  "storage" contract.storage in
  (* "parameter/2" *)
  let (_, env, _) = new_binding env "parameter" contract.parameter in

  let code = encode env contract.code in
  { contract with code }, ! (env.to_inline)


let encode_code env contract code =
  let env =
    {
      warnings=false;
      annot=false;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      env = env;
      clos_env = None;
      contract ;
    } in

  encode env code
