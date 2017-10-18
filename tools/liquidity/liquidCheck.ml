(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO: we should check that the keys of maps and sets are comparable,
  in particular to avoid using `key` in maps instead of `key_hash`. *)

(* TODO: we don't handle correctly all the occurrences of Tfail, i.e.
   when an error occurs in a sub-part of a type and another type is expected,
   we should probably unify.
 *)

(*
  Typecheck an AST.
  The following actions are also performed:
  * variables are replaced by uniq identifiers: STRING/INTEGER
  * Var(var,_loc,labels) -> Var(var,_loc,[]) with accesses thought "get"
  * SetVar(var,_loc,labels) -> Var(var,_loc,[]) with accesses thought "get"
                               and modification thought "set"
  * Lambda(_,_,_,Tunit) -> Lambda(_,_,_, body.ty)
  * Record(_,_) -> Apply("tuple", _)
  * Constructor(_) -> Apply("Left"|"Right", [arg; unused ty])

 *)

open LiquidTypes

let noloc env = LiquidLoc.loc_in_file env.env.filename

let error loc msg =
  LiquidLoc.raise_error ~loc ("Type error:  " ^^ msg ^^ "%!")

let comparable_ty ty1 ty2 =
  match ty1, ty2 with
  | (Tint|Tnat|Ttez), (Tint|Tnat|Ttez)
    | Ttimestamp, Ttimestamp
    | Tstring, Tstring
    | Tkey_hash, Tkey_hash -> true
  | _ -> false

let error_not_comparable loc prim ty1 ty2 =
  error loc "arguments of %s not comparable: %s\nwith\n%s\n"
    (LiquidTypes.string_of_primitive prim)
    (LiquidPrinter.Michelson.string_of_type ty1)
    (LiquidPrinter.Michelson.string_of_type ty2)

let mk =
  let bv = StringSet.empty in
  fun desc (ty : datatype) fail -> { desc; ty; bv; fail }

let mk_nat i =
  mk (Const (Tnat, CNat (LiquidPrinter.integer_of_int i))) Tnat false

let mk_nil list_ty =
  mk (Const (list_ty, CList [])) list_ty false

let mk_tuple loc l fail =
  let tuple_ty = Ttuple (List.map (fun t -> t.ty) l) in
  mk (Apply (Prim_tuple, loc, l)) tuple_ty fail

let const_unit = mk (Const (Tunit, CUnit)) Tunit false

let const_true = mk (Const (Tbool, CBool true)) Tbool false

let const_false = mk (Const (Tbool, CBool false)) Tbool false

let mk_untyped =
  let bv = StringSet.empty in
  fun desc -> { desc; ty = (); bv; fail = false }

let untyped_int i =
  mk_untyped (Const (Tint, CInt (LiquidPrinter.integer_of_int i)))

let untyped_nil list_ty =
  mk_untyped (Const (list_ty, CList []))

let mk_untyped_tuple loc l =
  mk_untyped (Apply (Prim_tuple, loc, l))

let unused env ty =
  mk (Apply(Prim_unused, noloc env, [const_unit])) ty false

let uniq_ident env name =
  env.counter := !(env.counter) + 1;
  Printf.sprintf "%s/%d" name !(env.counter)

let new_binding env name ty =
  let new_name = uniq_ident env name in
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (new_name, ty, count) env.vars } in
  (new_name, env, count)

let check_used env name loc count =
  if env.warnings && !count = 0 && name.[0] <> '_' then begin
      LiquidLoc.warn loc (Unused name)
  end

let check_used_in_env env name loc =
  try
    let (_, _, count) = StringMap.find name env.vars in
    check_used env name loc count;
  with Not_found ->
  match env.clos_env with
  | None -> check_used env name loc (ref 0)
  | Some ce ->
    try
      let _, (count, _) = StringMap.find name ce.env_bindings in
      check_used env name loc count;
    with Not_found ->
      check_used env name loc (ref 0)

(* Find variable name in either the global environment or the closure
   environment, returns a corresponding variable expression *)
let find_var ?(count_used=true) env loc name =
  try
    let (name, ty, count) = StringMap.find name env.vars in
    if count_used then incr count;
    mk (Var (name, loc, [])) ty false
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
      try
        let index = index + 1 in
        match env.clos_env with
        | None ->
          let (bname, btype, cpt_out) = StringMap.find v env.vars in
          (index,
           StringMap.add v (bname, btype, index, (ref 0, cpt_out)) free_vars)
        | Some ce ->
          let bname, btype, _, (cpt_in, cpt_out) =
            StringMap.find v ce.env_vars in
          (index,
           StringMap.add v (bname, btype, index, (cpt_in, cpt_out)) free_vars)
      with Not_found ->
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
    let env_arg_var = mk (Var (env_arg_name, loc, [])) env_arg_type false in
    let new_name = uniq_ident env arg_name in
    let env_vars =
      StringMap.add arg_name
        (new_name, arg_type, 0, (ref 0, ref 0)) free_vars in
    let size = StringMap.cardinal env_vars in
    let env_bindings =
      StringMap.map (fun (_, ty, index, count) ->
          let ei = mk_nat index in
          let accessor =
            if index + 1 = size then Prim_tuple_get_last
            else Prim_tuple_get in
          let exp = mk (Apply(accessor, loc, [env_arg_var; ei])) ty false in
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

let maybe_reset_vars env transfer =
  if transfer then
    { env with
      vars = StringMap.empty;
      clos_env = None;
    }
  else env

let fprint_2types fmt (ty1, ty2) =
  Format.fprintf fmt "First type:\n  %s\n"
    (LiquidPrinter.Liquid.string_of_type ty1);
  Format.fprintf fmt "Second type:\n  %s"
    (LiquidPrinter.Liquid.string_of_type ty2)

let type_error loc msg actual expected =
  error loc "%s.\nExpected type:\n  %s\nActual type:\n  %s"
    msg
    (LiquidPrinter.Liquid.string_of_type expected)
    (LiquidPrinter.Liquid.string_of_type actual)


let error_prim loc prim args expected_args =
  let prim = LiquidTypes.string_of_primitive prim in
  let nargs = List.length args in
  let nexpected = List.length expected_args in
  if nargs <> nexpected then
    error loc "Prim %S: %d args provided, %d args expected"
          prim nargs nexpected
  else
    let args = List.map (fun { ty } -> ty) args in
    List.iteri (fun i (arg, expected) ->
        if arg <> expected then
          error loc
                "Primitive %s, argument %d:\nExpected type:%sProvided type:%s"
                prim (i+1)
                (LiquidPrinter.Liquid.string_of_type expected)
                (LiquidPrinter.Liquid.string_of_type arg)

      ) (List.combine args expected_args);
    Printf.eprintf "Fatal error on typechecking primitive %S\n%!" prim;
    assert false


  (* approximate location *)
let rec loc_exp env e = match e.desc with
  | Var (_, loc, _)
    | SetVar (_, loc, _, _)
    | Apply (_, loc, _)
    | LetTransfer (_, _, loc, _, _, _, _, _)
    | MatchOption (_, loc, _, _, _)
    | MatchAbs (_, loc, _, _, _, _)
    | MatchList (_, loc, _, _, _, _)
    | Loop (_, loc, _, _)
    | Lambda (_, _, loc, _, _)
    | Closure (_, _, loc, _, _, _)
    | Record (loc, _)
    | Constructor (loc, _, _)
    | MatchVariant (_, loc, _) -> loc

  | Let (_, _, _, e) -> loc_exp env e

  | Const _ -> noloc env

  | If (e1, _, e2)
    | Seq (e1, e2) ->
     match loc_exp env e1, loc_exp env e2 with
     | ({ loc_pos = Some ( loc_begin , _ ) } as loc),
       { loc_pos = Some ( _, loc_end ) } ->
        { loc with loc_pos = Some (loc_begin, loc_end) }
     | loc, _ -> loc

  (* this function returns a triple with
   * the expression annotated with its type
   * whether the expression can fail
   * whether the expression performs a TRANSFER_TOKENS
   *)
let rec typecheck env ( exp : LiquidTypes.syntax_exp ) =
  match exp.desc with

  | Const (ty, cst ) ->
     let desc = Const (ty, cst ) in
     let fail = false in
     mk desc ty fail, fail, false

  | Let (name, loc, exp, body) ->
     let exp, fail1, transfer1 = typecheck env exp in
     if exp.ty = Tfail then
       error loc "cannot assign failure";
     let env = maybe_reset_vars env transfer1 in
     let (new_name, env, count) = new_binding env name exp.ty in
     let body, fail2, transfer2 = typecheck env body in
     let desc = Let (new_name, loc, exp, body ) in
     check_used env name loc count;
     if (not transfer1) && (not fail1) then begin
         match !count with
         | 0 ->
            env.to_inline := StringMap.add new_name const_unit
                                           ! (env.to_inline)
         | 1 ->
            env.to_inline := StringMap.add new_name exp
                                           ! (env.to_inline)
         | _ -> ()
       end;
     let fail = fail1 || fail2 in
     mk desc body.ty fail, fail, transfer1 || transfer2

  | Var (name, loc, labels) ->
     let e = find_var env loc name in
     let e =
       List.fold_left
         (fun e label ->
           let ty_name, ty = match e.ty with
             | Ttype (ty_name, ty) -> ty_name, ty
             | _ ->
                error loc "not a record"
           in
           let arg1 = mk e.desc ty false in
           let n =
             try
               let (ty_name', n, _label_ty) =
                 StringMap.find label env.env.labels in
               if ty_name' <> ty_name then
                 error loc "label for wrong record";
               n
             with Not_found ->
               error loc "bad label"
           in
           let arg2 = mk_nat n in
           let args = [ arg1; arg2] in
           let prim, ty = typecheck_prim1 env Prim_tuple_get loc args in
           mk (Apply(prim, loc, args)) ty false
         ) e labels in
     e, false, false

  | SetVar (name, loc, [], e) -> typecheck env e

  | SetVar (name, loc, label :: labels, arg) ->
     let arg1_t = find_var env loc name in
     let ty = arg1_t.ty in
     let ty_name, tuple_ty = match ty with
       | Ttype (ty_name, ty) -> ty_name, ty
       | _ ->
          error loc "not a record"
     in
     let arg1 = { arg1_t with ty = tuple_ty } in
     let n =
       try
         let (ty_name', n, _label_ty) =
           StringMap.find label env.env.labels in
         if ty_name' <> ty_name then
           error loc "label for wrong record";
         n
       with Not_found ->
         error loc "bad label"
     in
     let arg2 = mk_nat n in
     let arg, can_fail =
       match labels with
       | [] ->
          let (arg, can_fail, transfer) = typecheck env arg in
          if transfer then
            error loc "transfer within set-field";
          (arg, can_fail)
       | _::_ ->
          let args = [ arg1; arg2] in
          let prim, ty = typecheck_prim1 env Prim_tuple_get loc args in
          let get_exp = mk (Apply(prim, loc, args)) ty false in
          let tmp_name = uniq_ident env "tmp#" in
          let (new_name, env, count) = new_binding env tmp_name ty in
          let body, can_fail, _transfer =
            typecheck env
                      { exp with desc = SetVar (tmp_name, loc,
                                                labels, arg) }
          in
          let desc = Let (new_name, loc, get_exp, body ) in
          mk desc body.ty can_fail, can_fail

     in
     let args = [ arg1; arg2; arg] in
     let prim, tuple_ty' = typecheck_prim1 env Prim_tuple_set loc args in
     mk (Apply(prim, loc, args)) ty can_fail, can_fail, false

  | Seq (exp1, exp2) ->
     let exp1, fail1, transfer1 =
       typecheck_expected "sequence" env Tunit exp1 in
     let exp2, fail2, transfer2 = typecheck env exp2 in
     let desc = Seq (exp1, exp2) in
     (* TODO: if not fail1 then remove exp1 *)
     let can_fail = fail1 || fail2 in
     mk desc exp2.ty can_fail, can_fail, transfer1 || transfer2

  | If (cond, ifthen, ifelse) ->
     let cond, fail1, transfer1 =
       typecheck_expected "if-cond" env Tbool cond in
     let ifthen, fail2, transfer2 = typecheck env ifthen in
     let ifelse, fail3, transfer3, ty =
       if ifthen.ty = Tfail then
         let ifelse, fail3, transfer3 = typecheck env ifelse in
         ifelse, fail3, transfer3, ifelse.ty
       else
         let ifelse, fail3, transfer3 =
           typecheck_expected "if-result" env ifthen.ty ifelse in
         ifelse, fail3, transfer3, ifthen.ty
     in
     let desc = If(cond, ifthen, ifelse) in
     let can_fail = fail1 || fail2 || fail3 in
     mk desc ty can_fail,
     can_fail,
     transfer1 || transfer2 || transfer3

  | LetTransfer (storage_name, result_name,
                 loc,
                 contract_exp, tez_exp,
                 storage_exp, arg_exp, body) ->
     let tez_exp, fail1, transfer1 =
       typecheck_expected "call-amount" env Ttez tez_exp in
     let contract_exp, fail2, transfer2 = typecheck env contract_exp in
     begin
       match contract_exp.ty with
       | Tcontract (arg_ty, return_ty) ->
          let arg_exp, fail3, transfer3 =
            typecheck_expected "call-arg" env arg_ty arg_exp in
          let storage_exp, fail4, transfer4 =
            typecheck_expected "call-storage"
                               env env.contract.storage storage_exp in
          if transfer1 || transfer2 || transfer3 || transfer4 then
            error loc "transfer within transfer arguments";
          let (new_storage, env, storage_count) =
            new_binding env storage_name env.contract.storage in
          let (new_result, env, result_count) =
            new_binding env result_name return_ty in
          let body, fail5, transfer5 = typecheck env body in
          check_used env storage_name loc storage_count;
          check_used env result_name loc result_count;
          let desc = LetTransfer(new_storage, new_result,
                                 loc,
                                 contract_exp, tez_exp,
                                 storage_exp, arg_exp, body)
          in
          mk desc body.ty true,
          true, true
       | _ ->
          LiquidLoc.raise_error "typecheck error: Contract expected%!"
     end


  | Apply (Prim_unknown, loc,
           ({ desc = Var (name, varloc, [])} as f) :: ((_ :: _) as r))
       when StringMap.mem name env.vars ->
     let exp = List.fold_left (fun f x ->
                   { exp with desc = Apply (Prim_exec, loc, [x; f]) }
                 ) f r
     in
     typecheck env exp

  | Apply (Prim_unknown, loc,
           [ { desc = Var (name, _, _)} as f; x ])
       when StringMap.mem name env.vars ->
     typecheck env { exp with
                     desc = Apply(Prim_exec, loc, [x;f]) }

  | Apply (Prim_unknown, loc,
           ({ desc = Var (name, varloc, [])} ::args)) ->
     let prim =
       try
         LiquidTypes.primitive_of_string name
       with Not_found ->
         error loc "Unknown identifier %S" name
     in
     typecheck env { exp with
                     desc = Apply(prim, loc, args) }

  | Apply (Prim_unknown, loc, [f ; x ]) ->
     typecheck env { exp with
                     desc = Apply(Prim_exec, loc, [x; f]) }


  (* List.rev -> List.reduce (::) *)
  | Apply (Prim_list_rev, loc, [l]) ->
     let l, fail, transfer = typecheck env l in
     if transfer then error loc "transfer within List.rev args";
     let elt_ty = match l.ty with
       | Tlist ty -> ty
       | _ -> error loc "Argument of List.rev must be a list"
     in
     let arg_name = uniq_ident env "arg" in
     let list_ty = Tlist elt_ty in
     let arg_ty = Ttuple [elt_ty; list_ty] in
     let arg = mk (Var (arg_name, loc, [])) arg_ty false in
     let e = mk (Apply(Prim_tuple_get, loc, [arg; mk_nat 0])) elt_ty false in
     let acc =
       mk (Apply(Prim_tuple_get_last, loc, [arg; mk_nat 1])) list_ty false in
     let f_body = mk (Apply (Prim_Cons, loc, [e; acc])) list_ty false in
     let f_desc = Lambda (arg_name, arg_ty, loc, f_body, list_ty) in
     let f = mk f_desc (Tlambda (arg_ty, list_ty)) false in
     let empty_acc = mk_nil list_ty in
     let desc = Apply (Prim_list_reduce, loc, [f; l; empty_acc]) in
     mk desc list_ty fail, fail, false

  (* List.reduce (closure) -> Loop.loop *)
  | Apply (Prim_list_reduce, loc, [f; l; acc]) ->
     let f, _, _ = typecheck env f in
     let l, can_fail1, transfer1 = typecheck env l in
     let acc, can_fail2, transfer2 = typecheck env acc in
     if transfer1 || transfer2 then
       error loc "transfer within List.reduce args";
     let args = [f; l; acc] in
     let _, ty = typecheck_prim1 env Prim_list_reduce loc args in
     let can_fail = can_fail1 || can_fail2 in
     begin match f.ty with
     | Tclosure ((arg_ty, env_ty), acc_ty) ->
        let elt_ty = match l.ty with
          | Tlist ty -> ty
          | _ -> error loc "Argument of List.reduce must be a list"
        in
        let loop_arg_name = uniq_ident env "arg" in
        let head_name = uniq_ident env "head" in
        let tail_name = uniq_ident env "tail" in
        (* let loop_arg_ty = arg_ty in *)
        let loop_body_ty = Ttuple [Tbool; arg_ty] in
        let list_ty = Tlist elt_ty in
        let arg = mk (Var (loop_arg_name, loc, [])) arg_ty false in
        let head = mk (Var (head_name, loc, [])) elt_ty false in
        let tail = mk (Var (tail_name, loc, [])) list_ty false in
        let l' =
          mk (Apply(Prim_tuple_get, loc, [arg; mk_nat 0])) list_ty can_fail in
        let acc' =
          mk (Apply(Prim_tuple_get_last, loc, [arg; mk_nat 1]))
             acc_ty can_fail in
        let nil_case = mk_tuple loc [
                                  const_false;
                                  mk_tuple loc [mk_nil list_ty; acc'] can_fail
                                ] can_fail in
        let cons_case =
          mk_tuple loc [
                     const_true;
                     mk_tuple loc [
                                tail;
                                mk (Apply (Prim_exec, loc, [
                                               mk_tuple loc [head; acc'] can_fail;
                                               f
                                   ])) acc_ty can_fail
                              ] can_fail
                   ] can_fail
        in
        let loop_body = mk
                          (MatchList (l', loc, head_name, tail_name, cons_case, nil_case))
                          loop_body_ty can_fail
        in
        let loop = mk
                     (Loop (loop_arg_name, loc, loop_body,
                            mk_tuple loc [l; acc] can_fail1))
                     (Ttuple [list_ty; acc_ty]) can_fail
        in
        mk (Apply (Prim_tuple_get_last, loc, [loop; mk_nat 1]))
           acc_ty can_fail, can_fail, false
     | _ ->
        mk (Apply (Prim_list_reduce, loc, args)) ty can_fail, can_fail, false
     end

  (* List.map (closure) -> {List.rev(List.reduce (closure)} *)
  | Apply (Prim_list_map, loc, [f; l]) ->
     begin match is_closure env f with
     | None -> typecheck_apply env Prim_list_map loc [f; l]
     | Some ((arg_ty, _), ty_ret) ->
        let arg_name = uniq_ident env "arg" in
        let arg = mk_untyped (Var (arg_name, loc, [])) in
        let x =
          mk_untyped (Apply(Prim_tuple_get, loc, [arg; untyped_int 0])) in
        let acc =
          mk_untyped (Apply(Prim_tuple_get, loc, [arg; untyped_int 1])) in
        let f_body = mk_untyped (Apply(Prim_Cons, loc, [
                                           mk_untyped (Apply(Prim_exec, loc, [x; f]));
                                           acc
                                ])) in
        let f_red =
          mk_untyped (Lambda (arg_name, Ttuple [arg_ty; Tlist arg_ty],
                              loc, f_body, Tunit)) in
        let red =
          mk_untyped (Apply (Prim_list_reduce, loc,
                             [f_red; l; untyped_nil (Tlist arg_ty)])) in
        let rev_red = mk_untyped (Apply (Prim_list_rev, loc, [red])) in
        typecheck env rev_red
     end

  (* Map.reduce (closure) -> {Map.reduce (::) |> List.rev |> List.reduce} *)
  | Apply (Prim_map_reduce, loc, [f; m; acc]) ->
     begin match is_closure env f with
     | None -> typecheck_apply env Prim_map_reduce loc [f; m; acc]
     | Some ((Ttuple [kv_ty; acc_ty], _), ty_ret) ->
        let arg_name = uniq_ident env "arg" in
        let arg = mk_untyped (Var (arg_name, loc, [])) in
        let kv =
          mk_untyped (Apply(Prim_tuple_get, loc, [arg; untyped_int 0])) in
        let acc_elts =
          mk_untyped (Apply(Prim_tuple_get, loc, [arg; untyped_int 1])) in
        let gather_body =
          mk_untyped (Apply(Prim_Cons, loc, [kv; acc_elts])) in
        let gather_fun =
          mk_untyped (Lambda (arg_name, Ttuple [kv_ty; Tlist kv_ty],
                              loc, gather_body, Tunit)) in
        let rev_elts =
          mk_untyped (Apply(Prim_map_reduce, loc,
                            [gather_fun; m; untyped_nil (Tlist kv_ty)])) in
        let elts = mk_untyped (Apply(Prim_list_rev, loc, [rev_elts])) in
        let red = mk_untyped (Apply(Prim_list_reduce, loc, [f; elts; acc])) in
        typecheck env red
     | Some _ -> error loc "bad closure type in Map.reduce"
     end

  (* Map.map (closure) -> {Map.reduce (Map.update)} *)
  | Apply (Prim_map_map, loc, [f; m]) ->
     begin match is_closure env f with
     | None -> typecheck_apply env Prim_map_map loc [f; m]
     | Some ((Ttuple [k_ty; v_ty], _), ty_ret) ->
        let arg_name = uniq_ident env "arg" in
        let arg = mk_untyped (Var (arg_name, loc, [])) in
        let kv =
          mk_untyped (Apply(Prim_tuple_get, loc, [arg; untyped_int 0])) in
        let acc =
          mk_untyped (Apply(Prim_tuple_get, loc, [arg; untyped_int 1])) in
        let k =
          mk_untyped (Apply(Prim_tuple_get, loc, [kv; untyped_int 0])) in
        let acc_ty = Tmap (k_ty, ty_ret) in
        let update_body =
          mk_untyped (Apply(Prim_map_update, loc, [
                                k;
                                mk_untyped (Apply(Prim_Some, loc, [
                                                      mk_untyped (Apply(Prim_exec, loc, [kv; f]))
                                           ]));
                                acc
                     ])) in
        let update_fun =
          mk_untyped (Lambda (arg_name, Ttuple [Ttuple [k_ty; v_ty]; acc_ty],
                              loc, update_body, Tunit)) in
        let red =
          mk_untyped (Apply (Prim_map_reduce, loc, [
                                 update_fun; m;
                                 mk_untyped (Const (acc_ty, CMap []))
                     ])) in
        typecheck env red
     | Some _ -> error loc "bad closure type in Map.map"
     end

  (* Set.reduce (closure) -> {Set.reduce (::) |> List.rev |> List.reduce} *)
  | Apply (Prim_set_reduce, loc, [f; s; acc]) ->
     begin match is_closure env f with
     | None -> typecheck_apply env Prim_set_reduce loc [f; s; acc]
     | Some ((Ttuple [elt_ty; acc_ty], _), ty_ret) ->
        let arg_name = uniq_ident env "arg" in
        let arg = mk_untyped (Var (arg_name, loc, [])) in
        let elt =
          mk_untyped (Apply(Prim_tuple_get, loc, [arg; untyped_int 0])) in
        let acc_elts =
          mk_untyped (Apply(Prim_tuple_get, loc, [arg; untyped_int 1])) in
        let gather_body =
          mk_untyped (Apply(Prim_Cons, loc, [elt; acc_elts])) in
        let gather_fun =
          mk_untyped (Lambda (arg_name, Ttuple [elt_ty; Tlist elt_ty],
                              loc, gather_body, Tunit)) in
        let rev_elts =
          mk_untyped (Apply(Prim_set_reduce, loc,
                            [gather_fun; s; untyped_nil (Tlist elt_ty)])) in
        let elts = mk_untyped (Apply(Prim_list_rev, loc, [rev_elts])) in
        let red = mk_untyped (Apply(Prim_list_reduce, loc, [f; elts; acc])) in
        typecheck env red
     | Some _ -> error loc "bad closure type in Set.reduce"
     end

  | Apply (prim, loc, args) -> typecheck_apply env prim loc args

  | MatchOption (arg, loc, ifnone, name, ifsome) ->
     let arg, fail1, transfer1 = typecheck env arg in
     let arg_ty = match arg.ty with
       | Tfail ->
          error loc "cannot match failure"
       | Toption ty -> ty
       | _ ->
          error loc "not an option type"
     in
     let env = maybe_reset_vars env transfer1 in
     let ifnone, fail2, transfer2 = typecheck env ifnone in
     let (new_name, env, count) = new_binding env name arg_ty in
     let ifsome, fail3, transfer3 = typecheck env ifsome in
     check_used env name loc count;
     let desc = MatchOption (arg, loc, ifnone, new_name, ifsome ) in
     let ty =
       match ifnone.ty, ifsome.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
          if ty1 <> ty2 then type_error loc "Bad option type in match" ty2 ty1;
          ty1
     in
     let can_fail = fail1 || fail2 || fail3 in
     mk desc ty can_fail,
     can_fail,
     transfer1 || transfer2 || transfer3

  | MatchAbs (arg, loc, plus_name, ifplus, minus_name, ifminus) ->
     let arg, fail1, transfer1 =
       typecheck_expected "match%abs" env Tint arg in
     let env = maybe_reset_vars env transfer1 in
     let (plus_name, env2, count_p) = new_binding env plus_name Tnat in
     let ifplus, fail2, transfer2 = typecheck env2 ifplus in
     let (minus_name, env3, count_m) = new_binding env minus_name Tnat in
     let ifminus, fail3, transfer3 = typecheck env3 ifminus in
     check_used env plus_name loc count_p;
     check_used env minus_name loc count_m;
     let desc = MatchAbs (arg, loc, plus_name, ifplus, minus_name, ifminus) in
     let ty =
       match ifplus.ty, ifminus.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
         if ty1 <> ty2 then
           type_error loc "branches of match%abs must have the same type"
             ty2 ty1;
         ty1
     in
     let can_fail = fail1 || fail2 || fail3 in
     mk desc ty can_fail,
     can_fail,
     transfer1 || transfer2 || transfer3

  | Loop (name, loc, body, arg) ->
     let arg, fail1, transfer1 = typecheck env arg in
     if arg.ty = Tfail then
       error loc "loop arg is a failure";
     let env = maybe_reset_vars env transfer1 in
     let (new_name, env, count) = new_binding env name arg.ty in
     let body, fail2, transfer2 =
       typecheck_expected "loop-body" env
                          (Ttuple [Tbool; arg.ty])
                          body in
     check_used env name loc count;
     let can_fail = fail1 || fail2 in
     mk (Loop (new_name, loc, body, arg)) arg.ty can_fail,
     can_fail,
     transfer1 || transfer2

  | MatchList (arg, loc, head_name, tail_name, ifcons, ifnil) ->
     let arg, fail1, transfer1 = typecheck env arg in
     let arg_ty = match arg.ty with
       | Tfail ->
          error loc "cannot match failure"
       | Tlist ty -> ty
       | _ ->
          error loc "not a list type"
     in
     let env = maybe_reset_vars env transfer1 in
     let ifnil, fail2, transfer2 = typecheck env ifnil in
     let (new_head_name, env, count) = new_binding env head_name arg_ty in
     let (new_tail_name, env, count) =
       new_binding env tail_name (Tlist arg_ty) in
     let ifcons, fail3, transfer3 = typecheck env ifcons in
     check_used env head_name loc count;
     check_used env tail_name loc count;
     let desc = MatchList (arg, loc, new_head_name, new_tail_name, ifcons,
                           ifnil ) in
     let ty =
       match ifnil.ty, ifcons.ty with
       | ty, Tfail
         | Tfail, ty -> ty
       | ty1, ty2 ->
          if ty1 <> ty2 then
            error loc "not the same type";
          ty1
     in
     let can_fail = fail1 || fail2 || fail3 in
     mk desc ty can_fail,
     can_fail,
     transfer1 || transfer2 || transfer3

  | Lambda (arg_name, arg_type, loc, body, res_type) ->
     let env_at_lambda = env in
     let lambda_arg_type = arg_type in
     let lambda_arg_name = arg_name in
     let lambda_body = body in
     assert (res_type = Tunit);
     (* let env = { env with vars = StringMap.empty } in *)
     (* let (arg_name, env, arg_count) = new_binding env arg_name arg_type in *)
     let bvs = LiquidBoundVariables.bv exp in
     if StringSet.is_empty bvs then
       (* not a closure, create a real lambda *)
       let env = { env_at_lambda with vars = StringMap.empty } in
       let (new_arg_name, env, arg_count) =
         new_binding env lambda_arg_name lambda_arg_type in
       let body, _fail, transfer = typecheck env lambda_body in
       if transfer then
         error loc "no transfer in lambda";
       check_used env lambda_arg_name loc arg_count;
       let desc =
         Lambda (new_arg_name, lambda_arg_type, loc, body, body.ty) in
       let ty = Tlambda (lambda_arg_type, body.ty) in
       mk desc ty false, false, false
       else
         (* create closure with environment *)
         let env, arg_name, arg_type, call_env =
           env_for_clos env loc bvs arg_name arg_type in
         let body, _fail, transfer = typecheck env body in
         if transfer then
           error loc "no transfer in closure";
         (* begin match env.clos_env with *)
         (*   | None -> () *)
         (*   | Some clos_env -> *)
         (*     Format.eprintf "--- Closure %s (real:%b)---@." arg_name is_real_closure; *)
         (*     StringMap.iter (fun name (e, (cpt_in, cpt_out)) -> *)
         (*         Format.eprintf "%s -> %s , (%d, %d)@." *)
         (*           name (LiquidPrinter.Liquid.string_of_code e) !cpt_in !cpt_out *)
         (*       ) clos_env.env_bindings *)
         (* end; *)
         check_used_in_env env lambda_arg_name loc;
         let desc =
           Closure (arg_name, arg_type, loc, call_env, body, body.ty) in
         let call_env_type = match call_env with
           | [] -> assert false
           | [_, t] -> t.ty
           | _ -> Ttuple (List.map (fun (_, t) -> t.ty) call_env)
         in
         let ty = Tclosure ((lambda_arg_type, call_env_type), body.ty) in
         mk desc ty false, false, false

  | Closure _ -> assert false

  | Record (_loc, []) -> assert false
  | Record (loc, (( (label, _) :: _ ) as lab_x_exp_list)) ->
     let ty_name, _, _ =
       try
         StringMap.find label env.env.labels
       with Not_found ->
         error loc "unbound label %S" label
     in
     let record_ty, ty_kind = StringMap.find ty_name env.env.types in
     let len = List.length (match ty_kind with
                            | Type_record (tys,_labels) -> tys
                            | Type_variant _ | Type_alias -> assert false) in
     let t = Array.make len None in
     let record_can_fail = ref false in
     List.iteri (fun i (label, exp) ->
         let ty_name', label_pos, ty = try
             StringMap.find label env.env.labels
           with Not_found ->
             error loc "unbound label %S" label
         in
         if ty_name <> ty_name' then
           error loc "inconsistent list of labels";
         let exp, can_fail, transfer =
           typecheck_expected ("label "^ label) env ty exp in
         if transfer then
           error loc "transfer not allowed in record";
         t.(label_pos) <- Some exp;
         if can_fail then record_can_fail := true
       ) lab_x_exp_list;
     let args = Array.to_list t in
     let args = List.map (function
                          | None ->
                             error loc "some labels are not defined"
                          | Some exp -> exp) args in
     let ty = Ttype (ty_name, record_ty) in
     let desc = Apply(Prim_tuple, loc, args) in
     mk desc ty !record_can_fail, !record_can_fail, false

  | Constructor(loc, Constr constr, arg) ->
     let ty_name, arg_ty = StringMap.find constr env.env.constrs in
     let arg, can_fail, transfer =
       typecheck_expected "constr-arg" env arg_ty arg in
     if transfer then
       error loc "transfer not allowed in constructor argument";
     let constr_ty, ty_kind = StringMap.find ty_name env.env.types in
     let exp =
       match ty_kind with
       | Type_record _ | Type_alias -> assert false
       | Type_variant constrs ->
          let rec iter constrs =
            match constrs with
            | [] -> assert false
            | [c,_, _left_ty, _right_ty] ->
               assert (c = constr);
               arg
            | (c, ty, left_ty, right_ty) :: constrs ->
               let desc =
                 if c = constr then
                   (* We use an unused argument to carry the type to
                   the code generator *)
                   Apply(Prim_Left, loc, [arg; unused env right_ty])
                 else
                   let arg = iter constrs in
                   Apply(Prim_Right, loc, [arg; unused env left_ty])
               in
               mk desc ty can_fail
          in
          iter constrs
     in
     mk exp.desc (Ttype (ty_name, constr_ty)) can_fail, can_fail, false

  | Constructor(loc, Left right_ty, arg) ->
     let arg, can_fail, transfer = typecheck env arg in
     if transfer then
       error loc "transfer not allowed in constructor argument";
     let ty = Tor(arg.ty, right_ty) in
     let desc = Apply(Prim_Left,loc,[arg; unused env right_ty]) in
     mk desc ty can_fail, can_fail, false

  | Constructor(loc, Source (from_ty, to_ty), _arg) ->
     let ty = Tcontract(from_ty, to_ty) in
     let desc = Apply(Prim_Source,loc,[unused env from_ty; unused env to_ty]) in
     mk desc ty false, false, false

  | Constructor(loc, Right left_ty, arg) ->
     let arg, can_fail, transfer = typecheck env arg in
     if transfer then
       error loc "transfer not allowed in constructor argument";
     let ty = Tor(left_ty, arg.ty) in
     let desc = Apply(Prim_Right,loc,[arg; unused env left_ty]) in
     mk desc ty can_fail, can_fail, false

  | MatchVariant (arg, loc, cases) ->
    let arg, can_fail, transfer1 = typecheck env arg in
    let ty_name, constrs, is_left_right =
      try
        match arg.ty with
        | Tfail ->
          error loc "cannot match failure"
        | Ttype (ty_name, _) ->
          begin
            let constr_ty, ty_kind = StringMap.find ty_name env.env.types in
            match ty_kind with
            | Type_variant constrs ->
              (ty_name, List.map (fun (c, _, _, _) -> c) constrs, None)
            | Type_record _ | Type_alias -> raise Not_found
          end
        | Tor (left_ty, right_ty) ->
          (* Left, Right pattern matching *)
          let ty_name = LiquidPrinter.Liquid.string_of_type arg.ty in
          (ty_name, ["Left"; "Right"], Some (left_ty, right_ty))
        | _ -> raise Not_found
      with Not_found ->
        error loc "not a variant type: %s"
          (LiquidPrinter.Liquid.string_of_type arg.ty)
    in
    let env = maybe_reset_vars env transfer1 in
    let match_can_fail = ref can_fail in
    let expected_type = ref None in
    let has_transfer = ref transfer1 in
    let cases_extra_constrs =
      List.fold_left (fun acc -> function
          | CAny, _ -> acc
          | CConstr (c, _), _ -> StringSet.add c acc
        ) StringSet.empty cases
      |> ref
    in
    let cases = List.map (fun constr ->
        let cve = find_case loc env constr cases in
        cases_extra_constrs := StringSet.remove constr !cases_extra_constrs;
        cve
      ) constrs in

    if not (StringSet.is_empty !cases_extra_constrs) then
      error loc "constructors %s do not belong to type %s"
        (String.concat ", " (StringSet.elements !cases_extra_constrs))
        ty_name;

    let cases = List.map (fun (constr, vars, e) ->
        let var_ty = match is_left_right, constr with
          | Some (left_ty, _), "Left" -> left_ty
          | Some (_, right_ty), "Right" -> right_ty
          | Some _, _ -> error loc "unknown constructor %S" constr
          | None, _ ->
            let ty_name', var_ty =
              try StringMap.find constr env.env.constrs
              with Not_found -> error loc "unknown constructor %S" constr
            in
            if ty_name <> ty_name' then error loc "inconsistent constructors";
            var_ty
        in
        let name =
          match vars with
          | [] -> "_"
          | [ var ] -> var
          | _ ->
            error loc "cannot deconstruct constructor args"
        in
        let (new_name, env, count) =
          new_binding env name var_ty in
        let (e, can_fail, transfer) =
          match !expected_type with
          | Some expected_type ->
            typecheck_expected "pattern matching branch" env expected_type e
          | None ->
            let (e, can_fail, transfer) =
              typecheck env e in
            begin
              match e.ty with
              | Tfail -> ()
              | _ -> expected_type := Some e.ty
            end;
            (e, can_fail, transfer)
        in
        if can_fail then match_can_fail := true;
        if transfer then has_transfer := true;
        check_used env name loc count;
        (CConstr (constr, [new_name]), e)
      ) cases
    in

    let desc = MatchVariant (arg, loc, cases ) in
    let ty = match !expected_type with
      | None -> Tfail
      | Some ty -> ty
    in
    mk desc ty !match_can_fail, !match_can_fail, !has_transfer

and find_case loc env constr cases =
  match List.find_all (function
      | CAny, _ -> true
      | CConstr (cname, _), _ -> cname = constr
    ) cases
  with
  | [] ->
    error loc "non-exhaustive pattern. Constructor %s is not matched." constr
  | m :: unused ->
    List.iter (fun (_, e) ->
        LiquidLoc.warn (loc_exp env e) (UnusedMatched constr)
      ) unused;
    match m with
    | CAny, e -> constr, [], e
    | CConstr (_, vars), e -> constr, vars, e

and typecheck_case env name exp var_ty =
  let (new_name, env, count) =
    new_binding env name var_ty in
  let (exp, can_fail, transfer) = typecheck env  exp in
  (new_name, exp, can_fail, transfer)

and typecheck_prim1 env prim loc args =
  match prim, args with
  | Prim_tuple_get, [ { ty = Ttuple tuple };
                      { desc = Const (_, (CInt n | CNat n)) }] ->
     let n = LiquidPrinter.int_of_integer n in
     let size = List.length tuple in
     let prim =
       if size <= n then
         error loc "get outside tuple"
       else
         if size = n + 1 then
           Prim_tuple_get_last
         else
           Prim_tuple_get
     in
     let ty = List.nth tuple n in
     prim, ty

  | Prim_tuple_set, [ { ty = Ttuple tuple };
                      { desc = Const (_, (CInt n | CNat n)) };
                      { ty } ] ->
     let n = LiquidPrinter.int_of_integer n in
     let expected_ty = List.nth tuple n in
     let size = List.length tuple in
     let prim =
       if size <= n then
         error loc "set outside tuple"
       else
         if size = n + 1 then
           Prim_tuple_set_last
         else
           Prim_tuple_set
     in
     let ty = if ty <> expected_ty && ty <> Tfail then
                error loc "prim set bad type"
              else Ttuple tuple
     in
     prim, ty

  | _ ->
     let prim =
       (* Unqualified versions of primitives. They should not be used,
         but they can be generated by decompiling Michelson. *)
       match prim, args with
       | Prim_coll_update, [ _; _; { ty = Tset _ }] -> Prim_set_update
       | Prim_coll_update, [ _; _; { ty = Tmap _ }] -> Prim_map_update
       | Prim_coll_mem, [ _; { ty = Tset _ } ] -> Prim_set_mem
       | Prim_coll_mem, [ _; { ty = Tmap _ } ] -> Prim_map_mem
       | Prim_coll_find, [ _; { ty = Tmap _ } ] -> Prim_map_find
       | Prim_coll_map, [ _; { ty = Tlist _ } ] -> Prim_list_map
       | Prim_coll_map, [_; { ty = Tmap _ } ] -> Prim_map_map
       | Prim_coll_map, [_; { ty = Tset _ } ] -> Prim_set_map
       | Prim_coll_reduce, [_; { ty = Tlist _ }; _ ] -> Prim_list_reduce
       | Prim_coll_reduce, [_; { ty = Tset _ }; _ ] -> Prim_set_reduce
       | Prim_coll_reduce, [_; { ty = Tmap _ }; _ ] -> Prim_map_reduce
       | Prim_coll_size, [{ ty = Tlist _ } ] -> Prim_list_size
       | Prim_coll_size, [{ ty = Tset _ } ] -> Prim_set_size
       | Prim_coll_size, [{ ty = Tmap _ } ] -> Prim_map_size
       | _ -> prim
     in
     prim, typecheck_prim2 env prim loc args

and typecheck_prim2 env prim loc args =
  match prim, args with
  | ( Prim_neq | Prim_lt | Prim_gt | Prim_eq | Prim_le | Prim_ge ),
    [ { ty = ty1 }; { ty = ty2 } ] ->
     if comparable_ty ty1 ty2 then Tbool
     else error_not_comparable loc prim ty1 ty2
  | Prim_compare,
    [ { ty = ty1 }; { ty = ty2 } ] ->
     if comparable_ty ty1 ty2 then Tint
     else error_not_comparable loc prim ty1 ty2

  | (Prim_add|Prim_sub|Prim_mul),
    (  [ { ty = Ttez }; { ty = (Ttez | Tint | Tnat) } ]
       | [ { ty = (Tint | Tnat) }; { ty = Ttez } ])
    -> Ttez
  | (Prim_add|Prim_mul), [ { ty = Tnat }; { ty = Tnat } ] -> Tnat
  | (Prim_add|Prim_sub|Prim_mul), [ { ty = (Tint|Tnat) };
                                    { ty = (Tint|Tnat) } ] -> Tint
  | Prim_add, [ { ty = Ttimestamp }; { ty = Tint|Tnat } ] -> Ttimestamp
  | Prim_add, [ { ty = Tint|Tnat }; { ty = Ttimestamp } ] -> Ttimestamp

  (* TODO: improve types of ediv in Michelson ! *)
  | Prim_ediv, [ { ty = Tnat }; { ty = Tnat } ] ->
     Toption (Ttuple [Tnat; Tnat])
  | Prim_ediv, [ { ty = Tint|Tnat }; { ty = Tint|Tnat } ] ->
     Toption (Ttuple [Tint; Tnat])
  | Prim_ediv, [ { ty = Ttez }; { ty = Tnat } ] ->
     Toption (Ttuple [Ttez; Ttez])

  | Prim_xor, [ { ty = Tbool }; { ty = Tbool } ] -> Tbool
  | Prim_or, [ { ty = Tbool }; { ty = Tbool } ] -> Tbool
  | Prim_and, [ { ty = Tbool }; { ty = Tbool } ] -> Tbool
  | Prim_not, [ { ty = Tbool } ] -> Tbool

  | Prim_xor, [ { ty = Tnat }; { ty = Tnat } ] -> Tnat
  | Prim_xor, [ { ty = Tint|Tnat }; { ty = Tint|Tnat } ] -> Tint
  | Prim_or, [ { ty = Tnat }; { ty = Tnat } ] -> Tnat
  | Prim_or, [ { ty = Tint|Tnat }; { ty = Tint|Tnat } ] -> Tint
  | Prim_and, [ { ty = Tnat }; { ty = Tnat } ] -> Tnat
  | Prim_and, [ { ty = Tint|Tnat }; { ty = Tint|Tnat } ] -> Tint
  | Prim_not, [ { ty = Tint|Tnat } ] -> Tint

  | Prim_abs, [ { ty = Tint } ] -> Tnat
  | Prim_int, [ { ty = Tnat } ] -> Tint
  | Prim_sub, [ { ty = Tint|Tnat } ] -> Tint

  | (Prim_lsl|Prim_lsr), [ { ty = Tnat} ; { ty = Tnat } ] -> Tnat



  | Prim_tuple, args -> Ttuple (List.map (fun e -> e.ty) args)

  | Prim_map_find, [ { ty = key_ty }; { ty = Tmap (expected_key_ty, value_ty) }]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Map.find key type";
     Toption value_ty
  | Prim_map_update, [ { ty = key_ty };
                       { ty = Toption value_ty };
                       { ty = Tmap (expected_key_ty, expected_value_ty) }]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Map.update key type";
     if expected_value_ty <> value_ty then
       error loc "bad Map.update value type";
     Tmap (key_ty, value_ty)
  | Prim_map_mem, [ { ty = key_ty }; { ty = Tmap (expected_key_ty,_) }]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Mem.mem key type";
     Tbool

  | Prim_set_mem,[ { ty = key_ty }; { ty = Tset expected_key_ty }]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Set.mem key type";
     Tbool

  | Prim_list_size, [ { ty = Tlist _ }]  ->  Tnat
  | Prim_set_size, [ { ty = Tset _ }]  ->  Tnat
  | Prim_map_size, [ { ty = Tmap _ }]  ->  Tnat

  | Prim_set_update, [ { ty = key_ty }; { ty = Tbool };
                       { ty = Tset expected_key_ty }]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Set.update key type";
     Tset key_ty


  | Prim_Some, [ { ty } ] -> Toption ty
  | Prim_fail, [ { ty = Tunit } ] -> Tfail
  | Prim_self, [ { ty = Tunit } ] ->
     Tcontract (env.contract.parameter, env.contract.return)
  | Prim_now, [ { ty = Tunit } ] -> Ttimestamp
  | Prim_balance, [ { ty = Tunit } ] -> Ttez
  (*    | "Current.source", [ { ty = Tunit } ] -> ... *)
  | Prim_amount, [ { ty = Tunit } ] -> Ttez
  | Prim_gas, [ { ty = Tunit } ] -> Tnat
  | Prim_hash, [ _ ] -> Tstring
  | Prim_hash_key, [ { ty = Tkey } ] -> Tkey_hash
  | Prim_check, [ { ty = Tkey };
                  { ty = Ttuple [Tsignature; Tstring] } ] ->
     Tbool
  | Prim_check, args ->
     error_prim loc Prim_check args [Tkey; Ttuple [Tsignature; Tstring]]

  | Prim_manager, [ { ty = Tcontract(_,_) } ] ->
     Tkey_hash

  | Prim_create_account, [ { ty = Tkey_hash }; { ty = Toption Tkey_hash };
                           { ty = Tbool }; { ty = Ttez } ] ->
     Tcontract (Tunit, Tunit)
  | Prim_create_account, args ->
     error_prim loc Prim_create_account args
                [ Tkey_hash; Toption Tkey_hash; Tbool; Ttez ]

  | Prim_default_account, [ { ty = Tkey_hash } ] ->
     Tcontract (Tunit, Tunit)

  | Prim_create_contract, [ { ty = Tkey_hash }; (* manager *)
                            { ty = Toption Tkey_hash }; (* delegate *)
                            { ty = Tbool }; (* spendable *)
                            { ty = Tbool }; (* delegatable *)
                            { ty = Ttez }; (* initial amount *)
                            { ty = Tlambda (
                                       Ttuple [ arg_type;
                                                storage_arg],
                                       Ttuple [ result_type; storage_res]); };
                            { ty = storage_init }
                          ] ->
     if storage_arg <> storage_res then
       error loc "Contract.create: inconsistent storage in contract";
     if storage_res <> storage_init then
       error loc "Contract.create: wrong type for storage init";

     Tcontract (arg_type, result_type)

  | Prim_create_contract, args ->

     let expected_args = [ Tkey_hash; Toption Tkey_hash;
                           Tbool; Tbool; Ttez ] in
     let nexpected = 7 in
     let prim = LiquidTypes.string_of_primitive Prim_create_contract in
     let nargs = List.length args in
     if nargs <> nexpected then
       error loc "Prim %S: %d args provided, %d args expected"
             prim nargs nexpected
     else
       let args = List.map (fun { ty } -> ty) args in
       List.iteri (fun i (arg, expected) ->
           if arg <> expected then
             error loc
                   "Primitive %s, argument %d:\nExpected type:%sProvided type:%s"
                   prim (i+1)
                   (LiquidPrinter.Liquid.string_of_type expected)
                   (LiquidPrinter.Liquid.string_of_type arg)

         ) (List.combine args expected_args);
       error loc
             "Primitive %s, argument %d:\nExpected type: (arg * storage) -> (res * storage)\nProvided type:%s"
             prim 6
             (LiquidPrinter.Liquid.string_of_type (List.nth args 5))

  | Prim_exec, [ { ty };
                 { ty = ( Tlambda(from_ty, to_ty)
                          | Tclosure((from_ty, _), to_ty)) }] ->
     if ty <> from_ty then
       type_error loc "Bad argument type in function application" ty from_ty;
     to_ty

  | ( Prim_list_map
      (* | Prim_list_reduce *)
      | Prim_set_reduce
    | Prim_map_reduce
    | Prim_map_map
    | Prim_coll_map
    | Prim_coll_reduce
    ), { ty = Tclosure _ } :: _ ->
     error loc "Cannot use closures in %s" (LiquidTypes.string_of_primitive prim)

  | Prim_list_map, [
      { ty = Tlambda (from_ty, to_ty) };
      { ty = Tlist ty } ] ->
     if ty <> from_ty then
       type_error loc "Bad argument type in List.map" ty from_ty;
     Tlist to_ty

  | Prim_list_reduce, [
      { ty = ( Tlambda (Ttuple [src_ty; dst_ty], dst_ty')
               | Tclosure ((Ttuple [src_ty; dst_ty], _), dst_ty')) };
      { ty = Tlist src_ty' };
      { ty = acc_ty };
    ] ->
     if src_ty <> src_ty' then
       type_error loc "Bad argument source type in List.reduce" src_ty' src_ty;
     if dst_ty <> dst_ty' then
       type_error loc "Bad function type in List.reduce" dst_ty' dst_ty;
     if acc_ty <> dst_ty' then
       type_error loc "Bad accumulator type in List.reduce" acc_ty dst_ty';
     acc_ty

  | Prim_set_reduce, [
      { ty = Tlambda (Ttuple [src_ty; dst_ty], dst_ty') };
      { ty = Tset src_ty' };
      { ty = acc_ty };
    ] ->
     if src_ty <> src_ty' then
       type_error loc "Bad argument source type in Set.reduce" src_ty' src_ty;
     if dst_ty <> dst_ty' then
       type_error loc "Bad function type in Set.reduce" dst_ty' dst_ty;
     if acc_ty <> dst_ty' then
       type_error loc "Bad accumulator type in Set.reduce" acc_ty dst_ty';
     acc_ty

  | Prim_map_reduce, [
      { ty = Tlambda (Ttuple [Ttuple [key_ty; src_ty]; dst_ty], dst_ty') };
      { ty = Tmap (key_ty', src_ty') };
      { ty = acc_ty };
    ] ->
     if src_ty <> src_ty' then
       type_error loc "Bad argument source type in Map.reduce" src_ty' src_ty;
     if dst_ty <> dst_ty' then
       type_error loc "Bad function type in Map.reduce" dst_ty' dst_ty;
     if acc_ty <> dst_ty' then
       type_error loc "Bad accumulator type in Map.reduce" acc_ty dst_ty';
     if key_ty <> key_ty' then
       type_error loc "Bad function key type in Map.reduce" key_ty' key_ty;
     acc_ty

  | Prim_map_map, [
      { ty = Tlambda (Ttuple [from_key_ty; from_value_ty], to_value_ty) };
      { ty = Tmap (key_ty, value_ty) } ] ->
     if from_key_ty <> key_ty then
       type_error loc "Bad function key type in Map.map" key_ty from_key_ty;
     if from_value_ty <> value_ty then
       type_error loc "Bad function value type in Map.map"
                  value_ty from_value_ty;
     Tmap (key_ty, to_value_ty)


  | Prim_concat, [ { ty = Tstring }; { ty = Tstring }] -> Tstring
  | Prim_Cons, [ { ty = head_ty }; { ty = Tunit } ] ->
     Tlist head_ty
  | Prim_Cons, [ { ty = head_ty }; { ty = Tlist tail_ty } ] ->
     if head_ty <> tail_ty then
       type_error loc "Bad types for list" head_ty tail_ty;
     Tlist tail_ty
  | prim, args ->
     error loc "Bad %d args for primitive %S:\n    %s\n" (List.length args)
           (LiquidTypes.string_of_primitive prim)
           (String.concat "\n    "
                          (List.map
                             (fun arg ->
                               LiquidPrinter.Liquid.string_of_type arg.ty)
                             args))
    ;

and typecheck_expected info env expected_ty exp =
  let exp, fail, transfer = typecheck env exp in
  if exp.ty <> expected_ty && exp.ty <> Tfail then
    type_error (loc_exp env exp)
               ("Unexpected type for "^info) exp.ty expected_ty;
  exp, fail, transfer

and typecheck_apply env prim loc args =
  let can_fail = ref false in
  let args = List.map (fun arg ->
                 let arg, fail, transfer = typecheck env arg in
                 if transfer then
                   error loc "transfer within prim args";
                 if fail then can_fail := true;
                 arg
               ) args in
  let prim, ty = typecheck_prim1 env prim loc args in
  let can_fail =
    match prim with
    | Prim_fail -> true
    | _ -> !can_fail
  in
  mk (Apply (prim, loc, args)) ty can_fail, can_fail, false


(* FIXME ugly hack *)
and is_closure env exp =
  match typecheck env exp with
  | { ty = Tclosure ((ty_arg, ty_env), ty_ret) }, _, _ ->
     Some ((ty_arg, ty_env), ty_ret)
  | _ ->
     None


let typecheck_contract ~warnings env contract =
  let env =
    {
      warnings;
      counter = ref 0;
      vars = StringMap.empty;
      to_inline = ref StringMap.empty;
      env = env;
      clos_env = None;
      contract;
    } in

  (* "storage/1" *)
  let (_ , env, _) = new_binding env  "storage" contract.storage in
  (* "parameter/2" *)
  let (_, env, _) = new_binding env "parameter" contract.parameter in

  let expected_ty = Ttuple [contract.return; contract.storage] in

  let code, _can_fail, _transfer =
    typecheck_expected "return value" env expected_ty contract.code in
  { contract with code }, ! (env.to_inline)

let typecheck_code ~warnings env contract expected_ty code =
  let env =
    {
      warnings;
      counter = ref 0;
      vars = StringMap.empty;
      to_inline = ref StringMap.empty;
      env = env;
      clos_env = None;
      contract ;
    } in

  let code, _can_fail, _transfer =
    typecheck_expected "value" env expected_ty code in
  code


let check_const_type ~to_tez loc ty cst =
  let rec check_const_type ty cst =
    match ty, cst with
    | Tunit, CUnit -> CUnit
    | Tbool, CBool b -> CBool b

    | Tint, CInt s
      | Tint, CNat s -> CInt s

    | Tnat, CInt s
      | Tnat, CNat s -> CNat s

    | Tstring, CString s -> CString s

    | Ttez, CTez s -> CTez s
    | Ttez, CString s -> CTez (to_tez s)

    | Tkey, CKey s
    | Tkey, CString s -> CKey s

    | Tkey_hash, CKey_hash s
    | Tkey_hash, CString s -> CKey_hash s

    | Ttimestamp, CString s
    | Ttimestamp, CTimestamp s ->
      begin (* approximation of correct tezos timestamp *)
        try Scanf.sscanf s "%_d-%_d-%_dT%_d:%_d:%_dZ%!" ()
        with _ ->
        try Scanf.sscanf s "%_d-%_d-%_d %_d:%_d:%_dZ%!" ()
        with _ ->
        try Scanf.sscanf s "%_d-%_d-%_dT%_d:%_d:%_d-%_d:%_d%!" ()
        with _ ->
        try Scanf.sscanf s "%_d-%_d-%_dT%_d:%_d:%_d+%_d:%_d%!" ()
        with _ ->
        try Scanf.sscanf s "%_d-%_d-%_d %_d:%_d:%_d-%_d:%_d%!" ()
        with _ ->
        try Scanf.sscanf s "%_d-%_d-%_d %_d:%_d:%_d+%_d:%_d%!" ()
        with _ ->
          error loc "Bad format for timestamp"
      end;
      CTimestamp s


    | Tsignature, CSignature s
      | Tsignature, CString s -> CSignature s

    | Ttuple tys, CTuple csts ->
       begin
         try
           CTuple (List.map2 check_const_type tys csts)
         with Invalid_argument _ ->
           error loc "constant type mismatch (tuple length differs from type)"
       end

    | Toption _, CNone -> CNone
    | Toption ty, CSome cst -> CSome (check_const_type ty cst)

    | Tmap (ty1, ty2), CMap csts ->
       CMap (List.map (fun (cst1, cst2) ->
                 check_const_type ty1 cst1,
                 check_const_type ty2 cst2) csts)

    | Tlist ty, CList csts ->
       CList (List.map (check_const_type ty) csts)

    | Tset ty, CSet csts ->
       CSet (List.map (check_const_type ty) csts)

    | _ ->
       error loc "constant type mismatch"

  in
  check_const_type ty cst
