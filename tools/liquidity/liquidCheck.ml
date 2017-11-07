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
  match get_type ty1, get_type ty2 with
  | (Tint|Tnat), (Tint|Tnat)
  | Ttez, Ttez
  | Ttimestamp, Ttimestamp
  | Tstring, Tstring
  | Tkey_hash, Tkey_hash -> true
  | _ -> false

let error_not_comparable loc prim ty1 ty2 =
  error loc "arguments of %s not comparable: %s\nwith\n%s\n"
    (LiquidTypes.string_of_primitive prim)
    (LiquidPrinter.Liquid.string_of_type_expl ty1)
    (LiquidPrinter.Liquid.string_of_type_expl ty2)

let rec encode_type ?(keepalias=true) ty =
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
    let t1', t2', t3' = encode_type t1, encode_type t2, encode_type t3 in
    if t1 == t1' && t2 == t2' && t3 == t3' then ty
    else Tclosure ((t1', t2'), t3')
  | Ttype (name, t) ->
    let t' = encode_type t in
    if not keepalias then t'
    else if t' == t then ty
    else Ttype (name, t')
  | Trecord labels -> encode_record_type labels
  | Tsum cstys -> encode_sum_type cstys

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
      | Tsum constrs ->
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

let type_error loc msg actual expected =
  error loc "%s.\nExpected type:\n  %s\nActual type:\n  %s"
    msg
    (LiquidPrinter.Liquid.string_of_type_expl expected)
    (LiquidPrinter.Liquid.string_of_type_expl actual)


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
        if not (eq_types arg expected) then
          error loc
                "Primitive %s, argument %d:\nExpected type:%sProvided type:%s"
                prim (i+1)
                (LiquidPrinter.Liquid.string_of_type_expl expected)
                (LiquidPrinter.Liquid.string_of_type_expl arg)

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
  | MatchNat (_, loc, _, _, _, _)
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
     let desc = Const (ty, encode_const env cst) in
     let fail = false in
     mk desc ty fail, fail, false

  | Let (name, loc, exp, body) ->
     let exp, fail1, transfer1 = typecheck env exp in
     if eq_types exp.ty Tfail then
       error loc "cannot assign failure";
     let env = maybe_reset_vars env transfer1 in
     let (new_name, env, count) = new_binding env name exp.ty in
     let body, fail2, transfer2 = typecheck env body in
     let desc = Let (new_name, loc, exp, body ) in
     check_used env name loc count;
     if (not transfer1) && (not fail1) then begin
         match !count with
         | 0 ->
           env.to_inline :=
             StringMap.add new_name (const_unit) !(env.to_inline)
         | 1 ->
            env.to_inline := StringMap.add new_name exp !(env.to_inline)
         | _ -> ()
       end;
     let fail = fail1 || fail2 in
     mk desc body.ty fail, fail, transfer1 || transfer2

  | Var (name, loc, (_::_ as labels)) when env.only_typecheck ->
    begin match find_var env loc name with
      | { desc = Var (name, _, []); ty = (Ttype _) as ty; fail } ->
        let ty =
          List.fold_left (fun ty label ->
              match get_type ty with
              | Trecord ltys ->
                begin try
                    let _, _, ty = StringMap.find label env.env.labels in
                    let ty' = List.assoc label ltys in
                    if not (eq_types ty ty') then
                      error loc "label for wrong record";
                    ty'
                  with Not_found -> error loc "bad label"
                end
              | _ -> error loc "not a record"
            ) ty labels
        in
        mk (Var (name, loc, labels)) ty fail, fail, false
      | { desc = Var (name, _, []) } ->
        error loc "not a record"
      | _ -> assert false (* FIXME *)
    end

  | Var (name, loc, labels) ->
    let e = find_var env loc name in
    let e =
      List.fold_left
        (fun e label ->
           let ty_name, ty = match first_alias e.ty with
             | Some (ty_name, (Trecord _ as ty)) -> ty_name, ty
             | _ -> error loc "not a record"
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
           let args = [ arg1; arg2 ] in
           let prim, ty = typecheck_prim1 env Prim_tuple_get loc args in
           mk (Apply(prim, loc, args)) ty false
        ) e labels in
    e, false, false

  | SetVar (name, loc, labels, e) when env.only_typecheck ->
    let e, can_fail, transfer = typecheck env e in
    begin match find_var env loc name with
      | { desc = Var (name, _, []); ty = (Ttype _) as ty } ->
        mk (SetVar (name, loc, labels, e)) ty can_fail, can_fail, transfer
      | { desc = Var (name, _, []) } ->
        error loc "not a record"
      | _ -> assert false (* FIXME *)
    end

  | SetVar (name, loc, [], e) -> typecheck env e

  | SetVar (name, loc, label :: labels, arg) ->
     let arg1 = find_var env loc name in
     let ty = arg1.ty in
     let record_ty, label_types = match get_type ty with
       | (Trecord label_types) as ty -> ty, label_types
       | _ -> error loc "not a record %s"
                (LiquidPrinter.Liquid.string_of_type_expl ty)
     in
     let exception Return of int in
     let n =
       try
         List.iteri (fun n (l, _lty) ->
             if l = label then raise (Return n)
           ) label_types;
         error loc "bad label"
       with Return n -> n
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
       | ty ->
         error (loc_exp env contract_exp)
           "Bad contract type.\nExpected type:\n  ('a, 'b) contract\n\
            Actual type:\n  %s"
           (LiquidPrinter.Liquid.string_of_type_expl ty)
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
           ({ desc = Var ("Contract.call", varloc, [])} :: args)) ->
    let nb_args = List.length args in
    if nb_args <> 4 then
      error loc
        "Contract.call expects 4 arguments, it was given %d arguments."
        nb_args
    else
      error loc
        "The result of Contract.call must be bound under a \
         \"let (result, storage) =\" directly."

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
    let list_ty = l.ty in
    if env.only_typecheck then
      mk (Apply (Prim_list_rev, loc, [l])) list_ty fail, fail, false
    else
      let arg_name = uniq_ident env "arg" in
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
            const_false ;
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

  | MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus) ->
     let arg, fail1, transfer1 =
       typecheck_expected "match%nat" env Tint arg in
     let env = maybe_reset_vars env transfer1 in
     let (plus_name, env2, count_p) = new_binding env plus_name Tnat in
     let ifplus, fail2, transfer2 = typecheck env2 ifplus in
     let (minus_name, env3, count_m) = new_binding env minus_name Tnat in
     let ifminus, fail3, transfer3 = typecheck env3 ifminus in
     check_used env plus_name loc count_p;
     check_used env minus_name loc count_m;
     let desc = MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus) in
     let ty =
       match ifplus.ty, ifminus.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
         if ty1 <> ty2 then
           type_error loc "branches of match%nat must have the same type"
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
     if env.only_typecheck || StringSet.is_empty bvs then
       (* not a closure, create a real lambda *)
       let env =
         if env.only_typecheck then env_at_lambda
         else { env_at_lambda with vars = StringMap.empty } in
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
     let record_ty = StringMap.find ty_name env.env.types in
     let len = List.length (match record_ty with
         | Trecord rtys -> rtys
         | _ -> assert false) in
     let record_can_fail = ref false in
     if env.only_typecheck then
       let lab_exp = List.map (fun (label, exp) ->
           let ty_name', _, ty = try
               StringMap.find label env.env.labels
             with Not_found -> error loc "unbound label %S" label
           in
           if ty_name <> ty_name' then error loc "inconsistent list of labels";
           let exp, can_fail, transfer =
             typecheck_expected ("label "^ label) env ty exp in
           if transfer then error loc "transfer not allowed in record";
           if can_fail then record_can_fail := true;
           (label, exp)
         ) lab_x_exp_list in
       let ty = Ttype (ty_name, record_ty) in
       mk (Record (loc, lab_exp)) ty !record_can_fail, !record_can_fail, false
     else
       let t = Array.make len None in
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
     let constr_ty = StringMap.find ty_name env.env.types in
     let ty = Ttype (ty_name, constr_ty) in
     if env.only_typecheck then
       mk (Constructor(loc, Constr constr, arg)) ty can_fail, can_fail, false
     else
       let exp =
         match constr_ty with
         | Tsum constrs ->
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
               mk desc orty can_fail
           in
           iter constrs (encode_type ~keepalias:false ty)
         | _ -> assert false
       in
       mk exp.desc ty can_fail, can_fail, false

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
    let constrs, is_left_right =
      try
        match get_type arg.ty with
        | Tfail ->
          error loc "cannot match failure"
        | Tsum constrs ->
          (List.map fst constrs, None)
        | Tor (left_ty, right_ty) ->
          (* Left, Right pattern matching *)
          (["Left"; "Right"], Some (left_ty, right_ty))
        | _ -> raise Not_found
      with Not_found ->
        error loc "not a variant type: %s"
          (LiquidPrinter.Liquid.string_of_type_expl arg.ty)
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
        (LiquidPrinter.Liquid.string_of_type arg.ty);

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
            (* if ty_name <> ty_name' then error loc "inconsistent constructors"; *)
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
  | Prim_tuple_get, [ { ty = tuple_ty };
                      { desc = Const (_, (CInt n | CNat n)) }] ->
     let tuple = match (get_type tuple_ty) with
       | Ttuple tuple -> tuple
       | Trecord rtys -> List.map snd rtys
       | _ -> error loc "get takes a tuple as first argument, got:\n%s"
                (LiquidPrinter.Liquid.string_of_type_expl tuple_ty)
     in
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

  | Prim_tuple_set, [ { ty = tuple_ty };
                      { desc = Const (_, (CInt n | CNat n)) };
                      { ty } ] ->
     let tuple = match (get_type tuple_ty) with
       | Ttuple tuple -> tuple
       | Trecord rtys -> List.map snd rtys
       | _ -> error loc "set takes a tuple as first argument, got:\n%s"
                (LiquidPrinter.Liquid.string_of_type_expl tuple_ty)
     in
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
     let ty = if not (eq_types ty expected_ty || eq_types ty Tfail) then
                error loc "prim set bad type"
              else tuple_ty
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
  match prim, List.map (fun a -> get_type a.ty) args with
  | ( Prim_neq | Prim_lt | Prim_gt | Prim_eq | Prim_le | Prim_ge ),
    [ ty1; ty2 ] ->
     if comparable_ty ty1 ty2 then Tbool
     else error_not_comparable loc prim ty1 ty2
  | Prim_compare,
    [ ty1; ty2 ] ->
     if comparable_ty ty1 ty2 then Tint
     else error_not_comparable loc prim ty1 ty2

  | (Prim_add|Prim_sub|Prim_mul),
    (  [ Ttez; (Ttez | Tint | Tnat) ]
       | [ (Tint | Tnat); Ttez ])
    -> Ttez
  | (Prim_add|Prim_mul), [ Tnat; Tnat ] -> Tnat
  | (Prim_add|Prim_sub|Prim_mul), [ (Tint|Tnat);
                                    (Tint|Tnat) ] -> Tint
  | Prim_add, [ Ttimestamp; Tint|Tnat ] -> Ttimestamp
  | Prim_add, [ Tint|Tnat; Ttimestamp ] -> Ttimestamp

  (* TODO: improve types of ediv in Michelson ! *)
  | Prim_ediv, [ Tnat; Tnat ] ->
     Toption (Ttuple [Tnat; Tnat])
  | Prim_ediv, [ Tint|Tnat; Tint|Tnat ] ->
     Toption (Ttuple [Tint; Tnat])
  | Prim_ediv, [ Ttez; Tnat ] ->
     Toption (Ttuple [Ttez; Ttez])

  | Prim_xor, [ Tbool; Tbool ] -> Tbool
  | Prim_or, [ Tbool; Tbool ] -> Tbool
  | Prim_and, [ Tbool; Tbool ] -> Tbool
  | Prim_not, [ Tbool ] -> Tbool

  | Prim_xor, [ Tnat; Tnat ] -> Tnat
  | Prim_xor, [ Tint|Tnat; Tint|Tnat ] -> Tint
  | Prim_or, [ Tnat; Tnat ] -> Tnat
  | Prim_or, [ Tint|Tnat; Tint|Tnat ] -> Tint
  | Prim_and, [ Tnat; Tnat ] -> Tnat
  | Prim_and, [ Tint|Tnat; Tint|Tnat ] -> Tint
  | Prim_not, [ Tint|Tnat ] -> Tint

  | Prim_abs, [ Tint ] -> Tint
  | Prim_int, [ Tnat ] -> Tint
  | Prim_sub, [ Tint|Tnat ] -> Tint

  | (Prim_lsl|Prim_lsr), [ Tnat ; Tnat ] -> Tnat



  | Prim_tuple, ty_args -> Ttuple (List.map (fun e -> e.ty) args)

  | Prim_map_find, [ key_ty; Tmap (expected_key_ty, value_ty)]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Map.find key type";
     Toption value_ty
  | Prim_map_update, [ key_ty;
                       Toption value_ty;
                       Tmap (expected_key_ty, expected_value_ty)]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Map.update key type";
     if expected_value_ty <> value_ty then
       error loc "bad Map.update value type";
     Tmap (key_ty, value_ty)
  | Prim_map_mem, [ key_ty; Tmap (expected_key_ty,_)]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Mem.mem key type";
     Tbool

  | Prim_set_mem,[ key_ty; Tset expected_key_ty]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Set.mem key type";
     Tbool

  | Prim_list_size, [ Tlist _]  ->  Tnat
  | Prim_set_size, [ Tset _]  ->  Tnat
  | Prim_map_size, [ Tmap _]  ->  Tnat

  | Prim_set_update, [ key_ty; Tbool; Tset expected_key_ty]
    ->
     if expected_key_ty <> key_ty then
       error loc "bad Set.update key type";
     Tset key_ty


  | Prim_Some, [ ty ] -> Toption ty
  | Prim_fail, [ Tunit ] -> Tfail
  | Prim_self, [ Tunit ] ->
     Tcontract (env.contract.parameter, env.contract.return)
  | Prim_now, [ Tunit ] -> Ttimestamp
  | Prim_balance, [ Tunit ] -> Ttez
  (*    | "Current.source", [ Tunit ] -> ... *)
  | Prim_amount, [ Tunit ] -> Ttez
  | Prim_gas, [ Tunit ] -> Tnat
  | Prim_hash, [ _ ] -> Tstring
  | Prim_hash_key, [ Tkey ] -> Tkey_hash
  | Prim_check, [ Tkey; Ttuple [Tsignature; Tstring] ] ->
     Tbool
  | Prim_check, _ ->
     error_prim loc Prim_check args [Tkey; Ttuple [Tsignature; Tstring]]

  | Prim_manager, [ Tcontract(_,_) ] ->
     Tkey_hash

  | Prim_create_account, [ Tkey_hash; Toption Tkey_hash; Tbool; Ttez ] ->
     Tcontract (Tunit, Tunit)
  | Prim_create_account, _ ->
     error_prim loc Prim_create_account args
                [ Tkey_hash; Toption Tkey_hash; Tbool; Ttez ]

  | Prim_default_account, [ Tkey_hash ] ->
     Tcontract (Tunit, Tunit)

  | Prim_create_contract, [ Tkey_hash; (* manager *)
                            Toption Tkey_hash; (* delegate *)
                            Tbool; (* spendable *)
                            Tbool; (* delegatable *)
                            Ttez; (* initial amount *)
                            Tlambda (
                              Ttuple [ arg_type;
                                       storage_arg],
                              Ttuple [ result_type; storage_res]);
                            storage_init
                          ] ->
     if storage_arg <> storage_res then
       error loc "Contract.create: inconsistent storage in contract";
     if storage_res <> storage_init then
       error loc "Contract.create: wrong type for storage init";

     Tcontract (arg_type, result_type)

  | Prim_create_contract, _ ->

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
                   (LiquidPrinter.Liquid.string_of_type_expl expected)
                   (LiquidPrinter.Liquid.string_of_type_expl arg)

         ) (List.combine args expected_args);
       error loc
             "Primitive %s, argument %d:\n\
              Expected type: (arg * storage) -> (res * storage)\n\
              Provided type:%s"
             prim 6
             (LiquidPrinter.Liquid.string_of_type (List.nth args 5))

  | Prim_exec, [ ty;
                 ( Tlambda(from_ty, to_ty)
                 | Tclosure((from_ty, _), to_ty))] ->
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
    ), Tclosure _ :: _ ->
     error loc "Cannot use closures in %s" (LiquidTypes.string_of_primitive prim)

  | Prim_list_map, [
      Tlambda (from_ty, to_ty);
      Tlist ty ] ->
     if ty <> from_ty then
       type_error loc "Bad argument type in List.map" ty from_ty;
     Tlist to_ty

  | Prim_list_reduce, [
      ( Tlambda (Ttuple [src_ty; dst_ty], dst_ty')
               | Tclosure ((Ttuple [src_ty; dst_ty], _), dst_ty'));
      Tlist src_ty';
      acc_ty;
    ] ->
     if src_ty <> src_ty' then
       type_error loc "Bad argument source type in List.reduce" src_ty' src_ty;
     if dst_ty <> dst_ty' then
       type_error loc "Bad function type in List.reduce" dst_ty' dst_ty;
     if acc_ty <> dst_ty' then
       type_error loc "Bad accumulator type in List.reduce" acc_ty dst_ty';
     acc_ty

  | Prim_set_reduce, [
      Tlambda (Ttuple [src_ty; dst_ty], dst_ty');
      Tset src_ty';
      acc_ty;
    ] ->
     if src_ty <> src_ty' then
       type_error loc "Bad argument source type in Set.reduce" src_ty' src_ty;
     if dst_ty <> dst_ty' then
       type_error loc "Bad function type in Set.reduce" dst_ty' dst_ty;
     if acc_ty <> dst_ty' then
       type_error loc "Bad accumulator type in Set.reduce" acc_ty dst_ty';
     acc_ty

  | Prim_map_reduce, [
      Tlambda (Ttuple [Ttuple [key_ty; src_ty]; dst_ty], dst_ty');
      Tmap (key_ty', src_ty');
      acc_ty;
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
      Tlambda (Ttuple [from_key_ty; from_value_ty], to_value_ty);
      Tmap (key_ty, value_ty) ] ->
     if from_key_ty <> key_ty then
       type_error loc "Bad function key type in Map.map" key_ty from_key_ty;
     if from_value_ty <> value_ty then
       type_error loc "Bad function value type in Map.map"
                  value_ty from_value_ty;
     Tmap (key_ty, to_value_ty)


  | Prim_concat, [ Tstring; Tstring] -> Tstring
  | Prim_Cons, [ head_ty; Tunit ] ->
     Tlist head_ty
  | Prim_Cons, [ head_ty; Tlist tail_ty ] ->
     if head_ty <> tail_ty then
       type_error loc "Bad types for list" head_ty tail_ty;
     Tlist tail_ty
  | prim, _ ->
     error loc "Bad %d args for primitive %S:\n    %s\n" (List.length args)
           (LiquidTypes.string_of_primitive prim)
           (String.concat "\n    "
                          (List.map
                             (fun arg ->
                               LiquidPrinter.Liquid.string_of_type_expl arg.ty)
                             args))
    ;

and typecheck_expected info env expected_ty exp =
  let exp, fail, transfer = typecheck env exp in
  let exp_ty = get_type exp.ty in
  if exp_ty <> get_type expected_ty && exp_ty <> Tfail then
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


let typecheck_contract ~only_typecheck ~warnings env contract =
  let env =
    {
      warnings;
      only_typecheck;
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

let typecheck_code ~only_typecheck ~warnings env contract expected_ty code =
  let env =
    {
      warnings;
      only_typecheck;
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


let check_const_type ?(from_mic=false) ~to_tez loc ty cst =
  let rec check_const_type ty cst =
    match get_type ty, cst with
    | Tunit, CUnit -> CUnit
    | Tbool, CBool b -> CBool b

    | Tint, CInt s
      | Tint, CNat s -> CInt s

    | Tnat, CInt s
    | Tnat, CNat s -> CNat s

    | Tstring, CString s -> CString s

    | Ttez, CTez s -> CTez s

    | Tkey, CKey s -> CKey s
    | Tkey_hash, CKey_hash s -> CKey_hash s
    | Ttimestamp, CTimestamp s -> CTimestamp s
    | Tsignature, CSignature s -> CSignature s

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

    | Trecord labels, CRecord fields ->
      CRecord (List.map (fun (f, cst) ->
          try
            let ty = List.assoc f labels in
            f, check_const_type ty cst
          with Not_found ->
            error loc "Record field %s is not in type %s" f
              (LiquidPrinter.Liquid.string_of_type ty)
        ) fields)

    | Tsum constrs, CConstr (c, cst) ->
      CConstr (c,
               try
                 let ty = List.assoc c constrs in
                 check_const_type ty cst
               with Not_found ->
                 error loc "Constructor %s does not belong to type %s" c
                   (LiquidPrinter.Liquid.string_of_type ty)
              )

    | _ ->
      if from_mic then
        match ty, cst with
        | Ttimestamp, CString s ->
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

         | Ttez, CString s -> CTez (to_tez s)
         | Tkey_hash, CString s -> CKey_hash s
         | Tkey, CString s -> CKey s
         | Tsignature, CString s -> CSignature s

         | _ -> error loc "constant type mismatch"
       else
         error loc "constant type mismatch"

  in
  check_const_type ty cst
