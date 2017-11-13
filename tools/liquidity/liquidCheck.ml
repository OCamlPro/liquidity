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

let uniq_ident env name =
  env.counter := !(env.counter) + 1;
  Printf.sprintf "%s/%d" name !(env.counter)

let new_binding env name ty =
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (name, ty, count) env.vars } in
  (env, count)

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
    mk (Var (name, loc, [])) ty
  with Not_found ->
    error loc "unbound variable %S" name

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
let rec typecheck env ( exp : syntax_exp ) : typed_exp =
  match exp.desc with

  | Const (ty, cst) ->
    mk (Const (ty, cst)) ty

  | Let (name, loc, exp, body) ->
     let exp = typecheck env exp in
     if eq_types exp.ty Tfail then error loc "cannot assign failure";
     let env = maybe_reset_vars env exp.transfer in
     let (env, count) = new_binding env name exp.ty in
     let body = typecheck env body in
     let desc = Let (name, loc, exp, body ) in
     check_used env name loc count;
     mk desc body.ty

  | Var (name, loc, (_::_ as labels)) ->
    begin match find_var env loc name with
      | { desc = Var (name, _, []); ty = (Ttype _) as ty } ->
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
        mk (Var (name, loc, labels)) ty
      | _ -> assert false
    end

  | Var (name, loc, labels) -> find_var env loc name

  | SetVar (name, loc, [], e) ->
    let e = typecheck env e in
    mk (SetVar (name, loc, [], e)) e.ty

  | SetVar (name, loc, ((l :: _) as labels), arg) ->
    (* let arg = typecheck env arg in *)
    let { ty } = find_var env loc name in
    (* let label_types = match get_type ty with *)
    (*   | Trecord label_types -> label_types *)
    (*   | _ -> error loc "not a record %s" *)
    (*            (LiquidPrinter.Liquid.string_of_type_expl ty) *)
    (* in *)
    (* let lty = *)
    (*   try List.assoc l label_types *)
    (*   with Not_found -> *)
    (*     error loc "label %s does not belong to type %s" l *)
    (*       (LiquidPrinter.Liquid.string_of_type_expl ty) *)
    (* in *)
    let exp_ty =
      List.fold_left (fun lty label ->
          let ty_name, _, ty =
            try StringMap.find label env.env.labels
            with Not_found -> error loc "unbound label %S" label
          in
          let record_ty = StringMap.find ty_name env.env.types in
          if not (eq_types lty record_ty) then
            error loc "label %s does not belong to type %s" l
              (LiquidPrinter.Liquid.string_of_type_expl lty);
          ty
        ) ty labels
    in
    let arg = typecheck_expected "set" env exp_ty arg in
    mk (SetVar (name, loc, labels, arg)) ty

  | Seq (exp1, exp2) ->
    let exp1 = typecheck_expected "sequence" env Tunit exp1 in
    let exp2 = typecheck env exp2 in
    let desc = Seq (exp1, exp2) in
    (* TODO: if not fail1 then remove exp1 *)
    mk desc exp2.ty

  | If (cond, ifthen, ifelse) ->
     let cond =
       typecheck_expected "if-cond" env Tbool cond in
     let ifthen = typecheck env ifthen in
     let ifelse, ty =
       if ifthen.ty = Tfail then
         let ifelse = typecheck env ifelse in
         ifelse, ifelse.ty
       else
         let ifelse =
           typecheck_expected "if-result" env ifthen.ty ifelse in
         ifelse, ifthen.ty
     in
     let desc = If(cond, ifthen, ifelse) in
     mk desc ty

  | LetTransfer (storage_name, result_name,
                 loc,
                 contract_exp, tez_exp,
                 storage_exp, arg_exp, body) ->
     let tez_exp = typecheck_expected "call-amount" env Ttez tez_exp in
     let contract_exp = typecheck env contract_exp in
     begin
       match contract_exp.ty with
       | Tcontract (arg_ty, return_ty) ->
          let arg_exp = typecheck_expected "call-arg" env arg_ty arg_exp in
          let storage_exp =
            typecheck_expected "call-storage"
              env env.contract.storage storage_exp in
          if tez_exp.transfer || contract_exp.transfer
             || arg_exp.transfer || storage_exp.transfer then
            error loc "transfer within transfer arguments";
          let (env, storage_count) =
            new_binding env storage_name env.contract.storage in
          let (env, result_count) =
            new_binding env result_name return_ty in
          let body = typecheck env body in
          check_used env storage_name loc storage_count;
          check_used env result_name loc result_count;
          let desc = LetTransfer(storage_name, result_name,
                                 loc,
                                 contract_exp, tez_exp,
                                 storage_exp, arg_exp, body)
          in
          mk desc body.ty
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



  | Apply (prim, loc, args) -> typecheck_apply env prim loc args

  | MatchOption (arg, loc, ifnone, name, ifsome) ->
     let arg = typecheck env arg in
     let arg_ty = match arg.ty with
       | Tfail -> error loc "cannot match failure"
       | Toption ty -> ty
       | _ -> error loc "not an option type"
     in
     let env = maybe_reset_vars env arg.transfer in
     let ifnone = typecheck env ifnone in
     let (env, count) = new_binding env name arg_ty in
     let ifsome = typecheck env ifsome in
     check_used env name loc count;
     let desc = MatchOption (arg, loc, ifnone, name, ifsome ) in
     let ty =
       match get_type ifnone.ty, get_type ifsome.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
          if ty1 <> ty2 then type_error loc "Bad option type in match" ty2 ty1;
          ty1
     in
     mk desc ty

  | MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus) ->
     let arg = typecheck_expected "match%nat" env Tint arg in
     let env = maybe_reset_vars env arg.transfer in
     let (env2, count_p) = new_binding env plus_name Tnat in
     let ifplus = typecheck env2 ifplus in
     let (env3, count_m) = new_binding env minus_name Tnat in
     let ifminus = typecheck env3 ifminus in
     check_used env plus_name loc count_p;
     check_used env minus_name loc count_m;
     let desc = MatchNat (arg, loc, plus_name, ifplus, minus_name, ifminus) in
     let ty =
       match get_type ifplus.ty, get_type ifminus.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
         if ty1 <> ty2 then
           type_error loc "branches of match%nat must have the same type"
             ty2 ty1;
         ty1
     in
     mk desc ty

  | Loop (name, loc, body, arg) ->
     let arg = typecheck env arg in
     if get_type arg.ty = Tfail then error loc "loop arg is a failure";
     let env = maybe_reset_vars env arg.transfer in
     let (env, count) = new_binding env name arg.ty in
     let body =
       typecheck_expected "loop-body" env (Ttuple [Tbool; arg.ty]) body in
     check_used env name loc count;
     mk (Loop (name, loc, body, arg)) arg.ty

  | MatchList (arg, loc, head_name, tail_name, ifcons, ifnil) ->
     let arg  = typecheck env arg in
     let arg_ty = match get_type arg.ty with
       | Tfail -> error loc "cannot match failure"
       | Tlist ty -> ty
       | _ -> error loc "not a list type"
     in
     let env = maybe_reset_vars env arg.transfer in
     let ifnil = typecheck env ifnil in
     let (env, count) = new_binding env head_name arg_ty in
     let (env, count) = new_binding env tail_name (Tlist arg_ty) in
     let ifcons = typecheck env ifcons in
     check_used env head_name loc count;
     check_used env tail_name loc count;
     let desc = MatchList (arg, loc, head_name, tail_name, ifcons, ifnil) in
     let ty =
       match get_type ifnil.ty, get_type ifcons.ty with
       | ty, Tfail | Tfail, ty -> ty
       | ty1, ty2 ->
          if ty1 <> ty2 then
            error loc "not the same type";
          ty1
     in
     mk desc ty

  | Lambda (arg_name, arg_type, loc, body, res_type) ->
    let lambda_arg_type = arg_type in
    let lambda_arg_name = arg_name in
    let lambda_body = body in
    assert (res_type = Tunit);
    (* allow closures at typechecking, do not reset env *)
    let (env, arg_count) = new_binding env lambda_arg_name lambda_arg_type in
    let body = typecheck env lambda_body in
    if body.transfer then error loc "no transfer in lambda";
    check_used env lambda_arg_name loc arg_count;
    let desc = Lambda (arg_name, lambda_arg_type, loc, body, body.ty) in
    let ty = Tlambda (lambda_arg_type, body.ty) in
    mk desc ty

  | Closure _ -> assert false

  | Record (_loc, []) -> assert false
  | Record (loc, (( (label, _) :: _ ) as lab_x_exp_list)) ->
     let ty_name, _, _ =
       try StringMap.find label env.env.labels
       with Not_found -> error loc "unbound label %S" label
     in
     let record_ty = StringMap.find ty_name env.env.types in
     let remaining_labels = match record_ty with
       | Trecord rtys -> List.map fst rtys |> StringSet.of_list |> ref
       | _ -> assert false in
     let lab_exp = List.map (fun (label, exp) ->
         let ty_name', _, ty = try
             StringMap.find label env.env.labels
           with Not_found -> error loc "unbound label %S" label
         in
         if ty_name <> ty_name' then error loc "inconsistent list of labels";
         let exp = typecheck_expected ("label "^ label) env ty exp in
         if exp.transfer then error loc "transfer not allowed in record";
         remaining_labels := StringSet.remove label !remaining_labels;
         (label, exp)
       ) lab_x_exp_list in
     if not (StringSet.is_empty !remaining_labels) then
       error loc "label %s is not defined" (StringSet.choose !remaining_labels);
     let ty = Ttype (ty_name, record_ty) in
     mk (Record (loc, lab_exp)) ty

  | Constructor(loc, Constr constr, arg) ->
     let ty_name, arg_ty = StringMap.find constr env.env.constrs in
     let arg = typecheck_expected "constr-arg" env arg_ty arg in
     if arg.transfer then
       error loc "transfer not allowed in constructor argument";
     let constr_ty = StringMap.find ty_name env.env.types in
     let ty = Ttype (ty_name, constr_ty) in
     mk (Constructor(loc, Constr constr, arg)) ty

  | Constructor(loc, Left right_ty, arg) ->
     let arg = typecheck env arg in
     if arg.transfer then
       error loc "transfer not allowed in constructor argument";
     let ty = Tor (arg.ty, right_ty) in
     mk (Constructor(loc, Left right_ty, arg)) ty

  | Constructor(loc, Right left_ty, arg) ->
     let arg = typecheck env arg in
     if arg.transfer then
       error loc "transfer not allowed in constructor argument";
     let ty = Tor (left_ty, arg.ty) in
     mk (Constructor(loc, Right left_ty, arg)) ty

  | Constructor(loc, Source (from_ty, to_ty), arg) ->
    let arg = typecheck env arg in
    let ty = Tcontract(from_ty, to_ty) in
    mk (Constructor(loc, Source (from_ty, to_ty), arg)) ty

  | MatchVariant (arg, loc, cases) ->
    let arg = typecheck env arg in
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
    let env = maybe_reset_vars env arg.transfer in
    let expected_type = ref None in
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
        let env, count_opt =
          match vars with
          | [] -> env, None
          | [ var ] ->
            let (env, count) = new_binding env var var_ty in
            env, Some count
          | _ ->
            error loc "cannot deconstruct constructor args"
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
        begin match vars, count_opt with
          | [name], Some count -> check_used env name loc count
          | _ -> ()
        end;
        (CConstr (constr, vars), e)
      ) cases
    in

    let desc = MatchVariant (arg, loc, cases) in
    let ty = match !expected_type with
      | None -> Tfail
      | Some ty -> ty
    in
    mk desc ty

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
    | Prim_list_reduce
    | Prim_set_reduce
    | Prim_map_reduce
    | Prim_map_map
    | Prim_coll_map
    | Prim_coll_reduce
    ), Tclosure _ :: _ ->
    error loc "Cannot use closures in %s" (LiquidTypes.string_of_primitive prim)

  | Prim_list_rev, [ Tlist ty ] -> Tlist ty

  | Prim_list_map, [
      Tlambda (from_ty, to_ty);
      Tlist ty ] ->
     if ty <> from_ty then
       type_error loc "Bad argument type in List.map" ty from_ty;
     Tlist to_ty

  | Prim_list_reduce, [
      Tlambda (Ttuple [src_ty; dst_ty], dst_ty');
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
  let exp = typecheck env exp in
  let exp_ty = get_type exp.ty in
  if exp_ty <> get_type expected_ty && exp_ty <> Tfail then
    type_error (loc_exp env exp)
               ("Unexpected type for "^info) exp.ty expected_ty;
  exp

and typecheck_apply env prim loc args =
  let args = List.map (fun arg ->
                 let arg = typecheck env arg in
                 if arg.transfer then
                   error loc "transfer within prim args";
                 arg
               ) args in
  let prim, ty = typecheck_prim1 env prim loc args in
  mk (Apply (prim, loc, args)) ty


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

  let (env, _) = new_binding env  "storage" contract.storage in
  let (env, _) = new_binding env "parameter" contract.parameter in
  let expected_ty = Ttuple [contract.return; contract.storage] in
  let code =
    typecheck_expected "return value" env expected_ty contract.code in
  { contract with code }

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

  typecheck_expected "value" env expected_ty code


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
