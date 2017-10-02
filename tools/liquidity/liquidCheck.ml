(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

exception Unbound_variable of location * string

let noloc env = LiquidLoc.loc_in_file env.env.filename

let error loc msg =
  LiquidLoc.raise_error ~loc ("Type error:  " ^^ msg ^^ "%!")

let comparable_ty ty1 ty2 =
  match ty1, ty2 with
  | (Tint|Tnat|Ttez), (Tint|Tnat|Ttez)
    | Ttimestamp, Ttimestamp
    | Tstring, Tstring
    | Tkey, Tkey -> true
  | _ -> false

let error_not_comparable loc prim ty1 ty2 =
  error loc "arguments of %s not comparable: %s\nwith\n%s\n"
    (LiquidTypes.string_of_primitive prim)
    (LiquidPrinter.Michelson.string_of_type ty1)
    (LiquidPrinter.Michelson.string_of_type ty2)

let mk =
  let bv = StringSet.empty in
  fun desc (ty : datatype) fail -> { desc; ty; bv; fail }

let const_unit = mk (Const (Tunit, CUnit)) Tunit false

let unused env ty =
  mk (Apply(Prim_unused, noloc env, [const_unit])) ty false

let uniq_ident env name =
  let env = { env with counter = env.counter + 1 } in
  Printf.sprintf "%s/%d" name env.counter, env

let new_binding env name ty =
  let new_name, env = uniq_ident env name in
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (new_name, ty, count) env.vars } in
  (new_name, env, count)

let check_used env name loc count =
  if env.warnings && !count = 0 && name.[0] <> '_' then begin
      LiquidLoc.warn loc (Unused name)
  end


let find_var env loc name =
  try
    let (name, ty, count) = StringMap.find name env.vars in
    incr count;
    mk (Var (name, loc, [])) ty false
  with Not_found ->
  match env.clos_env with
  | None -> error loc "unbound variable %S" name
  | Some ce ->
    try
      StringMap.find name ce.env_bindings
    with Not_found ->
      error loc "unbound variable %S" name

let env_for_clos env loc arg_name arg_type =
  let free_vars = match env.clos_env with
    | Some ce ->
      StringMap.map
        (fun (bname, btype, index) -> (bname, btype, index + 1))
        ce.env_vars
    | None -> StringMap.empty
  in
  let _, free_vars =
    StringMap.fold (fun n (bname, btype, _) (index, free_vars) ->
      match btype with
      | Tlambda _ -> (index, free_vars)
      | _ ->
        let index = index + 1 in
        (index, StringMap.add n (bname, btype, index) free_vars)
    ) env.vars (StringMap.cardinal free_vars, free_vars)
  in
  let free_vars_l =
    StringMap.bindings free_vars
    |> List.sort (fun (_, (_,_,i1)) (_, (_,_,i2)) -> compare i1 i2)
  in
  let ext_env = env in
  let env = { env with vars = StringMap.empty } in
  match free_vars_l with
  | [] -> (* no closure environment *)
    let (new_name, env, _) = new_binding env arg_name arg_type in
    env, new_name, arg_type, []
  | _ ->
    let env_arg_name = uniq_ident "closure_env" in
    let env_arg_type =
      Ttuple (arg_type :: List.map (fun (_, (_,ty,_)) -> ty) free_vars_l) in
    let env_arg_var = mk (Var (env_arg_name, loc, [])) env_arg_type false in
    let env_vars =
      StringMap.add arg_name
        (uniq_ident arg_name, arg_type, 0) free_vars in
    let size = StringMap.cardinal env_vars in
    let env_bindings =
      StringMap.map (fun (_, ty, index) ->
          let ei = mk (Const (Tnat, CNat (string_of_int index))) Tnat false in
          let accessor = if index + 1 = size then "get_last" else "get" in
          mk (Apply(accessor, loc, [env_arg_var; ei])) ty false
        ) env_vars
    in
    let call_bindings = List.map (fun (name, _) ->
        name, find_var ext_env loc name
      ) free_vars_l
    in
    (* Format.eprintf "--- Closure %s ---@." env_arg_name; *)
    (* StringMap.iter (fun name e -> *)
    (*     Format.eprintf "%s -> %s@." *)
    (*       name (LiquidPrinter.Liquid.string_of_code e) *)
    (*   ) env_bindings; *)
    let env_closure = {
      free_vars;
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
    { env with vars = StringMap.empty }
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

  (* approximate location *)
let rec loc_exp env e = match e.desc with
  | Var (_, loc, _)
    | SetVar (_, loc, _, _)
    | Apply (_, loc, _)
    | LetTransfer (_, _, loc, _, _, _, _, _)
    | MatchOption (_, loc, _, _, _)
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
             let arg2 =
               mk (Const (Tnat, CNat (string_of_int n))) Tnat false in
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
       let arg2 =
         mk (Const (Tnat, CNat (string_of_int n))) Tnat false in
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
            let tmp_name, env = uniq_ident env "tmp#" in
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
       if transfer1 then
         error (noloc env) "transfer within if condition";
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
       transfer2 || transfer3

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
           error loc
                 "Unknown identifier %S" name
       in
       typecheck env { exp with
                       desc = Apply(prim, loc, args) }

    | Apply (Prim_unknown, loc, [f ; x ]) ->
       typecheck env { exp with
                       desc = Apply(Prim_exec, loc, [x; f]) }


    | Apply (prim, loc, args) ->
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
         | ty, Tfail
           | Tfail, ty -> ty
         | ty1, ty2 ->
           if ty1 <> ty2 then type_error loc "Bad option type in match" ty2 ty1;
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

       let lambda_arg_type = arg_type in
       assert (res_type = Tunit);
       (* let env = { env with vars = StringMap.empty } in *)
       (* let (arg_name, env, arg_count) = new_binding env arg_name arg_type in *)
       let env, arg_name, arg_type, call_env =
         env_for_clos env loc arg_name arg_type in
       let body, _fail, transfer = typecheck env body in
       if transfer then
         error loc "no transfer in lambda";
       let desc, ty = match call_env with
         | [] ->
           let desc = Lambda (arg_name, arg_type, loc, body, body.ty) in
           let ty = Tlambda (arg_type, body.ty) in
           desc, ty
         | _ ->
           let desc =
             Closure (arg_name, arg_type, loc, call_env, body, body.ty) in
           let call_env_type =
             Ttuple (List.map (fun (_, t) -> t.ty) call_env) in
           let ty = Tclosure ((lambda_arg_type, call_env_type), body.ty) in
           desc, ty
       in
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
                              | Type_variant _ -> assert false) in
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
         | Type_record _ -> assert false
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

    | MatchVariant (arg, loc,
                    [ "Left", [left_arg], left_exp;
                      "Right", [right_arg], right_exp;
                   ]) ->
       let arg, can_fail, transfer1 = typecheck env arg in
       let (left_ty, right_ty) = match arg.ty with
         | Tor(left_ty, right_ty) -> (left_ty, right_ty)
         | _ ->
            error loc "not a Left-Right variant type: %s"
              (LiquidPrinter.Michelson.string_of_type arg.ty)
       in
       let env = maybe_reset_vars env transfer1 in
       let left_arg, left_exp, can_fail1, transfer1 =
         typecheck_case env left_arg left_exp left_ty in
       let right_arg, right_exp, can_fail2, transfer2 =
         typecheck_case env right_arg right_exp right_ty in
       let desc = MatchVariant(arg, loc,
                               [ "Left", [left_arg], left_exp;
                                 "Right", [right_arg], right_exp;
                              ])
       in
       if left_exp.ty <> right_exp.ty then begin
           error loc "inconsistent return type"
         end;
       let can_fail = can_fail || can_fail1 || can_fail2 in
       mk desc left_exp.ty can_fail,
       can_fail,
       transfer1 || transfer2

    | MatchVariant (arg, loc,
                    [ ("Left" | "Right"), _, _; ]
                   ) ->
       error  loc "non-exhaustive pattern-matching"

    | MatchVariant (arg, loc, cases) ->
       let arg, can_fail, transfer1 = typecheck env arg in
       let ty_name, constrs =
         try
           match arg.ty with
           | Tfail ->
              error loc "cannot match failure"
           | Ttype (ty_name, _) ->
              begin
                let constr_ty, ty_kind = StringMap.find ty_name env.env.types in
                match ty_kind with
                | Type_variant constrs -> (ty_name, constrs)
                | Type_record _ -> raise Not_found
              end
           | _ -> raise Not_found
         with Not_found ->
           error loc "not a variant type: %s"
             (LiquidPrinter.Michelson.string_of_type arg.ty)
       in
       let env = maybe_reset_vars env transfer1 in
       let match_can_fail = ref can_fail in
       let expected_type = ref None in
       let has_transfer = ref transfer1 in
       let present_constrs = ref StringMap.empty in
       List.iter (fun (constr, vars, e) ->
           let ty_name', var_ty =
             try
               StringMap.find constr env.env.constrs
             with Not_found ->
               error loc "unknown constructor"
           in
           if ty_name <> ty_name' then
             error loc "inconsistent constructors";
           if StringMap.mem constr !present_constrs then
             error loc "constructor matched twice";
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
                typecheck_expected "constr-match" env expected_type e
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
           present_constrs := StringMap.add constr
                                            (new_name, e)
                                            !present_constrs
         ) cases;

       let cases = List.map (fun (constr, _ty, _left_ty, _right_ty) ->
                       let (new_name, e) =
                         try
                           StringMap.find constr !present_constrs
                         with Not_found ->
                           error loc "non-exhaustive pattern"
                       in
                       (constr, [new_name], e)
                     ) constrs in

       let desc = MatchVariant (arg, loc, cases ) in
       let ty = match !expected_type with
         | None -> Tfail
         | Some ty -> ty
       in
       mk desc ty !match_can_fail, !match_can_fail, !has_transfer

  and typecheck_case env name exp var_ty =
    let (new_name, env, count) =
      new_binding env name var_ty in
    let (exp, can_fail, transfer) = typecheck env  exp in
    (new_name, exp, can_fail, transfer)

  and typecheck_prim1 env prim loc args =
    match prim, args with
    | Prim_tuple_get, [ { ty = Ttuple tuple };
                               { desc = Const (_, (CInt n | CNat n)) }] ->
       let n = int_of_string n in
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
       let n = int_of_string n in
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
    | Prim_ediv, [ { ty = Ttez }; { ty = Tint } ] ->
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
         error loc "bad Map.add key type";
       if expected_value_ty <> value_ty then
         error loc "bad Map.add value type";
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
    | Prim_check, [ { ty = Tkey };
                        { ty = Ttuple [Tsignature; Tstring] } ] ->
       Tbool
    | Prim_manager, [ { ty = Tcontract(_,_) } ] ->
       Tkey
    | Prim_create_account, [ { ty = Tkey }; { ty = Toption Tkey };
                          { ty = Tbool }; { ty = Ttez } ] ->
       Tcontract (Tunit, Tunit)
    | Prim_default_account, [ { ty = Tkey } ] ->
       Tcontract (Tunit, Tunit)
    | Prim_create_contract, [ { ty = Tkey }; (* manager *)
                           { ty = Toption Tkey }; (* delegate *)
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

    | Prim_exec, [ { ty };
                   { ty = ( Tlambda(from_ty, to_ty)
                          | Tclosure((from_ty, _), to_ty)) }] ->
       if ty <> from_ty then
         type_error loc "Bad argument type in Lambda.pipe" ty from_ty;
       to_ty

    | Prim_list_map, [
        { ty = Tlambda (from_ty, to_ty) };
        { ty = Tlist ty } ] ->
       if ty <> from_ty then
         type_error loc "Bad argument type in List.map" ty from_ty;
       Tlist to_ty

    | Prim_list_reduce, [
        { ty = Tlambda (Ttuple [src_ty; dst_ty], dst_ty') };
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



let typecheck_contract ~warnings env contract =
  let env =
    {
      warnings;
      counter = 0;
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

  let expected_ty = Ttuple [ contract.return; contract.storage ] in

  let code, _can_fail, _transfer =
    typecheck_expected "final value" env expected_ty contract.code in
  { contract with code }, ! (env.to_inline)

let typecheck_code ~warnings env contract expected_ty code =
  let env =
    {
      warnings;
      counter = 0;
      vars = StringMap.empty;
      to_inline = ref StringMap.empty;
      env = env;
      clos_env = None;
      contract ;
    } in

  let code, _can_fail, _transfer =
    typecheck_expected "final value" env expected_ty code in
  code
