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
 *)

open LiquidTypes

let noloc env = LiquidLoc.loc_in_file env.env.filename

let error loc msg =
  LiquidLoc.raise_error ~loc ("Type error:  " ^^ msg ^^ "%!")

(* Two types are comparable if they are equal and of a comparable type *)
let check_comparable loc prim ty1 ty2 =
  if not (comparable_type ty1 && eq_types ty1 ty2) then
    error loc "arguments of %s not comparable: %s\nwith\n%s\n"
      (LiquidTypes.string_of_primitive prim)
      (LiquidPrinter.Liquid.string_of_type ty1)
      (LiquidPrinter.Liquid.string_of_type ty2)

let new_binding env name ?(effect=false) ty =
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (name, ty, effect) env.vars;
              vars_counts = StringMap.add name count env.vars_counts;
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
    let (name, ty, effect) = StringMap.find name env.vars in
    let count = StringMap.find name env.vars_counts in
    if count_used then incr count;
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
    (LiquidPrinter.Liquid.string_of_type expected)
    (LiquidPrinter.Liquid.string_of_type actual)

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
  | [ CConstr ("Left", l), case_l; CConstr ("Right", r), case_r ],
    [ c1, ty1; c2, ty2 ] ->
    List.rev @@ (CConstr (c2, r), case_r) ::
                (CConstr (c1, l), case_l) :: acc

  | [ CConstr ("Left", l), case_l;
      CConstr ("Right", [x]), { desc = Let { bnd_var = v; bnd_val = case_r;
                                             body = { desc = Var v' }}}],
    _ :: _
    when v.nname = v'
    ->
    merge_matches acc loc [ CConstr ("Left", l), case_l;
                            CConstr ("Right", [x]), case_r ] constrs

  | [ CConstr ("Left", l), case_l;
      CConstr ("Right", [x]), { desc =  Let { bnd_var = v; bnd_val = case_r;
                                              body = { desc = Const { const = CUnit } }}}],
    _ :: _ ->
    merge_matches acc loc [ CConstr ("Left", l), case_l;
                            CConstr ("Right", [x]), case_r ] constrs

  | [ CConstr ("Left", l), case_l; CConstr ("Right", [x]), case_r ],
    (c1, ty1) :: constrs ->
    begin match case_r.desc with
      | MatchVariant { arg = { desc = Var x' }; cases }
        when x = x' ->
        (* match arg with
           | Left l -> case_l
           | Right x -> match x with
                        | Left -> ...*)

        merge_matches ((CConstr (c1, l), case_l) :: acc) loc cases constrs
      | _ ->
        (* ==> match | C1 l -> case_l | _ -> case_r *)
        List.rev @@ (CAny, case_r) :: (CConstr (c1, l), case_l) :: acc
    end
  | _ -> raise Exit

(* Typecheck an expression. Returns a typed expression *)
let rec typecheck env ( exp : syntax_exp ) : typed_exp =
  let loc = exp.loc in
  match exp.desc with

  | Const { ty; const } ->
    mk ?name:exp.name ~loc (Const { ty; const }) (ty:datatype)

  | Let { bnd_var; inline; bnd_val; body } ->
    let bnd_val = typecheck env bnd_val in
    if bnd_val.ty = Tfail then
      LiquidLoc.warn bnd_val.loc AlwaysFails;
    let (env, count) =
      new_binding env bnd_var.nname ~effect:exp.effect bnd_val.ty in
    let body = typecheck env body in
    let desc = Let { bnd_var; inline; bnd_val; body } in
    check_used env bnd_var count;
    mk ?name:exp.name ~loc desc body.ty

  | Var v -> find_var env loc v

  | Project { field; record } ->
    let record = typecheck env record in
    let ty = match record.ty with
      | Trecord (record_name, ltys) ->
        begin
          try List.assoc field ltys
          with Not_found ->
            error loc "label %s does not belong to type %s"
              field record_name;
        end
      | rty -> error loc "not a record type: %s, has no field %s"
                 (LiquidPrinter.Liquid.string_of_type rty)
                 field
    in
    mk ?name:exp.name ~loc (Project { field; record }) ty

  | SetField { record; field; set_val } ->
    let record = typecheck env record in
    let exp_ty =
      let ty_name, _, ty =
        try find_label field env.env
        with Not_found -> error loc "unbound record field %S" field
      in
      let record_ty = find_type ty_name env.env in
      fail_neq ~loc ~info:"in record update" ~expected_ty:record.ty record_ty
        ~more:(Printf.sprintf
                 "field %s does not belong to type %s" field
                 (LiquidPrinter.Liquid.string_of_type record.ty));
      ty
    in
    let set_val = typecheck_expected "field update" env exp_ty set_val in
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

  | Call { contract; amount; entry; arg } ->
    let amount = typecheck_expected "call amount" env Ttez amount in
    let contract = typecheck env contract in
    let entry' = match entry with None -> "main" | Some e -> e in
    begin
      match contract.ty with
      | Tcontract contract_sig ->
        begin try
            let { parameter = arg_ty } =
              List.find (fun { entry_name } -> entry_name = entry')
                contract_sig.entries_sig in
            let arg = typecheck_expected "call argument" env arg_ty arg in
            if amount.transfer || contract.transfer || arg.transfer then
              error loc "transfer within transfer arguments";
            let desc = Call { contract; amount; entry; arg } in
            mk ?name:exp.name ~loc desc Toperation
          with Not_found ->
            error loc "contract has no entry point %s" entry';
        end
      | ty ->
        error contract.loc
          "Bad contract type.\nExpected type:\n  <Contract>.instance\n\
           Actual type:\n  %s"
          (LiquidPrinter.Liquid.string_of_type ty)
    end

  (* contract.main (param) amount *)
  | Apply { prim = Prim_unknown;
            args = { desc = Project { field = entry; record = contract }} ::
                   [param; amount] }
    when match (typecheck env contract).ty with
      | Tcontract _ -> true
      | _ -> false
    ->
    typecheck env
      (mk (Call { contract; amount; entry = Some entry; arg = param })
         ~loc ())

  | Apply { prim = Prim_unknown;
            args = { desc = Var "Contract.call" } :: args } ->
    let nb_args = List.length args in
    if nb_args <> 3 || nb_args <> 4  then
      error loc
        "Contract.call expects 3 or 4 arguments, it was given %d arguments."
        nb_args
    else
      error loc "Bad syntax for Contract.call."

  (* Extended primitives *)
  | Apply { prim = Prim_unknown;
            args = { desc = Var prim_name } :: args }
    when StringMap.mem prim_name env.env.ext_prims ->
    let eprim = StringMap.find prim_name env.env.ext_prims in
    let targs, args = List.fold_left (fun (targs, args) a ->
        match a.desc, targs with
        | Type ty, _ -> (ty :: targs, args)
        | _, [] -> (targs, (typecheck env a) :: args)
        | _, _ -> error loc "Type arguments must come first"
      ) ([], []) (List.rev args) in
    let tsubst =
      try List.combine eprim.tvs targs
      with Invalid_argument _ ->
        error loc "Expecting %d type arguments, got %d\n"
          (List.length eprim.tvs) (List.length targs) in
    let atys = List.map (tv_subst tsubst) eprim.atys in
    let rty = tv_subst tsubst eprim.rty in
    let prim = Prim_extension (prim_name, eprim.effect, targs,
                               eprim.nb_arg, eprim.nb_ret, eprim.minst) in
    begin try List.iter2 (fun a aty ->
      if not (eq_types a.ty aty) then
        error a.loc "Bad %d args for primitive %S:\n    %s\n"
          (List.length args) (LiquidTypes.string_of_primitive prim)
          (String.concat "\n    " (List.map (fun arg ->
            LiquidPrinter.Liquid.string_of_type arg.ty) args))
      ) args atys
    with Invalid_argument _ ->
      error loc "Primitive %S expects %d arguments, was given %d"
        (LiquidTypes.string_of_primitive prim)
        (List.length eprim.atys) (List.length args)
    end;
    mk ?name:exp.name ~loc (Apply { prim; args }) rty

  (* <unknown> (prim, args) -> prim args *)
  | Apply { prim = Prim_unknown;
            args = { desc = Var name } ::args }
    when not (StringMap.mem name env.vars) ->
    let prim =
      try
        LiquidTypes.primitive_of_string name
      with Not_found ->
        error loc "Unknown identifier %S" name
    in
    typecheck env { exp with
                    desc = Apply { prim; args } }

  (* <unknown> (f, x1, x2, x3) -> ((f x1) x2) x3) *)
  | Apply { prim = Prim_unknown; args = f :: ((_ :: _) as r) } ->
    let exp = List.fold_left (fun f x ->
        { exp with desc = Apply { prim = Prim_exec; args =  [x; f] }}
      ) f r
    in
    typecheck env exp

  | Apply { prim; args } ->
    typecheck_apply ?name:exp.name ~loc env prim loc args

  | Failwith arg ->
    let arg = typecheck env arg in
    mk (Failwith arg) ~loc Tfail (* no name *)

  | MatchOption { arg; ifnone; some_name; ifsome } ->
    let arg = typecheck env arg in
    let arg_ty = match arg.ty with
      | Tfail -> error loc "cannot match failure"
      | Toption ty -> ty
      | _ -> error loc "not an option type"
    in
    let ifnone = typecheck env ifnone in
    let (env, count) = new_binding env some_name.nname arg_ty in
    let ifsome = typecheck env ifsome in
    check_used env some_name count;
    let desc = MatchOption { arg; ifnone; some_name; ifsome } in
    let ty =
      match ifnone.ty, ifsome.ty with
      | ty, Tfail | Tfail, ty -> ty
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
      | Ttuple [Tor (left_ty, right_ty); acc_ty] ->
        fail_neq ~loc:acc.loc ~expected_ty:acc_ty acc.ty
          ~info:"in Loop.left accumulator";
        fail_neq ~loc:arg.loc ~expected_ty:left_ty arg.ty
          ~info:"in Loop.left argument";
        right_ty
      | _ ->
        error loc
          "Loop.left body must be of type (('a, 'b) variant * 'c), \
           got %s instead" (LiquidPrinter.Liquid.string_of_type body.ty) in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (LoopLeft { arg_name; body; arg; acc = Some acc })
      (Ttuple [res_ty; acc.ty])

  | LoopLeft { arg_name; body; arg; acc = None } ->
    let arg = typecheck env arg in
    let (env, count) = new_binding env arg_name.nname arg.ty in
    let body = typecheck env body in
    let res_ty = match body.ty with
      | Tor (left_ty, right_ty) ->
        fail_neq ~loc:arg.loc ~expected_ty:left_ty arg.ty
          ~info:"in Loop.left argument";
        right_ty
      | _ ->
        error loc
          "Loop.left body must be of type ('a, 'b) variant, \
           got %s instead" (LiquidPrinter.Liquid.string_of_type body.ty) in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (LoopLeft { arg_name; body; arg; acc = None }) res_ty

  (* For collections, replace generic primitives with their typed ones *)

  | Fold { prim; arg_name; body; arg; acc } ->
    let arg = typecheck env arg in
    let acc = typecheck env acc in
    let prim, arg_ty = match prim, arg.ty, acc.ty with
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
          (LiquidPrinter.Liquid.string_of_type arg.ty)
    in
    let (env, count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck_expected
        (LiquidTypes.string_of_fold_primitive prim ^" body") env acc.ty body in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (Fold { prim; arg_name; body; arg; acc }) acc.ty

  | Map { prim; arg_name; body; arg } ->
    let arg = typecheck env arg in
    let prim, arg_ty = match prim, arg.ty with
      | (Prim_map_map|Prim_coll_map), Tmap (k_ty, v_ty) ->
        Prim_map_map, Ttuple [k_ty; v_ty]
      | (Prim_list_map|Prim_coll_map), Tlist elt_ty ->
        Prim_list_map, elt_ty
      | _ ->
        error arg.loc "%s expects a collection, got %s"
          (LiquidTypes.string_of_map_primitive prim)
          (LiquidPrinter.Liquid.string_of_type arg.ty)
    in
    let (env, count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck env body in
    let ret_ty = match arg.ty with
      | Tmap (k_ty, _) -> Tmap (k_ty, body.ty)
      | Tset _ -> Tset body.ty
      | Tlist _ -> Tlist body.ty
      | _ -> assert false
    in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (Map { prim; arg_name; body; arg }) ret_ty

  | MapFold { prim; arg_name; body; arg; acc } ->
    let arg = typecheck env arg in
    let acc = typecheck env acc in
    let prim, arg_ty = match prim, arg.ty, acc.ty with
      | (Prim_map_map_fold|Prim_coll_map_fold), Tmap (k_ty, v_ty), acc_ty ->
        Prim_map_map_fold, Ttuple[Ttuple [k_ty; v_ty]; acc_ty]
      | (Prim_list_map_fold|Prim_coll_map_fold), Tlist elt_ty, acc_ty ->
        Prim_list_map_fold, Ttuple[elt_ty; acc_ty]
      | _ ->
        error arg.loc "%s expects a collection, got %s"
          (LiquidTypes.string_of_map_fold_primitive prim)
          (LiquidPrinter.Liquid.string_of_type arg.ty)
    in
    let (env, count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck env body in
    let body_r = match body.ty with
      | Ttuple [r; baccty] when eq_types baccty acc.ty -> r
      | _ ->
        error body.loc
          "body of %s must be of type 'a * %s, but has type %s"
          (LiquidTypes.string_of_map_fold_primitive prim)
          (LiquidPrinter.Liquid.string_of_type acc.ty)
          (LiquidPrinter.Liquid.string_of_type body.ty)
    in
    let ret_ty = match arg.ty with
      | Tmap (k_ty, _) -> Tmap (k_ty, body_r)
      | Tset _ -> Tset body_r
      | Tlist _ -> Tlist body_r
      | _ -> assert false
    in
    check_used env arg_name count;
    mk ?name:exp.name ~loc (MapFold { prim; arg_name; body; arg; acc })
      (Ttuple [ret_ty; acc.ty])

  | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
    let arg  = typecheck env arg in
    let arg_ty = match arg.ty with
      | Tfail -> error loc "cannot match failure"
      | Tlist ty -> ty
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
      | ty1, ty2 ->
        fail_neq ~loc:ifcons.loc ~expected_ty:ty1 ty2
          ~info:"in branches of match";
        ty1
    in
    mk ?name:exp.name ~loc desc ty

  | Lambda { arg_name; arg_ty; body; ret_ty; recursive = None } ->
    (* allow closures at typechecking, do not reset env *)
    let (env, arg_count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck env body in
    check_used env arg_name arg_count;
    let desc =
      Lambda { arg_name; arg_ty; body; ret_ty = body.ty; recursive = None } in
    let ty = Tlambda (arg_ty, body.ty) in
    mk ?name:exp.name ~loc desc ty

  | Lambda { arg_name; arg_ty; body; ret_ty;
             recursive = (Some f as recursive) } ->
    let ty = Tlambda (arg_ty, ret_ty) in
    let (env, f_count) = new_binding env f ty in
    let (env, arg_count) = new_binding env arg_name.nname arg_ty in
    let body = typecheck_expected "recursive function body" env ret_ty body in
    check_used env arg_name arg_count;
    check_used env { nname = f; nloc = loc} f_count;
    let desc = Lambda { arg_name; arg_ty; body; ret_ty; recursive } in
    mk ?name:exp.name ~loc desc ty

  (* This cannot be produced by parsing *)
  | Closure _ -> assert false

  (* Records with zero elements cannot be parsed *)
  | Record [] -> assert false

  | Record (( (label, _) :: _ ) as lab_x_exp_list) ->
    let ty_name, _, _ =
      try find_label label env.env
      with Not_found -> error loc "unbound label %S" label
    in
    let record_ty = find_type ty_name env.env in
    let labels = match record_ty with
      | Trecord (_, rtys) -> List.map fst rtys
      | _ -> assert false in
    let fields = List.map (fun (label, exp) ->
        let ty_name', _, ty = try
            find_label label env.env
          with Not_found -> error loc "unbound label %S" label
        in
        if ty_name <> ty_name' then error loc "inconsistent list of labels";
        let exp = typecheck_expected ("label "^ label) env ty exp in
        (label, exp)
      ) lab_x_exp_list in
    (* order record fields wrt type *)
    let fields = List.map (fun l ->
        try List.find (fun (l', _) -> l = l') fields
        with Not_found -> error loc "label %s is not defined" l;
      ) labels in
    mk ?name:exp.name ~loc (Record fields) record_ty

  (* TODO
     | Constructor(loc, Constr constr, arg)
      when env.decompiling && not @@ StringMap.mem constr env.env.constrs ->
      (* intermediate unknown constructor, add it *)
      let ty_name = "unknown_constructors" in
      let arg = typecheck env arg in
      let constr_ty = match StringMap.find_opt ty_name env.env.types with
        | Some (Tsum (n, constrs)) -> Tsum (n, (constr, arg.ty) :: constrs)
        | Some _ -> assert false
        | None -> Tsum (ty_name, [constr, arg.ty])
      in
      env.env.constrs <-
        StringMap.add constr (ty_name, arg.ty) env.env.constrs;
      env.env.types <- StringMap.add ty_name constr_ty env.env.types;
      mk ?name:exp.name ~loc (Constructor(loc, Constr constr, arg)) constr_ty
  *)

  | Constructor { constr = Constr constr; arg } ->
    begin try
        let ty_name, arg_ty = find_constr constr env.env in
        let arg = typecheck_expected "construtor argument" env arg_ty arg in
        let constr_ty = find_type ty_name env.env in
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
        let constrs, is_left_right =
          try
            match arg.ty with
            | Tfail ->
              error loc "cannot match failure"
            | Tsum (_, constrs) ->
              (List.map fst constrs, None)
            | Tor (left_ty, right_ty) ->
              (* Left, Right pattern matching *)
              (["Left"; "Right"], Some (left_ty, right_ty))
            | _ -> raise Not_found
          with Not_found ->
            error loc "not a variant type: %s"
              (LiquidPrinter.Liquid.string_of_type arg.ty)
        in
        let expected_type = ref None in
        let cases_extra_constrs =
          List.fold_left (fun acc -> function
              | CAny, _ -> acc
              | CConstr (c, _), _ -> StringSet.add c acc
            ) StringSet.empty cases
          |> ref
        in
        (* Normalize cases:
           - match cases in order
           - one (at most) wildcard at the end *)
        let cases = List.map (fun constr ->
            let pat, body = find_case loc env constr cases in
            cases_extra_constrs := StringSet.remove constr !cases_extra_constrs;
            constr, pat, body
          ) constrs in
        let are_unbound vars body =
          let body_vars = LiquidBoundVariables.bv body in
          not (List.exists (fun v -> StringSet.mem v body_vars) vars) in
        let rec normalize acc rev_cases = match rev_cases, acc with
          | [], _ -> acc
          | (_, CAny, body1) :: rev_cases, [CAny, body2]
            when eq_syntax_exp body1 body2 ->
            normalize [CAny, body1] rev_cases
          | (_, CAny, body1) :: rev_cases, [CConstr (_, vars2), body2]
            when are_unbound vars2 body2 && eq_syntax_exp body1 body2 ->
            normalize [CAny, body1] rev_cases
          | (_, CConstr (_, vars1) , body1) :: rev_cases, [CAny, body2]
            when are_unbound vars1 body1 && eq_syntax_exp body1 body2 ->
            normalize [CAny, body1] rev_cases
          | (_, CConstr (_, vars1) , body1) :: rev_cases,
            [CConstr (_, vars2), body2]
            when are_unbound vars1 body1 && are_unbound vars2 body2 &&
                 eq_syntax_exp body1 body2 ->
            normalize [CAny, body1] rev_cases
          | (c1, CAny, body1) :: rev_cases, _ ->
            (* body1 <> body2 *)
            normalize ((CConstr (c1, []), body1) :: acc)  rev_cases
          | (_, CConstr (c1, vars1), body1) :: rev_cases, _ ->
            normalize ((CConstr (c1, vars1), body1) :: acc)  rev_cases
        in
        let cases = normalize [] (List.rev cases) in

        if not (StringSet.is_empty !cases_extra_constrs) then
          error loc "constructors %s do not belong to type %s"
            (String.concat ", " (StringSet.elements !cases_extra_constrs))
            (LiquidPrinter.Liquid.string_of_type arg.ty);

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
              | CConstr ("Left", vars) ->
                let var_ty = match is_left_right with
                  | Some (left_ty, _) -> left_ty
                  | None -> error loc "expected variant, got %s"
                              (LiquidPrinter.Liquid.string_of_type arg.ty) in
                add_vars_env vars var_ty
              | CConstr ("Right", vars) ->
                let var_ty = match is_left_right with
                  | Some (_, right_ty) -> right_ty
                  | None -> error loc "expected variant, got %s"
                              (LiquidPrinter.Liquid.string_of_type arg.ty) in
                add_vars_env vars var_ty
              | CConstr (constr, vars) ->
                let ty_name', var_ty =
                  try find_constr constr env.env
                  with Not_found -> error loc "unknown constructor %S" constr
                in
                (* if ty_name <> ty_name' then
                   error loc "inconsistent constructors"; *)
                add_vars_env vars var_ty
              | CAny -> env, None
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
              | CConstr (_, [var]), Some count ->
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

  | ContractAt { arg; c_sig } ->
    let arg = typecheck_expected "Contract.at argument" env Taddress arg in
    let desc = ContractAt { arg; c_sig } in
    mk ?name:exp.name ~loc desc (Toption (Tcontract c_sig))

  | CreateContract { args; contract } ->
    let contract = typecheck_contract ~warnings:env.warnings
        ~decompiling:env.decompiling contract in
    begin match args with
    | [manager; delegate; spendable; delegatable; init_balance; init_storage] ->
      let manager = typecheck_expected "manager" env Tkey_hash manager in
      let delegate =
        typecheck_expected "delegate" env (Toption Tkey_hash) delegate in
      let spendable = typecheck_expected "spendable" env Tbool spendable in
      let delegatable =
        typecheck_expected "delegatable" env Tbool delegatable in
      let init_balance =
        typecheck_expected "initial balance" env Ttez init_balance in
      let init_storage = typecheck_expected "initial storage"
          env (lift_type contract.ty_env contract.storage) init_storage in
      let desc = CreateContract {
          args = [manager; delegate; spendable;
                  delegatable; init_balance; init_storage];
          contract } in
      mk ?name:exp.name ~loc desc (Ttuple [Toperation; Taddress])
    | _ ->
      error loc "Contract.create expects 7 arguments, was given %d"
        (List.length args)
    end

  | TypeAnnot { e; ty } ->
     typecheck_expected "annotated expression" env ty e

  | Type ty -> assert false (* Not supposed to be typechecked *)

and find_case loc env constr cases =
  match List.find_all (function
      | CAny, _ -> true
      | CConstr (cname, _), _ -> cname = constr
    ) cases
  with
  | [] ->
    error loc "non-exhaustive pattern. Constructor %s is not matched." constr
  | matched_case :: unused ->
    List.iter (function
        | CAny, _ -> ()
        | (CConstr _, (e : syntax_exp)) ->
          LiquidLoc.warn e.loc (UnusedMatched constr)
      ) unused;
    matched_case

and typecheck_prim1 env prim loc args =
  match prim, args with
  | Prim_tuple_get, [{ ty = tuple_ty };
                     { desc = Const { const = CInt n | CNat n }}] ->
    let tuple = match tuple_ty with
      | Ttuple tuple -> tuple
      | Trecord (_, rtys) -> List.map snd rtys
      | _ -> error loc "get takes a tuple as first argument, got:\n%s"
               (LiquidPrinter.Liquid.string_of_type tuple_ty)
    in
    let n = LiquidPrinter.int_of_integer n in
    let size = List.length tuple in
    if size <= n then error loc "get outside tuple";
    let ty = List.nth tuple n in
    prim, ty

  | Prim_tuple_set, [{ ty = tuple_ty ; loc = tuple_loc };
                     { desc = Const { const = CInt n | CNat n }};
                     { ty ; loc = val_loc }] ->
    let tuple = match tuple_ty with
      | Ttuple tuple -> tuple
      | Trecord (_, rtys) -> List.map snd rtys
      | _ -> error tuple_loc "set takes a tuple as first argument, got:\n%s"
               (LiquidPrinter.Liquid.string_of_type tuple_ty)
    in
    let n = LiquidPrinter.int_of_integer n in
    let expected_ty = List.nth tuple n in
    let size = List.length tuple in
    if size <= n then error loc "set outside tuple";
    if ty <> Tfail then
      fail_neq ~loc:val_loc ~expected_ty ty
        ~info:"in tuple update";
    let ty = tuple_ty in
    prim, ty

  | _ ->
    let prim =
      (* Unqualified versions of primitives. They should not be used,
         but they can be generated by decompiling Michelson. *)
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

  | Prim_add, [ Ttimestamp; Tint|Tnat ] -> Ttimestamp
  | Prim_add, [ Tint|Tnat; Ttimestamp ] -> Ttimestamp
  | Prim_sub, [ Ttimestamp; Tint|Tnat ] -> Ttimestamp
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
  | Prim_self, [ Tunit ] -> Tcontract (sig_of_full_sig env.t_contract_sig)
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

  | Prim_check, [ Tkey; Tsignature; Tbytes ] ->
    Tbool
  | Prim_check, _ ->
    error_prim loc Prim_check args [Tkey; Tsignature; Tbytes]

  | Prim_address, [ Tcontract _ ] ->
    Taddress

  | Prim_create_account, [ Tkey_hash; Toption Tkey_hash; Tbool; Ttez ] ->
    Ttuple [Toperation; Taddress]
  | Prim_create_account, _ ->
    error_prim loc Prim_create_account args
      [ Tkey_hash; Toption Tkey_hash; Tbool; Ttez ]

  | Prim_default_account, [ Tkey_hash ] ->
    Tcontract unit_contract_sig

  | Prim_set_delegate, [ Toption Tkey_hash ] ->
    Toperation

  | Prim_exec, [ ty;
                 ( Tlambda(from_ty, to_ty)
                 | Tclosure((from_ty, _), to_ty))] ->

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
           (List.map LiquidPrinter.Liquid.string_of_type args_tys))

and expected_prim_types = function
  | Prim_neq | Prim_lt | Prim_gt | Prim_eq | Prim_le | Prim_ge | Prim_compare ->
    2, "'a, 'a"
  | Prim_neg ->
    1, "int | nat"
  | Prim_add | Prim_sub ->
    2, "(nat | int | tez | timestamp), (nat | int | tez | timestamp)"
  | Prim_mul | Prim_ediv ->
    2, "(nat | int | tez), (nat | int | tez)"

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
  | Prim_gas ->
    1, "unit"

  | Prim_blake2b
  | Prim_sha256
  | Prim_sha512 ->
    1, "bytes"

  | Prim_hash_key ->
    1, "key"

  | Prim_check ->
    3, "key, signature, bytes"

  | Prim_address ->
    1, "<Contract>.instance"

  | Prim_create_account ->
    4, "manager:key_hash, delegate:(key_hash option), \
        delegatable:bool, amount:tez"

  | Prim_default_account ->
    1, "key_hash"

  | Prim_set_delegate ->
    1, "key_hash option"

  | Prim_exec ->
    2, "'a, ('a -> 'b)"

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

  | Prim_tuple -> assert false (* any types *)

  | Prim_tuple_get
  | Prim_tuple_set
  | Prim_unknown
  | Prim_coll_mem
  | Prim_coll_find
  | Prim_coll_update
  | Prim_coll_size
  | Prim_Left
  | Prim_Right
  | Prim_extension _
  | Prim_slice
  | Prim_concat
  | Prim_unused _
    -> assert false (* already handled *)


and typecheck_expected info env expected_ty exp =
  let exp = typecheck env exp in
  if exp.ty <> Tfail then
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
  let check_used v c =
    check_used env { nname = v; nloc = noloc env } c in
  check_used entry.entry_sig.parameter_name count_param;
  check_used entry.entry_sig.storage_name count_storage;
  { entry with code }

and typecheck_contract ~warnings ~decompiling contract =
  let env =
    {
      warnings;
      annot = false;
      decompiling;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      force_inline = ref StringMap.empty;
      env = contract.ty_env;
      clos_env = None;
      t_contract_sig = sig_of_contract contract;
    } in

  (* Add bindings to the environment for the global values *)
  let env, values, counts =
    List.fold_left (fun (env, values, counts) (name, inline, exp) ->
        let exp = typecheck env exp in
        let (env, count) = new_binding env name ~effect:exp.effect exp.ty in
        env, ((name, inline, exp) :: values), ((name, count) :: counts)
      ) (env, [], []) contract.values in
  (* Typecheck entries *)
  let entries = List.map (typecheck_entry env) contract.entries in
  (* Report unused global values *)
  List.iter (fun (name, count) ->
      check_used env { nname = name; nloc = noloc env } (* TODO *) count
    ) counts;
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
    c_init }

let typecheck_code env ?expected_ty code =
  match expected_ty with
  | Some expected_ty -> typecheck_expected "value" env expected_ty code
  | None -> typecheck env code


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

  | CBigMap [] -> Tbigmap (Tint, Tunit)
  | CBigMap ((k,e) :: _) -> Tbigmap (type_of_const k, type_of_const e)

  | CList [] -> Tlist (Tunit)
  | CList (e :: _) -> Tlist (type_of_const e)

  | CSet [] -> Tset (Tint)
  | CSet (e :: _) -> Tset (type_of_const e)

  | CLeft c -> Tor (type_of_const c, Tunit)
  | CRight c -> Tor (Tunit, type_of_const c)

  | CKey_hash _ -> Tkey_hash
  | CContract _ -> Tcontract unit_contract_sig

  (* XXX just for printing *)
  | CRecord _ -> Trecord ("<record>", [])
  | CConstr _ -> Tsum ("<sum>", [])


let check_const_type ?(from_mic=false) ~to_tez loc ty cst =
  let top_ty, top_cst = ty, cst in
  let rec check_const_type ty cst =
    match ty, cst with
    | Tunit, CUnit -> CUnit
    | Tbool, CBool b -> CBool b

    | Tint, CInt s
    | Tint, CNat s -> CInt s

    | Tnat, CInt s
    | Tnat, CNat s -> CNat s

    | Tstring, CString s -> CString s

    | Tbytes, CBytes s -> CBytes s

    | Ttez, CTez s -> CTez s

    | Tkey, CKey s -> CKey s
    | Tkey, CBytes s -> CKey s

    | Tkey_hash, CKey_hash s -> CKey_hash s
    | Tkey_hash, CBytes s -> CKey_hash s

    | Tcontract _, CContract s -> CContract s
    | Tcontract _, CAddress s -> CAddress s
    | Tcontract { entries_sig = [{ parameter= Tunit }] } , CKey_hash s ->
      CKey_hash s
    | Tcontract _, CBytes s -> CContract s

    | Taddress, CAddress s -> CAddress s
    | Taddress, CContract s -> CContract s
    | Taddress, CKey_hash s -> CKey_hash s
    | Taddress, CBytes s -> CContract s

    | Ttimestamp, CTimestamp s -> CTimestamp s

    | Tsignature, CSignature s -> CSignature s
    | Tsignature, CBytes s -> CSignature s

    | Ttuple tys, CTuple csts ->
      begin
        try
          CTuple (List.map2 check_const_type tys csts)
        with Invalid_argument _ ->
          error loc "constant type mismatch (tuple length differs from type)"
      end

    | Toption _, CNone -> CNone
    | Toption ty, CSome cst -> CSome (check_const_type ty cst)

    | Tor (left_ty, _), CLeft cst ->
      CLeft (check_const_type left_ty cst)

    | Tor (_, right_ty), CRight cst ->
      CRight (check_const_type right_ty cst)

    | Tmap (ty1, ty2), CMap csts ->
      CMap (List.map (fun (cst1, cst2) ->
          check_const_type ty1 cst1,
          check_const_type ty2 cst2) csts)

    | Tbigmap (ty1, ty2), (CMap csts | CBigMap csts) -> (* allow map *)
      CBigMap (List.map (fun (cst1, cst2) ->
          check_const_type ty1 cst1,
          check_const_type ty2 cst2) csts)

    | Tlist ty, CList csts ->
      CList (List.map (check_const_type ty) csts)

    | Tset ty, CSet csts ->
      CSet (List.map (check_const_type ty) csts)

    | Trecord (rname, labels), CRecord fields ->
      (* order record fields wrt type *)
      List.iter (fun (f, _) ->
          if not @@ List.mem_assoc f labels then
            error loc "Record field %s is not in type %s" f rname
        ) fields;
      let fields = List.map (fun (f, ty) ->
          try
            let cst = List.assoc f fields in
            f, check_const_type ty cst
          with Not_found ->
            error loc "Record field %s is missing" f
        ) labels in
      CRecord fields

    | Tsum (sname, constrs), CConstr (c, cst) ->
      CConstr (c,
               try
                 let ty = List.assoc c constrs in
                 check_const_type ty cst
               with Not_found ->
                 error loc "Constructor %s does not belong to type %s" c sname
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
        | Tcontract _, CString s -> CContract s
        | Tkey, CString s -> CKey s
        | Tsignature, CString s -> CSignature s

        | _ ->
          error loc "constant type mismatch, expected %s, got %s"
            (LiquidPrinter.Liquid.string_of_type top_ty)
            (LiquidPrinter.Liquid.string_of_type (type_of_const top_cst))
      else
        error loc "constant type mismatch, expected %s, got %s"
          (LiquidPrinter.Liquid.string_of_type top_ty)
          (LiquidPrinter.Liquid.string_of_type (type_of_const top_cst))

  in
  check_const_type ty cst
