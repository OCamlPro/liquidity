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

let noloc = "At unspecified location"

let error loc msg =
  Printf.eprintf "%s\nType error:  %s\n%!" loc msg;
  raise Error

let warning loc msg =
  Printf.eprintf "%s\nWarning:  %s\n%!" loc msg

let comparable_ty ty1 ty2 =
  match ty1, ty2 with
  | (Tint|Tnat|Ttez), (Tint|Tnat|Ttez)
    | Ttimestamp, Ttimestamp
    | Tstring, Tstring
    | Tkey, Tkey -> true
  | _ -> false

let error_not_comparable loc prim ty1 ty2 =
  error loc (
          Printf.sprintf "arguments of %s not comparable: %s\nwith\n%s\n"
                         prim
                         (LiquidPrinter.Michelson.string_of_type ty1)
                         (LiquidPrinter.Michelson.string_of_type ty2))

let mk =
  let bv = StringSet.empty in
  fun desc (ty : datatype) fail -> { desc; ty; bv; fail }

let const_unit = mk (Const (Tunit, CUnit)) Tunit false

let unused ty =
  mk (Apply("_unused_", noloc, [const_unit])) ty false

let counter = ref 0
let uniq_ident name =
  incr counter;
  Printf.sprintf "%s/%d" name !counter

let new_binding env name ty =
  let new_name = uniq_ident name in
  let count = ref 0 in
  let env = { env with
              vars = StringMap.add name (new_name, ty, count) env.vars } in
  (new_name, env, count)

let check_used name loc count =
  if !count = 0 && name.[0] <> '_' then begin
      warning
        loc
        (Printf.sprintf "unused variable %S" name)
    end

let maybe_reset_vars env transfer =
  if transfer then
    { env with vars = StringMap.empty }
  else env

let eprint_2types ty1 ty2 =
  Printf.eprintf "First type:\n  %s\n"
                 (LiquidPrinter.Liquid.string_of_type ty1);
  Printf.eprintf "Second type:\n  %s\n"
                 (LiquidPrinter.Liquid.string_of_type ty2);
  ()

let types env contract =

  let to_inline = ref StringMap.empty in

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
       check_used name loc count;
       if (not transfer1) && (not fail1) then begin
           match !count with
           | 0 ->
              to_inline := StringMap.add new_name const_unit !to_inline
           | 1 ->
              to_inline := StringMap.add new_name exp !to_inline
           | _ -> ()
         end;
       let fail = fail1 || fail2 in
       mk desc body.ty fail, fail, transfer1 || transfer2

    | Var (name, loc, labels) ->
       let (name, ty, count) =
         try
           StringMap.find name env.vars
         with Not_found ->
           Printf.kprintf (error loc) "unbound variable %S" name
       in
       incr count;
       let e = mk (Var (name, loc, [])) ty false in
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
                   StringMap.find label env.labels in
                 if ty_name' <> ty_name then
                   error loc "label for wrong record";
                 n
               with Not_found ->
                 error loc "bad label"
             in
             let arg2 =
               mk (Const (Tnat, CNat (string_of_int n))) Tnat false in
             let args = [ arg1; arg2] in
             let prim, ty = typecheck_prim1 "get" loc args in
             mk (Apply(prim, loc, args)) ty false
           ) e labels in
       e, false, false

    | SetVar (name, loc, [], e) -> typecheck env e

    | SetVar (name, loc, label :: labels, arg) ->
       let (name, ty, count) =
         try
           StringMap.find name env.vars
         with Not_found ->
           Printf.kprintf (error loc) "unbound variable %S" name
       in
       incr count;

       let ty_name, tuple_ty = match ty with
         | Ttype (ty_name, ty) -> ty_name, ty
         | _ ->
            error loc "not a record"
       in
       let arg1 = mk (Var (name, loc, [])) tuple_ty false in
       let n =
         try
           let (ty_name', n, _label_ty) =
             StringMap.find label env.labels in
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
            let prim, ty = typecheck_prim1 "get" loc args in
            let get_exp = mk (Apply(prim, loc, args)) ty false in
            let tmp_name = Printf.sprintf "tmp#%d" !counter in
            incr counter;
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
       let prim, tuple_ty' = typecheck_prim1 "set" loc args in
       mk (Apply(prim, loc, args)) ty can_fail, can_fail, false

    | Seq (exp1, exp2) ->
       let exp1, fail1, transfer1 =
         typecheck_expected (noloc, "sequence") env Tunit exp1 in
       let exp2, fail2, transfer2 = typecheck env exp2 in
       let desc = Seq (exp1, exp2) in
       (* TODO: if not fail1 then remove exp1 *)
       let can_fail = fail1 || fail2 in
       mk desc exp2.ty can_fail, can_fail, transfer1 || transfer2

    | If (cond, ifthen, ifelse) ->
       let cond, fail1, transfer1 =
         typecheck_expected (noloc, "if-cond") env Tbool cond in
       if transfer1 then
         error noloc "transfer within if condition";
       let ifthen, fail2, transfer2 = typecheck env ifthen in
       let ifelse, fail3, transfer3, ty =
         if ifthen.ty = Tfail then
           let ifelse, fail3, transfer3 = typecheck env ifelse in
           ifelse, fail3, transfer3, ifelse.ty
         else
           let ifelse, fail3, transfer3 =
             typecheck_expected (noloc, "if-result") env ifthen.ty ifelse in
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
         typecheck_expected (noloc, "call-amount") env Ttez tez_exp in
       let contract_exp, fail2, transfer2 = typecheck env contract_exp in
       begin
         match contract_exp.ty with
         | Tcontract (arg_ty, return_ty) ->
            let arg_exp, fail3, transfer3 =
              typecheck_expected (noloc,"call-arg") env arg_ty arg_exp in
            let storage_exp, fail4, transfer4 =
              typecheck_expected (noloc, "call-storage")
                                 env contract.storage storage_exp in
            if transfer1 || transfer2 || transfer3 || transfer4 then
              error noloc "transfer within transfer arguments";
            let (new_storage, env, storage_count) =
              new_binding env storage_name contract.storage in
            let (new_result, env, result_count) =
              new_binding env result_name return_ty in
            let body, fail5, transfer5 = typecheck env body in
            check_used storage_name loc storage_count;
            check_used result_name loc result_count;
            let desc = LetTransfer(new_storage, new_result,
                                   loc,
                                   contract_exp, tez_exp,
                                   storage_exp, arg_exp, body)
            in
            mk desc body.ty true,
            true, true
         | _ ->
            Printf.eprintf "typecheck error: Contract expected\n%!";
            raise Error
       end
    | Apply (prim, loc, args) ->
       let can_fail = ref false in
       let args = List.map (fun arg ->
                      let arg, fail, transfer = typecheck env arg in
                      if transfer then
                        error loc "transfer within prim args";
                      if fail then can_fail := true;
                      arg
                    ) args in
       let prim, ty = typecheck_prim1 prim loc args in
       let can_fail =
         match prim with
         | "Current.fail" -> true
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
       check_used name loc count;
       let desc = MatchOption (arg, loc, ifnone, new_name, ifsome ) in
       let ty =
         match ifnone.ty, ifsome.ty with
         | ty, Tfail
           | Tfail, ty -> ty
         | ty1, ty2 ->
            if ty1 <> ty2 then begin
                eprint_2types ty1 ty2;
                error loc "not the same type";
              end;
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
         typecheck_expected (noloc, "loop-body") env
                            (Ttuple [Tbool; arg.ty])
                            body in
       check_used name loc count;
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
       check_used head_name loc count;
       check_used tail_name loc count;
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

       assert (res_type = Tunit);
       let env = { env with vars = StringMap.empty } in
       let (new_name, env, arg_count) = new_binding env arg_name arg_type in
       let body, _fail, transfer = typecheck env body in
       if transfer then
         error loc "no transfer in lambda";
       let desc = Lambda (new_name, arg_type, loc, body, body.ty) in
       let ty = Tlambda (arg_type, body.ty) in
       mk desc ty false, false, false

    | Record (_loc, []) -> assert false
    | Record (loc, (( (label, _) :: _ ) as lab_x_exp_list)) ->
       let ty_name, _, _ =
         try
           StringMap.find label env.labels
         with Not_found ->
           error loc (Printf.sprintf "unbound label %S" label)
       in
       let record_ty, ty_kind = StringMap.find ty_name env.types in
       let len = List.length (match ty_kind with
                              | Type_record (tys,_labels) -> tys
                              | Type_variant _ -> assert false) in
       let t = Array.make len None in
       let record_can_fail = ref false in
       List.iteri (fun i (label, exp) ->
           let ty_name', label_pos, ty = try
               StringMap.find label env.labels
             with Not_found ->
               error loc (Printf.sprintf "unbound label %S" label)
           in
           if ty_name <> ty_name' then
             error loc "inconsistent list of labels";
           let exp, can_fail, transfer =
             typecheck_expected (noloc, "label "^ label) env ty exp in
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
       let desc = Apply("tuple", loc, args) in
       mk desc ty !record_can_fail, !record_can_fail, false

    | Constructor(loc, Constr constr, arg) ->
       let ty_name, arg_ty = StringMap.find constr env.constrs in
       let arg, can_fail, transfer =
         typecheck_expected (noloc, "constr-arg") env arg_ty arg in
       if transfer then
         error loc "transfer not allowed in constructor argument";
       let constr_ty, ty_kind = StringMap.find ty_name env.types in
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
                     Apply("Left", loc, [arg; unused right_ty])
                   else
                     let arg = iter constrs in
                     Apply("Right", loc, [arg; unused left_ty])
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
       let desc = Apply("Left",loc,[arg; unused right_ty]) in
       mk desc ty can_fail, can_fail, false

    | Constructor(loc, Source (from_ty, to_ty), _arg) ->
       let ty = Tcontract(from_ty, to_ty) in
       let desc = Apply("Source",loc,[unused from_ty; unused to_ty]) in
       mk desc ty false, false, false

    | Constructor(loc, Right left_ty, arg) ->
       let arg, can_fail, transfer = typecheck env arg in
       if transfer then
         error loc "transfer not allowed in constructor argument";
       let ty = Tor(left_ty, arg.ty) in
       let desc = Apply("Right",loc,[arg; unused left_ty]) in
       mk desc ty can_fail, can_fail, false

    | MatchVariant (arg, loc,
                    [ "Left", [left_arg], left_exp;
                      "Right", [right_arg], right_exp;
                   ]) ->
       let arg, can_fail, transfer1 = typecheck env arg in
       let (left_ty, right_ty) = match arg.ty with
         | Tor(left_ty, right_ty) -> (left_ty, right_ty)
         | _ ->
            error loc (Printf.sprintf
                         "not a Left-Right variant type: %s"
                         (LiquidPrinter.Michelson.string_of_type arg.ty))
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
                let constr_ty, ty_kind = StringMap.find ty_name env.types in
                match ty_kind with
                | Type_variant constrs -> (ty_name, constrs)
                | Type_record _ -> raise Not_found
              end
           | _ -> raise Not_found
         with Not_found ->
           error loc (Printf.sprintf
                        "not a variant type: %s"
                        (LiquidPrinter.Michelson.string_of_type arg.ty))
       in
       let env = maybe_reset_vars env transfer1 in
       let match_can_fail = ref can_fail in
       let expected_type = ref None in
       let has_transfer = ref transfer1 in
       let present_constrs = ref StringMap.empty in
       List.iter (fun (constr, vars, e) ->
           let ty_name', var_ty =
             try
               StringMap.find constr env.constrs
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
                typecheck_expected
                  (noloc, "constr-match") env expected_type e
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
           check_used name loc count;
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

  and typecheck_prim1 prim loc args =
    match prim, args with
    | ("get" | "Array.get"), [ { ty = Ttuple tuple };
                               { desc = Const (_, (CInt n | CNat n)) }] ->
       let n = int_of_string n in
       let size = List.length tuple in
       let prim =
         if size <= n then
           error loc "get outside tuple"
         else
           if size = n + 1 then
             "get_last"
           else
             "get"
       in
       let ty = List.nth tuple n in
       prim, ty
    | ("set"| "Array.set"), [ { ty = Ttuple tuple };
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
             "set_last"
           else
             "set"
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
         | "Coll.update", [ _; _; { ty = Tset _ }] -> "Set.update"
         | "Coll.update", [ _; _; { ty = Tmap _ }] -> "Map.update"
         | "Coll.mem", [ _; { ty = Tset _ } ] -> "Set.mem"
         | "Coll.mem", [ _; { ty = Tmap _ } ] -> "Map.mem"
         | "Coll.find", [ _; { ty = Tmap _ } ] -> "Map.find"
         | "Coll.map", [ _; { ty = Tlist _ } ] -> "List.map"
         | "Coll.reduce", [_; { ty = Tlist _ }; _ ] -> "List.reduce"
         | "Coll.reduce", [_; { ty = Tset _ }; _ ] -> "Set.reduce"
         | "Coll.reduce", [_; { ty = Tmap _ }; _ ] -> "Map.reduce"
         | "Coll.size", [{ ty = Tlist _ } ] -> "List.size"
         | "Coll.size", [{ ty = Tset _ } ] -> "Set.size"
         | "Coll.size", [{ ty = Tmap _ } ] -> "Map.size"
         | "Coll.map", [_; { ty = Tmap _ } ] -> "Map.map"
         | _ -> prim
       in
       prim, typecheck_prim2 prim loc args

  and typecheck_prim2 prim loc args =
    match prim, args with
    | ( "<>" | "<" | ">" | "=" | "<=" | ">=" ),
      [ { ty = ty1 }; { ty = ty2 } ] ->
       if comparable_ty ty1 ty2 then Tbool
       else error_not_comparable loc prim ty1 ty2
    | "compare",
      [ { ty = ty1 }; { ty = ty2 } ] ->
       if comparable_ty ty1 ty2 then Tint
       else error_not_comparable loc prim ty1 ty2
    | ("+"|"-"|"*"),
      (  [ { ty = Ttez }; { ty = (Ttez | Tint | Tnat) } ]
         | [ { ty = (Tint | Tnat) }; { ty = Ttez } ])
      -> Ttez
    | ("+"|"*"), [ { ty = Tnat }; { ty = Tnat } ] -> Tnat
    | ("+"|"-"|"*"), [ { ty = (Tint|Tnat) };
                       { ty = (Tint|Tnat) } ] -> Tint
    | "+", [ { ty = Ttimestamp }; { ty = Tint|Tnat } ] -> Ttimestamp
    | "+", [ { ty = Tint|Tnat }; { ty = Ttimestamp } ] -> Ttimestamp

    (* TODO: improve types of ediv in Michelson ! *)
    | "/", [ { ty = Tnat }; { ty = Tnat } ] ->
       Toption (Ttuple [Tnat; Tnat])
    | "/", [ { ty = Tint|Tnat }; { ty = Tint|Tnat } ] ->
       Toption (Ttuple [Tint; Tnat])
    | "/", [ { ty = Ttez }; { ty = Tint } ] ->
       Toption (Ttuple [Ttez; Ttez])

    | "xor", [ { ty = Tbool }; { ty = Tbool } ] -> Tbool
    | "or", [ { ty = Tbool }; { ty = Tbool } ] -> Tbool
    | "&", [ { ty = Tbool }; { ty = Tbool } ] -> Tbool
    | "not", [ { ty = Tbool } ] -> Tbool

    | "xor", [ { ty = Tnat }; { ty = Tnat } ] -> Tnat
    | "xor", [ { ty = Tint|Tnat }; { ty = Tint|Tnat } ] -> Tint
    | "or", [ { ty = Tnat }; { ty = Tnat } ] -> Tnat
    | "or", [ { ty = Tint|Tnat }; { ty = Tint|Tnat } ] -> Tint
    | "&", [ { ty = Tnat }; { ty = Tnat } ] -> Tnat
    | "&", [ { ty = Tint|Tnat }; { ty = Tint|Tnat } ] -> Tint
    | "not", [ { ty = Tint|Tnat } ] -> Tint

    | "abs", [ { ty = Tint } ] -> Tnat
    | "int", [ { ty = Tnat } ] -> Tint
    | "-", [ { ty = Tint|Tnat } ] -> Tint

    | (">>"|"<<"), [ { ty = Tnat} ; { ty = Tnat } ] -> Tnat



    | "tuple", args -> Ttuple (List.map (fun e -> e.ty) args)

    | "Map.find", [ { ty = key_ty }; { ty = Tmap (expected_key_ty, value_ty) }]
      ->
       if expected_key_ty <> key_ty then
         error loc "bad Map.find key type";
       Toption value_ty
    | "Map.update", [ { ty = key_ty };
                      { ty = Toption value_ty };
                      { ty = Tmap (expected_key_ty, expected_value_ty) }]
      ->
       if expected_key_ty <> key_ty then
         error loc "bad Map.add key type";
       if expected_value_ty <> value_ty then
         error loc "bad Map.add value type";
       Tmap (key_ty, value_ty)
    | "Map.mem", [ { ty = key_ty }; { ty = Tmap (expected_key_ty,_) }]
      ->
       if expected_key_ty <> key_ty then
         error loc "bad Mem.mem key type";
       Tbool

    | "Set.mem", [ { ty = key_ty }; { ty = Tset expected_key_ty }]
      ->
       if expected_key_ty <> key_ty then
         error loc "bad Set.mem key type";
       Tbool

    | "List.size", [ { ty = Tlist _ }]  ->  Tnat
    | "Set.size", [ { ty = Tset _ }]  ->  Tnat
    | "Map.size", [ { ty = Tmap _ }]  ->  Tnat

    | "Set.update", [ { ty = key_ty }; { ty = Tbool };
                      { ty = Tset expected_key_ty }]
      ->
       if expected_key_ty <> key_ty then
         error loc "bad Set.update key type";
       Tset key_ty


    | "Some", [ { ty } ] -> Toption ty
    | "Current.fail", [ { ty = Tunit } ] -> Tfail
    | "Current.contract", [ { ty = Tunit } ] ->
       Tcontract (contract.parameter, contract.return)
    | "Current.time", [ { ty = Tunit } ] -> Ttimestamp
    | "Current.balance", [ { ty = Tunit } ] -> Ttez
    (*    | "Current.source", [ { ty = Tunit } ] -> ... *)
    | "Current.amount", [ { ty = Tunit } ] -> Ttez
    | "Current.gas", [ { ty = Tunit } ] -> Tnat
    | "Crypto.hash", [ _ ] -> Tstring
    | "Crypto.check", [ { ty = Tkey };
                        { ty = Ttuple [Tsignature; Tstring] } ] ->
       Tbool
    | "Contract.manager", [ { ty = Tcontract(_,_) } ] ->
       Tkey
    | "Account.create", [ { ty = Tkey }; { ty = Toption Tkey };
                          { ty = Tbool }; { ty = Ttez } ] ->
       Tcontract (Tunit, Tunit)
    | "Account.default", [ { ty = Tkey } ] ->
       Tcontract (Tunit, Tunit)
    | "Contract.create", [ { ty = Tkey }; (* manager *)
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
    | ("Lambda.pipe" | "|>"), [ { ty }; { ty = Tlambda(from_ty, to_ty) }] ->
       if ty <> from_ty then
         error loc "Bad argument type in Lambda.exec";
       to_ty

    | "List.map", [
        { ty = Tlambda (from_ty, to_ty) };
        { ty = Tlist ty } ] ->
       if ty <> from_ty then
         error loc "Bad argument type in List.map";
       Tlist to_ty

    | "List.reduce", [
        { ty = Tlambda (Ttuple [src_ty; dst_ty], dst_ty') };
        { ty = Tlist src_ty' };
        { ty = acc_ty };
      ] ->
       if src_ty <> src_ty' then
         error loc "Bad argument source type in List.reduce";
       if dst_ty <> dst_ty' then
         error loc "Bad function type in List.reduce";
       if acc_ty <> dst_ty' then
         error loc "Bad accumulator type in List.reduce";
       acc_ty

    | "Set.reduce", [
        { ty = Tlambda (Ttuple [src_ty; dst_ty], dst_ty') };
        { ty = Tset src_ty' };
        { ty = acc_ty };
      ] ->
       if src_ty <> src_ty' then
         error loc "Bad argument source type in Set.reduce";
       if dst_ty <> dst_ty' then
         error loc "Bad function type in Set.reduce";
       if acc_ty <> dst_ty' then
         error loc "Bad accumulator type in Set.reduce";
       acc_ty

    | "Map.reduce", [
        { ty = Tlambda (Ttuple [Ttuple [key_ty; src_ty]; dst_ty], dst_ty') };
        { ty = Tmap (key_ty', src_ty') };
        { ty = acc_ty };
      ] ->
       if src_ty <> src_ty' then
         error loc "Bad argument source type in Map.reduce";
       if dst_ty <> dst_ty' then
         error loc "Bad function type in Map.reduce";
       if acc_ty <> dst_ty' then
         error loc "Bad accumulator type in Map.reduce";
       if key_ty <> key_ty' then
         error loc "Bad function key type in Map.reduce";
       acc_ty

    | "Map.map", [
        { ty = Tlambda (Ttuple [from_key_ty; from_value_ty], to_value_ty) };
        { ty = Tmap (key_ty, value_ty) } ] ->
       if from_key_ty <> key_ty then
         error loc "Bad function key type in Map.map";
       if from_value_ty <> value_ty then
         error loc "Bad function value type in Map.map";
       Tmap (key_ty, to_value_ty)


    | "@", [ { ty = Tstring }; { ty = Tstring }] -> Tstring
    | "::", [ { ty = head_ty }; { ty = Tunit } ] ->
       Tlist head_ty
    | "::", [ { ty = head_ty }; { ty = Tlist tail_ty } ] ->
       if head_ty <> tail_ty then
         error loc "Bad types for list";
       Tlist tail_ty
    | _ ->
       Printf.eprintf "Bad args for primitive %S:\n" prim;
       error loc "prim"

  and typecheck_expected (loc,info) env expected_ty exp =
    let exp, fail, transfer = typecheck env exp in
    if exp.ty <> expected_ty && exp.ty <> Tfail then begin
        Printf.eprintf "typecheck: actual type differs from expected type:\n%!";
        eprint_2types exp.ty expected_ty;
        error loc info
      end;
    exp, fail, transfer


  in
  counter := 0;
  (* "storage/1" *)
  let (_ , env, _) = new_binding env  "storage" contract.storage in
  (* "parameter/2" *)
  let (_, env, _) = new_binding env "parameter" contract.parameter in

  let expected_ty = Ttuple [ contract.return; contract.storage ] in

  let code, _can_fail, _transfer =
    typecheck_expected (noloc,"final value") env expected_ty contract.code in
  { contract with code }, !to_inline
