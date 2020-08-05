open Tezos_protocol.Protocol
open LiquidTypes
open Love_pervasives
open Log

let rec tfail_to_tvar ?loc ty = match ty with
  | Ttuple tyl -> Ttuple (List.map (tfail_to_tvar ?loc) tyl)
  | Toption ty -> Toption (tfail_to_tvar ?loc ty)
  | Tlist ty -> Tlist (tfail_to_tvar ?loc ty)
  | Tset ty -> Tset (tfail_to_tvar ?loc ty)
  | Tmap (ty1, ty2) -> Tmap (tfail_to_tvar ?loc ty1, tfail_to_tvar ?loc ty2)
  | Tbigmap (ty1, ty2) -> Tbigmap (tfail_to_tvar ?loc ty1, tfail_to_tvar ?loc ty2)
  | Tor (ty1, ty2) -> Tor (tfail_to_tvar ?loc ty1, tfail_to_tvar ?loc ty2)
  | Tlambda (ty1, ty2, u) -> Tlambda (tfail_to_tvar ?loc ty1, tfail_to_tvar ?loc ty2, u)
  | Tclosure ((ty1, ty2), ty3, u) ->
    Tclosure ((tfail_to_tvar ?loc ty1, tfail_to_tvar ?loc ty2),
              tfail_to_tvar ?loc ty3, u)
  | Trecord (rn, fl) ->
    Trecord (rn, List.map (fun (fn, fty) -> (fn, tfail_to_tvar ?loc fty)) fl)
  | Tsum (sn, cl) ->
    Tsum (sn, List.map (fun (cn, cty) -> (cn, tfail_to_tvar ?loc cty)) cl)
  | Tcontract (s, c) -> Tcontract (s, tfail_to_tvar ?loc c)
  | Tvar { contents = { contents = { tyo = Some ty; id }}} ->
    debug "[Preprocess.tfail_to_tvar] TVar %s aliased" id;
    tfail_to_tvar ?loc ty
  | Tvar {contents = {contents = {tyo = None; id}}} ->
    (* Remaining vars correspond to unused arguments *)
    (* unify (match loc with None -> noloc | Some loc -> loc)
     *   ty Tunit; *)
    debug "[Preprocess.tfail_to_tvar] Transforming %s to unit" id;
    Tunit
  | Tpartial _ -> ty
  | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes | Ttimestamp | Tkey
  | Tkey_hash | Tsignature | Toperation | Taddress | Tchainid -> ty
  | Tfail -> LiquidInfer.fresh_tvar ()

let sig_tfail_to_tvar ?loc (c : LiquidTypes.contract_sig) =
  { c with
    entries_sig =
      List.map (fun es ->
          { es with parameter = tfail_to_tvar ?loc es.parameter }
        ) c.entries_sig
  }


let rec ttfail_to_tvar ({ desc; ty; loc } as e) =
  debug "[Preprocess.ttfail_to_tvar] Preprocessing expression %s@."
        (LiquidPrinter.Liquid.string_of_code e);
  let desc, ty = match desc with
    | Var _ -> desc, tfail_to_tvar ty
    | Let { bnd_var; inline; bnd_val; body } ->
      let body = ttfail_to_tvar body in
      Let { bnd_var;
            inline;
            bnd_val = ttfail_to_tvar bnd_val;
            body}, body.ty
    | SetField { record; field; set_val } ->
      let record = ttfail_to_tvar record in
      SetField { field;
                 record;
                 set_val = ttfail_to_tvar set_val }, record.ty
    | Project { field; record } ->
      let record = ttfail_to_tvar record in
      let proj =
        Project { field; record } in
      let ty =
        match record.ty with
          Trecord (_, flds) ->
          snd (List.find (fun (name, _) -> String.equal name field) flds)
        | _ -> assert false
      in proj, ty
    | Const { ty; const } ->
      let const, ty = const_ttfail_to_tvar loc ty const in
      (Const ({ty; const})), ty

    | Apply { prim; args } ->
      let args = List.map ttfail_to_tvar args in
      Apply { prim; args }, tfail_to_tvar e.ty
    | If { cond; ifthen; ifelse } ->
      let ifthen = ttfail_to_tvar ifthen in let ifelse = ttfail_to_tvar ifelse in
      let () = LiquidInfer.unify loc ifthen.ty ifelse.ty in
      let ty = ifthen.ty in
      If { cond = ttfail_to_tvar cond;
           ifthen;
           ifelse}, ty
    | Seq (e1, e2) ->
      let e2 = ttfail_to_tvar e2 in
      Seq (ttfail_to_tvar e1, e2), e2.ty
    | Transfer { dest; amount } ->
      Transfer { dest = ttfail_to_tvar dest;
                 amount = ttfail_to_tvar amount }, tfail_to_tvar ty
    | Call { contract; amount; entry; arg } ->
      Call { contract = ttfail_to_tvar contract;
             amount = ttfail_to_tvar amount;
             entry;
             arg = ttfail_to_tvar arg }, tfail_to_tvar ty
    | MatchOption { arg; ifnone; some_name; ifsome } ->
      let ifnone = ttfail_to_tvar ifnone in
      let ifsome = ttfail_to_tvar ifsome in
      let () = LiquidInfer.unify loc ifsome.ty ifnone.ty in
      MatchOption { arg = ttfail_to_tvar arg;
                    ifnone;
                    some_name;
                    ifsome }, ifnone.ty
    | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
      let ifnil = ttfail_to_tvar ifnil in
      let ifcons = ttfail_to_tvar ifcons in
      MatchList { arg = ttfail_to_tvar arg;
                  head_name; tail_name;
                  ifcons;
                  ifnil }, ifnil.ty
    | Loop { arg_name; body; arg } ->
      let body = ttfail_to_tvar body in
      Loop { arg_name;
             body;
             arg = ttfail_to_tvar arg }, body.ty
    (* TODO : unify argument and body argument ? *)
    | LoopLeft { arg_name; body; arg; acc } ->
      let body = ttfail_to_tvar body in
      LoopLeft { arg_name;
                 body;
                 arg = ttfail_to_tvar arg;
                 acc = match acc with
                   | None -> None
                   | Some acc ->
                     let acc = ttfail_to_tvar acc in
                     Some acc }, body.ty
    (* TODO : unify argument and body argument ? *)
    | Fold { prim; arg_name; body; arg; acc } ->
      let body = ttfail_to_tvar body in
      let acc = ttfail_to_tvar acc in
      LiquidInfer.unify loc acc.ty body.ty;
      Fold { prim; arg_name;
             body;
             arg = ttfail_to_tvar arg;
             acc }, acc.ty
    | Map { prim; arg_name; body; arg } ->
      Map { prim; arg_name;
            body = ttfail_to_tvar body;
            arg = ttfail_to_tvar arg }, ty
    | MapFold { prim; arg_name; body; arg; acc } ->
      MapFold { prim; arg_name;
                body = ttfail_to_tvar body;
                arg = ttfail_to_tvar arg;
                acc = ttfail_to_tvar acc }, ty (* should not have fails *)
    | Lambda { arg_name; recursive; arg_ty; body; ret_ty } ->
      let u = match body.ty with Tlambda (_,_,u) -> u | _ -> ref @@ ref None in
      let body = ttfail_to_tvar body in
      let arg_ty = tfail_to_tvar ~loc:arg_name.nloc arg_ty in
      let body_ty = tfail_to_tvar ~loc:arg_name.nloc body.ty in
      Lambda { arg_name; recursive;
               arg_ty;
               body;
               ret_ty = body_ty }, Tlambda (arg_ty, body_ty,u)
    | Closure { arg_name; arg_ty; call_env; body; ret_ty } ->
      Closure { arg_name;
                arg_ty = tfail_to_tvar ~loc:arg_name.nloc arg_ty;
                call_env =
                  List.map (fun (v, e) -> (v, ttfail_to_tvar e)) call_env;
                body = ttfail_to_tvar body;
                ret_ty = tfail_to_tvar ~loc:arg_name.nloc ret_ty }, tfail_to_tvar ty
    | Record l ->
      let name = match ty with Trecord (name, _) -> name | _ -> assert false in
      let l, tl = List.map (
          fun (f, e) -> let e' = ttfail_to_tvar e in
            (f, e'), (f, e'.ty)) l |> List.split in
      Record l,
      Trecord (name, tl)

    | Constructor { constr; arg } ->
      let arg = ttfail_to_tvar arg in
      let constr = match constr with
        | Constr _ -> constr
        | Left ty -> Left arg.ty
        | Right ty -> Right arg.ty in
      Constructor { constr; arg }, tfail_to_tvar ty
    | MatchVariant { arg; cases } ->
      let ty = tfail_to_tvar ty in
      MatchVariant { arg = ttfail_to_tvar arg;
                     cases =
                       List.map (fun (p, e) ->
                           let e' = ttfail_to_tvar e in
                           LiquidInfer.unify loc e'.ty ty;
                           (p, e')) cases }, ty
    | MatchNat { plus_name; minus_name; arg; ifplus; ifminus } ->
      let ifplus = ttfail_to_tvar ifplus in
      let ifminus = ttfail_to_tvar ifminus in
      LiquidInfer.unify loc ifplus.ty ifminus.ty;
      MatchNat { plus_name; minus_name;
                 arg = ttfail_to_tvar arg;
                 ifplus;
                 ifminus }, ifplus.ty
    | Failwith e -> Failwith (ttfail_to_tvar e),  LiquidInfer.fresh_tvar ()
    | CreateContract { args; contract } ->
      CreateContract { args = List.map ttfail_to_tvar args;
                       contract = contract_ttfail_to_tvar contract }, tfail_to_tvar ty
    | ContractAt { arg; entry; entry_param } ->
      ContractAt { arg = ttfail_to_tvar arg;
                   entry;
                   entry_param = tfail_to_tvar entry_param }, tfail_to_tvar ty
    | Unpack { arg; ty } ->
      Unpack { arg = ttfail_to_tvar arg;
               ty = tfail_to_tvar ~loc:arg.loc ty }, tfail_to_tvar ty
    | Self _ -> desc, tfail_to_tvar ty
    | SelfCall {amount; entry; arg} ->
      SelfCall {
        amount = ttfail_to_tvar amount;
        entry;
        arg = ttfail_to_tvar arg
      }, tfail_to_tvar ty
    | TypeAnnot _ -> assert false (* Removed during typechecking *)
    | Type _ -> assert false (* Removed during typechecking*)
  in
  { e with desc; ty}

and const_ttfail_to_tvar loc (typ : datatype) c : 'a * datatype =
  debug "[Preprocess.contract_ttfail_to_tvar] Preprocessing constant %s : %s@."
    (LiquidPrinter.Liquid.string_of_const c)
    (LiquidPrinter.Liquid.string_of_type typ);
  match c with
  | CUnit -> c, Tunit
  | CBool _ -> c, Tbool
  | CInt _ -> c, Tint
  | CNat _ -> c, Tnat
  | CTez _ -> c, Ttez
  | CTimestamp _ -> c, Ttimestamp
  | CString _ -> c, Tstring
  | CBytes _ -> c, Tbytes
  | CKey _ -> c, Tkey
  | CContract _ -> c, tfail_to_tvar typ
  | CSignature _ -> c, Tsignature
  | CNone -> c, tfail_to_tvar typ
  | CKey_hash _ -> c, tfail_to_tvar typ
  | CSome x ->
    let xty = match typ with Toption t -> t | _ -> assert false in
    let c, t = const_ttfail_to_tvar loc xty x in CSome c, Toption t
  | CLeft x ->
    let typ = tfail_to_tvar typ in
    let xty = match typ with Tor (t, _) -> t | _ -> assert false in
    let c, t = const_ttfail_to_tvar loc xty x in
    let typ = tfail_to_tvar typ in
    LiquidInfer.unify loc typ (Tor (t, LiquidInfer.fresh_tvar ()));
    CLeft c, typ

  | CRight x ->
    let typ = tfail_to_tvar typ in
    let xty = match typ with Tor (_, t) -> t | _ -> assert false in
    let c, t = const_ttfail_to_tvar loc xty x in
    let typ = tfail_to_tvar typ in
    LiquidInfer.unify loc typ (Tor (LiquidInfer.fresh_tvar (), t));
    CRight c, typ

  | CTuple xs ->
    let arg_tl = match typ with Ttuple tl -> tl | _ -> assert false in
    let cl, tl = List.map2 (const_ttfail_to_tvar loc) arg_tl xs |> List.split in
    let () = List.iter2 (LiquidInfer.unify loc) arg_tl tl in
    CTuple cl, Ttuple tl
  | CList xs ->
      let t = match typ with Tlist t -> t | _ -> assert false in
      let cl =
        List.map
          (fun x -> let c, t' = const_ttfail_to_tvar loc t x in LiquidInfer.unify loc t t'; c)
          xs in
      CList cl, Tlist t

  | CSet xs ->
      let t = match typ with Tset t -> t | _ -> assert false in
      let cl =
        List.map
          (fun x -> let c, t' = const_ttfail_to_tvar loc t x in LiquidInfer.unify loc t t'; c)
          xs in
      CSet cl, Tset t
  | CMap l ->
      let tk, tx = match typ with Tmap (t,t') -> t,t' | _ -> assert false in
      let cl =
        List.map
          (fun (k,x) ->
             let k, tk' = const_ttfail_to_tvar loc tk k in LiquidInfer.unify loc tk tk';
             let x, tx' = const_ttfail_to_tvar loc tx x in LiquidInfer.unify loc tx tx';
             k,x
          )
          l in
      CMap cl, Tmap (tk, tx)
  | CBigMap bm -> begin
      match bm with
        BMList l ->
        let tk, tx = match typ with Tbigmap (t,t') -> t,t' | _ -> assert false in
        let cl =
          List.map
            (fun (k,x) ->
               let k, tk' = const_ttfail_to_tvar loc tk k in LiquidInfer.unify loc tk tk';
               let x, tx' = const_ttfail_to_tvar loc tx x in LiquidInfer.unify loc tx tx';
               k,x
            )
            l in
        CBigMap (BMList cl), Tbigmap (tk, tx)
      | BMId i ->
        CBigMap (BMId i), typ
    end
  | CRecord _labels -> c, typ (* todo *)
  | CConstr (constr, x) ->  c, typ (* todo *)
  | CLambda { arg_name; arg_ty; body; ret_ty; recursive } -> c, typ (* todo *)

and contract_ttfail_to_tvar (contract : typed_contract) =
  debug "[Preprocess.contract_ttfail_to_tvar] Preprocessing contract %s@." contract.contract_name;
  let subs = List.map contract_ttfail_to_tvar contract.subs in
  let values = List.map (fun v ->
      debug "[Preprocess.contract_ttfail_to_tvar] Preprocessing value %s@." v.val_name;
      { v with val_exp = ttfail_to_tvar v.val_exp }
    ) contract.values in
  let c_init = match contract.c_init with
    | None -> None
    | Some { init_name; init_args; init_body } ->
      debug "[Preprocess.contract_ttfail_to_tvar] Preprocessing init %s@." init_name;
      Some { init_name;
             init_args = List.map (fun (x, loc, ty) ->
                 debug "[Preprocess.contract_ttfail_to_tvar] Preprocessing arg %s : %s@."
                   x (LiquidPrinter.Liquid.string_of_type ty);
                 x, loc, tfail_to_tvar ~loc ty) init_args;
             init_body = (
               debug "[Preprocess.contract_ttfail_to_tvar] Preprocessing body of %s@." init_name;
               ttfail_to_tvar init_body
             )} in
  let entries = List.map (fun { entry_sig; code; view; _ } ->
      debug "[Preprocess.contract_ttfail_to_tvar] Preprocessing entry %s@." entry_sig.entry_name;
      { entry_sig = { entry_sig with
                      parameter =
                        tfail_to_tvar ~loc:(code : typed_exp).loc
                          entry_sig.parameter };
        code = ttfail_to_tvar code;
        fee_code = None; (* TODO *)
        view;
      }) contract.entries in
  let rec env_ttfail_to_tvar ty_env = {
    ty_env with
    contract_types = StringMap.map sig_tfail_to_tvar ty_env.contract_types;
    top_env = match ty_env.top_env with
      | None -> None
      | Some env -> Some (env_ttfail_to_tvar env);
  } in
  debug "[Preprocess.contract_ttfail_to_tvar] Preprocessing done, removing tfails from env@.";
  let ty_env = env_ttfail_to_tvar contract.ty_env in
  { contract with values; c_init; entries; ty_env ; subs }
