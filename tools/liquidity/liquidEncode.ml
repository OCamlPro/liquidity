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

open LiquidTypes
open LiquidNamespace
open LiquidPrinter.Liquid

(* Elements traversed by uncurrying transformation *)
type uncurry_elt =
  | ULet of { bnd_var: loc_name;
              inline: inline;
              bnd_val: typed_exp; }
  | UArg of int * loc_name * datatype

let noloc env = LiquidLoc.loc_in_file env.env.filename

let error loc msg =
  LiquidLoc.raise_error ~loc ("Encoding error: " ^^ msg ^^ "%!")

let mk_typed ?name ~loc (desc: (datatype, typed) exp_desc) ty =
  mk ?name ~loc desc ty
let mk ?name ~loc (desc: (datatype, encoded) exp_desc) ty =
  let name = match name with
    | Some n when n.[0] <> '_' -> name
    | _ -> None
  in
  mk ?name ~loc desc ty

let mk_typed_nat ~loc i =
  mk_typed ~loc
    (Const { ty = Tnat; const = CNat (LiquidNumber.integer_of_int i) })
    Tnat

let mk_nat ~loc i =
  mk ~loc
    (Const { ty = Tnat; const = CNat (LiquidNumber.integer_of_int i) })
    Tnat

let mk_nil ~loc list_ty =
  mk ~loc (Const { ty = list_ty; const = CList [] }) list_ty

let mk_typed_nil ~loc list_ty =
  mk_typed ~loc (Const { ty = list_ty; const = CList [] }) list_ty

let mk_tuple loc args =
  let tuple_ty = Ttuple (List.map (fun t -> t.ty) args) in
  mk ~loc (Apply { prim = Prim_tuple; args }) tuple_ty

let const_unit ~loc = mk ~loc (Const { ty = Tunit; const = CUnit }) Tunit

let const_true ~loc = mk ~loc (Const { ty = Tbool; const = CBool true }) Tbool

let const_false ~loc = mk ~loc (Const {ty = Tbool; const = CBool false }) Tbool

let unused env ~loc ?constr ty =
  mk ~loc (Apply { prim = Prim_unused constr; args = [const_unit ~loc] }) ty

let uniq_ident env name =
  env.counter := !(env.counter) + 1;
  Printf.sprintf "%s/%d" name !(env.counter)

let copy_env env =
  { env with
    counter = ref !(env.counter);
    to_inline = ref !(env.to_inline);
    vars_counts = StringMap.map (fun c -> ref !c) env.vars_counts;
  }

(* let fresh_tmp = *)
(*   let cpt = ref 0 in *)
(*   fun () -> *)
(*     incr cpt; *)
(*     Printf.sprintf "tmp#%d" !cpt *)


(* Create a new binding in the typechecking environment to uniquely
   rename variable for alpha-renaming (prevents capture). *)
let new_binding env name ?effect ty =
  let effect = match effect with
    | Some e -> e
    | None -> LiquidTypes.may_contain_arrow_type ty in
  let new_name = uniq_ident env name in
  let count = ref 0 in
  let tys = (StringSet.empty, ty) in
  let env = { env with
              vars = StringMap.add name (new_name, tys, effect) env.vars;
              vars_counts = StringMap.add new_name count env.vars_counts;
            } in
  (new_name, env, count)

let find_in_clos ~count_used loc ce name =
  let v, (cpt_in, cpt_out) = StringMap.find name ce.env_bindings in
  if count_used then begin
    incr cpt_in;
    incr cpt_out;
  end;
  v

(* Find variable name in either the global environment or the closure
   environment, returns a corresponding expression *)
let find_var ?(count_used=true) env loc name =
  try
    let vname = name in
    let (name, (_, ty), effect) =
      StringMap.find (unalias_name name env.env) env.vars in
    let count = StringMap.find name env.vars_counts in
    if count_used then incr count;
    let aname =
      if env.annot then Some vname
      else None in
    let exp = mk ?name:aname ~loc (Var name) ty in
    { exp with effect }
  with Not_found ->
  match env.clos_env with
  | None -> error loc "unbound variable %S" name
  | Some ce ->
    try find_in_clos ~count_used loc ce name
    with Not_found ->
      error loc "unbound variable %S" name

(* Create environment for closure *)
let env_for_clos env bvs arg_name arg_type =
  (* Look for free variables that are bound outside the lambda,
     remember their type and give them index (position) in the future
     call environment *)
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
  | [] ->
    (* If there are no free variables we don't need to build a closure *)
    let (new_name, env, _) = new_binding env arg_name.nname arg_type in
    env, { arg_name with nname = new_name }, arg_type, []
  | _ ->
    (* Build a closure environment and change lambda argument from arg to
       a tuple (arg, (x1, x2, x3, ...)) *)
    let loc = arg_name.nloc in
    let env_arg_name =
      uniq_ident env
        (String.concat "_" @@
         arg_name.nname :: "" :: List.map fst free_vars_l) in
    let env_arg_type =
      Ttuple (arg_type ::
              List.map (fun (_, (_, (_, ty), _, _)) -> ty) free_vars_l) in
    let env_arg_var = mk ~loc (Var env_arg_name) env_arg_type in
    (* No effect registered on [env_arg_var] here, will be added to
       individual bindings *)
    let new_name = uniq_ident env arg_name.nname in
    let env_vars =
      StringMap.add arg_name.nname
        (new_name, (StringSet.empty, arg_type), 0, (ref 0, ref 0)) free_vars in
    let env_bindings =
      StringMap.mapi (fun out_name (name, (_, ty), index, count) ->
          (* [name.(index)] has effect if variable outside closure has some *)
          let effect =
            if index = 0 then false
            else (find_var ~count_used:false ext_env loc out_name).effect in
          let ei = mk_nat ~loc index in
          let exp = { (mk ~name ~loc
              (Apply { prim = Prim_tuple_get; args = [env_arg_var; ei] })
              ty) with effect } in
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
    env, { arg_name with nname = env_arg_name}, env_arg_type, call_bindings

(* Encode a type. The only change perfomed here is to encode contract
   signatures to single entry form *)
let rec encode_type ?(decompiling=false) ty =
  (* if env.only_typecheck then ty *)
  (* else *)
  match ty with
  | Ttez | Tunit | Ttimestamp | Tint | Tnat | Tbool | Tkey | Tkey_hash
  | Tsignature | Tstring | Tbytes | Toperation | Taddress | Tfail | Tchainid -> ty
  | Ttuple tys ->
    let tys' = List.map (encode_type ~decompiling) tys in
    if List.for_all2 (==) tys tys' then ty
    else Ttuple tys'
  | Tset t | Tlist t | Toption t ->
    let t' = encode_type ~decompiling t in
    if t' == t then ty
    else begin match ty with
      | Tset t -> Tset t'
      | Tlist t -> Tlist t'
      | Toption t -> Toption t'
      | _ -> assert false
    end
  | Tor (t1, t2) | Tbigmap (t1, t2) | Tmap (t1, t2) ->
    let t1', t2' = encode_type ~decompiling t1, encode_type ~decompiling t2 in
    if t1 == t1' && t2 == t2' then ty
    else begin match ty with
      | Tor (t1, t2) -> Tor (t1', t2')
      | Tmap (t1, t2) -> Tmap (t1', t2')
      | Tbigmap (t1, t2) -> Tbigmap (t1', t2')
      | _ -> assert false
    end
  | Tlambda (t1, t2, u) when !(!u) = Some true ->
    get_lambda_type ~decompiling [] ty
  | Tlambda (t1, t2, u) ->
    let t1', t2' = encode_type ~decompiling t1, encode_type ~decompiling t2 in
    if t1 == t1' && t2 == t2' then ty
    else Tlambda (t1', t2', u)
  | Tclosure  ((t1, t2), t3, u) ->
    let t1' = encode_type ~decompiling t1 in
    let t2' = encode_type ~decompiling t2 in
    let t3' = encode_type ~decompiling t3 in
    if t1 == t1' && t2 == t2' && t3 == t3' then ty
    else Tclosure ((t1', t2'), t3', u)
  | Trecord (name, labels) ->
    Trecord (name, List.map (fun (l, ty) -> l, encode_type ~decompiling ty) labels)
  | Tsum (name, cstys) ->
    Tsum (name, List.map (fun (c, ty) -> c, encode_type ~decompiling ty) cstys)
  | Tcontract (entry, param) ->
    Tcontract (entry, encode_type ~decompiling param)
  | Tvar _ | Tpartial _ ->
    (* Removed during typechecking (if monomorphized)  *)
    ty

and encode_qual_type env ty =
  encode_type ~decompiling:env.decompiling (normalize_type ~in_env:env.env ty)

(* encode a contract signature to the corresponding single entry form
   sum type *)
and encode_contract_sig csig =
  match csig.entries_sig with
  | [] -> Tunit
    (* error (LiquidLoc.noloc)
     *   "Contract type %shas no entry points"
     *   (match csig.sig_name with
     *    | Some s -> Printf.sprintf "%s " s
     *    | None -> "") *)
  | [{ parameter }] -> parameter
  | entries ->
    Tsum (None,
          List.map (fun { entry_name; parameter = t } ->
              (entry_name, t)
            ) entries)

and get_lambda_type ~decompiling args = function
  | Tlambda (t1, t2, u) when !(!u) = Some true ->
    let t1 = encode_type ~decompiling t1 in
    get_lambda_type ~decompiling (t1 :: args) t2
  | t2 ->
    let t2 = encode_type ~decompiling t2 in
    let t1 = match args with
      | [t1] -> t1
      | _ -> Ttuple (List.rev args) in
    Tlambda (t1, t2, default_uncurry ())

let rec allowed_type
    ?(allow_big_map=true)
    ?(allow_operation=true)
    ?(allow_contract=true) = function
  | Ttez | Tunit | Ttimestamp | Tint | Tnat | Tbool | Tkey | Tkey_hash
  | Tsignature | Tstring | Taddress | Tbytes | Tchainid -> true
  | Toperation -> allow_operation
  | Tfail -> false
  | Ttuple tys ->
    List.for_all
      (allowed_type ~allow_big_map ~allow_operation ~allow_contract)
      tys
  | Tset t ->
    comparable_type t &&
    allowed_type ~allow_big_map ~allow_operation ~allow_contract t
  | Tlist t | Toption t ->
    allowed_type ~allow_big_map ~allow_operation ~allow_contract t
  | Tcontract (_, t) ->
    allow_contract &&
    allowed_type ~allow_big_map ~allow_operation:false ~allow_contract:true t
  | Tor (t1, t2) ->
    allowed_type ~allow_big_map ~allow_operation ~allow_contract t1 &&
    allowed_type ~allow_big_map ~allow_operation ~allow_contract t2
  | Tlambda (t1, t2, _) ->
    allowed_type t1 &&
    allowed_type t2
  | Tmap (t1, t2) ->
    comparable_type t1 &&
    allowed_type ~allow_big_map ~allow_operation ~allow_contract t1 &&
    allowed_type ~allow_big_map ~allow_operation ~allow_contract t2
  | Tclosure  ((t1, t2), t3, _) ->
    allowed_type t1 &&
    allowed_type t2 &&
    allowed_type t3
  | Trecord (_, ltys)
  | Tsum (_, ltys) ->
    List.for_all (fun (_, ty) ->
        allowed_type ~allow_big_map ~allow_operation ~allow_contract ty) ltys
  | Tbigmap (t1, t2) ->
    allow_big_map &&
    comparable_type t1 &&
    allowed_type ~allow_big_map:false ~allow_operation ~allow_contract t1 &&
    allowed_type
      ~allow_big_map:false ~allow_operation:false ~allow_contract:false t2
  | Tvar _ | Tpartial _ ->
    (* Removed during typechecking (if monomorphized) *)
    true

let check_allowed_type loc ?allow_operation ?allow_contract ty =
  if not @@ allowed_type ?allow_operation ?allow_contract ty then
    error loc
      "The following type is not allowed: %s"
      (string_of_type ty)

(* Encode storage type. This checks that big maps appear only as the
   first component of the toplevel tuple or record storage. *)
let encode_storage_type env ty =
  let ty = encode_qual_type env ty in
  check_allowed_type (noloc env) ~allow_operation:false ~allow_contract:false ty;
  ty

(* Encode parameter type. Parameter cannot have big maps. *)
let encode_parameter_type env ty =
  let ty = encode_qual_type env ty in
  check_allowed_type (noloc env) ~allow_operation:false ~allow_contract:true ty;
  ty


(* Unfortunately, operations and big maps are not allowed in Michelson
   constants, so when they appear in one, we have to turn them to non
   constant expressions. For instance (Set [op]) is turned into
   Set.add op (Set : operation set) *)
let rec deconstify env loc ty c =
  if env.decompiling || not @@ forbidden_constant_ty ty then
    mk ~loc (Const { ty; const = c }) ty
  else match c, (encode_qual_type env ty) with
    | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
      | CBytes _ | CKey _ | CSignature _ | CNone
      | CKey_hash _ | CContract _ ),
      _ ->
      mk ~loc (Const { ty; const = c }) ty

    | CSome c, Toption ty' ->
      mk ~loc (Apply { prim = Prim_Some; args = [deconstify env loc ty' c] }) ty

    | CLeft c, Tor (ty', _) ->
      mk ~loc (Apply { prim = Prim_Left; args = [deconstify env loc ty' c] }) ty
    | CRight c, Tor (_, ty') ->
      mk ~loc (Apply { prim = Prim_Right; args = [deconstify env loc ty' c] }) ty

    | CTuple cs, Ttuple tys ->
      mk ~loc (Apply { prim = Prim_tuple;
                       args = List.map2 (deconstify env loc) tys cs }) ty

    | CTuple cs, Trecord (_, ltys) ->
      mk ~loc (Apply { prim = Prim_tuple;
                       args = List.map2 (fun (_, ty) ->
                           deconstify env loc ty) ltys cs }) ty

    | CList cs, Tlist ty' ->
      List.fold_right (fun c acc ->
          mk ~loc (Apply { prim = Prim_Cons;
                           args = [deconstify env loc ty' c; acc] }) ty
        )
        cs
        (mk ~loc (Const { ty; const = CList [] }) ty)

    | CSet cs, Tset ty' ->
      List.fold_right (fun c acc ->
          mk ~loc (Apply { prim = Prim_set_add;
                           args = [deconstify env loc ty' c; acc] }) ty
        )
        cs
        (mk ~loc (Const { ty; const = CSet [] }) ty)

    | CMap cs, Tmap (tk, te) ->
      List.fold_right (fun (k, e) acc ->
          mk (Apply { prim = Prim_map_add;
                      args = [deconstify env loc tk k; deconstify env loc te e; acc] })
            ~loc ty
        )
        cs
        (mk ~loc (Const { ty; const = CMap [] }) ty)

    | CBigMap (BMList cs), Tbigmap (tk, te) ->
      List.fold_right (fun (k, e) acc ->
          mk (Apply { prim = Prim_map_add;
                      args = [deconstify env loc tk k; deconstify env loc te e; acc] })
            ~loc ty
        )
        cs
        (* (mk ~loc (Const { ty; const = CBigMap [] }) ty) *)
        (mk ~loc (Apply { prim = Prim_big_map_create;
                          (* Ghost unused arguments to carry big map type *)
                          args = [unused env ~loc tk; unused env ~loc te] }) ty)

    | CBigMap (BMId id ), Tbigmap (tk, te) ->
      mk ~loc (Const { ty; const = c }) ty

    (* Removed by encode const *)
    | CRecord _, _
    | CConstr _, _ -> assert false

    | _, _ ->
      error loc "Forbidden type %s in constants" (string_of_type ty)

(* Decrement counters for variables that appear in an expression *)
let rec decr_counts_vars env e =
  if e.effect then () else
    match e.desc with
    | Var name ->
      begin try
          let count = StringMap.find name env.vars_counts in
          decr count
        with Not_found -> ()
      end

    | Const _ | Self _ -> ()

    | Failwith e
    | Project { record = e }
    | Constructor { arg = e }
    | ContractAt { arg = e }
    | Unpack { arg = e }
    | Lambda { body = e } -> decr_counts_vars env e

    | SetField { record = e1; set_val = e2 }
    | Seq (e1, e2)
    | Let { bnd_val = e1; body = e2 }
    | Loop { body = e1; arg = e2 }
    | LoopLeft { body = e1; arg = e2; acc = None }
    | Map { body = e1; arg = e2 }
    | Transfer { dest = e1; amount = e2 }
    | SelfCall { amount = e1; arg = e2 } ->
      decr_counts_vars env e1;
      decr_counts_vars env e2;

    | If { cond = e1 ; ifthen = e2 ; ifelse = e3 }
    | MatchNat { arg = e1; ifplus = e2; ifminus = e3 }
    | MatchList { arg = e1; ifnil = e2; ifcons = e3 }
    | MatchOption { arg = e1; ifnone = e2; ifsome = e3 }
    | Fold { body = e1; arg = e2; acc = e3 }
    | MapFold { body = e1; arg = e2; acc = e3 }
    | LoopLeft { body = e1; arg = e2; acc = Some e3 }
    | Call { contract = e1; amount = e2; arg = e3 } ->
      decr_counts_vars env e1;
      decr_counts_vars env e2;
      decr_counts_vars env e3;

    | Apply { args }
    | CreateContract { args } ->
      List.iter (decr_counts_vars env) args

    | Closure { call_env; body } ->
      decr_counts_vars env body;
      List.iter (fun (_, e) -> decr_counts_vars env e) call_env;

    | Record fields ->
      List.iter (fun (_, e) -> decr_counts_vars env e) fields

    | MatchVariant { arg; cases } ->
      decr_counts_vars env arg;
      List.iter (fun (_, e) -> decr_counts_vars env e) cases

    | TypeAnnot { e } ->
      decr_counts_vars env e

    | Type _ -> ()

let effect_binding env bnd_val = match bnd_val.desc with
  | Lambda _ -> false
  | Closure _ -> false
  | _ -> bnd_val.effect

let register_inlining ~loc env new_name count inline bnd_val =
  match !count with
  | c when c <= 0 ->
    if effect_binding env bnd_val then
      () (* No inling of values with side effects which don't
            appear later on *)
    else begin
      decr_counts_vars env bnd_val;
      env.to_inline :=
        StringMap.add new_name (const_unit ~loc) !(env.to_inline)
    end
  | c when (c = 1 && inline = InAuto) || inline = InForced ->
    env.to_inline := StringMap.add new_name bnd_val !(env.to_inline)
  | _ -> ()

let register_inlining_value env v =
  try
    let count = StringMap.find v.val_name env.vars_counts in
    let loc = (v.val_exp : encoded_exp).loc in
    register_inlining ~loc env v.val_name count v.inline v.val_exp
  with Not_found -> ()

let uncurry_lambda = function
  | Tlambda (_, _, uncurry)
  | Tclosure (_, _, uncurry) ->
    begin match !(!uncurry) with
      | None -> false
      | Some u -> u
    end
  | _ -> false

let record_field_name_in_env env field record =
  let field_pos = match expand record.ty with
    | Trecord (_, ltys) ->
      (let exception Found of int in
       try
         List.iteri (fun i (l, _) -> if field = l then raise (Found i)) ltys;
         assert false
       with Found i -> i)
    | _ -> assert false in
  let record = { record with ty = encode_qual_type env record.ty } in
  let field = match expand record.ty with
    | Trecord (_, ltys) -> List.nth ltys field_pos |> fst
    | _ -> assert false in
  (field, record)

(* Encode a constant: constructors are turned into (netsed) Left/Right
   variant values *)
let rec encode_const env (c : typed_const) : encoded_const = match c with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CSignature _ | CNone  | CKey_hash _
    | CContract _ ) as c -> c

  | CSome x -> CSome (encode_const env x)
  | CLeft x -> CLeft (encode_const env x)
  | CRight x -> CRight (encode_const env x)

  | CTuple xs -> CTuple (List.map (encode_const env) xs)
  | CList xs -> CList (List.map (encode_const env) xs)
  | CSet xs -> CSet (List.map (encode_const env) xs)

  | CMap l ->
    CMap (List.map (fun (x,y) -> encode_const env x, encode_const env y) l)

  | CBigMap BMList l ->
    CBigMap
      (BMList
         (List.map (fun (x,y) -> encode_const env x, encode_const env y) l))

  | CBigMap BMId _ as c -> c

  | CRecord labels when env.decompiling ->
    CRecord (List.map (fun (f, x) -> f, encode_const env x) labels)

  | CRecord labels ->
    CTuple (List.map (fun (_, x) -> encode_const env x) labels)

  | CConstr (constr, x) when env.decompiling ->
    CConstr (constr, encode_const env x)

  | CConstr (constr, x) ->
    begin try
        let constr_ty, (_, _, constr_pos) =
          find_constr ~loc:(noloc env) constr env.env in
        (* This is a new instance of the type but we just look at the
           constructors *)
        let nb_constrs = match constr_ty with
          | Tsum (sum_name, constrs) -> List.length constrs
          | _ -> raise Not_found in
        let rec mk pos nb_constrs c =
          match pos, nb_constrs with
          | 0, 1 -> c
          | 0, _ -> CLeft c
          | pos, _ when pos > 0 -> CRight (mk (pos - 1) (nb_constrs - 1) c)
          | _ -> assert false in
        mk constr_pos nb_constrs (encode_const env x)
      with Not_found ->
        error (noloc env)  "unknown constructor %s" constr
    end

  | CLambda lam ->
    let enc_lam_exp =
      encode env
        (mk_typed ~loc:(noloc env)
           (Lambda lam) (Tlambda (lam.arg_ty, lam.ret_ty, default_uncurry ()))) in
    match enc_lam_exp.desc with
    | Lambda l -> CLambda l
    | Closure _ ->
      error (noloc env) "constant lambdas cannot be closures"
    | _ -> assert false

(* Encode a Liquidity expression *)
and encode env ( exp : typed_exp ) : encoded_exp =
  let loc = exp.loc in
  match exp.desc with

  | Const { ty; const } ->
    let const = encode_const env const in
    (* normalize wrt to top level env *)
    let ty = normalize_type ~in_env:env.env ty in
    (* use functions instead of constants if contains operations or big maps *)
    let c = deconstify env loc ty const in
    mk ?name:exp.name ~loc c.desc ty

  | Let { bnd_var; inline; bnd_val; body } ->
    let bnd_val = encode env bnd_val in
    let bnd_val = if env.annot && bnd_val.name = None
      then { bnd_val with name = Some bnd_var.nname }
      else bnd_val
    in
    let (new_name, env, count) =
      new_binding env bnd_var.nname ~effect:bnd_val.effect bnd_val.ty in
    let body = encode env body in
    register_inlining ~loc env new_name count inline bnd_val;
    mk ?name:exp.name ~loc
      (Let { bnd_var = { bnd_var with nname = new_name };
             inline; bnd_val; body }) body.ty

  | Var name -> find_var env loc name

  | Project { field; record } ->
    let record = encode env record in
    let field, record = record_field_name_in_env env field record in
    mk ?name:exp.name ~loc (Project { field; record }) exp.ty

  | SetField { record; field; set_val } ->
    let record = encode env record in
    let set_val = encode env set_val in
    let field, record = record_field_name_in_env env field record in
    mk ?name:exp.name ~loc (SetField { record; field; set_val }) exp.ty

  | Seq (exp1, exp2) ->
    (* TODO: if not exp.effect then remove exp1 *)
    let exp1 = encode env exp1 in
    let exp2 = encode env exp2 in
    mk ?name:exp.name ~loc (Seq (exp1, exp2)) exp.ty

  | If { cond; ifthen; ifelse } ->
    let cond = encode env cond in
    let ifthen = encode env ifthen in
    let ifelse = encode env ifelse in
    mk ?name:exp.name ~loc (If { cond; ifthen; ifelse }) exp.ty

  | Self { entry } ->
    mk ?name:exp.name ~loc (Self { entry }) exp.ty

  | Transfer { dest; amount } ->
    let dest = encode env dest in
    let amount = encode env amount in
    mk ?name:exp.name ~loc (Transfer { dest; amount }) exp.ty

  | Call { contract; amount; entry; arg } ->
    let amount = encode env amount in
    let contract = encode env contract in
    let arg = encode env arg in
    mk ?name:exp.name ~loc
      (Call { contract; amount; entry; arg }) Toperation

  | SelfCall { amount; entry; arg } ->
    let amount = encode env amount in
    let arg = encode env arg in
    mk ?name:exp.name ~loc
      (SelfCall { amount; entry; arg }) Toperation

  | Failwith arg ->
    let arg = encode env arg in
    mk ~loc (Failwith arg) Tfail

  (* List.rev -> List.reduce (::) *)
  | Apply { prim = Prim_list_rev; args = [l] } ->
    let l = encode env l in
    let elt_ty = match l.ty with
      | Tlist ty -> ty
      | _ -> assert false
    in
    let list_ty = l.ty in
    let arg_name = uniq_ident env "arg" in
    let arg_ty = Ttuple [elt_ty; list_ty] in
    let arg = mk ~loc (Var arg_name) arg_ty in
    let e =
      mk (Apply { prim = Prim_tuple_get; args = [arg; mk_nat ~loc 0] })
        ~loc elt_ty in
    let acc =
      mk (Apply { prim = Prim_tuple_get; args = [arg; mk_nat ~loc 1] })
        ~loc list_ty in
    let f_body =
      mk (Apply { prim = Prim_Cons; args = [e; acc] }) ~loc list_ty in
    let empty_acc = mk_nil ~loc list_ty in
    let desc = Fold { prim = Prim_list_fold;
                      arg_name = { nname = arg_name; nloc = loc };
                      body = f_body;
                      arg = l;
                      acc = empty_acc } in
    mk ?name:exp.name ~loc desc list_ty

  (* concat x y => concat [x; y] *)
  | Apply { prim = Prim_concat_two; args = [x; y] } ->
    let prim = match x.ty with
      | Tstring -> Prim_string_concat
      | Tbytes -> Prim_bytes_concat
      | _ -> assert false in
    let ty = Tlist x.ty in
    let l =
      mk_typed ~loc
        (Apply { prim = Prim_Cons;
                 args = [x; mk_typed ~loc (
                     Apply { prim = Prim_Cons;
                             args = [y; mk_typed_nil ~loc ty] }) ty] }) ty in
    encode env { exp with desc = Apply { prim; args = [l] } }

  | Apply { prim = Prim_big_map_create; args = [ _unit ] } ->
    let k_ty, v_ty = match exp.ty with
      | Tbigmap (k, v) -> k, v
      | _ -> assert false in
    (* Ghost unused arguments to carry big map type *)
    let k = unused env ~loc k_ty in
    let v = unused env ~loc v_ty in
    let desc = Apply { prim = Prim_big_map_create; args = [k; v] } in
    mk ?name:exp.name ~loc desc exp.ty

  | Apply { prim; args } ->
    encode_apply exp.name env prim loc args exp.ty

  | MatchOption { arg; ifnone; some_name; ifsome } ->
    let arg = encode env arg in
    let name_ty = match arg.ty with
      | Toption ty -> ty
      | _ -> assert false
    in
    let ifnone = encode env ifnone in
    let (new_some_name, env, count) =
      new_binding env some_name.nname name_ty in
    let ifsome = encode env ifsome in
    mk ?name:exp.name ~loc
      (MatchOption { arg;
                     ifnone;
                     some_name = { some_name with nname = new_some_name };
                     ifsome }) exp.ty

  | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
    let arg = encode env arg in
    let (new_plus_name, env2, count_p) =
      new_binding env plus_name.nname Tnat in
    let ifplus = encode env2 ifplus in
    let (new_minus_name, env3, count_m) =
      new_binding env minus_name.nname Tnat in
    let ifminus = encode env3 ifminus in
    let plus_name = { plus_name with nname = new_plus_name } in
    let minus_name = { minus_name with nname = new_minus_name } in
    (* check_used env plus_name loc count_p; *)
    (* check_used env minus_name loc count_m; *)
    mk ?name:exp.name ~loc
      (MatchNat { arg; plus_name; ifplus; minus_name; ifminus }) exp.ty

  | Loop { arg_name; body; arg } ->
    let arg = encode env arg in
    let (new_arg_name, env, count) = new_binding env arg_name.nname arg.ty in
    let arg_name = { arg_name with nname = new_arg_name } in
    let body = encode env body in
    (* check_used env name loc count; *)
    mk ?name:exp.name ~loc (Loop { arg_name; body; arg }) exp.ty

  | LoopLeft { arg_name; body; arg; acc = None } ->
    let arg = encode env arg in
    let (new_arg_name, env, count) = new_binding env arg_name.nname arg.ty in
    let arg_name = { arg_name with nname = new_arg_name } in
    let body = encode env body in
    (* check_used env name loc count; *)
    mk ?name:exp.name ~loc
      (LoopLeft { arg_name; body; arg; acc = None }) exp.ty

  | LoopLeft { arg_name; body; arg; acc = Some acc } ->
    let arg = encode env arg in
    let acc = encode env acc in
    let arg_ty = Ttuple [arg.ty; acc.ty] in
    let (new_arg_name, env, count) = new_binding env arg_name.nname arg_ty in
    let arg_name = { arg_name with nname = new_arg_name } in
    let body = encode env body in
    (* check_used env name loc count; *)
    mk ?name:exp.name ~loc
      (LoopLeft { arg_name; body; arg; acc = Some acc }) exp.ty

  | Fold { prim; arg_name; body; arg; acc } ->
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
    let (new_arg_name, env, count) = new_binding env arg_name.nname name_ty in
    let arg_name = { arg_name with nname = new_arg_name } in
    let body = encode env body in
    mk ?name:exp.name ~loc (Fold { prim; arg_name; body; arg; acc }) exp.ty

  | Map { prim; arg_name; body; arg } ->
    let arg = encode env arg in
    let name_ty = match prim, arg.ty with
      | Prim_map_map, Tmap (k_ty, v_ty) -> Ttuple [k_ty; v_ty]
      | Prim_list_map, Tlist elt_ty -> elt_ty
      | _ -> assert false
    in
    let (new_arg_name, env, count) = new_binding env arg_name.nname name_ty in
    let arg_name = { arg_name with nname = new_arg_name } in
    let body = encode env body in
    mk ?name:exp.name ~loc (Map { prim; arg_name; body; arg }) exp.ty

  | MapFold { prim; arg_name; body; arg; acc } ->
    let arg = encode env arg in
    let acc = encode env acc in
    let name_ty = match prim, arg.ty with
      | Prim_map_map_fold, Tmap (k_ty, v_ty) ->
        Ttuple [Ttuple [k_ty; v_ty]; acc.ty]
      | Prim_list_map_fold, Tlist elt_ty ->
        Ttuple [elt_ty; acc.ty]
      | _ -> assert false
    in
    let (new_arg_name, env, count) = new_binding env arg_name.nname name_ty in
    let arg_name = { arg_name with nname = new_arg_name } in
    let body = encode env body in
    mk ?name:exp.name ~loc (MapFold { prim; arg_name; body; arg; acc }) exp.ty

  | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
    let arg = encode env arg in
    let elt_ty = match arg.ty with
      | Tlist ty -> ty
      | _ -> assert false
    in
    let ifnil = encode env ifnil in
    let (new_head_name, env, count) = new_binding env head_name.nname elt_ty in
    let (new_tail_name, env, count) = new_binding env tail_name.nname arg.ty in
    let head_name = { head_name with nname = new_head_name } in
    let tail_name = { head_name with nname = new_tail_name } in
    let ifcons = encode env ifcons in
    (* check_used env head_name loc count; *)
    (* check_used env tail_name loc count; *)
    mk ?name:exp.name ~loc
      (MatchList { arg; head_name; tail_name; ifcons; ifnil })
      exp.ty

  (* Uncurry non partially applied lambdas *)
  | Lambda { arg_name; arg_ty; body; ret_ty; recursive }
    when uncurry_lambda exp.ty && not env.decompiling ->
    (* compute arguments of nested fun x -> fun y -> ..., can be
       interlaced with let constructs (introduced at parsing), so got
       through them as well *)
    let rec all_args acc i body = match body.desc with
      | Lambda { arg_name; arg_ty; body } ->
        all_args (UArg (i, arg_name, arg_ty) :: acc) (i + 1) body
      | Let { bnd_var; inline; bnd_val; body } ->
        all_args (ULet { bnd_var; inline; bnd_val } :: acc) i body
      | _ ->
        (* Done, put back trailing let constructs on top of body *)
        let args, _, body =
          List.fold_left (fun (acc, dropped_lets, body) -> function
            | ULet { bnd_var; bnd_val; inline } when not dropped_lets ->
              let body = mk_typed ~loc:exp.loc
                  (Let { bnd_var; bnd_val; inline; body }) body.ty in
              (acc, dropped_lets, body)
            | a -> (a :: acc, true, body)
          ) ([], false, body) acc in
        (body, args) in
    let body', args = all_args [UArg (0, arg_name, arg_ty)] 1 body in
    let exp = match args with
      | [ UArg _ ] ->
        (* There is only one argument, no uncurrying necessary,
           reset the uncurry flag *)
        { exp with ty = Tlambda (arg_ty, ret_ty, default_uncurry ()) }
      | _ ->
        (* Multiple arguments to lambda:
             fun a0 -> let x = x1 in fun a1 -> let y = y2 in fun a2 -> ...
           Build:
             fun a ->
               let a0 = a.(0) in
               let x = x1 in
               let a1 = a.(1) in
               let y = y2 in
               let a2 = a.(2) in ...*)
        let body = body' in
        let args_ty = List.fold_left (fun acc -> function
            | ULet _ -> acc
            | UArg (_, _, ty) -> ty :: acc
          ) [] args |> List.rev in
        let arg_ty = Ttuple args_ty in
        let name =
          String.concat "_" @@
          List.fold_left (fun acc -> function
              | ULet _ -> acc
              | UArg (_, { nname }, _) -> nname :: acc) [] (List.rev args) in
        let mk = mk_typed ~loc:exp.loc in
        let v_arg = mk (Var name) arg_ty in
        let mk_i i = mk (Const { const = CNat (LiquidNumber.integer_of_int i);
                                 ty = Tnat }) Tnat in
        let body = List.fold_left (fun body -> function
            | ULet { bnd_var; inline; bnd_val } ->
              mk (Let { bnd_var ; bnd_val; inline; body }) body.ty
            | UArg (i, bnd_var, ty) ->
              mk
                (Let { bnd_var ;
                       bnd_val =
                         mk (Apply { prim = Prim_tuple_get;
                                     args = [v_arg; mk_i i]}) ty;
                       inline = InAuto; body })
                  body.ty
          ) body (List.rev args) in
        (* new (typed) term, change type to new one and reset uncurry flag *)
        { exp with
          ty = Tlambda (arg_ty, body.ty, default_uncurry ());
          desc = Lambda { arg_name = { arg_name with nname = name };
                          arg_ty; body; ret_ty = body.ty; recursive } }
    in
    (* transformation is done on typed ast, still need to go through
       the other encoding phases *)
    encode env exp

  | Lambda { arg_name; arg_ty; body; ret_ty; recursive = Some f } ->
    encode_rec_fun env ~loc ?name:exp.name
      f arg_name arg_ty ret_ty exp.ty body

  | Lambda { arg_name; arg_ty; body; recursive = None } ->
    let env_at_lambda = env in
    let lambda_arg_type = arg_ty in
    let lambda_arg_name = arg_name in
    let lambda_body = body in
    let bvs = LiquidBoundVariables.bv exp in
    if StringSet.is_empty bvs then
      (* not a closure, create a real lambda *)
      let env = { env_at_lambda with
                  vars = StringSet.fold (fun bv ->
                      StringMap.add bv (StringMap.find bv env.vars)
                    ) bvs StringMap.empty
                } in
      let (new_arg_name, env, count) =
        new_binding env lambda_arg_name.nname lambda_arg_type in
      let arg_name = { arg_name with nname = new_arg_name } in
      let body = encode env lambda_body in
      (* check_used env lambda_arg_name loc arg_count; *)
      let ty = exp.ty in
      mk ?name:exp.name  ~loc
        (Lambda { arg_name; arg_ty = lambda_arg_type;
                  body; ret_ty = body.ty; recursive = None }) ty
    else
      (* create closure with environment *)
      let env, arg_name, arg_ty, call_env =
        env_for_clos env bvs arg_name arg_ty in
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
        Closure { arg_name; arg_ty; call_env; body; ret_ty = body.ty } in
      let call_env_type = match call_env with
        | [] -> assert false
        | [_, t] -> t.ty
        | _ -> Ttuple (List.map (fun (_, t) -> t.ty) call_env)
      in
      let uncurry = match exp.ty with
        | Tlambda (_, _, uncurry) -> uncurry
        | Tclosure (_, _, uncurry) -> uncurry
        | _ -> assert false in
      let ty = Tclosure ((lambda_arg_type, call_env_type), body.ty, uncurry) in
      mk ?name:exp.name ~loc desc ty

  (* Closures are created by encoding phase *)
  | Closure _ -> assert false

  | Record fields ->
    let ty = encode_qual_type env exp.ty in
    let ltys = match ty with
      | Trecord (_, ltys) -> ltys
      | _ -> assert false in
    let fields = List.map2 (fun (_label, exp) (label, lty) ->
        label, encode env { exp with ty = lty }
      ) fields ltys in
    let desc = Record fields in
    mk ?name:exp.name ~loc desc exp.ty

  | Constructor { constr = Constr c; arg } when env.decompiling ->
    let arg = encode env arg in
    let desc = Constructor { constr = Constr c; arg } in
    mk ?name:exp.name ~loc desc exp.ty

  | Constructor { constr = Constr constr; arg } ->

    let arg = encode env arg in
    let exp =
      match exp.ty with
      | Tsum (sum_name, constrs) ->
        let rec iter constrs orty =
          match constrs, orty with
          | [], _ -> assert false
          | [c, _], orty ->
            (* assert (c = constr); *)
            arg
          | (c, cty) :: constrs, orty ->
            let left_ty, right_ty = match orty with
              | Tor (left_ty, right_ty) -> left_ty, right_ty
              | Tsum (_, [_, left_ty; _, right_ty]) -> left_ty, right_ty
              | Tsum (_, (_, left_ty) :: rcstrs) ->
                left_ty, Tsum (None, rcstrs)
              | _ -> assert false
            in
            let desc =
              if c = constr then
                (* We use an unused argument to carry the type to
                   the code generator *)
                Apply { prim = Prim_Left;
                        args = [arg; unused env ~loc ~constr right_ty] }
              else
                let arg = iter constrs right_ty in
                let u = match constrs with
                  | [_] -> unused env ~loc ~constr left_ty
                  | _ ->
                    (* marker for partially contructed values *)
                    unused env ~loc ~constr:"_" left_ty
                in
                Apply { prim = Prim_Right; args =  [arg; u] }
            in
            mk ~loc desc orty
        in
        iter constrs (encode_qual_type env exp.ty)
      | _ -> assert false
    in
    mk ?name:exp.name ~loc exp.desc exp.ty

  | Constructor { constr = Left right_ty; arg } ->
    let right_ty = normalize_type ~in_env:env.env right_ty in
    let arg = encode env arg in
    let ty = Tor(arg.ty, right_ty) in
    let desc = Apply { prim = Prim_Left;
                       args = [arg; unused env ~loc right_ty] } in
    mk ?name:exp.name ~loc desc ty

  | Constructor { constr = Right left_ty; arg } ->
    let left_ty = normalize_type ~in_env:env.env left_ty in
    let arg = encode env arg in
    let ty = Tor (left_ty, arg.ty) in
    let desc = Apply { prim = Prim_Right;
                       args = [arg; unused env ~loc left_ty] } in
    mk ?name:exp.name ~loc desc ty

  | MatchVariant { arg; cases } ->
    let arg = encode env arg in
    let constrs = match arg.ty with
      | Tsum (_, constrs) -> constrs
      | Tor (left_ty, right_ty) ->
        [ "Left", left_ty; "Right", right_ty]
      | _ -> assert false
    in
    let cases = List.map (fun (pat, e) ->
        let pat, env = match pat with
          | PAny | PConstr (_, []) -> pat, env
          | PConstr (c, [var]) ->
            let var_ty = List.assoc c constrs in
            let var_ty = normalize_type ~in_env:env.env var_ty in
            let (var, env, _) = new_binding env var var_ty in
            PConstr (c, [var]), env
          | PConstr _ -> assert false in
        (pat, encode env e)
      ) cases
    in
    mk ?name:exp.name ~loc (MatchVariant { arg; cases }) exp.ty

  | ContractAt { arg; entry; entry_param } ->
    let arg = encode env arg in
    mk ?name:exp.name ~loc (ContractAt { arg; entry; entry_param }) exp.ty

  | Unpack { arg; ty } ->
    let arg = encode env arg in
    mk ?name:exp.name ~loc (Unpack { arg; ty }) exp.ty

  | CreateContract { args; contract } ->
    let args = List.map (encode env) args in
    let contract, c_to_inline =
      encode_contract
        ~annot:env.annot
        ~decompiling:env.decompiling
        { contract with
          subs = env.visible_contracts @ contract.subs } in
    (* Performed inlining and simplifications on subcontract at encoding time *)
    let contract =
      LiquidSimplify.simplify_contract ~decompile_annoted:env.decompiling
        contract c_to_inline in
    mk ?name:exp.name ~loc (CreateContract { args; contract }) exp.ty

  | TypeAnnot _ | Type _ -> assert false (* Removed during typechecking *)

(*
and encode_lambda ~loc env
    { arg_name; arg_ty; body; recursive } =
  match recursive with
  | Some f ->
    encode_rec_fun env ~loc ?name:exp.name
      f arg_name arg_ty ret_ty body
  | None ->
    let env_at_lambda = env in
    let lambda_arg_type = arg_ty in
    let lambda_arg_name = arg_name in
    let lambda_body = body in
    let bvs = LiquidBoundVariables.bv exp in
    if StringSet.is_empty bvs
    then
      (* not a closure (or will be pure after inlining),
         create a real lambda *)
      let env = { env_at_lambda with
                  vars = StringSet.fold (fun bv ->
                      StringMap.add bv (StringMap.find bv env.vars)
                    ) bvs StringMap.empty
                } in
      let (new_arg_name, env, count) =
        new_binding env lambda_arg_name.nname lambda_arg_type in
      let arg_name = { arg_name with nname = new_arg_name } in
      let body = encode env lambda_body in
      (* check_used env lambda_arg_name loc arg_count; *)
      let ty = Tlambda (lambda_arg_type, body.ty) in
      mk ?name:exp.name  ~loc
        (Lambda { arg_name; arg_ty = lambda_arg_type;
                  body; ret_ty = body.ty; recursive = None }) ty
    else
      (* create closure with environment *)
      let env, arg_name, arg_ty, call_env =
        env_for_clos env bvs arg_name arg_ty in
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
        Closure { arg_name; arg_ty; call_env; body; ret_ty = body.ty } in
      let call_env_type = match call_env with
        | [] -> assert false
        | [_, t] -> t.ty
        | _ -> Ttuple (List.map (fun (_, t) -> t.ty) call_env)
      in
      let ty = Tclosure ((lambda_arg_type, call_env_type), body.ty) in
      mk ?name:exp.name ~loc desc ty
*)

and encode_apply name env prim loc args ty =
  match prim, args with
  | Prim_exec _, [ f; x ] ->
    let x = encode env x in
    let f, x =
      if not (uncurry_lambda f.ty) then (encode env f, x)
      else
        (* Encode application f x when f is uncurried *)
        (* Compute arguments and function in ((((f x) y) z) ...) *)
        let rec all_args acc f = match f.desc with
          | Apply { prim = Prim_exec _; args = [f'; x] } ->
            if not (uncurry_lambda f'.ty) then (encode env f, acc)
            else
              let acc = (encode env x :: acc) in
              all_args acc f'
          | _ -> encode env f, acc in
        let f, args = all_args [x] f in
        let x = match args with
          | [x] ->
            (* only one argument, nothing to do *)
            x
          | _ ->
            (* build argument (x, y, z, ...) *)
            mk ~loc
              (Apply { prim = Prim_tuple; args })
              (Ttuple (List.map (fun x -> x.ty) args)) in
        (f, x) in
    mk ~loc ?name (Apply { prim; args = [f ; x] }) ty
  | _ ->
    let args = List.map (encode env) args in
    mk ~loc ?name (Apply { prim; args }) ty


(* Encode tail-recursive function with Loop.left *)
and encode_rec_fun env ~loc ?name f arg_name arg_ty ret_ty lam_ty body =
  let body = LiquidBoundVariables.bound body in
  let fail_if_called exp =
    if StringSet.mem f exp.bv then
      error exp.loc
        "Expression contains a non tail-recursive call to %s" f
  in
  (* Replace tail calls with (Left arg) and results with (Right res) *)
  let rec replace_tail_calls (body:typed_exp) =
    let loc = body.loc in
    if not @@ StringSet.mem f body.bv then
      (* no recursive calls in body *)
      if body.ty = Tfail then
        body
      else
        mk_typed ~loc ?name:body.name (
          Constructor { constr = Right arg_ty; arg = body }
        ) (Tor (arg_ty, body.ty))
    else match body.desc with
      | Var v when v = f ->
        error loc "Call to %s is not a tail-recursive call" f
      | Apply { prim = Prim_exec _; args = [{ desc = Var ff }; arg] }
        when ff = f ->
        mk_typed ~loc ?name:body.name (
          Constructor { constr = Left ret_ty; arg }
        ) (Tor (arg.ty, ret_ty))
      | Seq (e1, e2) ->
        fail_if_called e1;
        let e2 = replace_tail_calls e2 in
        let desc = Seq (e1, e2) in
        mk_typed ~loc ?name:body.name desc e2.ty
      | Let { bnd_var; inline; bnd_val; body = let_body } ->
        fail_if_called bnd_val;
        let let_body = replace_tail_calls let_body in
        let desc = Let { bnd_var; inline; bnd_val; body = let_body } in
        mk_typed ~loc ?name:body.name desc let_body.ty
      | If { cond; ifthen; ifelse } ->
        fail_if_called cond;
        let ifthen = replace_tail_calls ifthen in
        let ifelse = replace_tail_calls ifelse in
        let desc = If { cond; ifthen; ifelse } in
        mk_typed ~loc ?name:body.name desc ifthen.ty
      | MatchOption { arg; ifnone; some_name; ifsome } ->
        fail_if_called arg;
        let ifnone = replace_tail_calls ifnone in
        let ifsome = replace_tail_calls ifsome in
        let desc = MatchOption { arg; ifnone; some_name; ifsome } in
        mk_typed ~loc ?name:body.name desc ifsome.ty
      | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
        fail_if_called arg;
        let ifcons = replace_tail_calls ifcons in
        let ifnil = replace_tail_calls ifnil in
        let desc = MatchList { arg; head_name; tail_name; ifcons; ifnil } in
        mk_typed ~loc ?name:body.name desc ifcons.ty
      | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
        fail_if_called arg;
        let ifplus = replace_tail_calls ifplus in
        let ifminus = replace_tail_calls ifminus in
        let desc = MatchNat { arg; plus_name; ifplus; minus_name; ifminus } in
        mk_typed ~loc ?name:body.name desc ifplus.ty
      | MatchVariant { arg; cases } ->
        fail_if_called arg;
        let cases = List.map (fun (pat, e) ->
            pat, replace_tail_calls e
          ) cases in
        let ty = match cases with
          | [] -> body.ty
          | (_, e) :: _ -> e.ty in
        let desc = MatchVariant { arg; cases } in
        mk_typed ~loc ?name:body.name desc ty
      | _ ->
        error loc "Expression contains a non tail-recursive call to %s" f
  in
  let body_desc =
    LoopLeft { arg_name;
               body = replace_tail_calls body;
               arg = mk_typed ~loc:arg_name.nloc (Var arg_name.nname) arg_ty;
               acc = None } in
  let body = mk_typed ~loc:body.loc ?name:body.name body_desc ret_ty in
  let lam =
    mk_typed ~loc ?name
      (Lambda { arg_name; arg_ty; body; ret_ty; recursive = None })
      lam_ty in
  encode env lam

(* Contract is encoded to single entry point form (with name "default"):
   {contract C = struct
      ...
      let%entry e1 (p1 : ty1) s1 = code_entry_1
      let%entry e2 (p2 : ty2) s2 = code_entry_2
      let%entry e3 (p3 : ty3) s3 = code_entry_3
    end}
   ===>
   contract C = struct
     ...
     type p =
      | _Liq_entry_e1 of ty1
      | _Liq_entry_e2 of ty2
      | _Liq_entry_e3 of ty3

     let%entry root (paramter : p) storage =
       match parameter with
       | _Liq_entry_e1 p1 ->
         let s1 = storage in {code_entry_1}
       | _Liq_entry_e2 p2 ->
         let s2 = storage in {code_entry_2}
       | _Liq_entry_e3 p3 ->
         let s3 = storage in {code_entry_3}
   end
*)

(* Encode modules global functions for export *)
and encode_modules top_env contracts =
  let env, values =
    List.fold_left (fun (env, old_values) c ->
        let env, sub_values = encode_modules env c.subs in

        if !LiquidOptions.verbosity > 0 then
          Format.eprintf "Encode module %s@." (qual_contract_name c);

        (* create env local to module *)
        let env =
          { env with
            env = c.ty_env;
            t_contract_sig = full_sig_of_contract c;
          } in

        let env, new_values =
          List.fold_left (fun (env, values) v ->
              let val_name = v.val_name in
              let val_exp = encode env v.val_exp in
              let val_exp = if env.annot && val_exp.name = None
                then { val_exp with
                       name = Some (add_path_name c.ty_env.path val_name) }
                else val_exp
              in
              let (new_name, env, count) =
                new_binding env val_name ~effect:val_exp.effect val_exp.ty in
              let v = { v with val_exp } in
              env, (v, val_name) :: values)
            (env, []) c.values
        in
        let new_values = List.rev new_values in

        (* rename values *)
        let env, values = List.fold_left (fun (env, values) (v, cur_name) ->
            try
              let (new_name, (tv, ty), effect) =
                StringMap.find cur_name env.vars in
              let out_name = add_path_name [c.contract_name] cur_name in
              let vars =
                StringMap.remove cur_name env.vars
                |> StringMap.add out_name (new_name, (tv, ty), effect) in
              let v = { v with val_name = new_name } in
              { env with vars }, (v, out_name) :: values
            with Not_found -> env, values
          ) (env, []) (new_values @ sub_values)
        in

        (* return in order *)
        (env, List.(rev @@ rev_append old_values values))
      ) (top_env, []) contracts
  in
  { env with env = top_env.env;
             t_contract_sig = top_env.t_contract_sig },
  values



and encode_contract ?(annot=false) ?(decompiling=false) contract =

  let env =
    {
      warnings=false;
      annot;
      decompiling;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      env = contract.ty_env;
      clos_env = None;
      t_contract_sig = full_sig_of_contract contract;
      ftvars = StringSet.empty;
      visible_contracts = contract.subs;
    } in

  (* Global exported values from modules *)
  let env, values =
    encode_modules env contract.subs in
  let values = List.map fst values in

  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Encode contract %s@." (qual_contract_name contract);

  let parameter = encode_contract_sig (sig_of_full_sig env.t_contract_sig) in
  let loc = LiquidLoc.loc_in_file env.env.filename in

  let rec values_on_top mk l exp = match l with
    | [] -> exp
    | v :: rest ->
      mk ?name:None ~loc
        (Let { bnd_var = { nname = v.val_name; nloc = loc };
               inline = v.inline;
               bnd_val = v.val_exp;
               body = values_on_top mk rest exp }) exp.ty in

  let code_desc, fee_code_desc, parameter_name, storage_name =
    match contract.entries with
    | [] ->
      Const { ty = Tunit; const = CUnit }, None, "parameter", "storage"
    | [e] ->
      e.code.desc,
      (match e.fee_code with
       | None -> None
       | Some fee_code -> Some fee_code.desc),
      e.entry_sig.parameter_name, e.entry_sig.storage_name
    | _ ->
      let parameter = mk_typed ~loc (Var "parameter") parameter in
      (* let ecstrs = List.mapi (fun i e ->
       *     let constr = e.entry_sig.entry_name in
       *     env.env.constrs <- StringMap.add constr ("_entries", i) env.env.constrs;
       *     constr, e.entry_sig.parameter
       *   ) contract.entries in
       * env.env.types <-
       *   StringMap.add "_entries" (fun _ -> Tsum("_entries", ecstrs)) env.env.types; *)
      let mk_pattern_matching_case entry_name parameter_name storage_name code =
        let constr = entry_name in
        let pat =
          PConstr (constr, [parameter_name]) in
        let body =
          mk_typed ~loc
            (Let { bnd_var = { nname = storage_name;
                               nloc = loc };
                   inline = InAuto;
                   bnd_val =
                     mk_typed ~loc (Var "storage")
                       env.t_contract_sig.f_storage;
                   body = code }) code.ty in
        pat, body in

      let code =
        MatchVariant {
          arg = parameter;
          cases = List.mapi (fun i e ->
              mk_pattern_matching_case e.entry_sig.entry_name
                e.entry_sig.parameter_name e.entry_sig.storage_name
                e.code
            ) contract.entries }
      in
      let fee_code =
        if List.for_all (function { fee_code = None } -> true | _ -> false)
            contract.entries then
          None
        else Some (MatchVariant {
            arg = parameter;
            cases = (List.mapi (fun i (e : typed_exp entry) ->
                match e.fee_code with
                | None ->
                  error e.code.loc
                    "Entry point %s does not have code to compute fees, while \
                     other entry points do. All (or no) entry points must \
                     provide code to compute fees." e.entry_sig.entry_name
                | Some fee_code ->
                  mk_pattern_matching_case e.entry_sig.entry_name
                    e.entry_sig.parameter_name e.entry_sig.storage_name
                    fee_code
              ) contract.entries) }) in

      code,
      fee_code,
      "parameter",
      "storage"
  in

  (* "storage/1" *)
  let (storage_name, env, _) =
    new_binding env storage_name env.t_contract_sig.f_storage in
  (* "parameter/2" *)
  let (pname, env, _) = new_binding env parameter_name parameter in

  let fee_env = copy_env env in

  let code =
    values_on_top mk values @@
    encode env @@
    values_on_top mk_typed contract.values @@
    mk_typed ~loc code_desc (Ttuple [Tlist Toperation; contract.storage]) in

  let fee_code = match fee_code_desc with
    | None -> None
    | Some fee_code_desc ->
      Some (
        values_on_top mk values @@
        encode env @@
        values_on_top mk_typed contract.values @@
        mk_typed ~loc fee_code_desc (Ttuple [Ttez; Tnat])
      ) in

  (* Register global values for inlining if necessary *)
  List.iter (register_inlining_value env) values;
  List.iter (register_inlining_value fee_env) values;

  let c_init = match contract.c_init with
    | None -> None
    | Some i ->
      let env, rargs = List.fold_left (fun (env, acc) (arg, loc, arg_ty) ->
          let (arg, env, _) = new_binding env arg arg_ty in
          env, (arg, loc, arg_ty) :: acc
        ) (env, []) i.init_args in
      let init_args = List.rev rargs in
      let init_body = encode env i.init_body in
      Some { i with init_body; init_args }
  in
  let root_name = match contract.entries with
    | [e] -> e.entry_sig.entry_name (* Keep entry name as root if only one entry *)
    | _ -> "" in
  let contract = {
    contract_name = contract.contract_name;
    values = [];
    storage = encode_storage_type env contract.storage;
    entries = [{
        entry_sig = {
          entry_name = root_name;
          parameter_name = pname;
          storage_name;
          parameter = encode_parameter_type env parameter;
        };
        code;
        fee_code;
      }];
    ty_env = contract.ty_env;
    c_init;
    subs = [];
  } in
  contract, (!(env.to_inline), !(fee_env.to_inline))


let encode_code tenv code =
  encode tenv code


let encode_const env t_contract_sig const =
  let env =
    {
      warnings = false;
      annot = false;
      decompiling = false;
      counter = ref 0;
      vars = StringMap.empty;
      vars_counts = StringMap.empty;
      to_inline = ref StringMap.empty;
      env = env;
      clos_env = None;
      t_contract_sig;
      ftvars = StringSet.empty;
      visible_contracts = [];
    } in

  encode_const env const

let encode_type ?decompiling ty =
  let ty = encode_type ?decompiling ty in
  check_allowed_type (LiquidLoc.loc_in_file "_none_") ty;
  ty
