open Dune_Network_Lib
open Protocol
open Environment
open LiquidTypes
open Love_ast
open Love_type
open Love_ast_utils
open Love_ast_types
open Love_ast_types.TYPE
open Love_type_utils
open Compil_utils
open Love_pervasives
open Log

module SMap = Collections.StringMap
module SIMap = Collections.StringIdentMap

type env = Love_tenv.t

exception UnknownType of (string * (TYPE.t -> TYPE.t) * env)

module TypeVarMap = struct
  include TYPE.TypeVarMap
  let find k m =
    match TYPE.TypeVarMap.find_opt k m with
      Some t -> t
    | None -> assert false
end

let add_typedef_to_contract name tdef env = Love_tenv.add_typedef name TPublic tdef env

let find_type (tname : string) (env: env) =
  match Love_tenv.find_tdef (string_to_ident tname) env with
    None -> None
  | Some {result; _} -> Some result

let env_of_subcontract name skind env = Love_tenv.new_subcontract_env name skind env

let empty_env skind () = Love_tenv.empty skind ()

let get_opt o = match o with None -> assert false | Some o -> o

let rec apply_cont cont name love_ty =
  try cont love_ty with
    UnknownType (n, cont,_) when String.equal n name ->
    apply_cont cont name love_ty
  | UnknownType (n, cont,_) when String.equal n "Partial" ->
    cont (TVar (Love_type.fresh_typevar ()))
(* Transforms a tv Ref.t into a TVar.t.
   If a tv aliases another tv, the same Tvar will be generated.  *)

let rec fvars =
  function
  | Tunit
  | Tbool
  | Tint
  | Tnat
  | Ttez
  | Tstring
  | Tbytes
  | Ttimestamp
  | Tkey
  | Tkey_hash
  | Tsignature
  | Toperation
  | Taddress
  | Tchainid -> StringSet.empty

  | Ttuple l ->
    List.fold_left (fun acc elt -> StringSet.union acc @@ fvars elt) StringSet.empty l

  | Toption t
  | Tlist t
  | Tset t -> fvars t

  | Tmap (t1, t2)
  | Tbigmap (t1, t2)
  | Tor (t1, t2)
  | Tlambda (t1, t2, _) -> StringSet.union (fvars t1) (fvars t2)
  | Tcontract _ -> StringSet.empty

  | Trecord (_, l)
  | Tsum  (_, l) ->
    List.fold_left (fun acc (_,t) -> StringSet.union acc @@ fvars t)
      StringSet.empty
      l
  | Tclosure ((t1, t2), t3, _) ->
    StringSet.union (StringSet.union (fvars t1) (fvars t2)) @@ fvars t3
  | Tfail -> StringSet.empty

  | Tvar v -> StringSet.singleton !(v.contents).id
  | (Tpartial p) as t -> (
      match p with
        Peqn _ | Pcont _ | Ppar ->
        error
          "Partial type %s not supported"
          (LiquidPrinter.Liquid.string_of_type t)
      | Ptup l ->
        List.fold_left
          (fun acc (_,elt) -> StringSet.union acc @@ fvars elt)
          StringSet.empty
          l
      | Pmap (t1, t2) -> StringSet.union (fvars t1) (fvars t2)
    )

let rec liqtype_to_lovetypedef (env : env) t : TYPE.typedef =
  let params = StringSet.elements @@ fvars t in (* what if the type has no parameter ? *)
  match t with
  | Tsum (None, l) ->
    error
      "[liqtype_to_lovetypedef] Sum type %s expects a name"
      (LiquidPrinter.Liquid.string_of_type t)
  | Tsum (Some sname, l) ->
    debug "[liqtype_to_lovetypedef] Sum type@.";
    let sparams, aliases =
      List.fold_left
        (fun (acc_list, acc_set) t ->
           let tv = Love_type.fresh_typevar ~name:t () in
           tv :: acc_list, StringMap.add t tv acc_set
        )
        ([], StringMap.empty)
        params in
    let sparams = List.rev sparams in
    let love_name = string_to_ident sname in
    let love_ty =
      TUser (love_name, List.map (fun p -> TVar p) sparams)
    in
    let scons =
      List.map
        (fun (name, cons) ->
           let t =
             try liqtype_to_lovetype ~aliases env cons
             with UnknownType (n, cont, _) when String.equal n sname ->
               apply_cont cont sname love_ty
           in
           debug "[liqtype_to_lovedef] %s of type %a@." name Love_type.pretty t;
           name, [t]
        )
        l
    in Love_ast_utils.mk_sumtype sparams scons NonRec
  | Trecord (rname, l) ->
    debug "[liqtype_to_lovetypedef] Record type@.";
    let rparams, aliases =
      List.fold_left
        (fun (acc_list, acc_set) t ->
           let tv = Love_type.fresh_typevar ~name:t () in
           tv :: acc_list, StringMap.add t tv acc_set
        )
        ([], StringMap.empty)
        params in
    let rparams = List.rev rparams in
    let love_name = string_to_ident rname in
    let love_ty =
      TUser (love_name, List.map (fun p -> TVar p) rparams)
    in
    let rcons =
      List.map
        (fun (name, cons) ->
           let t =
             try  (liqtype_to_lovetype ~aliases env) cons
             with UnknownType (n, cont, _) when String.equal n rname ->
               apply_cont cont rname love_ty
           in name, t
        )
        l
    in Love_ast_utils.mk_rectype rparams rcons NonRec
  | t ->
    debug "[liqtype_to_lovetypedef] Alias type@.";
    let aparams, aliases =
      List.fold_left
        (fun (acc_list, acc_set) t ->
           let tv = Love_type.fresh_typevar ~name:t () in
           tv :: acc_list, StringMap.add t tv acc_set
        )
        ([], StringMap.empty)
        params in
    let aparams = List.rev aparams in
    let atype = liqtype_to_lovetype ~aliases env t in
    Alias {aparams; atype}
and tvref_to_tvar ?(aliases=StringMap.empty) (env : env) tvref : TYPE.t =
  debug "[tvref_to_tvar] Creating a TVar@.";
  let tv = !(tvref.Ref.contents) in
  match tv.tyo with
    Some t -> (
    try liqtype_to_lovetype env t
    with
      UnknownType (n, f, _) when String.equal n "Partial" ->
      debug "[tvref_to_tvar] Partial TVar@.";
      f (TVar (Love_type.fresh_typevar ()))
  )
  | None -> ( (* Type has not been found for tvref. Checking on aliases *)
      match StringMap.find_opt tv.id aliases with
        Some t ->
        debug "[tvref_to_tvar] Tvar %s is aliased to TVar %s@."
          tv.id t.tv_name
        ;
        TVar t
      | None -> (
          let aliases = !(tvref.Ref.aliases) in
          let rec check_aliases_info =
            function
              [] -> None
            | {Ref.contents; _}   :: tl -> (
                match (!contents).tyo with
                  None -> check_aliases_info tl
                | Some t -> Some (liqtype_to_lovetype env t)
              )
          in
          match check_aliases_info aliases with
            Some t -> t (* One of the aliases had the type *)
          | None -> ( (* No aliases, creating a TVar *)
              let sorted_aliases = (* Maybe not necesary to sort, ask Alain *)
                List.fast_sort
                  (fun (t1 : LiquidTypes.tv LiquidTypes.Ref.t) t2 ->
                     String.compare !(t1.contents).id !(t2.contents).id
                  )
                  aliases
              in
              let new_name =
                List.fold_left
                  (fun acc (v: LiquidTypes.tv LiquidTypes.Ref.t)
                    -> acc ^ (!(v.contents).id)
                  )
                  "_liq_" (* Starting by '_liq_' to avoid redefinition. *)
                  sorted_aliases
              in
              debug "[tvref_to_tvar] TVar %s@." new_name;
              TVar {tv_name = new_name; tv_traits = {tcomparable = true}}
            )
        )
    )

and get_tvar_from_tlist env tlist : TYPE.t list =
  let rec get_tvars (acc: TYPE.t SMap.t) =
    function
    | [] -> acc
    | Tvar tv :: tl -> (
        let new_t = tvref_to_tvar env tv in
        match new_t with
        | TVar {tv_name; _} -> get_tvars (SMap.add tv_name new_t acc) tl
        | _ -> get_tvars acc tl
      )
    | _ :: tl -> get_tvars acc tl
  in
  SMap.fold
    (fun _ bnd acc -> bnd :: acc)
    (get_tvars SMap.empty tlist)
    []

and liqcontract_sig_to_lovetype
    (env : env)
    (mod_name : string option)
    {sig_name; entries_sig} : structure_type =
  debug "[liqcontract_sig_to_lovetype] Creating type from Liquidity Contract@.";
  match mod_name with
    Some name -> (
      debug "[liqcontract_sig_to_lovetype] Module is named %s@." name;
      match Love_tenv.find_contract (string_to_ident name) env with
        Some _ -> (* The signature has already been registered *)
        debug "[liqcontract_sig_to_lovetype] Already registered@.";
        Named (string_to_ident name)
      | None ->
        debug "[liqcontract_sig_to_lovetype] Not registered, treating as anonymous@.";
        liqcontract_sig_to_lovetype env None {sig_name = None; entries_sig}
    )
  | None -> (
      debug "[liqcontract_sig_to_lovetype] Anonymous contract@.";
      let entry_sig_to_love_content env entry =
        let name = entry.entry_name in
        let typ, tdef =
          match entry.parameter with
          | Tsum (None, _) ->
            error
              "[liqcontract_sig_to_lovetype] Error in sum type: %s expects a name"
              (LiquidPrinter.Liquid.string_of_type entry.parameter)
          | Tsum (Some name, _) as t -> (
              try liqtype_to_lovetype env t, None with
                UnknownType (n, _, _) when String.equal name n ->
                debug "[liqcontract_sig_to_lovetype] Sum type %s defined in signature@." name;
                (* entry.parameter has not been defined yet. *)
                let tdef = liqtype_to_lovetypedef env t in
                match tdef with
                  TYPE.SumType {sparams; _} ->
                  TUser (string_to_ident name, List.map (fun v -> TVar v) sparams), Some (name, tdef)
                | _ -> assert false
            )
          | Trecord (name, _) as t -> (
              try liqtype_to_lovetype env t, None with
                UnknownType (n, _, _) when String.equal name n ->
                debug "[liqcontract_sig_to_lovetype] Record type %s defined in signature@." name;
                (* entry.parameter has not been defined yet. *)
                let tdef = liqtype_to_lovetypedef env t in
                match tdef with
                  TYPE.RecordType {rparams; _} ->
                  TUser (string_to_ident name, List.map (fun v -> TVar v) rparams), Some (name, tdef)
                | _ -> assert false)
          | t -> liqtype_to_lovetype env t, None

        in
        debug "[liqcontract_sig_to_lovetype] Entry %s : %a@." name Love_type.pretty typ;
        name, typ, tdef
      in
      let sig_content, _env =
        List.fold_left
          (fun (content, env) entry ->
             debug "[liqcontract_sig_to_lovetype] Entry %s@." entry.entry_name;
             let name, t1, tdef = entry_sig_to_love_content env entry in
             debug "[liqcontract_sig_to_lovetype] Entry %s has type %a@."
               name
               Love_type.pretty t1
             ;
             match tdef with
               None -> (name, SEntry t1) :: content, env
             | Some (n,t) ->
               (n, SType (SPublic t)) :: (name, SEntry t1) :: content,
               add_typedef_to_contract n t env
          )
          ([], env)
          entries_sig
      in
      Anonymous {
        sig_kind = Contract [];
        sig_content = List.rev sig_content
      }
    )

and liqtype_to_lovetype ?(aliases=StringMap.empty) (env : env) tv =
  debug "[liqtype_to_lovetype] Transpiling type %s@." (LiquidPrinter.Liquid.string_of_type tv);
  let ltl = liqtype_to_lovetype env in
  let action res t =
    let t =
      try ltl t
      with
        UnknownType (name, f, env) ->
        debug "[liqtype_to_lovetype] Unknown type %s, sending task to sender@." name;
        raise (UnknownType (name, (fun t -> res @@ f t), env))
    in res t
  in
  match tv with
    Tunit -> debug "[liqtype_to_lovetype] Unit@."; unit ()
  | Tbool -> debug "[liqtype_to_lovetype] Bool@."; bool ()
  | Tint  -> debug "[liqtype_to_lovetype] Int@."; int ()
  | Tnat  -> debug "[liqtype_to_lovetype] Nat@."; nat ()
  | Ttez  -> debug "[liqtype_to_lovetype] Tez = Dun@."; dun ()
  | Tstring -> debug "[liqtype_to_lovetype] String@."; string ()
  | Tbytes -> debug "[liqtype_to_lovetype] Bytes@."; bytes ()
  | Ttimestamp -> debug "[liqtype_to_lovetype] Timestamp@."; timestamp ()
  | Tkey -> debug "[liqtype_to_lovetype] TKey@."; key ()
  | Tkey_hash ->  debug "[liqtype_to_lovetype] TKeyHash@."; keyhash ()
  | Tsignature ->  debug "[liqtype_to_lovetype] Signature@."; signature ()
  | Toperation ->  debug "[liqtype_to_lovetype] Operation@."; operation ()
  | Taddress ->  debug "[liqtype_to_lovetype] Address@."; address ()

  | Ttuple l ->
    debug "[liqtype_to_lovetype] Tuple of size %i@." (List.length l);
    let rec loop acc =
      function
        [] -> TTuple (List.rev acc)
      | hd :: tl ->
        debug "[liqtype_to_lovetype] Next tuple element@.";
        let res t = loop (t :: acc) tl
        in action res hd
    in
    let res = loop [] l in
    debug "[liqtype_to_lovetype] Output : %a@." Love_type.pretty res; res
  | Toption t ->
    debug "[liqtype_to_lovetype] Option@.";
    action Compil_utils.option t

  | Tlist t ->
    debug "[liqtype_to_lovetype] List@.";
    let res t = list t in
    action res t

  | Tset t ->
    debug "[liqtype_to_lovetype] Set@.";
    let res t = set t in
    action res t

  | Tmap (t1, t2) ->
    debug "[liqtype_to_lovetype] Map@.";
    let res t1 t2 = map (t1, t2) in
    let t2 t1 =
      try res t1 (ltl t2) with
        UnknownType (name, f, e) ->
        raise (UnknownType (name, (fun t2 -> res t1 @@ f t2), e))
    in
    let t1 =
      try ltl t1
      with
        UnknownType (name, f, e) ->
        raise (UnknownType (name, (fun t -> let t1 = f t in t2 t1), e))
    in t2 t1

  | Tbigmap (t1, t2) ->
    debug "[liqtype_to_lovetype] Bigmap@.";
    let res t1 t2 = bigmap (t1, t2) in
    let t2 t1 =
      try res t1 (ltl t2) with
        UnknownType (name, f, e) ->
        raise (UnknownType (name, (fun t2 -> res t1 @@ f t2), e))
    in
    let t1 =
      try ltl t1
      with
        UnknownType (name, f, e) ->
        raise (UnknownType (name, (fun t -> let t1 = f t in t2 t1), e))
    in t2 t1
  | Tcontract (s, c) ->
    begin
      let ty = ltl c in
      TContractInstance (Compil_utils.get_signature_from_name s ty env)
    end
  | Tor (t1,t2) -> (
      let t1 = ltl t1 and t2 = ltl t2 in
      let sumtyp = variant (t1,t2) in
      match t1, t2 with
        TVar v1, TVar v2 ->
        TForall (v1, TForall (v2, sumtyp))
      | TVar v, _
      | _, TVar v -> TForall (v, sumtyp)
      | _,_ -> sumtyp
    )
  | Tlambda (t1, t2, _) ->
    debug "[liqtype_to_lovetype] Lambda@.";
    let t2 t1 =
      try debug "[liqtype_to_lovetype] t1 = %a, calculating t2@." Love_type.pretty t1;
        t1 @=> (ltl t2) with
        UnknownType (name, f, e) ->
        raise (UnknownType (name, (fun t2 -> t1 @=> f t2), e))
    in
    let t1 =
      try ltl t1
      with
        UnknownType (name, f, e) ->
        raise (UnknownType (name, (fun t -> let t1 = f t in t2 t1), e))
    in t2 t1
  | Trecord (name, fields) -> (
      debug "[liqtype_to_lovetype] Record@.";
      match find_type name env with
        None -> raise (UnknownType (name, (fun t -> t), env))
      | Some (RecordType {rparams;rfields}) ->
        let params =
          let rec instanciate_params params_map poly_fields fields =
            match poly_fields with
              (name, TVar t) :: tl_p ->
              let _, t' = List.find (fun (n, _) -> String.equal name n) fields in
              instanciate_params (TypeVarMap.add t t' params_map) tl_p fields
            | _ :: tl_p ->
              instanciate_params params_map tl_p fields
            | [] -> params_map
          in
          instanciate_params TypeVarMap.empty rfields fields
        in
        let rparams_replaced =
          List.map (
            fun param ->
              ltl @@
              match TypeVarMap.find_opt param params with
                Some t -> t
              | None -> assert false) rparams
        in
        let t = TUser (string_to_ident name, rparams_replaced)
        in
        debug "[liqtype_to_lovetype] Record type = %a@." Love_type.pretty t;
        t
      | Some _ -> assert false
    )
  | Tsum (None, _) ->
    error
      "[liqtype_to_lovetype] Error in sum type: %s expects a name"
      (LiquidPrinter.Liquid.string_of_type tv)
  | Tsum (Some name, cons) -> (
      debug "[liqtype_to_lovetype] Sum@.";
      match find_type name env with
        None -> raise (UnknownType (name, (fun t -> t), env))
      | Some (SumType {sparams; scons}) ->
        let params =
          let rec instanciate_params params_map poly_cons cons =
            match poly_cons with
              (name, l) :: tl_p ->
              debug "[liqtype_to_lovetype] Constructor %s@." name;
              let l =
                match List.map to_poly_variant l with
                  [`Type (TTuple l)] ->
                  debug "[liqtype_to_lovetype] %s of %a, %i arguments @."
                    name
                    Love_type.pretty (TTuple l)
                    (List.length l);
                  l
                | [`TUnit] -> []
                | [_] -> l
                | _ -> assert false in
              debug "[liqtype_to_lovetype] Arguments : %a@."
                (Format.pp_print_list Love_type.pretty) l;
              let l' =
                match
                  List.find
                    (fun (n, _) -> String.equal (Ident.get_final (string_to_ident n)) name)
                    cons
                with
                  _, Ttuple l -> l
                | _, Tunit -> if Compare.Int.equal (List.length l) 1 then [Tunit] else []
                | _, ty -> [ty]
                | exception Not_found ->
                  debug "[liqtype_to_lovetype] Constructors list = %a@."
                    (Format.pp_print_list (fun fmt (s, _) -> Format.fprintf fmt "%s," s)) cons;
                  raise Not_found
              in
              debug "[liqtype_to_lovetype] Types : %a@."
                (Format.pp_print_list
                   (fun fmt t -> Format.fprintf fmt "%s" (LiquidPrinter.Liquid.string_of_type t)))
                   l';
              let new_map =
                try
                  List.fold_left2
                    (fun acc polyt t ->
                       match polyt with
                         TVar tv ->
                         TypeVarMap.add tv t acc
                       | _ -> acc
                    )
                    params_map
                    l
                    l'
                with Invalid_argument _ ->
                  debug "[liqtype_to_lovetype] Error: Size l = %i; size l' = %i@."
                    (List.length l)
                    (List.length l');
                  error
                    "Sum type %s defines %d constructors, but it is registered \
                     in the typing environment with %d constructors"
                    name
                    (List.length l')
                    (List.length l)
              in
              instanciate_params new_map tl_p cons
            | [] -> params_map
          in
          instanciate_params TypeVarMap.empty scons cons
        in
        let sparams_replaced =
          List.map (fun param -> ltl @@ TypeVarMap.find param params) sparams
        in
        let t = TUser (string_to_ident name, sparams_replaced) in
        debug "[liqtype_to_lovetype] Sum type = %a" Love_type.pretty t;
        t
      | Some _ -> assert false
    )
  | Tvar v ->
    debug "[liqtype_to_lovetype] Tvar@.";
    tvref_to_tvar ~aliases env v
  | Tfail ->
    debug "[liqtype_to_lovetype] Fail@.";
    raise (UnknownType ("Fail", (fun t -> t), env))
  | Tpartial p -> (
      debug "[liqtype_to_lovetype] Partial %s@." (LiquidPrinter.Liquid.string_of_type tv);
      match p with
        Peqn _ | Pcont _ | Ppar | Pmap _ -> raise (UnknownType ("Partial", (fun t -> t), env))
      | Ptup l ->
        debug "[liqtype_to_lovetype] Partial tuple@.";
        let l = List.fast_sort (fun (i1, _) (i2, _) -> i1 - i2) l in
        let rec create_tuple_content acc cpt l =
        debug "[liqtype_to_lovetype] Tuple index %i@." cpt;
          match l with
            (index, typ) :: tl ->
            if cpt = index
            then (
              debug "[liqtype_to_lovetype] Defined type@.";
              create_tuple_content (typ :: acc) (cpt + 1) tl
            )
            else (
              debug "[liqtype_to_lovetype] Undefined type@.";
              let tvar = {id = Format.asprintf "'a.%i" cpt; tyo = None} in
              create_tuple_content
                (Tvar (LiquidTypes.Ref.create tvar) :: acc)
                (cpt + 1)
                tl
            )
          | [] -> List.rev acc
        in
        let tuple_content = create_tuple_content [] 0 l in
        debug "[liqtype_to_lovetype] Tuple has size %i@." (List.length tuple_content);
        ltl (Ttuple tuple_content)
    )
  | Tclosure ((t1,t2), t3, _) ->
    debug "[liqtype_to_lovetype] Closure@.";
    let t1 =
      let res = ltl t1 in
      debug "[liqtype_to_lovetype] t1=%a@." Love_type.pretty res; res
    in
    let _t2 =
      let res = ltl t2 in
      debug "[liqtype_to_lovetype] t2=%a@." Love_type.pretty res; res
    in
    let t3 =
      let res = ltl t3 in
      debug "[liqtype_to_lovetype] t3=%a@." Love_type.pretty res; res
    in
    TArrow (t1, t3)
  | Tchainid ->
    error "[liqtype_to_lovetype] No equivalent of Tchainid in Love"
let rec remove_forall t =
  match t with
    TForall (_, t) -> remove_forall t
  | _ -> t
(*
let mk_prim name args spec_args =
  match Love_primitive.from_string name with
    None -> failwith ("Primitive " ^ name ^ " not found")
  | Some prim ->
    let ty = Love_primitive.type_of (prim,spec_args) in
    let exp =
      let pexp = mk_var_with_args  (string_to_ident name) spec_args in
      match args with
        [] -> pexp
      | _ -> mk_apply pexp args
    in
    exp, ty
*)
let liqprim_to_loveprim env (p : primitive) (args : TYPE.t list) =
  debug "[liqprim_to_loveprim] Primitive tranpilation@.";
  let eq_type l =
    debug "[liqprim_to_loveprim] Types of arguments are equal@.";
    match l with
      [a1; a2] ->  (
        debug "[liqprim_to_loveprim] %a = %a@." Love_type.pretty a1 Love_type.pretty a2;
        match a1, a2 with
          TVar _, t| t, TVar _ | t, _ -> [t;t]
      )
    | [a] -> [a]
    | [] ->  []
    | _ ->
      error
        "Too many arguments for equality primitive %s: Expected 2, got %d"
        (LiquidTypes.string_of_primitive p)
        (List.length l)
  in
  let args = List.map (fun t -> Love_tenv.normalize_type ~relative:true t env) args in
  match p with
  | Prim_balance         -> "Account.balance", args
  | Prim_now             -> "Current.time", args
  | Prim_amount          -> "Current.amount", args
  | Prim_gas             -> "Current.gas", args
  | Prim_source          -> "Current.source", args
  | Prim_sender          -> "Current.sender", args
  | Prim_eq  -> debug "[liqprim_to_loveprim] Eq@."; "=", eq_type args
  | Prim_neq -> debug "[liqprim_to_loveprim] Ne@."; "<>", eq_type args
  | Prim_lt  -> debug "[liqprim_to_loveprim] Lt@."; "<", eq_type args
  | Prim_le  -> debug "[liqprim_to_loveprim] Le@."; "<=", eq_type args
  | Prim_gt  -> debug "[liqprim_to_loveprim] Gt@."; ">", eq_type args
  | Prim_ge  -> debug "[liqprim_to_loveprim] Ge@."; ">=", eq_type args
  | Prim_compare ->
    debug "[liqprim_to_loveprim] Compare";
    "compare", eq_type args
  | Prim_add -> (
      debug "[liqprim_to_loveprim] Addition@.";
      match args with
      | [t1; t2] -> (
          match to_poly_variant t1, to_poly_variant t2 with
          | `TInt, `TInt             -> "+", args
          | `TNat, `TNat             -> "++", args
          | `TNat, `TInt             -> "++!", args
          | `TInt, `TNat             -> "+!+", args
          | `TNat, `TTimestamp       -> "++:", args
          | `TTimestamp, `TNat       -> "+:+", args
          | `TDun, (`TDun | `TVar _) -> "+$", [dun (); dun ()]
          | `TTimestamp, `TInt       -> "+:!", [timestamp (); int ()]
          | `TTimestamp, `TVar _     -> "+:!", [timestamp (); int ()]
          | `TInt, `TTimestamp       -> "+!:", [int (); timestamp ()]
          | `TVar _, `TTimestamp     -> "+!:", [int (); timestamp ()]
          | _,_ ->
            debug "[liqprim_to_loveprim] Forbidden addition on %a and %a."
              Love_type.pretty t1 Love_type.pretty t2;
            error "Cannot add %a with %a"
              Love_type.pretty t1 Love_type.pretty t2;
      )
    | [arg] -> (
        match to_poly_variant arg with
          `TDun -> "+$", args
        | `TTimestamp -> "+$!", args
        | _ ->
          debug
            "[liqprim_to_loveprim] Type %a is insufficient to guess the correct \
             addition primitive."
            Love_type.pretty arg;
          error "I cannot guess the return type of adding an element of type %a with \
                 something unknown."
            Love_type.pretty arg;
      )
    | [] -> error "Type of addition cannot be inferred from no arguments."
    | _ -> bad_number_of_args "(+)" (List.length args) 2
  )
  | Prim_sub -> (
    debug "[liqprim_to_loveprim] Substraction@.";
    match args with
    | [t1; t2] -> (
        match to_poly_variant t1, to_poly_variant t2 with
        | `TInt, `TInt ->  "-", args
        | `TNat, `TNat -> "-+", args
        | `TNat, `TInt -> "-+!", args
        | `TInt, `TNat -> "-!+", args
        | `TTimestamp, `TNat -> "-:+", args
        | `TDun, (`TDun | `TVar _) -> "-$", [dun (); dun ()]
        | `TTimestamp, `TInt -> "-:!", [timestamp (); int ()]
        | (`TTimestamp | `TVar _), `TTimestamp ->  "-:", [timestamp (); timestamp ()]
        | _,_ ->
          debug "[liqprim_to_loveprim] Forbidden substraction on %a and %a."
            Love_type.pretty t1 Love_type.pretty t2;
          error "Cannot substract %a with %a"
            Love_type.pretty t1 Love_type.pretty t2;
      )
    | [arg] -> (
        match to_poly_variant arg with
          `TDun -> "-$", args
        | _ ->
          debug "[liqprim_to_loveprim] Type %a is insufficient to guess \
                 the correct substraction primitive."
            Love_type.pretty arg;
          error "I cannot guess the return type of substracting an element of \
                 type %a with something unknown."
            Love_type.pretty arg;
      )
    | [] -> error "Type of substraction cannot be inferred from no arguments."
    | _   -> bad_number_of_args "(-)" (List.length args) 2
  )
  | Prim_mul -> (
      debug "[liqprim_to_loveprim] Multiplication@.";
      match args with
        [t1; t2] -> (
          match to_poly_variant t1, to_poly_variant t2 with
            `TInt, `TInt ->  "*", args
          | `TNat, `TNat -> "*+", args
          | `TNat, `TInt -> "*+!", args
          | `TInt, `TNat -> "*!+", args
          | `TDun, `TInt -> "*$!", args
          | `TDun, `TNat -> "*$+", args
          | `TNat, `TDun -> "*+$", args
          | `TInt, `TDun -> "*!$", args
          | _,_ ->
            debug "[liqprim_to_loveprim] Forbidden multiplication on %a and %a."
              Love_type.pretty t1 Love_type.pretty t2;
          error "Cannot multiply %a with %a"
            Love_type.pretty t1 Love_type.pretty t2;
        )
      | [t] ->
        error
          "Cannot infer multiplication primitive with only 1 argument of type %a"
          Love_type.pretty t
      | [] -> error "Type of multiplication cannot be inferred from no arguments"
      | _   -> bad_number_of_args "(*)" (List.length args) 2
    )
  | Prim_ediv -> (
      debug "[liqprim_to_loveprim] Division@.";
      match args with
        [t1; t2] -> (
        match to_poly_variant t1, to_poly_variant t2 with
          `TInt, `TInt -> "/", args
        | `TDun, `TInt -> "/$!", args
        | `TDun, `TDun -> "/$", args
        | `TInt, `TNat -> "/!+", args
        | `TNat, `TInt -> "/+!", args
        | `TDun, `TNat -> "/$+", args
        | `TNat, `TNat -> "/+$", args
        | _,_ ->
          debug "[liqprim_to_loveprim] Forbidden division on %a and %a."
            Love_type.pretty t1 Love_type.pretty t2;
          error "Cannot divide %a with %a"
            Love_type.pretty t1 Love_type.pretty t2;
      )
      | [t] ->
        error
          "Cannot infer correct division primitive with only 1 argument of type %a"
          Love_type.pretty t
    | []  -> error "Type of division cannot be inferred from no arguments"
    | _   -> bad_number_of_args "(/)" (List.length args) 2
  )
  | Prim_map_find -> (
      debug "[liqprim_to_loveprim] Map.find@.";
      match List.map to_poly_variant args with
        [_;`TMap _] -> "Map.find", args
      | [_;`TBigMap _ ] -> "BigMap.find", args
      | [_;_] ->
        error "Bad argument type %a for Map.find" Love_type.pretty (List.nth args 1)
      | _ -> bad_number_of_args "Map.find" (List.length args) 2
    )
  | Prim_map_add  -> (
      debug "[liqprim_to_loveprim] Map.add@.";
      match List.map to_poly_variant args with
        [_;_;`TMap _] -> "Map.add", args
      | [_;_;`TBigMap _ ] -> "BigMap.add", args
      | _ -> bad_number_of_args "Map.add" (List.length args) 3
    )
  | Prim_map_remove -> (
      debug "[liqprim_to_loveprim] Map.remove@.";
      match List.map to_poly_variant args with
        [_;`TMap _] -> "Map.remove", args
      | [_;`TBigMap _ ] -> "BigMap.remove", args
      | _ -> bad_number_of_args "Map.remove" (List.length args) 2
    )
  | Prim_map_mem -> (
      debug "[liqprim_to_loveprim] Map.mem@.";
      match List.map to_poly_variant args with
        [_;`TMap _] -> "Map.mem", args
      | [_;`TBigMap _] -> "BigMap.mem", args
      | _ -> bad_number_of_args "Map.mem" (List.length args) 2
    )
  | Prim_map_size -> (
      debug "[liqprim_to_loveprim] Map.size@.";
      match List.map to_poly_variant args with
        [`TMap _] -> "Map.cardinal", args
      | [`TBigMap _ ] -> error "Cannot calculate bigmap size in Love"
      | _ -> bad_number_of_args "Map.size" (List.length args) 1
    )

  | Prim_set_add -> debug "[liqprim_to_loveprim] Set.add@."; "Set.add", args
  | Prim_set_remove -> debug "[liqprim_to_loveprim] Set.remove@."; "Set.remove", args
  | Prim_set_mem -> debug "[liqprim_to_loveprim] Set.mem@."; "Set.mem", args
  | Prim_set_size -> debug "[liqprim_to_loveprim] Set.size@."; "Set.cardinal", args

  | Prim_list_size -> debug "[liqprim_to_loveprim] List.size@."; "List.length", args
  | Prim_list_rev  -> debug "[liqprim_to_loveprim] List.rev@."; "List.rev", args
  | Prim_blake2b -> debug "[liqprim_to_loveprim] PCBlake2b@."; "Crypto.blake2b", args
  | Prim_sha256 -> debug "[liqprim_to_loveprim] Sha256@."; "Crypto.sha256", args
  | Prim_sha512 -> debug "[liqprim_to_loveprim] Sha512@."; "Crypto.sha512", args
  | Prim_hash_key -> debug "[liqprim_to_loveprim] Hashkey@."; "Crypto.hash_key", args
  | Prim_check -> debug "[liqprim_to_loveprim] PCCheck@."; "Crypto.check", args
  | Prim_default_account -> debug "[liqprim_to_loveprim] Default@."; "Account.default", args
  | Prim_set_delegate -> debug "[liqprim_to_loveprim] SetDelegate@."; "Contract.set_delegate", args
  | Prim_address -> debug "[liqprim_to_loveprim] Address@."; "Contract.address", args
  | Prim_pack -> debug "[liqprim_to_loveprim] Pack@."; "Bytes.pack", args
  | Prim_Cons -> debug "[liqprim_to_loveprim] (::)@."; "List.cons", args
  | Prim_or -> (
      debug "[liqprim_to_loveprim] Or@.";
      match args with
        [t1; t2] -> (
          match to_poly_variant t1, to_poly_variant t2 with
          | `TBool, (`TBool | `TVar _ )
          | `TVar _ , `TBool              -> "||", [bool (); bool ()]
          | `TInt, (`TInt | `TVar _)
          | `TVar _, `TInt -> "lor", [int (); int ()]
          | _,_ ->
            error "Cannot apply OR on %a and %a"
              Love_type.pretty t1 Love_type.pretty t2;
        )
      | [t] -> (
          match to_poly_variant t with
          | `TBool -> "||", args
          | `TInt -> "lor", args
          | _ ->
            error "Cannot apply OR on %a and anything"
              Love_type.pretty t
        )
      | [] ->
        error "Cannot use OR without knowning the type of its arguments."
      | _ -> bad_number_of_args "OR" (List.length args) 2
    )
  | Prim_and -> (
      debug "[liqprim_to_loveprim] And@.";
      match args with
        [t1; t2] -> (
          match to_poly_variant t1, to_poly_variant t2 with
          | `TBool, (`TBool | `TVar _ )
          | `TVar _ , `TBool -> "&&", [bool (); bool ()]
          | `TInt, (`TInt | `TVar _)
          | `TVar _, `TInt -> "land", [int (); int ()]
          | _,_ ->
            error "Cannot apply AND on %a and %a"
              Love_type.pretty t1 Love_type.pretty t2;
        )
      | [t] -> (
          match to_poly_variant t with
            `TBool -> "&&", args
          | `TInt -> "land", args
          | _ ->
            error "Cannot apply AND on %a and anything"
              Love_type.pretty t
        )
      | [] -> error "Cannot use AND without knowning the type of its arguments."
      | _ -> bad_number_of_args "AND" (List.length args) 2
    )
  | Prim_xor -> (
      debug "[liqprim_to_loveprim] Primitive Xor";
      match args with
        [t1; t2] -> (
          match to_poly_variant t1, to_poly_variant t2 with
          | `TBool, (`TBool | `TVar _ )
          | `TVar _ , `TBool -> "|&", [bool (); bool ()]
          | `TInt, (`TInt | `TVar _)
          | `TVar _, `TInt -> "lxor",  [int (); int ()]
          | _,_ ->
            error "Cannot apply XOR on %a and %a"
              Love_type.pretty t1 Love_type.pretty t2;
        )
      | [t] -> (
          match to_poly_variant t with
            `TBool -> "|&", args
          | `TInt  -> "lxor", args
          | _ ->
            error "Cannot apply XOR on %a and anything"
              Love_type.pretty t
        )
      | [] ->
        error "Cannot use XOR without knowning the type of its arguments."
      | _ ->
        bad_number_of_args "XOR" (List.length args) 2
    )
  | Prim_not -> (
    debug "[liqprim_to_loveprim] Not@.";
    match List.map to_poly_variant args with
      [`TBool] -> "not", args
    | [`TInt]  -> "lnor", args
    | [_]->
      error "Cannot apply NOT on %a"
        Love_type.pretty (List.hd args)
    | _ ->
      bad_number_of_args "NOT" (List.length args) 1
  )
  | Prim_abs -> "abs", args
  | Prim_neg -> "~-", args
  | Prim_lsr -> (
      match List.map to_poly_variant args with
        `TInt :: _ -> "lsr", args
      | `TNat :: _ -> "nlsr", args
      | _ :: _ ->
        error "Cannot apply lsr on %a"
          Love_type.pretty (List.hd args)
      | _ ->
        bad_number_of_args "lsl" (List.length args) 2
    )

  (* primitives *)

  | Prim_lsl -> (
      match List.map to_poly_variant args with
        `TInt :: _ -> "lsl", args
      | `TNat :: _ -> "nlsl", args
      | _ :: _ ->
        error "Cannot apply lsr on %a"
          Love_type.pretty (List.hd args)
      | _ ->
        bad_number_of_args "lsl" (List.length args) 2
    )

  | Prim_bytes_size -> "Bytes.length", args
  | Prim_string_size -> "String.length", args

  | Prim_slice -> (
    debug "[liqprim_to_loveprim] Slice@.";
    match args with
      [] -> bad_number_of_args "slice" 0 1
    | _ -> let last = List.last_exn args in
      match to_poly_variant last with
      | `TString -> "String.slice", args
      | `TBytes -> "Bytes.slice", args
      | _ ->
        error "Cannot apply slice on %a" Love_type.pretty last
  )
  | Prim_bytes_sub -> "Bytes.slice", args
  | Prim_string_sub -> "String.slice", args

  (*(
    match typ with
      TString -> PSConcat
    | TBytes -> PBConcat
    | _ -> failwith "Concat on non string and non byte type"
                                   ) *)
  | Prim_concat_two -> assert false (* must be treated elsewhere *)
  | Prim_string_concat -> "String.concat", args
  | Prim_bytes_concat -> "Bytes.concat", args
  | Prim_block_level -> "Current.level", args
  | Prim_get_balance -> "Current.balance", args


  | Prim_tuple_get | Prim_tuple_set| Prim_tuple
  | Prim_Left | Prim_Right
  | Prim_map_update
  | Prim_set_update
  | Prim_Some
  | Prim_is_nat
  | Prim_int
  | Prim_exec _
  | Prim_concat ->
    error
      "Invariant broken: Primitive %s should have been treated elsewhere"
      (LiquidTypes.string_of_primitive p)

  (* resolved in LiquidCheck *)
  | Prim_coll_find
  | Prim_coll_update
  | Prim_coll_mem
  | Prim_coll_size ->
    error
      "Invariant broken: After LiquidCheck, primitive %s should not be in the AST"
      (LiquidTypes.string_of_primitive p)

  (* extended primitives *)
  | Prim_extension (s1, _b, _l, _i1, _i2, _s2) ->
    error "Primitive %s (%s) is unsupported" s1 (LiquidTypes.string_of_primitive p)
  (* generated in LiquidCheck *)
  | Prim_unused _
  | Prim_is_implicit
  | Prim_collect_call
  | Prim_big_map_create
  | Prim_address_untype
  | Prim_chain_id ->
    error "Primitive %s is unsupported" (LiquidTypes.string_of_primitive p)

let liqprimfold_to_loveexp
    (env : env)
    (prim : LiquidTypes.prim_fold)
    (arg_name : string)
    (arg : AST.exp)
    (arg_typ : TYPE.t)
    (acc : AST.exp)
    (acc_typ : TYPE.t)
    (body_maker: env -> typed_exp -> AST.exp * TYPE.t)
    (body : typed_exp)
  : AST.exp * TYPE.t =
  let v i = mk_var @@ string_to_ident i in
    match prim with
    | Prim_map_iter ->
      debug "[liqprimfold_to_loveexp] Map.iter@.";
      let cprim, t1, t2 =
        match to_poly_variant arg_typ with
          `TMap (t1, t2) -> "Map.iter", t1, t2
        | `TBigMap _ -> error "BigMap.iter does not exist in Love"
        | _ ->
          cannot_apply "Map.iter" arg arg_typ
      in
      let env = Compil_utils.add_var arg_name (TTuple [t1; t2]) env
      in
      let bdy, typ = body_maker env body in
      debug "[liqprimfold_to_loveexp] Body type = %a@." Love_type.pretty typ;

      mk_apply
        (Compil_utils.mk_primitive_lambda
           env
           cprim
           ((t1 @=> t2 @=> unit ()) @=> arg_typ @=> unit ())
           ANone
        )
        [Compil_utils.put_in_arrow
            ["__key", t1; "__bind", t2]
            (mk_let
               (mk_pvar arg_name)
               (mk_tuple [v "__key"; v "__bind"] )
               bdy
            );
         arg], typ
    | Prim_set_iter ->
      debug "[liqprimfold_to_loveexp] Set.iter@.";
      let t =
        match to_poly_variant arg_typ with
          `TSet t -> t
        | _ -> cannot_apply "Set.iter" arg arg_typ
      in
      let env = Compil_utils.add_var arg_name t env
      in
      let bdy, typ = body_maker env body in
      debug "[liqprimfold_to_loveexp] Body type = %a@." Love_type.pretty typ;
      mk_apply
        (mk_primitive_lambda
           env
           "Set.iter"
           ((t @=> unit ()) @=> set t)
           ANone
        )
        [mk_lambda (mk_pvar arg_name) bdy t; arg], typ
    | Prim_list_iter ->
      debug "[liqprimfold_to_loveexp] List.iter@.";
      let t =
        match to_poly_variant arg_typ with
          `TList t -> t
        | _ -> cannot_apply "List.iter" arg arg_typ
      in
      let env = Compil_utils.add_var arg_name t env
      in
      let bdy, typ = body_maker env body in
      debug "[liqprimfold_to_loveexp] Body type = %a@." Love_type.pretty typ;
      mk_apply
        (mk_primitive_lambda env "List.iter" ((t @=> unit ()) @=> list t @=> unit ()) ANone)
        [mk_lambda (mk_pvar arg_name) bdy t; arg], typ
    | Prim_map_fold ->
      debug "[liqprimfold_to_loveexp] Map.fold@.";
      let cprim, t1, t2 =
        match to_poly_variant arg_typ with
          `TMap (t1, t2) -> "Map.fold", t1, t2
        | `TBigMap _ ->
          error "Expression %a is a bigmap. Fold on big maps is forbidden in Love"
            Love_printer.Ast.print_exp arg
        | _ -> cannot_apply "Map.fold" arg arg_typ
      in
      let vars = ["__key", t1; "__bind", t2; "__acc", acc_typ] in
      let env = Compil_utils.add_var arg_name (TTuple [TTuple [t1; t2]; acc_typ]) env
      in
      let bdy, typ = body_maker env body in
      debug "[liqprimfold_to_loveexp] Body type = %a@." Love_type.pretty typ;
      let prim =
        mk_primitive_lambda env
          cprim
          ((t1 @=> t2 @=> acc_typ @=> acc_typ) @=> arg_typ @=> acc_typ @=> acc_typ) ANone in
      debug "[liqprimfold_to_loveexp] Primitive = %a@." Love_printer.Ast.print_exp prim;
      mk_apply
        prim
        [Compil_utils.put_in_arrow
           vars
           (mk_let
              (mk_pvar arg_name)
              (mk_tuple [mk_tuple [v "__key"; v "__bind"]; v "__acc"])
              bdy
           );
         arg;
         acc], typ
    | Prim_set_fold ->
      debug "[liqprimfold_to_loveexp] Set.fold@.";
      let t =
        match to_poly_variant arg_typ with
          `TSet t -> t
        | _ -> cannot_apply "Set.fold" arg arg_typ
      in
      let vars = ["__key", t; "__acc", acc_typ] in
      let env =
          Compil_utils.add_var
            arg_name
            (TTuple [t; acc_typ])
            env
      in
      let bdy, typ = body_maker env body in
      debug "[liqprimfold_to_loveexp] Body type = %a@." Love_type.pretty typ;
      mk_apply
        (mk_primitive_lambda env
           "Set.fold"
           ((t @=> acc_typ @=> acc_typ) @=> set t @=> acc_typ @=> acc_typ)
           ANone
        )
        [Compil_utils.put_in_arrow
           vars
           (mk_let
              (mk_pvar arg_name)
              (mk_tuple [v "__key";v "__acc"])
              bdy
           );
         arg;
         acc], typ
    | Prim_list_fold ->
      debug "[liqprimfold_to_loveexp] List.fold@.";
      let t =
        match to_poly_variant arg_typ with
          `TList t -> t
        | _ -> cannot_apply "List.fold" arg arg_typ
      in
      let vars = ["__key", t; "__acc", acc_typ] in
      let env = Compil_utils.add_var arg_name (TTuple [t; acc_typ]) env in
      let bdy, typ = body_maker env body in
      debug "[liqprimfold_to_loveexp] Body type = %a@." Love_type.pretty typ;
      mk_apply
        (mk_primitive_lambda env
           "List.fold"
           ((t @=> acc_typ @=> acc_typ) @=> list t @=> acc_typ @=> acc_typ)
           ANone
        )
        [Compil_utils.put_in_arrow
           vars
           (mk_let
              (mk_pvar arg_name)
              (mk_tuple [v "__key";v "__acc"])
              bdy
           );
         arg;
         acc], t
    | Prim_coll_iter
    | Prim_coll_fold ->
      error
        "Generic iterator %s is forbidden in Love"
        (LiquidTypes.string_of_fold_primitive prim)

let liqprimmap_to_loveexp
    (env : env)
    (prim : LiquidTypes.prim_map)
    (arg_name : string)
    (body_maker : env -> typed_exp -> AST.exp * TYPE.t)
    (body : typed_exp)
    arg
    arg_typ =
  let v i = mk_var @@ string_to_ident i in
  match prim with
  | Prim_map_map ->
    let cprim, t1, t2, tbuilder =
      match to_poly_variant arg_typ with
        `TMap (t1, t2) -> "Map.map", t1, t2, (fun t -> map (t1, t))
      | `TBigMap _ ->
        error
          "Expression %a is a bigmap. Mapping on a big map is forbidden in Love"
          Love_printer.Ast.print_exp arg
      | _ ->
        cannot_apply
          "Map.map"
          arg
          arg_typ
    in
    let vars = ["__key", t1; "__bnd", t2] in
    let env = Compil_utils.add_var arg_name (TTuple [t1; t2]) env in
    let bdy, typ = body_maker env body in
    mk_apply
      (mk_primitive_lambda env
         cprim
         ((t1 @=> t2 @=> typ) @=> arg_typ @=> tbuilder typ)
         ANone
      )
      [Compil_utils.put_in_arrow
         vars
         (mk_let
            (mk_pvar arg_name)
            (mk_tuple [v "__key";v "__bnd"])
            bdy
         );
       arg
      ], map (t1, typ)
  | Prim_list_map ->
      let t =
        match to_poly_variant arg_typ with
          `TList t -> t
        | _ ->
          cannot_apply
            "List.map"
            arg
            arg_typ
      in
      let env = Compil_utils.add_var arg_name t env in
      let bdy, typ = body_maker env body in
      mk_apply
        (mk_primitive_lambda env
           "List.map"
           ((t @=> typ) @=> list t @=> list typ)
        ANone)
        [mk_lambda (mk_pvar arg_name) bdy t; arg], list typ
  | Prim_coll_map ->
    error "Generic map %s forbidden" (LiquidTypes.string_of_map_primitive prim)

let liqprimmapfold_to_loveexp
    (env : env)
    prim
    (arg_name : string)
    (body_maker : env -> typed_exp -> AST.exp * TYPE.t)
    (body : typed_exp)
    arg
    arg_typ
    acc
    acc_typ =
  let v i = mk_var @@ string_to_ident i in
  match prim with
  | Prim_map_map_fold ->
    let cprim, t1, t2, tbuilder =
      match to_poly_variant arg_typ with
        `TMap (t1, t2) -> "Map.map_fold", t1, t2, (fun t -> map (t1,t))
      | `TBigMap (t1, t2) ->
        error
          "Expression %a is a bigmap. Fold-mapping on a big map is forbidden in Love"
          Love_printer.Ast.print_exp arg
      | _ -> cannot_apply "Map.map_fold" arg arg_typ
    in
    let vars = ["__key", t1; "__bnd", t2; "__acc", acc_typ] in
    let env =
        Compil_utils.add_var arg_name (TTuple [TTuple [t1; t2]; acc_typ]) env in
    let bdy, typ = body_maker env body in
    let typ_newbnd, typ_acc =
      match typ with
        TTuple [t1; t2] -> t1, t2
      | t ->
        debug
          "[liqprimmapfold_to_loveexp] Return type should be a pair, but it is %a"
          Love_type.pretty t;
        error
          "The return type of %a should be a tuple, not of type %a."
          Love_printer.Ast.print_exp bdy
          Love_type.pretty arg_typ
    in
    mk_apply
      (mk_primitive_lambda env
         cprim
         ((t1 @=> t2 @=> acc_typ @=> TTuple [typ_newbnd; typ_acc])
          @=> arg_typ @=> typ_acc @=> TTuple [tbuilder typ_newbnd; typ_acc])
         ANone
      )
      [Compil_utils.put_in_arrow
         vars
         (mk_let
            (mk_pvar arg_name)
            (mk_tuple [mk_tuple [v "__key";v "__bnd"]; v "__acc"])
            bdy
         );
       arg;
       acc
      ], TTuple [map (t1, typ_newbnd); typ_acc]
  | Prim_list_map_fold ->
      let t =
        match to_poly_variant arg_typ with
          `TList t -> t
        | _ ->
          cannot_apply
            "List.map_fold"
            arg
            arg_typ
      in
      let vars = ["__elt", t; "__acc", acc_typ] in
      let env = Compil_utils.add_var arg_name (TTuple [t; acc_typ]) env in
      let bdy, typ = body_maker env body in
      let typ_newbnd, typ_acc =
        match typ with
          TTuple [t1; t2] -> t1, t2
        | t ->
          debug
            "[liqprimmapfold_to_loveexp] Return type should be a pair, but it is %a"
            Love_type.pretty t;
        error
          "The return type of %a should be a tuple, not of type %a."
          Love_printer.Ast.print_exp bdy
          Love_type.pretty arg_typ
      in
      mk_apply
        (mk_primitive_lambda env
           "List.map_fold"
           ((t @=> typ_acc @=> TTuple [typ_newbnd; typ_acc])
            @=> list t @=> typ_acc @=> TTuple [list typ_newbnd; typ_acc])
           ANone
        )
        [Compil_utils.put_in_arrow
           vars
           (mk_let
              (mk_pvar arg_name)
              (mk_tuple [v "__elt"; v "__acc"])
              bdy
           );
         arg;
         acc
        ], TTuple [list typ_newbnd; typ_acc]
  | Prim_coll_map_fold ->
    error
      "Generic map fold %s is forbidden"
      (LiquidTypes.string_of_map_fold_primitive prim)


let rec liqconst_to_loveexp
    ?typ (env : env) (c : (datatype, typed) LiquidTypes.exp LiquidTypes.const)
  : AST.exp * TYPE.t =
  let ltl ?typ = liqconst_to_loveexp ?typ env in
  let () = match typ with
      None ->
      debug "[liqconst_to_loveexp] Warning : calling const to exp with no type.@."
    | Some t ->
      debug "[liqconst_to_loveexp] Calling const to exp with type %a.@." Love_type.pretty t in
  let res =
    match c with
      CUnit   -> mk_const @@ mk_cunit (), unit ()
    | CBool b -> mk_const @@ mk_cbool b, bool ()
    | CInt i  -> mk_const @@ mk_cint i.integer, int ()
    | CNat n  -> mk_const @@ mk_cnat n.integer, nat ()
    | CTez {tezzies;mutez} -> (
        let str =
          let mutez = match mutez with
            | Some mutez -> mutez
            | None  -> "000000"
          in
          tezzies ^ mutez
        in
        match Tez_repr.of_string str with
          None -> error "%s is not a correct dun amount" str
        | Some d -> mk_const @@ mk_cdun @@ Tez_repr.to_int64 d
      ), dun ()
    | CTimestamp s -> (
        debug "[liqconst_to_loveexp] CTimestamp : %s@." s;
        match Script_timestamp_repr.of_string s with
          None -> error "Timestamp %s has no integer representation" s
        | Some ts -> mk_const @@ mk_ctimestamp ts
      ), timestamp ()
    | CString s -> mk_const @@ mk_cstring s, string ()
    | CBytes b ->
      mk_const @@ mk_cbytes (MBytes.of_string b), bytes ()
    | CKey k -> (
      match Signature.Public_key.of_b58check_opt k with
          None -> error "Key %s is invalid" k
        | Some k -> mk_const @@ mk_ckey k, key ()
    )
    | CSignature s -> (
      match Signature.of_b58check_opt s with
          None -> error "Signature %s is invalid" s
        | Some s -> mk_const @@ mk_csig s, signature ()
    )
    | CTuple ([] | [_] as l) ->
      debug "[liqconst_to_loveexp] Error : tuple has %i elements@." (List.length l);
      error "Expression %s is a tuple with %d elements: this is an invalid expression"
        (LiquidPrinter.Liquid.string_of_const c)
        (List.length l)
    | CTuple l -> (
        match typ with
          None ->
          let cl, tl = List.split @@ List.map ltl l
          in
          mk_tuple cl, TTuple tl (* Set as a tuple expression because it may contain
                                    polymorphic types *)
        | Some ((TTuple tl) as ty) ->
          debug "[liqconst_to_loveexp] Tuple of type %a matched with tuple with %i elements@."
            Love_type.pretty ty (List.length l);
          let tuple_elt, tup_typ = (
            List.fold_left2
              (fun acc elt typ -> ltl ~typ elt :: acc)
              []
              l
              tl) |> List.rev |> List.split
          in
          mk_tuple tuple_elt, TTuple tup_typ
        | Some t ->
          debug "[liqconst_to_loveexp] Error : CTuple has type %a" Love_type.pretty t;
          error "Expression %s has type %a, while it was expected to be a tuple"
            (LiquidPrinter.Liquid.string_of_const c)
            Love_type.pretty t
      )

    | CNone -> (
        match typ with
          None ->
          debug "[liqconst_to_loveexp] Error : Making CNone with no type.";
          error "None constructor expects a type, but it is not provided."
        | Some t -> (
            match is_option t with
              Some opt -> mk_none opt, t
            | None ->
              debug "[liqconst_to_loveexp] Error : CNone has type %a" Love_type.pretty t;
              bad_const_type c t "option"
          )
      )
    | CSome c -> (
        match typ with
          None -> error "Error : CSome must be typed"
        | Some t ->
          let typ =
            match is_option t with
            | Some t -> t
            | None -> bad_const_type c t "option"
          in
          let c, t = ltl ~typ c in
          mk_some typ c, option t
      )

    | CMap l ->  (
        let empty, typ, tkey, telt =
          match Option.map ~f:to_poly_variant typ, l with
            None,[] ->
            mk_emptymap (),
            Love_type.type_empty_map,
            TVar Love_type.tvmkey,
            TVar Love_type.tvmbnd
          | None, (hdk, hdb) :: _ ->
            let _, tk = liqconst_to_loveexp env hdk in (* todo : done twice on list head *)
            let _, tb = liqconst_to_loveexp env hdb in (* todo : done twice on list head *)
            mk_emptymap ~typs:(tk, tb) (), map (tk,tb), tk, tb
          | Some (`TMap (typ_key,typ_bnd)),_ ->
            mk_emptymap ~typs:(typ_key, typ_bnd) (),
            map (typ_key, typ_bnd),
            typ_key,
            typ_bnd
          | Some t,_ -> bad_const_type c (the typ) "map"
        in (
          List.fold_left
            (fun map (new_key,new_bnd) ->
               mk_apply (mk_primitive_lambda env "Map.add" (tkey @=> telt @=> typ @=> typ) ANone)
                 [fst @@ liqconst_to_loveexp ~typ:tkey env new_key;
                  fst @@ liqconst_to_loveexp ~typ:telt env new_bnd;
                  map]
            )
            empty
            l), typ
      )
    | CBigMap (BMId _) ->
      error
        "Compilation failed on %s: bigmaps are not yet accessible by their id in Love"
         (LiquidPrinter.Liquid.string_of_const c)
    | CBigMap (BMList l) -> begin
        let empty, typ, tkey, telt =
          match Option.map ~f:to_poly_variant typ, l with
            None, [] ->
            error
              "Constant %s is a typeless bigmap: bigmaps types must be known in Love"
              (LiquidPrinter.Liquid.string_of_const c)
          | None, (hdk, hdb) :: _ ->
            let _, tk = liqconst_to_loveexp env hdk in (* todo : done twice on list head *)
            let _, tb = liqconst_to_loveexp env hdb in (* todo : done twice on list head *)
            mk_emptybigmap tk tb, bigmap (tk,tb), tk, tb
          | Some (`TBigMap (typ_key,typ_bnd)),_ ->
            mk_emptybigmap typ_key typ_bnd,
            bigmap (typ_key,typ_bnd),
            typ_key,
            typ_bnd
          | Some t,_ ->
            debug "[liqconst_to_loveexp] Error : CBigMap has incorrect type";
            bad_const_type c (the typ) "bigmap"
        in (
          List.fold_left
            (fun bm (new_key,new_bnd) ->
               mk_apply (mk_primitive_lambda env "BigMap.add" (tkey @=> telt @=> typ @=> typ) ANone)
                 [fst @@ liqconst_to_loveexp ~typ:tkey env new_key;
                  fst @@ liqconst_to_loveexp ~typ:telt env new_bnd;
                  bm]
            )
            empty
            l), typ
      end
    | CList [] -> (
        debug "[liqconst_to_loveexp] Empty CList";
        match Option.map ~f:to_poly_variant typ with
          None ->
          debug "[liqconst_to_loveexp] Empty CList without type";
          mk_enil (), Love_type.type_empty_list
        | Some (`TList t) -> mk_enil ~typ:t (), list t
        | Some t ->
          bad_const_type c (the typ) "list"
      )
    | CList l ->
      let new_l, t =
        List.fold_left
          (fun (acc_l, acc_t) elt ->
             match acc_t with
               TVar _ -> let e, t = ltl elt in e :: acc_l, t
             | _ ->
               let e, t = ltl ~typ:acc_t elt in
               match acc_t, t with
               | TForall _, TForall _ -> e :: acc_l, t
               | TForall _, _ ->
                 let new_acc_l =
                   List.map (fun e -> mk_tapply e t) acc_l in
                 (e :: new_acc_l), t
               | _,_ -> e :: acc_l, t
          )
          ([], TVar (Love_type.fresh_typevar ()))
          l
      in mk_list ~typ:t (List.rev new_l), list t

    | CSet l -> (
        let empty, typ, telt =
          match Option.map ~f:to_poly_variant typ, l with
            None,[] -> mk_emptyset (), Love_type.type_empty_set, TVar Love_type.tvset
          | None, hd :: _ ->
            let _, thd = liqconst_to_loveexp env hd in (* todo : done twice on list head *)
            mk_emptyset ~typ:thd (), set thd, thd
          | Some (`TSet t),_ -> mk_emptyset ~typ:t (), set t, t
          | Some t,_ ->
            bad_const_type c (the typ) "set"
        in (
          List.fold_left
            (fun set new_val ->
               mk_apply (mk_primitive_lambda env "Set.add" (telt @=> typ @=> typ) ANone)
                 [fst @@ liqconst_to_loveexp ~typ:telt env new_val;
                  set]
            )
            empty
            l), typ
      )

    | CLeft c ->
      let (c, t1),t2 =
        match Option.map to_poly_variant typ with
          None ->
          error
            "Constant %s must be typed: no type has been provided"
            (LiquidPrinter.Liquid.string_of_const c)
        | Some (`TVariant (typ, t2)) ->
          debug "Type for left : (%a, %a) tor"
            Love_type.pretty typ
            Love_type.pretty t2;
            liqconst_to_loveexp ~typ env c, t2
        | Some _ ->
          bad_const_type c (the typ) "variant"
      in
      mk_constr (string_to_ident "Left") [t1; t2] [c],
      variant (t1, t2)

    | CRight c ->
      let (c, t2),t1 =
        match typ with
          None ->
          error
            "Constant %s must be typed: no type has been provided"
            (LiquidPrinter.Liquid.string_of_const c)
        | Some (TUser (_, [t1; typ])) ->
          debug "Type for left : (%a, %a) tor"
            (Love_type.pretty ) t1 (Love_type.pretty ) typ;
          liqconst_to_loveexp ~typ env c, t1
        | Some _ -> bad_const_type c (the typ) "variant"
      in
      mk_constr (string_to_ident "Right") [t1; t2] [c],
      variant (t1, t2)

    | CKey_hash kh -> (
        match Signature.Public_key_hash.of_b58check_opt kh with
          None -> error "Keyhash %s is invalid" kh
        | Some k -> mk_const @@ mk_ckeyhash k, keyhash ()
      )
    | CContract (c, _) ->
      error "Constant contracts (here %s) has no Love representation." c
    | CRecord [] ->
      error "Empty records are forbidden in Love"
    | CRecord (((name, _) :: _) as l) -> (* May be source of errors on polymorphic records *)
      (*let id_name = string_to_ident name in *)
      debug "[liqconst_to_loveexp] Constant record";
      let field_path = string_to_ident name in
      let path, field_name = ident_split_end field_path in
      let find_field =
        match path with
          None -> Love_tenv.find_field
        | Some path -> Love_tenv.find_field_in path
      in
      let parent_typ =
        match find_field field_name env with
          None -> debug "[liqconst_to_loveexp] Field %s is unknown" name;
          error "Unknown field %s in record %s"
            name
            (LiquidPrinter.Liquid.string_of_const c)
        | Some {result = {fparent; _}} -> fparent
      in
      mk_record path (
        List.map
          (fun (name, c) -> (*
          let (id_name : string Ident.t) = string_to_ident name in *)
             let field_name = Ident.get_final (string_to_ident name) in
             let typ =
               match find_field field_name env with
                 None -> debug "[liqconst_to_loveexp] Field %s is unknown" name;
                 error "Unknown field %s in record %s"
                   name
                   (LiquidPrinter.Liquid.string_of_const c)
               | Some {result = {ftyp; _}} -> ftyp
             in
             name, (fst @@ ltl ~typ c)
          )
          l
      ), parent_typ
    | CConstr (name, args) ->
      (* Todo : add type arguments to CConstr *)
      debug "[liqconst_to_loveexp] Creating constant constructor %s@." name;
      let id_name = string_to_ident name in
      let t, targs =
        match Love_tenv.find_constr id_name env with
          None ->
          debug "[liqconst_to_loveexp] Constructor %s is unknown" name;
          error "Error in constant %s: constructor %s is unknown"
            (LiquidPrinter.Liquid.string_of_const c)
            name
        | Some {result = {cparent;cargs = [t];_}; _} ->
          (* In liquidity, constructors arguments are tuples. *)
          cparent, t
        | _ -> assert false
      in mk_constr id_name [] [fst @@ ltl ~typ:targs args], t

    | CLambda {arg_name; arg_ty; body; ret_ty; recursive} ->
      error "Constant lambdas are forbidden in Love"
  in
  let () = match typ with
      None ->
      debug
        "[liqconst_to_loveexp] Untyped const %a : %a.@."
        Love_printer.Ast.print_exp (fst res) Love_type.pretty (snd res)
    | Some t ->
      debug
        "[liqconst_to_loveexp] Const %a : %a@."
        Love_printer.Ast.print_exp (fst res) Love_type.pretty (snd res)
  in res
(** Some primitives need a special treatment done by this function  *)
and liqapply_to_loveexp env typ prim args : AST.exp * TYPE.t =
  let ltl = liqexp_to_loveexp env in
  match prim,args with
  | Prim_tuple_get, tuple :: index :: [] -> begin
    debug "[liqapply_to_loveexp] Creating tuple projection@.";
    let index = 
      match index with
        {desc = Const {const = CInt i; _}; _}
      | {desc = Const {const = CNat i; _}; _} -> i.integer
      | sthg -> (
          debug
            "[liqapply_to_loveexp] Index is not a liquidity constant,\
             checking love representation@.";
          match (fst @@ ltl sthg) with
            AST.{ content = Const { content = (CInt i); _ } } -> i
          | cst ->
            debug "[liqapply_to_loveexp] Tuple projections must have an integer as index.\n\
                   Index = %a@."
              Love_printer.Ast.print_exp cst;
            error "Tuple %s is projected on %a: invalid projection@."
              (LiquidPrinter.Liquid.string_of_code tuple)
              Love_printer.Ast.print_exp cst
        )
    in
    let iindex = Z.to_int index in
    debug "[liqapply_to_loveexp] Projection with index = %a@." Z.pp_print index;
      let tup, ty = ltl tuple in
      let ty = Love_tenv.normalize_type ~relative:true ty env in
      debug
        "[liqapply_to_loveexp] Tuple %a : %a@."
        Love_printer.Ast.print_exp tup Love_type.pretty ty;
      match ty with
        TTuple typ_list -> mk_projection tup [iindex], List.nth typ_list iindex
      | t ->
        debug "[liqapply_to_loveexp] Tuple has type %a@." Love_type.pretty t;
        bad_exp_type tup ty "tuple"
  end
  | Prim_tuple_get, l ->
    error
      "Tuple projection expects 2 arguments : here it is applied to %i"
      (List.length l)

  | Prim_tuple_set, tuple :: index :: content :: [] -> begin
    debug "[liqapply_to_loveexp] Creating tuple update@.";
    let index =
      match index with
        {desc = Const {const = CInt i; _}; _} -> i.integer
      | {desc = Const {const = CNat i; _}; _} -> i.integer
      | sthg -> (
          debug
            "[liqapply_to_loveexp] Index is not a liquidity constant,\
             checking love representation@.";
          match fst @@ ltl sthg with
            {content = Const {content = CInt i; _}; _} -> i
          | cst ->
            debug "[liqapply_to_loveexp] Tuple update must have an integer as index.\n\
                   Index = %a@."
              Love_printer.Ast.print_exp cst;
            error "Tuple %s is updated on invalid component %a@."
              (LiquidPrinter.Liquid.string_of_code tuple)
              Love_printer.Ast.print_exp cst
        )
    in
    let iindex = Z.to_int index in
    debug "[liqapply_to_loveexp] Projection with index = %a@." Z.pp_print index;
    let tup, typ = ltl tuple in
    let typ = Love_tenv.normalize_type ~relative:true typ env in
    let content, _ = ltl content in
    match typ with
      TTuple _ -> mk_update tup [iindex, content], typ
    | t -> bad_exp_type tup t "tuple"
  end
  | Prim_tuple_set, l ->
    error "Tuple update expects 3 arguments : here it is applied to %i" (List.length l)

  | Prim_tuple,_ ->
    debug "[liqapply_to_loveexp] Creating a tuple@.";
    let args, typs = List.split @@ List.map ltl args in
    mk_tuple args, TTuple typs
  | Prim_Some, [arg] ->
    debug "[liqapply_to_loveexp] Creating Some@.";
    let e, t = ltl arg in
    mk_some t e, option t

  | Prim_concat, []
  | Prim_concat_two, [] ->
    error "Invariant broken: Concatenation of nothing is forbidden."
  | Prim_concat, ((hd :: _) as l)
  | Prim_concat_two, ((hd :: _) as l) ->
    debug "[liqapply_to_loveexp] Creating a concatenation@.";
    let ty = liqtype_to_lovetype env typ in
    let prim =
      match to_poly_variant ty with
        `TString -> "String.concat"
      | `TBytes -> "Bytes.concat"
      | _ ->
        error "Error while applying concat on %s of type %a: \
               concatenation is only possible on strings and bytes"
          (LiquidPrinter.Liquid.string_of_code hd)
          Love_type.pretty ty
    in
    mk_apply
      (Compil_utils.mk_primitive_lambda env prim (list ty @=> ty) ANone)
      [mk_list ~typ:ty @@ List.map (fun e -> fst @@ ltl e) l], ty

  | Prim_is_nat, [e] -> (
    (* if e >= 0 then Some e else None *)
      let lovee, te = ltl e in
      mk_if
        (mk_apply (Compil_utils.mk_primitive_lambda env "<=" (te @=> te @=> bool ()) ANone)
           [lovee; mk_const @@ mk_cint Z.zero])
        (mk_some (nat ()) lovee)
        (mk_none (nat ())), option (nat ())
    )
  | Prim_int, [cst] -> (
      debug "[liqapply_to_loveexp] Creating a cast@.";
      let c, ty = liqexp_to_loveexp env cst in
      match to_poly_variant ty with
        `TInt -> c, ty
      | `TNat -> mk_apply (mk_var (string_to_ident "Int.of_nat")) [c], int ()
      | _ ->
        error
          "Unsafe cast of %a : %a to an integer@."
          Love_printer.Ast.print_exp c
          Love_type.pretty ty
    )
  | Prim_int, l ->
    bad_number_of_args "Prim_int" (List.length l) 1

  | Prim_set_update, [elt; b; set] ->
    let elt,key = ltl elt in
    let b,_bool = ltl b in
    let set,t = ltl set in
    let prim_typ = key @=> t @=> t in
    mk_if
      b
      (mk_apply (Compil_utils.mk_primitive_lambda env "Set.add"    prim_typ ANone) [elt; set])
      (mk_apply (Compil_utils.mk_primitive_lambda env "Set.remove" prim_typ ANone) [elt; set]), t
  | Prim_set_update, l ->
    bad_number_of_args "Set.update" (List.length l) 3
  | Prim_map_update, [elt; valopt; emap] ->
    debug "[liqapply_to_loveexp] Map.update@.";
    let elt, telt = ltl elt in
    let valopt, tvalopt = ltl valopt in
    let tval =
      match is_option tvalopt with
        Some t -> t
      | None -> bad_exp_type valopt tvalopt "option" in
    let emap, t = ltl emap in
    let prim_add, prim_rem =
      match to_poly_variant t with
        `TMap _ -> "Map.add", "Map.remove"
      | `TBigMap _ -> "BigMap.add", "BigMap.remove"
      | _ ->
        debug "[liqapply_to_loveexp] Incorrect type %a for map" Love_type.pretty t;
        bad_exp_type emap t "map"
    in
    mk_match
      valopt
      [mk_pnone (),
       mk_apply (
         Compil_utils.mk_primitive_lambda
           env
           prim_rem
           (telt @=> map (telt, tval) @=> map (telt, tval))
           ANone
       )
         [elt; emap];
       mk_psome (mk_pvar "__v"),
       mk_apply (
           Compil_utils.mk_primitive_lambda
            env
            prim_add
            (telt @=> tval @=> map (telt, tval) @=> map (telt, tval))
            ANone
         )
         [elt; mk_var (string_to_ident "__v"); emap]
      ], t
  | Prim_map_update, l ->
    bad_number_of_args "Map.update" (List.length l) 3
  | Prim_Left, [arg] ->
    debug "[liqapply_to_loveexp] Primitive Left@.";
    let a, t = ltl arg in
    let t2 =
      match typ with
        Tor (_, t2) -> liqtype_to_lovetype env t2
      | t ->
        error
          "Applying Prim_left is expected to return a variant type, but it returns %s"
          (LiquidPrinter.Liquid.string_of_type t)
    in
    (mk_constr (string_to_ident "Left") [t; t2] [a]), variant (t, t2)
  | Prim_Right, [arg] ->
    debug "[liqapply_to_loveexp] Primitive Right @.";
    let a, t = ltl arg in
    let t1 =
      match typ with
        Tor (t1, _) -> liqtype_to_lovetype env t1
      | t ->
        error
          "Applying Prim_right is expected to return a variant type, but it returns %s"
          (LiquidPrinter.Liquid.string_of_type t)
    in
    mk_constr (string_to_ident "Right") [t1; t] [a],
    variant (t1, t)
  | Prim_exec _, _ -> error "Invariant broken: Prim_exec is solved before"
  | _ ->
    debug "[liqapply_to_loveexp] Love primitive@.";
    let love_args_typ =
      List.fold_left
        (fun acc a ->
           let arg, t = ltl a in
           debug "[liqapply_to_loveexp] Primitive argument is %a@."
             Love_printer.Ast.print_exp arg;
           (arg, t) :: acc)
        []
        args |> List.rev
    in
    let first_version_of_typs = snd (List.split love_args_typ) in
    let prim_name, lovetlist = liqprim_to_loveprim env prim first_version_of_typs in
    let prim =
      match Love_primitive.from_string prim_name with
        None -> error "Primitive %s does not exist in Love" prim_name
      | Some p -> p in
    let prim_typ = Love_primitive.type_of (prim, ANone) in
    debug
      "[liqapply_to_loveexp] Applying correct types based on %a@."
      Love_type.pretty prim_typ;
    let love_args_typ = Compil_utils.merge_args_with_funtyp love_args_typ prim_typ in
    let loveargs, lovetlist = List.split love_args_typ in
    debug "[liqapply_to_loveexp] Type of prim is %a, with %i args.@."
      Love_type.pretty prim_typ
      (List.length loveargs);
    debug "[liqapply_to_loveexp] Args = %a.@."
      (Format.pp_print_list Love_printer.Ast.print_exp) loveargs;
    let t = Compil_utils.return_type_with_args env prim_typ lovetlist
    in
    debug "[liqapply_to_loveexp] Return type of primitive is %a.@."
      Love_type.pretty t;
    (* Post processing : some primitives have a special type *)
    match prim_name, args with
    | "/$", [_;_] -> (
        mk_let
          (mk_pvar "tmp")
          (mk_apply
             (Compil_utils.mk_primitive_lambda
                env
                prim_name
                (Compil_utils.arrow_from_tlist lovetlist)
                ANone
             )
             loveargs
          )
          (mk_match
             (mk_var @@ string_to_ident "tmp")
             [
               (mk_pnone ()), mk_none (TTuple [(nat ()); dun ()]);
               (mk_psome (mk_ptuple [mk_pvar "q"; mk_pvar "r"]),
                mk_match (mk_apply
                              (mk_var (string_to_ident "Nat.of_int"))
                              [mk_var (Ident.create_id "q")])
                  [mk_pconstr "Some" [mk_pvar "q"],
                      mk_some
                        (TTuple [(nat ()); dun ()])
                        (mk_tuple [
                            mk_var (string_to_ident "q");
                            mk_var (string_to_ident "r")]
                        )
                  ]
               )
             ]
          ), option (TTuple [nat (); dun ()])
      )
    | "abs", [_] -> (* In liquidity, abs returns an int. In Love, it returns a nat. *)
      (* Love code =
         Int.of_nat 'exp'
      *)
      let exp =
        mk_apply (
          Compil_utils.mk_primitive_lambda
            env
            prim_name
            (Compil_utils.arrow_from_tlist lovetlist)
            ANone
        )
          loveargs
      in
      mk_apply
          (mk_var (string_to_ident "Int.of_nat"))
          [exp], int ()
    | _ ->
      mk_apply (
        Compil_utils.mk_primitive_lambda
          env
          prim_name
          (Compil_utils.arrow_from_tlist lovetlist)
          ANone
      )
        loveargs, t

and liqexp_to_loveexp (env : env) (e : typed_exp) : AST.exp * TYPE.t =
  let exp, t =
    let ltl = liqexp_to_loveexp env in
    match e.desc with
      Let { bnd_var = {nname; _}; inline; bnd_val; body} ->
      debug "[liqexp_to_loveexp] let %s = ...@." nname;
      let bnd_val,btyp = ltl bnd_val in
      debug "[liqexp_to_loveexp] let %s = %a in...@." nname Love_printer.Ast.print_exp bnd_val;
      let body, body_typ =
        liqexp_to_loveexp (Compil_utils.add_var nname btyp env) body in
      debug "[liqexp_to_loveexp] let %s = %a in %a@." nname Love_printer.Ast.print_exp bnd_val Love_printer.Ast.print_exp body;
      mk_let
        (Love_ast_utils.mk_pvar nname)
        bnd_val
        body, body_typ
    | Var v -> (
        debug "[liqexp_to_loveexp] Creating Var (%s)@." v;
        let vi = string_to_ident v in
        match Love_tenv.find_var vi env with
          None -> error "Unknown variable %s" v
        | Some {result = _,t; _} -> mk_var vi, t
      )
    | SetField {record; field; set_val} ->
      debug "[liqexp_to_loveexp] Creating a SetField@.";
      let rcd, typ = ltl record in
      mk_set_field
        rcd
        [field, fst @@ ltl set_val], typ
    | Project {field; record} ->
      debug "[liqexp_to_loveexp] Creating a Projection@.";
      mk_get_field (fst @@ ltl record) field, liqtype_to_lovetype env e.ty
    | Const {ty; const} ->
      debug "[liqexp_to_loveexp] Creating a Const@.";
      let typ = liqtype_to_lovetype env ty in
      let res = liqconst_to_loveexp ~typ env const in
      debug "[liqexp_to_loveexp] Const %a created@."
        Love_printer.Ast.print_exp (fst @@ res); res
    | Apply {prim = Prim_exec _; args} -> (
        debug "[liqexp_to_loveexp] Creating a lambda application@.";
        match args with
          [] | _ :: [] -> assert false
        | fct :: args -> (
            let fct', ftyp = ltl fct in
            debug "[apply_types] Function %a : %a"
              Love_printer.Ast.print_exp fct'
              Love_type.pretty ftyp;
            let eargs, targs = List.split (List.map ltl args) in
            debug "[apply_types] Arguments : %a"
              (Format.pp_print_list Love_printer.Ast.print_exp) eargs;
            mk_apply fct' eargs, Compil_utils.return_type_with_args env ftyp targs
          )
      )
    | Apply {prim; args} -> (
        debug "[liqexp_to_loveexp] Creating a Primitive application@.";
        liqapply_to_loveexp env e.ty prim args
      )
    | If {cond; ifthen; ifelse} ->
      debug "[liqexp_to_loveexp] Creating an ITE@.";
      let cond, _ = ltl cond in
      let ifthen, t = ltl ifthen in
      let ifelse, t' = ltl ifelse in
      let ifthen,ifelse,t = Compil_utils.choose_best_and_merge (ifthen,t) (ifelse,t') in
      mk_if cond ifthen ifelse, t
    | Seq (e1, e2) ->
      debug "[liqexp_to_loveexp] Creating a sequence@.";
      let first,tf = ltl e1 in
      let first =
        match tf with
          TForall (_,_) -> mk_tapply first (unit ())
        | _ -> first in
      let last, t = ltl e2 in
      mk_seq [first; last], t
    | Transfer {dest; amount} ->
      debug "[liqexp_to_loveexp] Creating a Transfer@.";
      let dest =
        let d, t = ltl dest in
        match to_poly_variant t with
        | `TKeyhash ->
          mk_apply (mk_var (string_to_ident "Address.of_keyhash")) [d]
        | `TAddress -> d
        | _ ->
          error
            "Transfer destination %a should be a keyhash or an address; it has type %a"
            Love_printer.Ast.print_exp d
            Love_type.pretty t
      in
      mk_apply
        (mk_var (string_to_ident "Account.transfer"))
        [dest; fst @@ ltl amount], operation ()
    | Call {contract; amount; entry; arg} -> begin
        debug "[liqexp_to_loveexp] Creating a Call@.";
        let name =
          match entry with
            None -> "default"
          | Some name -> name in
        debug "[liqexp_to_loveexp] Calling %s@." name;
        let ctrct_or_addr, _typ = ltl contract in
        let tmp_ctrname = "CalledContract" in
        let tmp_entrypt = "entry_pt" in
        match contract.ty with
          Tcontract (cname, ty) ->
          let cssig, entry_typ =
            let ty = liqtype_to_lovetype env ty in
            Compil_utils.get_signature_from_name cname ty env, ty
          in
          mk_let
            (* let (CalledContract : cssig) *)
            (mk_pcontract tmp_ctrname cssig)
            (* = ctrct *)
            ctrct_or_addr
            (* in let entry_pt = CalledContract.name *)
            (mk_let
               (mk_pvar tmp_entrypt)
               (mk_var (Ident.put_in_namespace tmp_ctrname (string_to_ident name)))
               (* in call *)
               (mk_apply
                  (Compil_utils.mk_primitive_lambda env
                     "Contract.call"
                     (entrypoint entry_typ @=> dun () @=> entry_typ @=> operation ()) ANone)
                  [mk_var @@ string_to_ident tmp_entrypt;
                   fst @@ ltl amount;
                   fst @@ ltl arg
                  ]
               )
            )
        , operation ()
        | Taddress ->
          (* In liquidity, contracts can be called through their addresses. *)
          let arg, typ_arg = ltl arg in
          let ctr_sig = Anonymous {sig_kind = Contract []; sig_content = [name, SEntry typ_arg]}
          in
          let ctrct =
            (* match (Contract.at arg) with *)
            mk_match
              (mk_apply
                  (Compil_utils.mk_primitive_lambda env
                     "Contract.at"
                     ((address ()) @=> option (TContractInstance ctr_sig))
                     (AContractType ctr_sig)
                  )
                  [ctrct_or_addr]
              )
              [
                (* Some ctr -> ctr *)
                mk_pconstr "Some" [mk_pvar "ctr"], mk_var (Ident.create_id "ctr")
              ]
          in            
          mk_let
            (* let (CalledContract : cssig) *)
            (mk_pcontract tmp_ctrname ctr_sig)
            (* = ctrct *)
            ctrct
            (* in let entry_pt = CalledContract.name *)
            (mk_let
               (mk_pvar tmp_entrypt)
               (mk_var (Ident.put_in_namespace tmp_ctrname (string_to_ident name)))
               (* in call *)
               (mk_apply
                  (Compil_utils.mk_primitive_lambda env
                     "Contract.call"
                     (entrypoint typ_arg @=> dun () @=> typ_arg @=> operation ()) ANone)
                  [mk_var @@ string_to_ident tmp_entrypt;
                   fst @@ ltl amount;
                   arg
                  ]
               )
            ), operation ()            
          
        | t ->
          debug
            "[liqexp_to_loveexp] \
             Error : Contract has type %s, which is not a contract type."
            (LiquidPrinter.Liquid.string_of_type t);
          error
            "Expression %a has Liquidity type %s, but it was expected to be a contract"
            Love_printer.Ast.print_exp ctrct_or_addr
            (LiquidPrinter.Liquid.string_of_type t)
      end
    | MatchOption {arg; ifnone; some_name; ifsome} -> (
        debug "[liqexp_to_loveexp] Creating a Option match@.";
        let arg, targ = ltl arg in
        let env =
          match is_option targ with
          | Some opt ->
            Compil_utils.add_var
              some_name.nname
              opt
              env
          | _ ->
            debug
              "[liqexp_to_loveexp] Error : argument has type %a, this is not an option.@."
              Love_type.pretty targ;
            bad_exp_type arg targ "option"
        in
        let ifnone, t = liqexp_to_loveexp env ifnone in
        let ifsome, t' = liqexp_to_loveexp env ifsome in

        let ifnone, ifsome, t =
          Compil_utils.choose_best_and_merge (ifnone,t) (ifsome,t') in
        mk_match
          arg
          [mk_pnone (), ifnone; mk_psome (mk_pvar (some_name.nname)),ifsome], t
      )
    | MatchList { arg; head_name; tail_name; ifcons; ifnil} -> (
        debug "[liqexp_to_loveexp] Creating a List Match@.";
        let arg, targ = ltl arg in
        debug "[liqexp_to_loveexp] Argument %a : %a@."
          Love_printer.Ast.print_exp arg
          Love_type.pretty targ;
        match to_poly_variant targ with
        | `TList elttyp ->
          let new_env =
               Compil_utils.add_var head_name.nname elttyp env
          in
          let new_env =
               Compil_utils.add_var tail_name.nname targ new_env in
          let ifnil, t = ltl ifnil in
          let ifcons, t' = liqexp_to_loveexp new_env ifcons in
          let ifnil,ifcons,t = Compil_utils.choose_best_and_merge (ifnil,t) (ifcons,t') in
          mk_match
            arg
            [mk_plist [], ifnil;
             mk_plist [
               mk_pvar head_name.nname;
               mk_pvar tail_name.nname],  ifcons
            ], t
        | _ -> failwith "List matching with argument that is not a list"
      )
    | Loop l ->
      debug "[liqexp_to_loveexp] Creating a Loop@.";
      let arg, arg_type = ltl l.arg in
      debug "[liqexp_to_loveexp] Loop argument = %a:%a@."
        Love_printer.Ast.print_exp arg Love_type.pretty arg_type
      ;
      debug "[liqexp_to_loveexp] The whole expression has type %s@."
        (LiquidPrinter.Liquid.string_of_type e.ty);
      let env = Compil_utils.add_var l.arg_name.nname arg_type env in
      let body =
        mk_lambda (mk_pvar l.arg_name.nname) (fst @@ liqexp_to_loveexp env l.body) arg_type in

      mk_apply
        (Compil_utils.mk_primitive_lambda env
           "Loop.loop"
           ((arg_type @=> TTuple [bool ();arg_type]) @=> arg_type @=> arg_type)
           ANone
        )
        [body; arg], arg_type
    | LoopLeft _ -> failwith "TODO LoopLeft"
    | Fold {prim; arg_name; body; arg; acc} ->
      debug "[liqexp_to_loveexp] Creating a fold@.";
      (* In Liquidity, args of fold/iter are tuples. *) (*
    mk_apply
      (mk_const @@ mk_cprimitive @@ liqprimfold_to_loveprim prim)
      [ltl body; ltl arg; ltl acc] *)
      let arg, arg_typs = ltl arg in
      let acc, acc_typ = ltl acc in
      fst @@
      liqprimfold_to_loveexp
        env
        prim
        arg_name.nname
        arg
        arg_typs
        acc
        acc_typ
        liqexp_to_loveexp
        body, acc_typ

    | Map {prim; arg_name; body; arg} ->
      debug "[liqexp_to_loveexp] Creating a map@.";
      let arg, arg_type = ltl arg in
      liqprimmap_to_loveexp
        env
        prim
        arg_name.nname
        liqexp_to_loveexp
        body
        arg
        arg_type

    | MapFold {prim; arg_name; body; arg; acc} ->
      let arg, arg_typ = ltl arg in
      let acc, acc_typ = ltl acc in
      liqprimmapfold_to_loveexp
        env
        prim
        arg_name.nname
        liqexp_to_loveexp
        body
        arg
        arg_typ
        acc
        acc_typ
    | Lambda {recursive = Some f; _} ->
      debug "[liqexp_to_loveexp] Creating a recursive lambda@.";
      error "Recursive lambda %s not supported." f
    | Lambda {arg_name; arg_ty; body; recursive = None; _} -> (
        debug "[liqexp_to_loveexp] Creating a non recursive lambda@.";
        let arg_ty = liqtype_to_lovetype env arg_ty
        in
        let env = Compil_utils.add_var arg_name.nname arg_ty env in
        debug "[liqexp_to_loveexp] fun (%s : %a) -> ...@."
          arg_name.nname Love_type.pretty arg_ty;
        let new_fvars = (Love_type.fvars arg_ty)
        in
        let quant = Love_tenv.get_free env in
        if TypeVarSet.subset new_fvars quant
        then (
          debug "[liqexp_to_loveexp] Introducing no new free var@.";
          let body, t = liqexp_to_loveexp env body in
          mk_lambda
            (mk_pvar arg_name.nname)
            body
            arg_ty, arg_ty @=> t
        )
        else (
          debug "[liqexp_to_loveexp] Introducing new free vars@.";
          let new_body, t =
            liqexp_to_loveexp (
              TypeVarSet.fold
                (fun tv acc -> Love_tenv.add_forall tv acc)
                (* Type is not really TUnit, it is just to keep track that tv
                   is polymorphic as it belongs to the forall map of the environment. *)
                new_fvars
                env) body
          in
          TypeVarSet.fold
            (fun tv (tlam, ty) ->
               if TypeVarSet.mem tv quant
               then tlam, ty
               else (
                 debug "[liqexp_to_loveexp] Adding parameter %a@." Love_type.pp_typvar tv;
                 mk_tlambda tv tlam, TForall (tv, ty)
               )
            )
            new_fvars
            ((mk_lambda (mk_pvar arg_name.nname) new_body arg_ty), arg_ty @=> t)
        )
      )
    | Closure _ ->
      debug "[liqexp_to_loveexp] Creating a closure (expect failure)@.";
      error "Closures are forbidden in Love"
    | Record [] ->
      error
        "%s is an empty record: empty records are forbidden in Love"
        (LiquidPrinter.Liquid.string_of_code e)
    | Record l -> (
        debug "[liqexp_to_loveexp] Creating a record@.";
        let fexp, ftyp =
          let whole_list =
            List.fold_left
              (fun acc (name, exp) ->
                 let e, t = ltl exp in
                 let field_name = Ident.get_final (string_to_ident name) in
                 (field_name,e,t) :: acc
              )
              []
              l
          in
          List.fold_left
            (fun (acc_e, acc_t) (n,e,t) -> ((n,e) :: acc_e, (n,t) :: acc_t))
            ([], [])
            whole_list
        in
        let path =
          let (field,_) = List.hd l in
          fst @@ ident_split_end (string_to_ident field)
        in
        let t = Love_tenv.record_type path ftyp env in
        debug "[liqexp_to_loveexp] Record type is %a@." Love_type.pretty t;
        mk_record path fexp, t
      )

    | Constructor {constr = Constr c; arg} -> (
        debug "[liqexp_to_loveexp] Creating a construction %s@." c;
        let id = string_to_ident c in
        let arg, typ = ltl arg in
        let targs = match liqtype_to_lovetype env e.ty with
            TUser (_, l) -> l
          | t ->
            bad_exp_type arg typ "defined by the user"
        in
        mk_constr id targs [arg], (Love_tenv.constr_type ((string_to_ident c),[typ]) env)
      )

    | Constructor {constr; arg} -> (
        debug "[liqexp_to_loveexp] Creating a Left/Right constructor@.";
        let arg, targ = ltl arg in
        let tor_typs t =
          match t with
            Tor (t1, t2) -> t1, t2
          | t ->
            error
              "Liquidity expression %s has type %s, while it is expected to have \
               type variant."
              (LiquidPrinter.Liquid.string_of_code e)
              (LiquidPrinter.Liquid.string_of_type t)
        in
        match constr with
          Left _ ->
          debug "[liqexp_to_loveexp] Left constructor";
          let t2 = liqtype_to_lovetype env @@ snd @@ tor_typs e.ty in
          (mk_constr (string_to_ident "Left")  [targ; t2] [arg]),
          variant (targ, t2)
        | Right _ ->
          debug "[liqexp_to_loveexp] Right constructor";
          let t1 = liqtype_to_lovetype env @@ fst @@ tor_typs e.ty in
          (mk_constr (string_to_ident "Right") [t1; targ] [arg]),
          variant (t1, targ)
        | Constr _ -> assert false (* Treated just before *)
      )
    | MatchVariant {arg; cases} ->
      debug "[liqexp_to_loveexp] Creating a constructor match@.";
      let arg, targ = ltl arg in
      let patterns, t =
        List.fold_left
          (fun (acc_p, acc_t) (pat, exp) ->
             let pat, env =
               match pat with
                 LiquidTypes.PConstr (name, []) ->
                 debug "[liqexp_to_loveexp] Empty constructor %s.@." name;
                 mk_pconstr
                   name
                   [mk_pany ()], env
               | PConstr (name, [arg]) ->
                 debug "[liqexp_to_loveexp] Simple constructor %s.@." name;
                 let n = string_to_ident name in
                 let type_of_carg =
                   let typeargs =
                     match targ with
                       TUser (_, l) -> l
                     | t ->
                       bad_pat_type pat targ "defined by the user"
                   in
                   let poly_typ_constr =
                     match Love_tenv.find_constr n env with
                       None -> error "Unknown constructor %s" name
                     | Some t -> t
                   in
                   let typ_constr =
                     Love_tenv.constr_with_targs poly_typ_constr.result typeargs env
                   in
                   match typ_constr.Love_tenv.cargs with
                     [t] -> t
                   | l ->
                     bad_number_of_args
                       ("pattern constructor " ^ name)
                       (List.length l)
                       1
                 in
                 mk_pconstr name [mk_pvar arg], (Compil_utils.add_var arg type_of_carg env)
               | PConstr (name, args) ->
                 debug "[liqexp_to_loveexp] Constructor %s.@." name;
                 let n = string_to_ident name in
                 let types_of_cargs =
                   let typeargs =
                     match targ with
                       TUser (_, l) -> l
                     | t -> bad_pat_type pat targ "defined by the user"
                   in
                   let poly_typ_constr =
                     match Love_tenv.find_constr n env with
                       None -> error "Unknown constructor %s" name
                     | Some t -> t
                   in
                   let typ_constr =
                     Love_tenv.constr_with_targs poly_typ_constr.result typeargs env
                   in
                   typ_constr.Love_tenv.cargs in
                 debug "[liqexp_to_loveexp] Constructor %s with %i arguments.@." name
                   (List.length args);
                 let new_env =
                   List.fold_left2
                     (fun env arg arg_typ -> Compil_utils.add_var arg arg_typ env)
                     env
                     args
                     types_of_cargs
                 in
                 mk_pconstr
                   name
                   [mk_ptuple (List.map (fun a -> mk_pvar a) args)], new_env
               | PAny -> mk_pany (), env
             in
             debug "[liqexp_to_loveexp] Building the expression case@.";
             let exp, t = liqexp_to_loveexp env exp in
             match acc_t, t with
               TVar _, _
             | TForall _, TForall _ -> (pat, exp) :: acc_p, t

             | TForall _, _ ->
               let new_acc_p =
                 List.map (fun (p, e) -> p, mk_tapply e t) acc_p in
               ((pat, exp) :: new_acc_p), t
             | _,_ -> (pat, exp) :: acc_p, t
          )
          ([], TVar (Love_type.fresh_typevar ()))
          cases
      in
      let m = mk_match arg (List.rev patterns)
      in m, t

    | MatchNat {arg; plus_name; ifplus; minus_name; ifminus} ->
      debug "[liqexp_to_loveexp] Creating a sign match@.";
      let tmp_name = "__tmp" in
      let plusenv = Compil_utils.add_var plus_name.nname (nat ()) env in
      let minenv = Compil_utils.add_var minus_name.nname (nat ()) env in
      let ifplus, t = liqexp_to_loveexp plusenv ifplus in
      let ifminus, t' = liqexp_to_loveexp minenv ifminus in
      let ifplus, ifminus, t = Compil_utils.choose_best_and_merge (ifplus,t) (ifminus,t')
      in
      let tmp_varid = string_to_ident tmp_name in
      let arg, targ = ltl arg in
      let nat_var =
        mk_apply (mk_var (string_to_ident "abs")) [mk_var tmp_varid] in
      mk_let (mk_pvar tmp_name) arg (
        mk_if
          (mk_apply
             (mk_tapply
                (mk_var (string_to_ident ">="))
                targ
             ) [mk_var tmp_varid; mk_const (mk_cint (Z.zero))])
          (mk_let
             (mk_pvar plus_name.nname)
             nat_var
             ifplus
          )
          (mk_let
             (mk_pvar minus_name.nname)
             nat_var
             ifminus
          )
      ), t

    | Failwith fail ->
      let fail', tfail = liqexp_to_loveexp env fail in
      debug "[liqexp_to_loveexp] Fail = %a@." Love_printer.Ast.print_exp fail';
      let rettyp = liqtype_to_lovetype env e.ty in
      mk_tapply (mk_raise (Fail tfail) [fail']) rettyp
    , rettyp
    | CreateContract {args; contract} -> (* Todo : bad argument to contract *)
      debug "[liqexp_to_loveexp] Creating a contract creation@.";
      let name_id = string_to_ident contract.contract_name in
      let gprim =
        match Love_primitive.from_string "Contract.create" with
          None ->
          error
            "Invariant broken: Contract.create has not been found in Love primitives list"
        | Some p -> p in
      let ctr =
        match Love_tenv.find_contract name_id env with
          None ->
          let ctr, ctrt = liqcontract_to_lovecontract ~env false contract in
          let first_class_ctr : AST.reference = Anonymous ctr in
          mk_packstruct first_class_ctr
        | Some {result = env; _} ->
          mk_packstruct (Named name_id)
      in
      let args, storage =
        match args with
          manager :: amount :: storage :: _ ->
          let stor, stor_typ = ltl storage in
          [
            fst @@ ltl manager;
            fst @@ ltl amount;
            ctr;
            stor
          ], stor_typ
        | _ ->
          failwith ("TODO: check correct arguments of Contract.create")
      in
      let prim =
        mk_tapply
          (mk_var (string_to_ident "Contract.create"))
          storage in

      mk_apply
        prim
        args,
      Love_type.return_type (Love_primitive.(type_of (gprim, ANone)))

    | ContractAt {arg; entry; entry_param} ->
      debug "[liqexp_to_loveexp] Creating a contract at@.";
      let entry_ty = liqtype_to_lovetype env entry_param in
      let contract = Compil_utils.get_signature_from_name (Some entry) entry_ty env in
      mk_apply
        (mk_var_with_arg (string_to_ident "Contract.at") (AContractType contract))
        [fst @@ ltl arg], option (TContractInstance contract)

    | Unpack {arg; ty} ->
      debug "[liqexp_to_loveexp] Creating an unpack@.";
      let t = liqtype_to_lovetype env ty in
      debug "[liqexp_to_loveexp] Content type : %a. Expression type = %s@."
        Love_type.pretty t
        (LiquidPrinter.Liquid.string_of_type e.ty);
      mk_apply
        (mk_tapply (mk_var (string_to_ident "Bytes.unpack")) t)
        [fst @@ ltl arg], option t

    | TypeAnnot {e; ty} ->
      debug "[liqexp_to_loveexp] Creating a type annoted expression (discarding type)@.";
      ltl e

    | Self _
    | SelfCall _ -> error "Reentrance is forbidden in Love"

    | Type t ->
      error
        "Error at expression %s: Type expressions are forbidden in love"
        (LiquidPrinter.Liquid.string_of_type t)
  in
  debug "[liqexp_to_loveexp] Expression %a : %a@." Love_printer.Ast.print_exp exp Love_type.pretty t;
  let expected_typ =
    match e.ty, e.desc with
      Tfail,_ -> debug "[liqexp_to_loveexp] Failure type, treated differently@."; t
    | Tpartial _,_ -> debug "[liqexp_to_loveexp] Partial type, not treated@."; t
    | _,Loop _ -> begin
        (* Liquidity says the type of a loop expression is bool * arg_type, while it
           should be arg_type only. *)
        match e.ty with
          Ttuple [Tbool; t] -> liqtype_to_lovetype env t
        | _ ->
          error "Liquidity return type of loop body must be (bool * 'a), but here it is %s"
            (LiquidPrinter.Liquid.string_of_type e.ty)
      end
    | t, Unpack _ ->
      (* Liquidity says the type of an unpack expression is arg_type, while it should
         be arg_type option. *)
      option (liqtype_to_lovetype env t)
    | _,_ ->
      debug
        "[liqexp_to_loveexp] Liquidity type : %s"
        (LiquidPrinter.Liquid.string_of_type e.ty);
      liqtype_to_lovetype env e.ty
  in
  let t = Love_tenv.normalize_type ~relative:true t env in
  let expected_typ = Love_tenv.normalize_type ~relative:true expected_typ env in
  debug
    "[liqexp_to_loveexp] Expected type for expression %a is %a:\n\
     Matching type %a with expected type %a@."
    Love_printer.Ast.print_exp exp Love_type.pretty expected_typ
    Love_type.pretty t Love_type.pretty expected_typ;
  let e, t =
    try
      Compil_utils.apply_types env exp t expected_typ
    with
      _ -> (
        debug "[liqexp_to_loveexp] Error: Types %a and %a are incompatible."
          Love_type.pretty t Love_type.pretty expected_typ;
        error "Error trying to merge types: types %a of expression %a and %a are incompatible"
          Love_type.pretty t Love_printer.Ast.print_exp exp Love_type.pretty expected_typ
      )
  in
  debug "[liqexp_to_loveexp] New expression %a : %a@." Love_printer.Ast.print_exp e Love_type.pretty t;
  let fvars = Love_type.fvars t in
  let t,e =
    TypeVarSet.fold
      (fun tv (acc_t, acc_e) ->
         TForall (tv, acc_t),
         mk_tlambda tv acc_e
      )
      fvars
      (t,e) in
  debug "[liqexp_to_loveexp] Forall type %a : %a@." Love_printer.Ast.print_exp e Love_type.pretty t;
  e, t

and liqvalue_to_lovecontent env {val_name; inline; val_private; val_exp} :
  (string * AST.content) * env =
  debug "[liqvalue_to_lovecontent] Creating value %s@." val_name;
  let code, typ = liqexp_to_loveexp env val_exp in
  let visibility = (if val_private then AST.Private else AST.Public) in
  debug "[liqvalue_to_lovecontent] Value %s = %a:%a@."
    val_name
    Love_printer.Ast.print_exp code
    Love_type.pretty typ;
  (val_name, mk_value code typ visibility Rec),
  Compil_utils.add_var ~kind:(Value visibility) val_name typ env


and liqentry_to_lovecontent env {entry_sig; code} =
  let param_type = liqtype_to_lovetype env entry_sig.parameter in
  let stor_typ =
    stor_typ_from_opt_typ @@ Love_tenv.get_storage_type env in
  let env = Compil_utils.add_var entry_sig.parameter_name param_type env in
  let env = Compil_utils.add_var entry_sig.storage_name stor_typ env in
  let full_love_code = fst @@ liqexp_to_loveexp env code in
  let () =
    debug "[liqentry_to_lovecontent] \
           Creating entry point with storage type = %a and parameter type = %a@."
      Love_type.pretty stor_typ
      Love_type.pretty param_type in
  (* let code =
   *   mk_entry_point_lambda
   *     entry_sig.storage_name
   *     stor_typ
   *     entry_sig.parameter_name
   *     param_type
   *     full_love_code in *)
  let mk_lambda arg ty body = mk_lambda (mk_pvar arg) body ty in
  let code =
    mk_lambda
      entry_sig.storage_name
      stor_typ @@
    mk_lambda
      "amount"
      (dun ()) @@
    mk_lambda
      entry_sig.parameter_name
      param_type @@
    full_love_code in
  let name = entry_sig.entry_name in
  (name, mk_entry code None param_type),
  Compil_utils.add_var
    ~kind:Entry
    name
    (entrypoint (param_type))
    env

and liqinit_to_loveinit env init_args init_body =
  let init_code, init_typ = match init_args with
    | [] ->
      let body, _t = liqexp_to_loveexp env init_body in
      let ty = unit () in
      mk_lambda (mk_pany ()) body ty, ty
    | [x, _, ty] ->
      let ty = liqtype_to_lovetype env ty in
      let env = Love_tenv.add_var x ty env in
      let arg = mk_pvar x in
      let body, _t = liqexp_to_loveexp env init_body in
      mk_lambda arg body ty, ty
    | _ ->
      let args_typ, env =
        List.fold_left (fun (tlist, env) (x, _, ty) ->
            debug "[liqcontract_to_lovecontract] Preprocessing arg %s : %s@."
              x (LiquidPrinter.Liquid.string_of_type ty);
            let ty = (liqtype_to_lovetype env ty) in
            ty :: tlist, Love_tenv.add_var x ty env
          ) ([], env) init_args
      in
      let arg = mk_ptuple (List.map (fun (x,_,_) -> mk_pvar x) init_args) in
      let arg_typ = TTuple (List.rev args_typ) in
      let body, _t = liqexp_to_loveexp env init_body in
      mk_lambda arg body arg_typ, arg_typ
  in
  AST.Init {
    init_code;
    init_typ;
    init_persist = false;
  }

and liqcontract_to_lovecontent
    env (c : typed_contract) : string * AST.content * env =
  debug "[liqcontract_to_lovecontent] Sub structure %s@." c.contract_name;
  let is_module = LiquidTypes.is_only_module c in
  let ckind = if is_module then Module else Contract [] in
  debug "[liqcontract_to_lovecontent] Structure %s is a %s@."
    c.contract_name
    (if is_module then "module" else "contract") ;
  let ctr, subenv =
    liqcontract_to_lovecontract
      ~ctr_name:c.contract_name
      ~env:(env_of_subcontract c.contract_name ckind env)
      is_module
      c in
  debug "[liqcontract_to_lovecontent] Signature of sub structure : %a@."
    Love_tenv.pp_env subenv;
  let new_env =
    Love_tenv.add_subcontract_env
      c.contract_name
      subenv
      env in
  debug "[liqcontract_to_lovecontent] New environment: %a@."
    Love_tenv.pp_env new_env;
  c.contract_name, mk_structure ctr, new_env

and liqcontract_to_lovecontract
    ?env ?(ctr_name="") (is_module : bool) (c : typed_contract) : AST.structure * env =
  debug "[liqcontract_to_lovecontract] Transpiling liquidity contract@.";
  let env : env =
    match env with
      None -> empty_env (if is_module then Module else Contract []) ()
    | Some e -> e in
  let env, types = (*  Adding type definitions *)
    let rec fill_env_with_types
        (types : (datatype list -> datatype) StringMap.t) (str : string) typ (acc_env, acc_tdef) =
      let typedef_registered n =
        match find_type n acc_env with
          None -> false
        | Some _ -> true in
      let res =
        if
          typedef_registered str ||
          Collections.StringSet.mem str Compil_utils.reserved_types
        then (
          debug "[liqcontract_to_lovecontract] Type %s is already registered" str;
          (acc_env, acc_tdef)
        ) else (
          debug "[liqcontract_to_lovecontract] Adding type %s to environment@." str;
          try
            let tdef = liqtype_to_lovetypedef acc_env (typ []) in
            debug "[liqcontract_to_lovecontract] %a@." (pp_typdef ~name:"" ~privacy:"") tdef;
            add_typedef_to_contract str tdef acc_env, ((str, AST.DefType (TPublic, tdef)) :: acc_tdef)
          with
            UnknownType (s,_, _) ->
            debug "[liqcontract_to_lovecontract] %s is not in the environment@." s;
            try
              let acc = fill_env_with_types types s (StringMap.find s types) (acc_env, acc_tdef)
              in
              fill_env_with_types types str typ acc
            with
              UnknownType (s', _, _) when String.equal s' str ->
              error "Type %s is inter-recursive: forbidden in Love." s'
        )
      in
      debug "[liqcontract_to_lovecontract] Type %s added@." str;
      res
    in
    StringMap.fold
      (fill_env_with_types c.ty_env.types)
      c.ty_env.types
      (env, [])
  in
  let types = List.rev types in
  let env, signatures =
    StringMap.fold (
      fun name cs (env, sigs) ->
        if
          Collections.StringSet.mem name Compil_utils.reserved_structures ||
          String.equal ctr_name name
        then env, sigs
        else
          let name = name^"__signature" in
          debug "[liqcontract_to_lovecontract] Registering signature %s@." name;
          match liqcontract_sig_to_lovetype env (Some name) cs with
            Anonymous s ->
            debug "[liqcontract_to_lovecontract] Signature %s = %a@."
              name (Love_type.pp_contract_sig ) s;
            Love_tenv.add_signature name
              (Love_tenv.contract_sig_to_env (Some name) s env)
              env,
            (name,(AST.Signature s)) :: sigs
          | Named _ -> failwith "TODO : add named signatures to signature definition"
    )
      c.ty_env.contract_types
      (env, []) in

  debug "[liqcontract_to_lovecontract] %i subcontracts/modules to handle@." (List.length c.subs);
  let subc, env =
    List.fold_left
      (fun (acc_subc, acc_env) c ->
         if Collections.StringSet.mem c.contract_name Compil_utils.reserved_structures
         then (acc_subc, acc_env)
         else
           let n, sc, new_env = liqcontract_to_lovecontent acc_env c in
           (n, sc) :: acc_subc, new_env
      )
      ([], env)
      c.subs
  in
  let storage_type = liqtype_to_lovetypedef env c.storage
  in
  debug "[liqcontract_to_lovecontract] Storage type def = %a@."
    Love_type.(pp_typdef ~name:"storage" ~privacy:"") storage_type;
  let subc = List.rev subc in
  let values, env =
    List.fold_left
      (fun (acc_v, env) v ->
         let v', env = liqvalue_to_lovecontent env v in
         v' :: acc_v, env)
      ([], env)
      c.values in
  let values = List.rev values in
  let entries, env =
    List.fold_left
      (fun (acc_v, env) v ->
         let v', env = liqentry_to_lovecontent env v in
         v' :: acc_v, env)
      ([], env)
      c.entries in
  let entries = List.rev entries in
  let init =
    match c.c_init with
      None when not is_module ->
      debug
        "[liqcontract_to_lovecontract] Contract %s has no initializer : adding one@."
        ctr_name;
      let init_typ = TUser (Ident.create_id "storage", []) in
      let init_code =
        mk_lambda
          (mk_pvar "i")
          (mk_var @@ Ident.create_id "i")
          init_typ in
      [
        CONSTANTS.init_storage, AST.Init {
          init_code;
          init_typ;
          init_persist = false;
        }]
    | None -> []
    | Some {init_args; init_body; _} -> [
        CONSTANTS.init_storage,
        liqinit_to_loveinit env init_args init_body
      ]
  in
  let str =
    let kind = if is_module then TYPE.Module else TYPE.Contract [] in
    { AST.structure_content = (types @ signatures @ values @ subc @ entries @ init);
      kind }
  in
  str, env

let liqcontract_to_lovecontract ~(ctr_name:string) (c : typed_contract) : AST.structure * env =
  debug "[liqcontract_to_lovecontract] Registering contract %s@." ctr_name;
  let initial_env = empty_env (Contract []) () in
  try
    liqcontract_to_lovecontract ~ctr_name ~env:initial_env false c
  with
    UnknownType (s,_,e) ->
    debug "Failing with typing environment =\n%a@."
      Love_tenv.pp_env e;
    error "Unknown type %s" s


(* 2. LiqConst to LoveValue *)

let rec liqconst_to_lovevalue
    (env : env) (c : (datatype, typed) LiquidTypes.exp LiquidTypes.const)
  : Love_value.Value.t =
  let ltl = liqconst_to_lovevalue env in
  let res : Love_value.Value.t =
    match c with
      CUnit   -> VUnit
    | CBool b -> VBool b
    | CInt i  -> VInt i.integer
    | CNat n  -> VNat n.integer
    | CTez {tezzies;mutez} -> begin
        let str =
          let mutez = match mutez with
            | Some mutez -> mutez
            | None  -> "000000"
          in
          tezzies ^ mutez
        in
        match Tez_repr.of_string str with
          None -> error "%s is not a correct dun amount" str
        | Some d -> Love_value.Value.VDun d
      end
    | CTimestamp s -> begin
        debug "[liqconst_to_lovevalue] CTimestamp : %s@." s;
        match Script_timestamp_repr.of_string s with
          None -> error "Timestamp %s has no integer representation" s
        | Some ts ->
          VTimestamp ts
      end
    | CString s -> VString s
    | CBytes b -> VBytes (MBytes.of_string b)
    | CKey k -> (
      match Signature.Public_key.of_b58check_opt k with
          None -> error "Key %s is invalid" k
        | Some k -> VKey k
    )
    | CSignature s -> (
      match Signature.of_b58check_opt s with
          None -> error "Signature %s is invalid" s
        | Some s -> VSignature s
    )
    | CTuple ([] | [_] as l) ->
      debug "[liqconst_to_lovevalue] Error : tuple has %i elements@." (List.length l);
      error "Expression %s is a tuple with %d elements: this is an invalid expression"
        (LiquidPrinter.Liquid.string_of_const c)
        (List.length l)
    | CTuple l ->  VTuple (List.map ltl l)

    | CNone -> VConstr ("None", [])
    | CSome c -> VConstr ("Some", [ltl c])
    | CMap l ->
      let map =
        List.fold_left
          (fun acc (key, bnd) -> Love_value.ValueMap.add (ltl key) (ltl bnd) acc)
          Love_value.ValueMap.empty
          l
      in
      VMap map
    | CBigMap _ ->
      error
        "Value compilation failed on %s: bigmaps are not yet compilable in Love"
        (LiquidPrinter.Liquid.string_of_const c)
    | CList l -> VList (List.map ltl l)

    | CSet l ->
      let set =
        List.fold_left
          (fun acc key -> Love_value.ValueSet.add (ltl key) acc)
          Love_value.ValueSet.empty
          l
      in
      VSet set
    | CLeft c -> VConstr ("Left", [ltl c])
    | CRight c -> VConstr ("Right", [ltl c])

    | CKey_hash kh -> (
        match Signature.Public_key_hash.of_b58check_opt kh with
          None -> error "Keyhash %s is invalid" kh
        | Some k -> VKeyHash k
      )
    | CContract (addr, None) ->
      (match Contract_repr.of_b58check addr with
       | Ok c -> VAddress c
       | Error _ ->
         error "Love compile const: Not a contract (here %s)." addr
      )
    | CContract (c, _) ->
      error "Constant contracts (here %s) has no Love representation." c
    | CRecord [] ->
      error "Empty records are forbidden in Love"
    | CRecord l ->
      VRecord (List.map (fun (s, c) -> (s, ltl c)) l)

    | CConstr (name, args) -> VConstr (name, [ltl args])

    | CLambda {arg_name; arg_ty; body; ret_ty; recursive} ->
      error "Todo: CLambda -> VClosure"
  in
  let () =
    debug
      "[liqconst_to_lovevalue] Const %a@."
      Love_printer.Value.print res
  in res

let rec lovevalue_to_liqconst (c : Love_value.Value.t) =
  debug "[lovevalue_to_liqconst] %a -->%!" Love_printer.Value.print c;
  let ltl = lovevalue_to_liqconst in
  let res =
    match c with
    | VUnit   -> CUnit
    | VBool b -> CBool b
    | VInt integer  -> CInt { LiquidNumber.integer }
    | VNat integer  -> CNat { LiquidNumber.integer }
    | VDun d ->
      Tez_repr.to_mutez d
      |> Z.of_int64
      |> LiquidNumber.tez_of_mic_mutez
      |> fun c -> CTez c
    | VTimestamp s -> CTimestamp (Script_timestamp_repr.to_string s)
    | VString s -> CString s
    | VBytes b -> CBytes (MBytes.to_string b)
    | VKey k -> CKey (Signature.Public_key.to_b58check k)
    | VSignature s -> CSignature (Signature.to_b58check s)
    | VKeyHash k -> CKey_hash (Signature.Public_key_hash.to_b58check k)
    | VAddress c -> CContract(Contract_repr.to_b58check c, None)
    | VOperation _ -> error "Love value decomilation: operation constant"

    | VConstr ("None", []) -> CNone
    | VConstr ("Some", [c]) -> CSome (ltl c)
    | VConstr ("Left", [c]) -> CLeft (ltl c)
    | VConstr ("Right", [c]) -> CRight (ltl c)
    | VConstr (name, [args]) -> CConstr (name, ltl args)
    | VConstr (name, args) -> CConstr (name, ltl (VTuple args))

    | VTuple l -> CTuple (List.map ltl l)
    | VList l -> CList (List.map ltl l)
    | VSet set ->
      CSet (List.map ltl (Love_value.ValueSet.elements set))
    | VMap map ->
      CMap (List.map (fun (k, v) -> ltl k, ltl v)
              (Love_value.ValueMap.bindings map))
    | VBigMap { id = Some integer; diff; _ }
      when Love_value.ValueMap.is_empty diff ->
      CBigMap (BMId { integer })
    | VBigMap { id = None; diff ; _ }
      when Love_value.ValueMap.for_all
          (fun _ -> function None -> false | Some _ -> true) diff ->
      let l = List.map (function
          | (k, Some v) -> ltl k, ltl v
          | _ -> assert false) (Love_value.ValueMap.bindings diff) in
      CBigMap (BMList l)
    | VBigMap _ -> error "Love value decompilation failed on big map diff"
    | VRecord l -> CRecord (List.map (fun (s, c) -> (s, ltl c)) l)

    | VPackedStructure _ -> error "decompilation VPackedStructure"
    | VContractInstance _ -> error "decompilation VContractInstance"
    | VEntryPoint _ -> error "decompilation VEntryPoint"
    | VView _ -> error "decompilation VView"
    | VPrimitive _ -> error "decompilation VPrimitive"

    | VClosure _ -> error "Todo: VClosure -> CLambda"
  in
  debug " %s" (LiquidPrinter.LiquidDebug.string_of_const res);
  res


let liqconst_to_lovevalue const =
  let initial_env = empty_env (Contract []) () in
  liqconst_to_lovevalue initial_env const

let const_encoding : Love_value.Value.t Json_encoding.encoding =
  Json_encoding.obj1 @@
  Json_encoding.req "dune_expr" @@
  Environment.Data_encoding.Json.convert @@
  Love_json_encoding.Value.encoding

let contract_encoding =
  Json_encoding.obj1 @@
  Json_encoding.req "dune_code" @@
  Environment.Data_encoding.Json.convert @@
  Love_json_encoding.Ast.top_contract_encoding

let datatype_encoding : TYPE.t Json_encoding.encoding =
  Json_encoding.obj1 @@
  Json_encoding.req "dune_expr" @@
  Environment.Data_encoding.Json.convert @@
  Love_json_encoding.Type.encoding

let print_contract_json ?minify code =
  Json_encoding.construct contract_encoding
    { AST.version = 1, 0; code }
  |> Ezjsonm.value_to_string ?minify

let init () =
  if !LiquidOptions.verbosity>0 then
    Format.eprintf "Initialize Love environments... %!";
  let _ =
    Love_pervasives.update_protocol_revision
      (Base.Config.config.protocol_revision) in
  Love_type_list.init ();
  Love_prim_list.init ();
  Love_tenv.init_core_env ();
  Love_env.initialize ();
  if !LiquidOptions.verbosity>0 then Format.eprintf "Done@.";
  ()

let () = init ()
