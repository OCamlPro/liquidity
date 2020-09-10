open Dune_Network_Lib
open Love_parsing
open Protocol
open Love_type
open Love_ast
open Love_ast_types
open Love_ast_types.TYPE
open Love_ast_types.AST
open Love_type_utils
open Love_ast_utils
open Love_pervasives
open Love_runtime_ast
open Log

let shift = 15

let love_loc loc =
  match loc.LiquidTypes.loc_pos with
  | None -> None
  | Some ((l1, c1), (l2, c2)) ->
     (* Nothing for end position *)
     (* use bol to encode *)
     Some (AST.{
           pos_lnum = l1;
           pos_cnum = c1;
           pos_bol = l2 lsl shift + c2;
       })

let liq_loc loc_file = function
  | None -> { LiquidTypes.loc_file; loc_pos = None }
  | Some AST.{ pos_lnum = l1; pos_cnum = c1; pos_bol } ->
     let l2 = pos_bol lsr shift in
     let c2 = pos_bol - l2 lsl shift in
     { LiquidTypes.loc_file;
       loc_pos = Some ((l1, c1), (l2, c2)) }

let error ?loc msg =
  LiquidLoc.raise_error ?loc ("Love compiler error:  " ^^ msg ^^ "%!")

let cannot_apply ?loc p exp ty =
  error ?loc
    "Cannot apply %s on expression %a of type %a"
    p
    Love_printer.Ast.print_exp exp
    Love_type.pretty ty

let bad_number_of_args ?loc fname got expected =
  error ?loc
    "Bad number of arguments for %s: got %d, expected %d"
    fname
    got
    expected

let bad_const_type ?loc cst ty expected =
  error ?loc
    "Constant %s has type %a, but it was expected to be of type %s"
    (LiquidPrinter.Liquid.string_of_const cst)
    Love_type.pretty ty
    expected

let pp_pat fmt = function
  | LiquidTypes.PConstr (constr, vars) ->
    Format.fprintf fmt "%s (%s) ->\n" constr (String.concat ", " vars)
  | PAny -> Format.fprintf fmt "_"

let bad_pat_type ?loc pat ty expected =
  error ?loc
    "Pattern %a has type %a, but it was expected to be of type %s"
    pp_pat pat
    Love_type.pretty ty
    expected

let bad_exp_type ?loc exp ty expected =
  error ?loc
    "Expression %a has type %a, but it was expected to be of type %s"
    Love_printer.Ast.print_exp exp
    Love_type.pretty ty
    expected

let user_comp env = (fun x -> Love_tenv.isComp x env)

let unit () = Love_type_list.get_type "unit" []
let int () = Love_type_list.get_type "int" []
let bool () = Love_type_list.get_type "bool" []
let nat () = Love_type_list.get_type "nat" []
let dun () = Love_type_list.get_type "dun" []
let string () = Love_type_list.get_type "string" []
let bytes () = Love_type_list.get_type "bytes" []
let timestamp () = Love_type_list.get_type "timestamp" []
let key () = Love_type_list.get_type "key" []
let keyhash () = Love_type_list.get_type "keyhash" []
let signature () = Love_type_list.get_type "signature" []
let operation () = Love_type_list.get_type "operation" []
let address () = Love_type_list.get_type "address" []
let list t = Love_type_list.get_type "list" [t]
let set t = Love_type_list.get_type "set" [t]
let map (t, t') = Love_type_list.get_type "map" [t; t']
let bigmap (t, t') = Love_type_list.get_type "bigmap" [t; t']
let option t = Love_type_list.get_type "option" [t]
let variant (t, t') = Love_type_list.get_type "variant" [t; t']
let entrypoint t = Love_type_list.get_type "entrypoint" [t]
let tview t = Love_type_list.get_type "view" [t]

let to_poly_variant = function
  | TUser (LName "unit", [])      -> `TUnit
  | TUser (LName "bool", [])      -> `TBool
  | TUser (LName "int", [])       -> `TInt
  | TUser (LName "nat", [])       -> `TNat
  | TUser (LName "dun", [])       -> `TDun
  | TUser (LName "string", [])    -> `TString
  | TUser (LName "bytes", [])     -> `TBytes
  | TUser (LName "timestamp", []) -> `TTimestamp
  | TUser (LName "key", [])       -> `TKey
  | TUser (LName "keyhash", [])   -> `TKeyhash
  | TUser (LName "address", [])   -> `TAddress
  | TUser (LName "signature", []) -> `TSignature
  | TUser (LName "operation", []) -> `TOperation
  | TUser (LName "list", [t1]) -> `TList t1
  | TUser (LName "set", [t1]) -> `TSet t1
  | TUser (LName "map", [t1; t2]) -> `TMap (t1, t2)
  | TUser (LName "bigmap", [t1; t2]) -> `TBigMap (t1, t2)
  | TUser (LName "variant", [t1; t2]) -> `TVariant (t1, t2)
  | TUser (LName "entrypoint", [t1]) -> `TEntrypoint t1
  | TVar tv -> `TVar tv
  | t -> `Type t

let stor_typ_from_opt_typ = function
    None -> unit ()
  | Some t -> t

let pp_opt_stor fmt = function
  | None -> Format.fprintf fmt "(TUnit)"
  | Some t -> Format.fprintf fmt "%a" pretty t

let string_to_ident str =
  debug "[string_to_ident] Creating ident for %s@." str;
  let str_list = String.split_on_char '.' str in
  debug "[string_to_ident] After split: %i elements@." (List.length str_list);
  let namespaces, elt =
    match List.rev str_list with
      [] -> assert false
    | elt :: namespaces -> namespaces, elt
  in
  debug "[string_to_ident] Namespaces:%a@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt _ -> Format.fprintf fmt ", ")
       (fun fmt s -> Format.fprintf fmt "%s" s)
    ) namespaces
  ;
  let res =
    Ident.(put_in_namespaces namespaces (create_id elt))
  in
  debug "[string_to_ident] Id = %a@." Ident.print_strident res; res

let choose_best_and_merge (e1,t1) (e2,t2) =
  match t1, t2 with
    t, TVar _
  | TVar _, t -> failwith "Invalid merge between tvars"
  | TForall _, TForall _ -> e1,e2,t1
  | t, TForall _ -> e1, (mk_tapply e2 t), t
  | TForall _, t -> (mk_tapply e1 t), e2, t
  | _,_ -> e1,e2,t1

let arrow_from_tlist tlist =
  let rec aft = function
      [] -> unit ()
    | [hd] -> hd
    | hd :: tl -> hd @=> aft tl
  in
  aft tlist

let rec put_in_arrow =
  function
    []         -> (fun body -> mk_lambda (mk_pany ()) body (unit ()))
  | [arg, typ] -> (fun body -> mk_lambda (mk_pvar arg) body typ)
  | (arg, typ) :: tl -> (fun body -> mk_lambda (mk_pvar arg) (put_in_arrow tl body) typ)

let rec remove_foralls t = match t with TForall (_, t) -> remove_foralls t | _ -> t

let get_ctr_name filename =
  let rawname =
    let l = String.split_on_char '/' filename in
    let last = List.hd @@ List.rev l in
    List.hd (String.split_on_char '.' last) in
  String.capitalize_ascii rawname

(** Takes a list of arguments of a function and the type of the polymorphic
    function. Returns the list of arguments with eventual type applications.
*)
let merge_args_with_funtyp args arrowtyp =
  let arrowtyp = remove_foralls arrowtyp in
  debug "[merge_args_with_funtyp] Raw type : %a@." pretty arrowtyp;
  let choose_best_and_merge_list l =
    let rec choose_best_list acc (i,(e,t)) l =
      match l with
        [] -> (i, (e, t)) :: acc
      | (i',(e', t')) :: tl ->
        let new_e, new_e', t = choose_best_and_merge (e,t) (e',t') in
        choose_best_list ((i',(new_e', t)) :: acc) (i, (new_e, t)) tl
    in
    match l with
      [] -> []
    | hd :: l -> choose_best_list [] hd l
  in
  let args = List.mapi (fun i a -> i, a) args in
  let reserved_tvar = Love_type.fresh_typevar ~name:"__reserved" () in
  let rec loop aliases args (* (args : (int * (exp * Love_type.t)) list) *) atyp =
    match args with
      [] -> TypeVarMap.fold (fun _ l acc -> l @ acc) aliases []
    | (i, (arg, typ)) :: tl -> (
        debug "[merge_args_with_funtyp] Arg %a : %a@."
          Love_printer.Ast.print_exp arg pretty typ;
        match atyp with
        | TArrow (TVar tv, t2) -> (
            match TypeVarMap.find_opt tv aliases with
            | None -> (* tv = typ *)
              loop
                (TypeVarMap.add tv [i,(arg,typ)] aliases)
                tl
                t2
            | Some l -> (* tv already has a type *)
              let new_l = (i,(arg, typ)) :: l in
              let new_l = choose_best_and_merge_list new_l in
              loop
                (TypeVarMap.add tv new_l aliases)
                tl
                t2
          )
        | TArrow (_, t2) ->
          let bnd =
            match TypeVarMap.find_opt reserved_tvar aliases
            with None -> []
               | Some l -> l in
          let aliases = TypeVarMap.add reserved_tvar ((i, (arg, typ)) :: bnd) aliases
          in
          loop
            aliases
            tl
            t2
        | t ->
          failwith (
            Format.asprintf "Remaining arguments for type %a" pretty t
          )
      ) in
  let args = loop TypeVarMap.empty args arrowtyp |> List.fast_sort (fun (i1,_) (i2,_) -> i1 - i2)
  in
  List.map (fun (_, e) -> e) args

let add_var ?kind s t env =
  match s with
    "_" -> env
  | _ -> Love_tenv.add_var ?kind s t env


(** Returning the corresponding types in t2 that are polymorphic in t1.
    The argument aliases corresponds to the type aliases (such as type t = int). *)
let search_aliases
    ?loc
    t1
    t2 =
  let rec search_in_lists (acc : 'a TypeVarMap.t) l1 l2 =
    (* assert List.length l1 = List.length l2 *)
    match l1,l2 with
      [],[] -> acc
    | hd1::tl1, hd2::tl2 ->
      search_in_lists (search acc hd1 hd2) tl1 tl2
    | _,_ -> assert false

  and search_content acc name content1 content2 =
    match content1, content2 with
      SType st1, SType st2 -> (
        match st1, st2 with
          (SPublic td1 | SPrivate td1), (SPublic td2 | SPrivate td2) ->
          search_in_typedef acc td1 td2
        | SAbstract _, SAbstract _ -> acc
        | (SPublic _ | SPrivate _ | SAbstract _),
          (SPublic _ | SPrivate _ | SAbstract _) ->
          error ?loc "Signature and definition of type %s do not have the same visibility." name
      )
    | SException e1, SException e2 ->
      let le1 = List.length e1 in
      let le2 = List.length e2 in
      if le1 = le2 then
        search_in_lists acc e1 e2
      else
        error ?loc "Exception %s expects %d arguments, but it is defined with %d arguments."
          name le1 le2
    | SInit t1, SInit t2 -> search acc t1 t2
    | SEntry t1, SEntry t2
    | SValue t1, SValue t2
    | SView t1, SView t2 -> search acc t1 t2
    | SStructure (Anonymous a1), SStructure (Anonymous a2)
    | SSignature a1, SSignature a2 -> search_structures acc a1 a2
    | SStructure (Named _), SStructure (Named _) -> acc
    | (SType _ | SException _ | SInit _ | SEntry _ | SView _ | SValue _ | SStructure _ | SSignature _),
      (SType _ | SException _ | SInit _ | SEntry _ | SView _ | SValue _ | SStructure _ | SSignature _)
      -> error ?loc "Signature element %s does not correspond to implementation" name

  and search_structures acc c1 c2 =
    let rec cross_content acc l1 l2 =
      match l1, l2 with
        [], _ -> acc
      | (name1,content1) :: l1, (name2,content2) :: l2 when String.equal name1 name2 ->
        let acc = search_content acc name1 content1 content2 in
        cross_content acc l1 l2
      | _, _ :: l2 -> cross_content acc l1 l2
      | (name,_) :: _, [] -> error ?loc "Cannot find all aliases : %s is in signature, but not in the structure" name
    in
    let sort_content = List.sort (fun (s,_) (s',_) -> String.compare s s') in
    cross_content
      acc
      (sort_content c1.sig_content)
      (sort_content c2.sig_content)

  and search_in_typedef acc td1 td2 =
    let map_params =
      try
        List.fold_left2
          (fun (orig_bindings, fake_bindings) p1 p2 ->
             (TypeVarMap.add p1 (TypeVarMap.find_opt p1 acc) orig_bindings),
             (TypeVarMap.add p1 (TVar p2) fake_bindings)
          )
          (TypeVarMap.empty, acc)
      with
        Invalid_argument s -> failwith ( s)
    in
    let replace_map =
      TypeVarMap.fold
        (fun param topt new_acc ->
           match topt with
             None -> TypeVarMap.remove param new_acc
           | Some t -> TypeVarMap.add param t new_acc
        )
    in
    match td1, td2 with
      Alias {aparams = p1; atype = t1}, Alias {aparams = p2; atype = t2} ->
      let orig, fake = map_params p1 p2 in
      let acc = search fake t1 t2 in
      replace_map orig acc
    | SumType {sparams = p1; scons = s1; _}, SumType {sparams = p2; scons = s2; _} ->
      let orig, fake = map_params p1 p2 in
      let acc =
        try
          List.fold_left2
            (fun acc (_, tl1) (_, tl2) -> List.fold_left2 search acc tl1 tl2)
            fake
            s1
            s2
        with
          Invalid_argument s -> failwith ( s) in
      replace_map orig acc
    | RecordType {rparams = p1; rfields = s1; _}, RecordType {rparams = p2; rfields = s2; _} ->
      let orig, fake = map_params p1 p2 in
      let acc =
        try
          List.fold_left2
            (fun acc (_,t1) (_, t2) -> search acc t1 t2)
            fake
            s1
            s2
        with
          Invalid_argument s -> failwith ( s) in
      replace_map orig acc
    | (Alias _ | SumType _ | RecordType _), (Alias _ | SumType _ | RecordType _) ->
      failwith ( "Incompatible type def for alias search")

  and search acc t1 t2 =
    Log.debug "[search_aliases] Matching %a with %a@." pretty t1 pretty t2;
    match t1, t2 with
    | TVar tv, t2 | t2, TVar tv -> (
      let add_if_ok () =
        Log.debug "[search_aliases] Binding %a to %a@."pretty t1 pretty t2;
        TypeVarMap.add tv t2 acc
      in
      match TypeVarMap.find_opt tv acc with
      | None -> add_if_ok ()
      | Some t ->
         match equal t t2 with
         | Aliases _ -> add_if_ok ()
         | _ ->
            Log.debug
              "[search_aliases] Error : %a is already matched to %a,\
               cannot be matched to %a."
              pp_typvar tv pretty t pretty t2;
            failwith ( "Search alias error")
    )
    (* | _, TVar _ ->
     *   Log.debug "[search_aliases] Trying to merge %a into %a : error@.."
     *     pretty t1 pretty t2;
     *   failwith ( "Search : genericity error.") *)

    | TTuple l1, TTuple l2 ->
      Log.debug "[search_aliases] Tuples@.";
      let le1 = List.length l1 in
      let le2 = List.length l2 in
      if le1 = le2 then
        search_in_lists acc l1 l2
      else
        error ?loc "Tuple %a expects %d arguments, while tuple %a expects %d."
          Love_type.pretty t1 le1 Love_type.pretty t2 le2

    | TUser (n1, l1),TUser (n2, l2)  ->
      Log.debug "[search_aliases] User defined type@.";
      if Ident.equal String.equal n1 n2
      then
        begin
          let le1 = List.length l1 in
          let le2 = List.length l2 in
          if le1 = le2 then
            search_in_lists acc l1 l2
          else
            error ?loc "User type %a expects %d arguments, while its \
                        implementation expects %d arguments."
              Ident.print_strident n1 le1 le2
        end
      else (
        Log.debug "[search_aliases] Different id types : %a <> %a@."
          pretty_typename n1 pretty_typename n2;
        error ?loc
          "Type %a is incompatible with type %a"
          Love_type.pretty t1 Love_type.pretty t2
      )

    | TArrow (t1, t2), TArrow(t1', t2') -> (
        Log.debug "[search_aliases] (Big)Map/Arrow@.";
        let acc = search acc t1 t1' in
        search acc t2 t2'
      )
    | TArrow (t1, _), t -> search acc t1 t
    | t, TArrow (t1, _) -> search acc t t1
    | TContractInstance (Named  _n1), TContractInstance (Named _n2)
    | TPackedStructure (Named _n1), TPackedStructure (Named _n2) ->
      Log.debug "[search_aliases] Aliases between named contracts cannot be found";
      acc
    | TContractInstance (Anonymous c1), TContractInstance (Anonymous c2)
    | TPackedStructure (Anonymous c1), TPackedStructure (Anonymous c2) ->
      search_structures acc c1 c2
    | TContractInstance (Named n), TContractInstance (Anonymous _c)
    | TPackedStructure (Named n), TPackedStructure (Anonymous _c) -> (
        Log.debug "[search_aliases] Aliases search : Named VS anonymous";
        match Ident.split n with
          "UnitContract", None -> acc
        | _ ->
          error ?loc
            "Searching aliases between %a and anonymous contracts"
            Ident.print_strident n
      )
    | TContractInstance (Anonymous _c), TContractInstance (Named n)
    | TPackedStructure (Anonymous _c), TPackedStructure (Named n) -> (
        Log.debug "[search_aliases] Aliases search : Anonymous VS Named";
        match Ident.split n with
          "UnitContract", None -> acc
        | _ ->
          error ?loc "Searching aliases between anonymous and %a" Ident.print_strident n
      )
    | TForall (arg, t), TForall (_arg', t') -> (
        Log.debug "[search_aliases] Foralls@.";
        let old_binding = TypeVarMap.find_opt arg acc in
        let acc = search acc t t' in
        match old_binding with
          None -> TypeVarMap.remove arg acc
        | Some t -> TypeVarMap.add arg t acc
      )
    | TForall (arg, t), t' ->
       let old_binding = TypeVarMap.find_opt arg acc in
       let acc = search (TypeVarMap.remove arg acc) t t' in
       (match old_binding with
          None -> acc
        | Some t -> TypeVarMap.add arg t acc
       )

    | ( TUser _ | TContractInstance _ | TPackedStructure _
       | TTuple _ ),
      ( TContractInstance _ | TPackedStructure _
       | TTuple _ | TForall _ | TUser _ ) ->
      error ?loc "Type %a is not compatible with type %a"
        Love_type.pretty t1 Love_type.pretty t2
  in
  search
    TypeVarMap.empty
    t1
    t2

let apply_types ?loc env e te expected_ty =
  let aliases = search_aliases ?loc (remove_foralls te) expected_ty in
  let rec cross_exp_typ (e, t) = match e, t with
    | {Utils.content = TLambda {exp;_}; _}, TForall (targ, t')
    | exp, TForall (targ, t') -> (
        match TypeVarMap.find_opt targ aliases with
        | Some (TVar v) ->
          let new_e, new_t = cross_exp_typ (exp,t') in
          debug "[apply_types] %a is substitued by %a@."
            Love_type.pp_typvar targ Love_type.pp_typvar v;
          mk_tlambda v new_e, TForall (v, Love_type.replace (user_comp env) targ (TVar v) new_t)

        | Some alias ->
          let new_e, new_t = cross_exp_typ (exp,t') in
          debug "[apply_types] %a is replaced by %a@."
            Love_type.pp_typvar targ pretty alias;
          let e = match e.content with
            | TLambda _ -> new_e
            | _ -> mk_tapply new_e alias in
          e, Love_type.replace (user_comp env) targ alias new_t

        | None ->
          let new_e, new_t = cross_exp_typ (exp,t') in
          debug "[apply_types] %a remains polymorphic@."
            Love_type.pp_typvar targ;
          mk_tlambda targ new_e, TForall (targ,new_t)
      )
    | e,t -> e,t
  in
  cross_exp_typ (e, te)

let mk_primitive_lambda ?loc expected_typ spec_args prim_name =
  Log.debug "[mk_primitive_lambda] Primitive %s%a@."
    prim_name
    (fun fmt -> function
      | None -> ()
      | Some t -> Format.fprintf fmt " with normalized expected type %a" Love_type.pretty t
    ) expected_typ ;
  let prim =
    match Love_primitive.from_string prim_name with
      None -> error ?loc "Unknown Love primitive %s" prim_name
    | Some p -> p
  in
  let loc, liqloc = match loc with
    | None -> None, None | Some l -> love_loc l, loc in
  let poly_prim = match spec_args with
    | ANone -> mk_var ?loc (string_to_ident prim_name)
    | args -> mk_var_with_arg ?loc (string_to_ident prim_name) args in
  let tprim = Love_primitive.type_of (prim, spec_args) in
  let aliases = match expected_typ with
    | None -> TypeVarMap.empty
    | Some expected_typ ->
       Log.debug "[mk_primitive_lambda] Primitive type : %a@." pretty tprim;
       let puretprim = remove_foralls tprim in
       Log.debug "[mk_primitive_lambda] Expected type : %a@." pretty expected_typ;
       Log.debug "[mk_primitive_lambda] Matching %a and %a@."
         pretty puretprim
         pretty expected_typ;
       let aliases = search_aliases ?loc:liqloc puretprim expected_typ in
       Log.debug "[mk_primitive_lambda] Matching done@.";
       aliases
  in
  let rec add_not_binded_tvars ptyp exp =
    match ptyp with
      TForall (tv, t) -> (
        match TypeVarMap.find_opt tv aliases with
        | None -> exp
          (* let arg = Love_type.fresh_typevar () in
           * add_not_binded_tvars
           *   t
           *   (mk_tlambda ?loc arg (mk_tapply ?loc exp (TVar arg))) *)
        | Some typ ->
          add_not_binded_tvars
            t
            (mk_tapply ?loc exp typ)
    )
    | _ -> exp
  in add_not_binded_tvars tprim poly_prim

let mk_primitive_lambda ?loc env ?expected_typ ?(spec_args=ANone) prim_name =
  Log.debug "[mk_primitive_lambda] Primitive %s%a@."
    prim_name
    (fun fmt -> function
      | None -> ()
      | Some t -> Format.fprintf fmt " with expected type %a" Love_type.pretty t
    ) expected_typ ;
  let expexted_typ = match expected_typ with
    | None -> None
    | Some t -> Some (Love_tenv.normalize_type ~relative:true t env) in
  mk_primitive_lambda ?loc expexted_typ spec_args prim_name

let reserved_types =
  Collections.StringSet.of_list
    [
      "address";
      "big_map";
      "bool";
      "bytes";
      "dun";
      "instance";
      "key";
      "key_hash";
      "list";
      "map";
      "nat";
      "operation";
      "set";
      "signature";
      "string";
      "timestamp";
      "unit";
      "tez";
      "int";
      "chain_id";
    ]

let reserved_structures =
  Collections.StringSet.of_list
    [
      "UnitContract"
    ]

(** Type utils *)

let is_int = function
  | TUser (LName "int", []) -> true
  | _ -> false

let option t = Love_type_list.get_type "option" [t]
let is_option = function
  | TUser (LName "option", [t]) -> Some t
  | _ -> None

let mk_psome arg = mk_pconstr "Some" [arg]
let mk_pnone ()  = mk_pconstr "None" []

let mk_some t arg = mk_constr (Ident.create_id "Some") [t] [arg]
let mk_none t = mk_constr (Ident.create_id "None") [t] []

let mk_emptyset ?typ () =
  let map = mk_var (string_to_ident "Set.empty") in
  match typ with
    Some t -> mk_tapply map t
  | None -> map

let mk_emptymap ?typs () =
  let map = mk_var (string_to_ident "Map.empty") in
  match typs with
    Some (tk, tb) ->
    mk_tapply (mk_tapply map tk) tb
  | None -> map

let mk_emptybigmap tk tb =
  let map = mk_var (string_to_ident "BigMap.empty") in
  mk_tapply (mk_tapply map tk) tb

let get_signature_from_name name ty tenv =
  let name = match name with
      None -> "default"
    | Some n -> n in
  TYPE.Anonymous {
    TYPE.sig_kind = Contract [];
    sig_content = [name, SEntry ty]
  }

let view_signature tenv name ty =
  TYPE.Anonymous {
    TYPE.sig_kind = Contract [];
    sig_content = [name, SView ty]
  }

let the = function
    None -> failwith "Compil_utils.the"
  | Some o -> o

let rec return_type_with_args ?loc env arrowtyp args =
  match arrowtyp, args with
  | t, [] -> t
  | TForall (_, tarr), _ -> return_type_with_args env tarr args
  | TArrow (t1, t2), t::tl -> (
      debug "[return_type_with_args] Search aliases between %a and %a@."
        pretty t1 pretty t;
      let aliases = search_aliases ?loc t1 t in
      return_type_with_args
        env
        (replace_map (fun _ -> true) aliases t2) tl
    )
  | t, l ->
    debug "[return_type_with_args] Type %a is called with %i arguments@."
      pretty t
      (List.length l);
    error ?loc "Type %a is called with %i arguments"
      pretty t
      (List.length l)

let ident_split_end i =
  let rec loop id =
    match id with
      Ident.LName m -> None, m
    | LDot (m, i) ->
      begin
        let id, name = loop i in
        let new_id =
          match id with
            None -> Some (Ident.create_id m)
          | Some id -> Some (Ident.put_in_namespace m id) in
        new_id, name
      end
  in
  loop i

let rec has_lambda ty =
  let open LiquidTypes in
  match expand ty with
  | Tlambda _ | Tclosure _ -> true
  | Toperation | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes
  | Ttimestamp | Tkey | Tkey_hash | Tsignature | Taddress | Tfail | Tchainid
  | Tcontract _ ->
     false
  | Ttuple l -> List.exists has_lambda l
  | Toption ty | Tlist ty | Tset ty | Tcontract_handle (_, ty) ->
     has_lambda ty
  | Tmap (t1, t2) | Tbigmap (t1, t2) | Tor (t1, t2) | Tcontract_view (_, t1, t2) ->
     has_lambda t1 || has_lambda t2
  | Trecord (_, l) | Tsum (_, l) ->
     List.exists (fun (_, t) -> has_lambda t) l
  | Tvar _ | Tpartial _ -> false
