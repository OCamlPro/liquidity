(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* The version that will be required to compile the generated files. *)
let output_version = "1.02"

open Asttypes
open Longident
open Parsetree
open LiquidTypes

open Ast_helper

let loc_of_loc loc =
  let open Lexing in
  let open Location in
  match loc.loc_pos with
  | None -> !default_loc
  | Some ((begin_line, begin_col), (end_line, end_col)) ->
    {
      loc_start = {
        pos_fname = loc.loc_file;
        pos_lnum = begin_line;
        pos_bol = 0;
        pos_cnum = begin_col
      };
      loc_end = {
        pos_fname = loc.loc_file;
        pos_lnum = end_line;
        pos_bol = 0;
        pos_cnum = end_col
      };
      loc_ghost = false;
    }

let id ?loc txt =
  { txt; loc = match loc with None -> !default_loc | Some l -> l }

let lid s = id (Longident.parse s)

let typ_constr s args = Typ.constr (lid s) args

let pat_of_name ?loc ?ty name = match name, ty with
  | "_", Some Tunit -> Pat.construct ?loc (id ?loc (Lident "()")) None
  | "_", _ -> Pat.any () (* unused pattern *)
  | _, _ -> Pat.var (id ?loc name)

let pat_of_lname ?ty n =
  pat_of_name ~loc:(loc_of_loc n.nloc) ?ty n.nname

type abbrev_kind =
  | TypeName of core_type
  | ContractType of module_type

let cpt_abbrev = ref 0
let abbrevs = Hashtbl.create 101
let rev_abbrevs = Hashtbl.create 101
let top_level_contracts = ref []

let get_abbrev ty =
  match Hashtbl.find abbrevs ty with
  | s, TypeName _, _ -> typ_constr s []
  | s, ContractType _, _ -> typ_constr (s ^ ".instance") []

let add_abbrev s ty kind =
  try
    match Hashtbl.find abbrevs ty with
    | s', kind', _ when s' <> s || kind' <> kind -> raise Not_found
    | _, TypeName _, _ -> typ_constr s []
    | _, ContractType _, _ -> typ_constr (s ^ ".instance") []
  with Not_found ->
    incr cpt_abbrev;
    let s =
      if Hashtbl.mem rev_abbrevs s then
        s ^ string_of_int !cpt_abbrev
      else s in
    Hashtbl.add abbrevs ty (s, kind, !cpt_abbrev);
    Hashtbl.replace rev_abbrevs s ty;
    match kind with
    | TypeName _ -> typ_constr s []
    | ContractType _ -> typ_constr (s ^ ".instance") []

let reset_env () =
  cpt_abbrev := 0;
  Hashtbl.reset abbrevs;
  Hashtbl.reset rev_abbrevs;
  top_level_contracts := []

let save_env () =
  let cpt = !cpt_abbrev in
  let contracts = !top_level_contracts in
  let save_abbrevs = Hashtbl.copy abbrevs in
  let save_rev_abbrevs = Hashtbl.copy rev_abbrevs in
  (* reset_env (); *)
  (fun () ->
     reset_env ();
     cpt_abbrev := cpt;
     top_level_contracts := contracts;
     Hashtbl.iter (Hashtbl.add abbrevs) save_abbrevs;
     Hashtbl.iter (Hashtbl.add rev_abbrevs) save_rev_abbrevs;
  )

let list_caml_abbrevs_in_order () =
  Hashtbl.fold (fun ty v l -> (ty, v) :: l) abbrevs []
  |> List.fast_sort (fun (_, (_, _, i1)) (_, (_, _, i2)) -> i1 - i2)
  |> List.map (fun (ty, (s, caml_ty, _)) -> s, caml_ty, ty)

let rec convert_type ~abbrev ?name ty =
  let params =
    LiquidTypes.free_tvars ty
    |> StringSet.elements
    |> List.map (fun v -> Typ.var v) in
  match ty with
  | Ttez ->  typ_constr "tez" []
  | Tunit -> typ_constr "unit" []
  | Ttimestamp -> typ_constr "timestamp" []
  | Tint -> typ_constr "int" []
  | Tnat -> typ_constr "nat" []
  | Tbool -> typ_constr "bool" []
  | Tkey -> typ_constr "key" []
  | Tkey_hash -> typ_constr "key_hash" []
  | Tsignature -> typ_constr "signature" []
  | Tstring -> typ_constr "string" []
  | Tbytes -> typ_constr "bytes" []
  | Toperation -> typ_constr "operation" []
  | Taddress -> typ_constr "address" []
  | Tsum (name, _)
  | Trecord (name, _) ->
    let args =
      try
        let known_ty = Hashtbl.find rev_abbrevs name in
        let subst = LiquidTypes.build_subst known_ty ty in
        List.map (fun (_, t) -> convert_type ~abbrev t)
          (StringMap.bindings subst)
      with Not_found -> params in
    typ_constr name args
  | Tcontract contract_sig -> convert_contract_sig ~abbrev contract_sig
  | Tvar { contents = { contents = { id; tyo = None | Some Tpartial _ }}} ->
    Typ.var id
  | Tvar { contents = { contents = { tyo = Some ty }}} ->
    convert_type ~abbrev ?name ty
  | Tfail -> assert false
  | Tpartial _ -> assert false
  | _ ->
    try get_abbrev ty
    with Not_found ->
      let caml_ty, t_name = match ty with
        | Ttez | Tunit | Ttimestamp | Tint | Tnat | Tbool
        | Tkey | Tkey_hash | Tsignature | Tstring | Tbytes | Toperation | Taddress
        | Tfail | Trecord _ | Tsum _ | Tcontract _ -> assert false
        | Ttuple args ->
          Typ.tuple (List.map (convert_type ~abbrev) args), "pair_t"
        | Tor (x,y) ->
          typ_constr "variant" [convert_type ~abbrev x; convert_type ~abbrev y], "variant_t"
        | Tlambda (x,y) ->
          Typ.arrow Nolabel (convert_type ~abbrev x) (convert_type ~abbrev y), "lambda_t"
        | Tclosure ((x,e),r) ->
          Typ.arrow Nolabel (convert_type ~abbrev x) (convert_type ~abbrev r), "closure_t"
        | Tmap (x,y) ->
          typ_constr "map" [convert_type ~abbrev x;convert_type ~abbrev y], "map_t"
        | Tbigmap (x,y) ->
          typ_constr "big_map" [convert_type ~abbrev x;convert_type ~abbrev y], "big_map_t"
        | Tset x ->
          typ_constr "set" [convert_type ~abbrev x], "set_t"
        | Tlist x ->
          typ_constr "list" [convert_type ~abbrev x], "list_t"
        | Toption x ->
          typ_constr "option" [convert_type ~abbrev x], "option_t"
        | Tvar _ | Tpartial _ -> assert false
      in
      let name = match name with
        | Some name -> name
        | None -> t_name
      in
      match ty with
      | Tlambda _ | Tclosure _ | Tor _ | Ttuple _ when abbrev ->
        add_abbrev name ty (TypeName caml_ty)
      | _ ->
        caml_ty

and convert_contract_sig ~abbrev csig =
  let name = match csig.sig_name with
    | None -> "ContractType" (* ^ (string_of_int !cpt_abbrev) *)
    | Some name -> name in
  let val_items = List.map (fun e ->
      let parameter = convert_type ~abbrev e.parameter in
      Sig.extension (
        id "entry",
        PSig [
          Sig.value
            (Val.mk (id e.entry_name)
               (Typ.arrow (Labelled e.parameter_name) parameter
                  (Typ.arrow Nolabel (typ_constr "storage" [])
                     (Typ.tuple [typ_constr "list" [typ_constr "operation" []];
                                 typ_constr "storage" []]))))
        ])
    ) csig.entries_sig in
  let abstr_storage = Sig.type_ Recursive [
      Type.mk ~kind:Ptype_abstract (id "storage")
    ] in
  let signature = Mty.signature (abstr_storage :: val_items) in
  let typ = StringMap.fold (fun n csig' -> function
      | Some _ as acc -> acc
      | None ->
        match csig'.sig_name with
        | Some name when eq_types (Tcontract csig') (Tcontract csig) ->
          Some (typ_constr (name ^ ".instance") [])
        | _ -> None
    ) predefined_contract_types None in
  match typ with
  | Some typ -> typ
  | None -> add_abbrev name (Tcontract csig) (ContractType signature)


let convert_primitive prim args =
  match prim, args with
  | Prim_and, x :: _ when x.ty = Tnat -> "land"
  | Prim_or, x :: _ when x.ty = Tnat -> "lor"
  | Prim_xor, x :: _ when x.ty = Tnat -> "lxor"
  | Prim_not, [x] when x.ty = Tnat || x.ty = Tint -> "lnot"
  | _ -> LiquidTypes.string_of_primitive prim

let rec convert_const ~abbrev (expr : (datatype, 'a) exp const) =
  match expr with
  | CInt n -> Exp.constant (Const.integer (LiquidNumber.liq_of_integer n))
  | CNat n -> Exp.constant (Const.integer ~suffix:'p'
                              (LiquidNumber.liq_of_integer n))
  | CString s -> Exp.constant (Const.string s)
  | CUnit -> Exp.construct (lid "()") None
  | CBool false -> Exp.construct (lid "false") None
  | CBool true -> Exp.construct (lid "true") None
  | CNone -> Exp.construct (lid "None") None
  | CSome x -> Exp.construct (lid "Some")
                 (Some (convert_const ~abbrev x))
  | CLeft x -> Exp.construct (lid "Left")
                 (Some (convert_const ~abbrev x))
  | CRight x -> Exp.construct (lid "Right")
                  (Some (convert_const ~abbrev x))
  | CConstr (c, CUnit) -> Exp.construct (lid c) None
  | CConstr (c, x) -> Exp.construct (lid c)
                        (Some (convert_const ~abbrev x))
  | CTuple args -> Exp.tuple (List.map (convert_const ~abbrev) args)
  | CTez n ->
    begin match n.mutez with
      | None ->
        Exp.constant (Const.integer ~suffix:'\231' (LiquidNumber.liq_of_tez n))
      | Some _ ->
        Exp.constant (Const.float ~suffix:'\231' (LiquidNumber.liq_of_tez n))
    end
  | CTimestamp s -> Exp.constant (Pconst_integer (s, Some '\232'))
  | CKey_hash n -> Exp.constant (Pconst_integer (n, Some '\233'))
  | CKey n -> Exp.constant (Pconst_integer (n, Some '\234'))
  | CSignature n -> Exp.constant (Pconst_integer (n, Some '\235'))
  | CContract n -> Exp.constant (Pconst_integer (n, Some '\236'))
  | CAddress n -> Exp.constant (Pconst_integer (n, Some '\236'))
  | CBytes n -> Exp.constant (Pconst_integer (n, Some '\237'))

  | CList [] -> Exp.construct (lid "[]") None
  | CList (head :: tail) ->
    Exp.construct (lid "::") (Some
                                (Exp.tuple [convert_const ~abbrev head;
                                            convert_const ~abbrev (CList tail)]))
  | CSet [] ->
    Exp.construct (lid "Set") None
  | CSet list ->
    Exp.construct (lid "Set")
      (Some (convert_const ~abbrev (CList list)))
  | CMap [] ->
    Exp.construct (lid "Map") None
  | CBigMap [] ->
    Exp.construct (lid "BigMap") None
  | CMap list | CBigMap list ->
    let args =
      List.fold_left (fun tail (key,value) ->
          Exp.construct (lid "::")
            (Some
               (Exp.tuple
                  [
                    Exp.tuple [
                      convert_const ~abbrev key;
                      convert_const ~abbrev value;
                    ];
                    tail
                  ]))
        ) (Exp.construct (lid "[]") None) list
    in
    let m = match expr with
      | CMap _ -> "Map"
      | CBigMap _ -> "BigMap"
      | _ -> assert false
    in
    Exp.construct (lid m) (Some args)
  | CRecord labels ->
    Exp.record
      (List.map (fun (f, x) -> lid f, convert_const ~abbrev x) labels)
      None
  | CLambda { arg_name; arg_ty; body } ->
    Exp.fun_ Nolabel None
      (Pat.constraint_
         (pat_of_lname arg_name ~ty:arg_ty)
         (convert_type ~abbrev ~name:(arg_name.nname ^ "_t") arg_ty))
      (convert_code ~abbrev body)


and convert_code ~abbrev (expr : (datatype, 'a) exp) =
  let loc = loc_of_loc expr.loc in
  match expr.desc with
  | Var name -> Exp.ident ~loc (lid name)

  | Project { field; record } ->
    Exp.field ~loc
      (convert_code ~abbrev record) (lid field)

  | SetField {record; field; set_val } ->
    (* TODO pretty print x.y.z <- w *)
    Exp.setfield ~loc
      (convert_code ~abbrev record)
      (lid field)
      (convert_code ~abbrev set_val)

  | If { cond; ifthen; ifelse = { desc = Const { const = CUnit }}} ->
    Exp.ifthenelse (convert_code ~abbrev cond)
      (convert_code ~abbrev ifthen) None
  | If { cond; ifthen; ifelse } ->
    Exp.ifthenelse
      (convert_code ~abbrev cond)
      (convert_code ~abbrev ifthen)
      (Some (convert_code ~abbrev ifelse))

  | Seq (x, { desc = Const { const = CUnit }}) ->
    convert_code ~abbrev x

  | Seq (x, y) ->
    Exp.sequence (convert_code ~abbrev x) (convert_code ~abbrev y)

  | Const { ty; const } -> begin
      match ty, const with
      | ( Tint
        | Tnat
        | Tstring
        | Tunit
        | Ttimestamp
        | Ttez
        | Tbool
        | Toperation
        | Tlambda _ ), _ -> convert_const ~abbrev const
      | _, (CList (_ :: _) | CMap (_ :: _) | CBigMap (_ :: _)) ->
        convert_const ~abbrev const
      | (Tsignature, CSignature s
        | Tkey, CKey s
        | Tkey_hash, CKey_hash s) when s.[0] <> '0' -> convert_const ~abbrev const
      | _ ->
        Exp.constraint_
          ~loc (convert_const ~abbrev const) (convert_type ~abbrev ty)
    end

  | Let { bnd_var; bnd_val; body } ->
    Exp.let_ ~loc Nonrecursive
      [ Vb.mk (pat_of_lname bnd_var ~ty:bnd_val.ty)
          (convert_code ~abbrev bnd_val)]
      (convert_code ~abbrev body)

  | Lambda { arg_name; arg_ty; body } ->
    Exp.fun_ ~loc Nolabel None
      (Pat.constraint_
         (pat_of_lname arg_name ~ty:arg_ty)
         (convert_type ~abbrev ~name:(arg_name.nname ^ "_t") arg_ty))
      (convert_code ~abbrev body)

  | Closure _ -> assert false

  | Apply { prim = Prim_Cons; args} ->
    let args = match List.rev args with
      | { desc = Const { const = CList [] as cst }} :: r_args ->
        List.fold_left (fun l a -> convert_code ~abbrev a :: l)
          [convert_const ~abbrev cst]
          r_args
      | _ ->
        List.map (convert_code ~abbrev) args
    in
    Exp.construct ~loc (lid "::") (Some (Exp.tuple args))

  | Apply { prim = Prim_Some; args = [arg] } ->
    Exp.construct ~loc
      (lid "Some") (Some (convert_code ~abbrev arg))
  | Apply { prim = Prim_tuple; args } ->
    Exp.tuple ~loc
      (List.map (convert_code ~abbrev) args)
  | Apply { prim =  Prim_exec; args =  [x; f] } ->
    Exp.apply ~loc
      (convert_code ~abbrev f) [Nolabel, convert_code ~abbrev x]

  | Apply { prim; args } ->
    let prim_name =
      try convert_primitive prim args
      with Not_found -> assert false
    in
    Exp.apply ~loc
      (Exp.ident (lid prim_name))
      (List.map (fun arg ->
           Nolabel,
           convert_code ~abbrev arg) args)

  | Failwith arg  ->
    Exp.apply ~loc
      (Exp.ident (lid "failwith"))
      [Nolabel, convert_code ~abbrev arg]

  | MatchOption { arg; ifnone; some_name; ifsome } ->
    let some_ty = match arg.ty with
      | Toption ty -> ty
      | _ -> assert false in
    Exp.match_ ~loc (convert_code ~abbrev arg)
      [
        Exp.case (Pat.construct (lid "None") None)
          (convert_code ~abbrev ifnone);
        Exp.case (Pat.construct (lid "Some")
                    (Some (pat_of_lname some_name ~ty:some_ty)))
          (convert_code ~abbrev ifsome);
      ]

  | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
    Exp.extension ~loc (id ~loc "nat", PStr [
        Str.eval (
          Exp.match_ (convert_code ~abbrev arg)
            [
              Exp.case (Pat.construct (lid "Plus")
                          (Some (pat_of_lname plus_name)))
                (convert_code ~abbrev ifplus);
              Exp.case (Pat.construct (lid "Minus")
                          (Some (pat_of_lname minus_name)))
                (convert_code ~abbrev ifminus);
            ])
      ])

  | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
    let elt_ty = match arg.ty with
      | Tlist ty -> ty
      | _ -> assert false in
    Exp.match_ ~loc (convert_code ~abbrev arg)
      [
        Exp.case (Pat.construct (lid "[]") None)
          (convert_code ~abbrev ifnil);
        Exp.case (Pat.construct (lid "::")
                    (Some (
                        Pat.tuple
                          [pat_of_lname head_name ~ty:elt_ty;
                           pat_of_lname tail_name ~ty:arg.ty]
                      )))
          (convert_code ~abbrev ifcons);
      ]

  | Transfer { dest; amount } ->
    Exp.apply ~loc (Exp.ident (lid "Account.transfer"))
      [
        Labelled "dest", convert_code ~abbrev dest;
        Labelled "amount", convert_code ~abbrev amount;
      ]

  | Call { contract; amount; entry = None; arg } ->
    Exp.apply ~loc (Exp.ident (lid "Contract.call"))
      [
        Labelled "dest", convert_code ~abbrev contract;
        Labelled "amount", convert_code ~abbrev amount;
        Labelled "parameter", convert_code ~abbrev arg;
      ]

  | Call { contract; amount; entry = Some entry; arg } ->
    let contract_exp = convert_code ~abbrev contract in
    Exp.apply ~loc
      (Exp.field contract_exp (lid entry))
      [
        Nolabel, convert_code ~abbrev arg;
        Labelled "amount", convert_code ~abbrev amount;
      ]

  | Loop { arg_name; body; arg } ->
    Exp.apply ~loc
      (Exp.ident (lid "Loop.loop"))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_lname arg_name ~ty:arg.ty)
          (convert_code ~abbrev body);
        Nolabel, convert_code ~abbrev arg
      ]

  | LoopLeft { arg_name; body; arg; acc } ->
    Exp.apply ~loc
      (Exp.ident (lid "Loop.left"))
      (
        (Nolabel, Exp.fun_ Nolabel None
           (pat_of_lname arg_name)
           (convert_code ~abbrev body)) ::
        (Nolabel, convert_code ~abbrev arg) ::
        (match acc with
         | None -> []
         | Some acc -> [Nolabel, convert_code ~abbrev acc])
      )

  | Fold { prim = (Prim_map_iter|Prim_set_iter|Prim_list_iter as prim);
           arg_name;
           body = { desc = Apply {
               prim = Prim_exec;
               args = [{ desc = Var iter_arg }; f] }};
           arg } when iter_arg = arg_name.nname ->
    Exp.apply ~loc
      (Exp.ident (lid (LiquidTypes.string_of_fold_primitive prim)))
      [ Nolabel, convert_code ~abbrev f;
        Nolabel, convert_code ~abbrev arg;
      ]
  | Fold { prim = (Prim_map_iter|Prim_set_iter|Prim_list_iter as prim);
           arg_name; body; arg } ->
    Exp.apply ~loc
      (Exp.ident (lid (LiquidTypes.string_of_fold_primitive prim)))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_lname arg_name)
          (convert_code ~abbrev body);
        Nolabel, convert_code ~abbrev arg;
      ]
  | Fold { prim; arg_name;
           body = { desc = Apply {
               prim = Prim_exec;
               args = [{ desc = Var iter_arg }; f] }};
           arg; acc } when iter_arg = arg_name.nname ->
    Exp.apply ~loc
      (Exp.ident (lid (LiquidTypes.string_of_fold_primitive prim)))
      [
        Nolabel, convert_code ~abbrev f;
        Nolabel, convert_code ~abbrev arg;
        Nolabel, convert_code ~abbrev acc;
      ]
  | Fold { prim; arg_name; body; arg; acc } ->
    Exp.apply ~loc
      (Exp.ident (lid (LiquidTypes.string_of_fold_primitive prim)))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_lname arg_name)
          (convert_code ~abbrev body);
        Nolabel, convert_code ~abbrev arg;
        Nolabel, convert_code ~abbrev acc;
      ]

  | Map { prim; arg_name;
          body = { desc = Apply {
              prim = Prim_exec;
              args = [{ desc = Var map_arg }; f] }};
          arg } when map_arg = arg_name.nname ->
    Exp.apply ~loc
      (Exp.ident (lid (LiquidTypes.string_of_map_primitive prim)))
      [
        Nolabel, convert_code ~abbrev f;
        Nolabel, convert_code ~abbrev arg;
      ]
  | Map { prim; arg_name; body; arg } ->
    Exp.apply ~loc
      (Exp.ident (lid (LiquidTypes.string_of_map_primitive prim)))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_lname arg_name)
          (convert_code ~abbrev body);
        Nolabel, convert_code ~abbrev arg;
      ]

  | MapFold { prim; arg_name;
              body = { desc = Apply {
                  prim = Prim_exec;
                  args = [{ desc = Var map_arg }; f] }};
              arg; acc } when map_arg = arg_name.nname ->
    Exp.apply ~loc
      (Exp.ident (lid (LiquidTypes.string_of_map_fold_primitive prim)))
      [
        Nolabel, convert_code ~abbrev f;
        Nolabel, convert_code ~abbrev arg;
        Nolabel, convert_code ~abbrev acc;
      ]
  | MapFold { prim; arg_name; body; arg; acc } ->
    Exp.apply ~loc
      (Exp.ident (lid (LiquidTypes.string_of_map_fold_primitive prim)))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_lname arg_name)
          (convert_code ~abbrev body);
        Nolabel, convert_code ~abbrev arg;
        Nolabel, convert_code ~abbrev acc;
      ]

  | Record fields ->
    Exp.record ~loc
      (List.map (fun (name, exp) ->
           lid name, convert_code ~abbrev exp
         ) fields) None

  | MatchVariant { arg; cases } ->
    Exp.match_ ~loc (convert_code ~abbrev arg)
      (List.map (function
           | PAny, exp ->
             Exp.case (Pat.any ()) (convert_code ~abbrev exp)
           | PConstr (constr, var_args), exp ->
             Exp.case
               (Pat.construct (lid constr)
                  (match var_args with
                   | [] -> None
                   | [var_arg] ->
                     let var_ty = match arg.ty with
                       | Tsum (_, l) -> List.assoc constr l
                       | Tor (l, _) when constr = "Left" -> l
                       | Tor (_, r) when constr = "Right" -> r
                       | _ -> assert false in
                     Some (pat_of_name ~loc var_arg ~ty:var_ty)
                   | var_args ->
                     let tuple_tys = match arg.ty with
                       | Tsum (_, l) ->
                         begin match List.assoc constr l with
                           | Ttuple ts -> ts
                           | _ -> assert false
                         end
                       | _ -> assert false in
                     Some
                       (Pat.tuple (List.map2
                                     (fun var_arg var_ty->
                                        pat_of_name ~loc var_arg ~ty:var_ty
                                     ) var_args tuple_tys))
                  ))
               (convert_code ~abbrev exp)
         ) cases)

  | Constructor { constr =  Constr id;
                  arg = { desc = Const { const = CUnit }}}  ->
    Exp.construct ~loc (lid id) None
  | Constructor { constr = Constr id; arg } ->
    Exp.construct ~loc (lid id)
      (Some (convert_code ~abbrev arg))
  | Constructor { constr = Left right_ty; arg } ->
    Exp.constraint_ ~loc
      (Exp.construct (lid "Left")
         (Some
            (convert_code ~abbrev arg)))
      (Typ.constr (lid "variant")
         [Typ.any (); convert_type ~abbrev right_ty])
  | Constructor { constr =  Right left_ty; arg } ->
    Exp.constraint_ ~loc
      (Exp.construct (lid "Right")
         (Some
            (convert_code ~abbrev arg)))
      (Typ.constr (lid "variant")
         [convert_type ~abbrev left_ty; Typ.any ()])

  | CreateContract { args = [manager; delegate; spendable;
                             delegatable; init_balance; init_storage];
                     contract } ->
    let restore_env = save_env () in
    let structure = structure_of_contract ~abbrev contract in
    restore_env ();
    (* let contract_struct_item =
     *   Str.module_
     *   (Mb.mk
     *      (id contract.contract_name)
     *      (Mod.structure structure)) in *)
    (* top_level_contracts := contract_struct_item :: !top_level_contracts; *)
    Exp.apply ~loc
      (Exp.ident (lid "Contract.create"))
      [Labelled "manager", convert_code ~abbrev manager;
       Labelled "delegate", convert_code ~abbrev delegate;
       Labelled "spendable", convert_code ~abbrev spendable;
       Labelled "delegatable", convert_code ~abbrev delegatable;
       Labelled "amount", convert_code ~abbrev init_balance;
       Labelled "storage", convert_code ~abbrev init_storage;
       Nolabel, Exp.pack (Mod.structure structure)
       (* (Mod.ident (lid contract.contract_name)) *)]

  | CreateContract _ -> assert false

  | ContractAt { arg; c_sig } ->
    Exp.constraint_ ~loc
      (Exp.apply ~loc
         (Exp.ident (lid "Contract.at"))
         [ Nolabel, convert_code ~abbrev arg ])
      (convert_type ~abbrev (Toption (Tcontract c_sig)))

  | Unpack { arg; ty } ->
    Exp.constraint_ ~loc
      (Exp.apply ~loc
         (Exp.ident (lid "Bytes.unpack"))
         [ Nolabel, convert_code ~abbrev arg ])
      (convert_type ~abbrev (Toption ty))

  | TypeAnnot { e; ty } ->
    Exp.constraint_ ~loc
      (convert_code ~abbrev e)
      (convert_type ~abbrev ty)

  | Type ty ->
    Exp.extension ~loc
      ({ txt = "type"; loc }, PTyp (convert_type ~abbrev ty))


and structure_item_of_entry ~abbrev storage_ty storage_caml entry =
  (* ignore (convert_type ~abbrev ~name:entry.entry_sig.parameter_name
   *           entry.entry_sig.parameter); *)
  let code = convert_code ~abbrev entry.code in
  Str.extension (
    { txt = "entry"; loc = !default_loc },
    PStr    [
      Str.value Nonrecursive
        [
          Vb.mk (pat_of_name entry.entry_sig.entry_name)
            (Exp.fun_ Nolabel None
               (Pat.constraint_
                  (pat_of_name entry.entry_sig.parameter_name
                     ~ty:entry.entry_sig.parameter)
                  (convert_type ~abbrev entry.entry_sig.parameter)
               )
               (Exp.fun_ Nolabel None
                  (Pat.constraint_
                     (pat_of_name entry.entry_sig.storage_name ~ty:storage_ty)
                     storage_caml
                  )
                  code
               ))
        ]
    ])

and structure_of_contract
    ?(abbrev=true) ?type_annots ?(types=[]) contract =
  reset_env ();
  let ignore_type s =
    StringMap.mem s predefined_types
    || s = "_entries" in
  List.iter (fun (s, ty) ->
      if not (ignore_type s) then
        ignore (add_abbrev s ty (TypeName (convert_type ~abbrev:false ty)))
    ) types;
  begin match type_annots with
    | Some type_annots ->
      Hashtbl.iter (fun ty s ->
          match ty with
          | Tlambda _ | Tclosure _ | Tor _ | Ttuple _
            when abbrev && not @@ Hashtbl.mem abbrevs ty ->
            ignore (add_abbrev s ty (TypeName (convert_type ~abbrev:false ty)))
          | _ -> ()
        ) type_annots
    | None -> () end;
  let storage_caml =
    add_abbrev "storage" contract.storage
      (TypeName (convert_type ~abbrev contract.storage)) in
  let version_caml = Str.extension (
      id "version",
      PStr [
        Str.eval
          (Exp.constant (Const.float output_version))
      ])
  in
  let values = List.map (fun v ->
      Str.value Nonrecursive [
        Vb.mk (pat_of_name v.val_name ~ty:v.val_exp.ty)
          (convert_code ~abbrev v.val_exp)
      ]
    ) contract.values in
  let entries =
    List.map (structure_item_of_entry ~abbrev contract.storage storage_caml)
      contract.entries in
  let types_caml =
    let seen = ref StringSet.empty in
    list_caml_abbrevs_in_order ()
    |> List.map (fun (txt, kind, liq_ty) ->
        let params =
          LiquidTypes.free_tvars liq_ty
          |> StringSet.elements
          |> List.map (fun v -> Typ.var v, Invariant) in
        match kind with
        | TypeName manifest ->
          Str.type_ Recursive [
            match liq_ty with
            | Trecord (name, fields) when not @@ StringSet.mem name !seen ->
              seen := StringSet.add name !seen;
              Type.mk (id txt)
                ~params
                ~kind:(Ptype_record (
                    List.map (fun (label, ty) ->
                        Type.field (id label) (convert_type ~abbrev:false ty)
                      ) fields))
            | Tsum (name, cstrs) when not @@ StringSet.mem name !seen ->
              seen := StringSet.add name !seen;
              Type.mk (id txt)
                ~params
                ~kind:(Ptype_variant (
                    List.map (fun (cstr, ty) ->
                        Type.constructor (id cstr)
                          ~args:(Pcstr_tuple [(convert_type ~abbrev:false ty)])
                      ) cstrs))
            | _ ->
              Type.mk (id txt) ~params ~manifest
          ]
        | ContractType typ -> Str.modtype (Mtd.mk (id txt) ~typ)
      )
  in
  [ version_caml ] @ types_caml @
  List.rev !top_level_contracts @ values @ entries


let structure_of_contract ?(abbrev=false) ?type_annots ?(types=[]) contract =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Contract %s to AST@."
      (LiquidNamespace.qual_contract_name contract);
  reset_env ();
  structure_of_contract ~abbrev ?type_annots ~types contract

let translate_expression = convert_code ~abbrev:false

let convert_type ?(abbrev=false) ty = convert_type ~abbrev ty

let convert_code ?(abbrev=false) code = convert_code ~abbrev code

let convert_const  ?(abbrev=false) c = convert_const ~abbrev c
