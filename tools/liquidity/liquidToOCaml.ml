(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* The version that will be required to compile the generated files. *)
let output_version = "0.36"

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
  { txt; loc = match loc with None -> !default_loc | Some l -> loc_of_loc l }
let lid s = id (Longident.parse s)

let typ_constr s args = Typ.constr (lid s) args

let pat_of_name ?loc = function
  | "_" -> Pat.any ()
  | name -> Pat.var (id ?loc name)

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
  | s, ContractType _, _ -> Typ.package (lid s) []
let add_abbrev s ty kind =
  try get_abbrev ty
  with Not_found ->
    incr cpt_abbrev;
    let s =
      try Hashtbl.find rev_abbrevs s; s ^ string_of_int !cpt_abbrev
      with Not_found -> s in
    Hashtbl.add abbrevs ty (s, kind, !cpt_abbrev);
    Hashtbl.replace rev_abbrevs s ();
    match kind with
    | TypeName _ -> typ_constr s []
    | ContractType  _ -> Typ.package (lid s) []

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
  | Trecord (name, _) -> typ_constr name []
  | Tcontract contract_sig -> convert_contract_sig ~abbrev contract_sig
  | Tfail -> assert false
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
  add_abbrev name (Tcontract csig) (ContractType signature)

let rec convert_const expr =
  match expr with
  | CInt n -> Exp.constant (Const.integer (LiquidPrinter.liq_of_integer n))
  | CNat n -> Exp.constant (Const.integer ~suffix:'p'
                                          (LiquidPrinter.liq_of_integer n))
  | CString s -> Exp.constant (Const.string s)
  | CUnit -> Exp.construct (lid "()") None
  | CBool false -> Exp.construct (lid "false") None
  | CBool true -> Exp.construct (lid "true") None
  | CNone -> Exp.construct (lid "None") None
  | CSome x -> Exp.construct (lid "Some")
                             (Some (convert_const x))
  | CLeft x -> Exp.construct (lid "Left")
                             (Some (convert_const x))
  | CRight x -> Exp.construct (lid "Right")
                             (Some (convert_const x))
  | CConstr (c, CUnit) -> Exp.construct (lid c) None
  | CConstr (c, x) -> Exp.construct (lid c)
                        (Some (convert_const x))
  | CTuple args -> Exp.tuple (List.map convert_const args)
  | CTez n ->
    begin match n.mutez with
      | None ->
        Exp.constant (Const.integer ~suffix:'\231' (LiquidPrinter.liq_of_tez n))
      | Some _ ->
        Exp.constant (Const.float ~suffix:'\231' (LiquidPrinter.liq_of_tez n))
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
                                 (Exp.tuple [convert_const head;
                                             convert_const (CList tail)]))
  | CSet [] ->
     Exp.construct (lid "Set") None
  | CSet list ->
     Exp.construct (lid "Set")
                   (Some (convert_const (CList list)))
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
                                     convert_const key;
                                     convert_const value;
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
      (List.map (fun (f, x) -> lid f, convert_const x) labels)
      None


let convert_primitive prim args =
  match prim, args with
  | Prim_and, x :: _ when x.ty = Tnat -> "land"
  | Prim_or, x :: _ when x.ty = Tnat -> "lor"
  | Prim_xor, x :: _ when x.ty = Tnat -> "lxor"
  | Prim_not, [x] when x.ty = Tnat || x.ty = Tint -> "lnot"
  | _ -> LiquidTypes.string_of_primitive prim


let rec convert_code ~abbrev expr =
  match expr.desc with
  | Var (name, loc) -> Exp.ident ~loc:(loc_of_loc loc) (lid name)

  | Project (loc, field, exp) ->
    Exp.field ~loc:(loc_of_loc loc)
      (convert_code ~abbrev exp) (lid field)

  | SetField (exp, loc, field, arg) -> (* TODO pretty print x.y.z <- w *)
    Exp.setfield ~loc:(loc_of_loc loc)
      (convert_code ~abbrev exp)
      (lid field)
      (convert_code ~abbrev arg)

  | If (cond, ifthen, { desc = Const(_loc, Tunit, CUnit) }) ->
     Exp.ifthenelse (convert_code ~abbrev cond)
                    (convert_code ~abbrev ifthen) None
  | If (cond, ifthen, ifelse) ->
     Exp.ifthenelse (convert_code ~abbrev cond)
                    (convert_code ~abbrev ifthen) (Some (convert_code ~abbrev ifelse))
  | Seq (x, { desc = Const(_loc, Tunit, CUnit) }) ->
     convert_code ~abbrev x

  | Seq (x, y) ->
     Exp.sequence (convert_code ~abbrev x) (convert_code ~abbrev y)

  | Const (loc, ty, cst) -> begin
      match ty, cst with
      | (Tint
        | Tnat
        | Tstring
        | Tunit
        | Ttimestamp
        | Ttez
        | Tbool
        | Toperation), _ -> convert_const cst
      | _, (CList (_ :: _) | CMap (_ :: _) | CBigMap (_ :: _)) ->
        convert_const cst
      | (Tsignature, CSignature s
        | Tkey, CKey s
        | Tkey_hash, CKey_hash s) when s.[0] <> '0' -> convert_const cst
      | _ ->
        Exp.constraint_
          ~loc:(loc_of_loc loc) (convert_const cst) (convert_type ~abbrev ty)
    end

  | Let (var, _inline, loc, exp, body) ->
     Exp.let_ ~loc:(loc_of_loc loc) Nonrecursive
       [ Vb.mk (pat_of_name ~loc var)
           (convert_code ~abbrev exp)]
       (convert_code ~abbrev body)

  | Lambda (arg_name, arg_type, loc, body, _res_type) ->
     Exp.fun_ ~loc:(loc_of_loc loc) Nolabel None
       (Pat.constraint_
          (pat_of_name ~loc arg_name)
          (convert_type ~abbrev ~name:(arg_name^"_t") arg_type))
       (convert_code ~abbrev body)

  | Closure _ -> assert false

  | Apply (Prim_Cons, loc, args) ->
    let args = match List.rev args with
      | { desc = Const (_, _, (CList [] as cst)) } :: r_args ->
        List.fold_left (fun l a -> convert_code ~abbrev a :: l)
          [convert_const cst]
          r_args
      | _ ->
        List.map (convert_code ~abbrev) args
    in
    Exp.construct ~loc:(loc_of_loc loc) (lid "::") (Some (Exp.tuple args))

  | Apply (Prim_Some, loc, [arg]) ->
    Exp.construct ~loc:(loc_of_loc loc)
      (lid "Some") (Some (convert_code ~abbrev arg))
  | Apply (Prim_tuple, loc, args) ->
    Exp.tuple ~loc:(loc_of_loc loc)
      (List.map (convert_code ~abbrev) args)
  | Apply (Prim_exec, loc, [x; f]) ->
    Exp.apply ~loc:(loc_of_loc loc)
      (convert_code ~abbrev f) [Nolabel, convert_code ~abbrev x]

  | Apply (prim, loc, args) ->
     let prim_name =
       try convert_primitive prim args
       with Not_found -> assert false
     in
     Exp.apply ~loc:(loc_of_loc loc)
       (Exp.ident (lid prim_name))
       (List.map (fun arg ->
            Nolabel,
            convert_code ~abbrev arg) args)

  | Failwith (err, loc) ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid "Current.failwith"))
      [Nolabel, convert_code ~abbrev err]

  | MatchOption (exp, loc, ifnone, some_pat, ifsome) ->
     Exp.match_ ~loc:(loc_of_loc loc) (convert_code ~abbrev exp)
                [
                  Exp.case (Pat.construct (lid "None") None)
                           (convert_code ~abbrev ifnone);
                  Exp.case (Pat.construct (lid "Some")
                                          (Some (pat_of_name ~loc some_pat)))
                           (convert_code ~abbrev ifsome);
                ]

  | MatchNat (exp, loc, p, ifplus, m, ifminus) ->
    Exp.extension ~loc:(loc_of_loc loc) (id ~loc "nat", PStr [
        Str.eval (
          Exp.match_ (convert_code ~abbrev exp)
                [
                  Exp.case (Pat.construct (lid "Plus")
                              (Some (pat_of_name ~loc p)))
                    (convert_code ~abbrev ifplus);
                  Exp.case (Pat.construct (lid "Minus")
                              (Some (pat_of_name ~loc m)))
                    (convert_code ~abbrev ifminus);
                ])
      ])

  | MatchList (exp, loc, head_pat, tail_pat, ifcons, ifnil) ->
     Exp.match_ ~loc:(loc_of_loc loc) (convert_code ~abbrev exp)
                [
                  Exp.case (Pat.construct (lid "[]") None)
                           (convert_code ~abbrev ifnil);
                  Exp.case (Pat.construct (lid "::")
                                          (Some (
                                               Pat.tuple
                                                 [pat_of_name ~loc head_pat;
                                                  pat_of_name ~loc tail_pat]
                           )))
                           (convert_code ~abbrev ifcons);
                ]

  | Transfer (loc, contract_exp, amount_exp,
              None, { desc = Const (_, _, CUnit) } ) ->
    Exp.apply ~loc:(loc_of_loc loc) (Exp.ident (lid "Contract.transfer"))
      [
        Nolabel, convert_code ~abbrev contract_exp;
        Nolabel, convert_code ~abbrev amount_exp;
      ]

  | Transfer (loc, contract_exp, amount_exp, None, arg_exp) ->
    Exp.apply ~loc:(loc_of_loc loc) (Exp.ident (lid "Contract.call"))
      [
        Nolabel, convert_code ~abbrev contract_exp;
        Nolabel, convert_code ~abbrev amount_exp;
        Nolabel, convert_code ~abbrev arg_exp;
      ]

  | Transfer (loc, contract_exp, amount_exp, Some entry, arg_exp) ->
    let contract_exp = convert_code ~abbrev contract_exp in
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.field contract_exp (lid entry))
      [
        Nolabel, convert_code ~abbrev arg_exp;
        Nolabel, convert_code ~abbrev amount_exp;
      ]

  | Loop (var_arg, loc, body_exp, arg_exp) ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid "Loop.loop"))
               [
                 Nolabel, Exp.fun_ Nolabel None
                                   (pat_of_name ~loc var_arg)
                                   (convert_code ~abbrev body_exp);
                 Nolabel, convert_code ~abbrev arg_exp
               ]

  | Fold ((Prim_map_iter|Prim_set_iter|Prim_list_iter as prim),
          var_arg, loc,
          { desc = Apply(Prim_exec, _, [ { desc = Var (iter_arg, _) }; f])},
          arg_exp, _acc_exp) when iter_arg = var_arg ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid (LiquidTypes.string_of_fold_primitive prim)))
      [ Nolabel, convert_code ~abbrev f;
        Nolabel, convert_code ~abbrev arg_exp;
      ]
  | Fold ((Prim_map_iter|Prim_set_iter|Prim_list_iter as prim),
          var_arg, loc, body_exp, arg_exp, _acc_exp) ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid (LiquidTypes.string_of_fold_primitive prim)))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_name ~loc var_arg)
          (convert_code ~abbrev body_exp);
        Nolabel, convert_code ~abbrev arg_exp;
      ]
  | Fold (prim, var_arg, loc,
          { desc = Apply(Prim_exec, _, [ { desc = Var (iter_arg, _) }; f])},
          arg_exp,
          acc_exp) when iter_arg = var_arg ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid (LiquidTypes.string_of_fold_primitive prim)))
      [
        Nolabel, convert_code ~abbrev f;
        Nolabel, convert_code ~abbrev arg_exp;
        Nolabel, convert_code ~abbrev acc_exp;
      ]
  | Fold (prim, var_arg, loc, body_exp, arg_exp, acc_exp) ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid (LiquidTypes.string_of_fold_primitive prim)))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_name ~loc var_arg)
          (convert_code ~abbrev body_exp);
        Nolabel, convert_code ~abbrev arg_exp;
        Nolabel, convert_code ~abbrev acc_exp;
      ]

  | Map (prim, var_arg, loc,
          { desc = Apply(Prim_exec, _, [ { desc = Var (map_arg, _) }; f])},
          arg_exp) when map_arg = var_arg ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid (LiquidTypes.string_of_map_primitive prim)))
      [
        Nolabel, convert_code ~abbrev f;
        Nolabel, convert_code ~abbrev arg_exp;
      ]
  | Map (prim, var_arg, loc, body_exp, arg_exp) ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid (LiquidTypes.string_of_map_primitive prim)))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_name ~loc var_arg)
          (convert_code ~abbrev body_exp);
        Nolabel, convert_code ~abbrev arg_exp;
      ]

  | MapFold (prim, var_arg, loc,
          { desc = Apply(Prim_exec, _, [ { desc = Var (map_arg, _) }; f])},
          arg_exp,
          acc_exp) when map_arg = var_arg ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid (LiquidTypes.string_of_map_fold_primitive prim)))
      [
        Nolabel, convert_code ~abbrev f;
        Nolabel, convert_code ~abbrev arg_exp;
        Nolabel, convert_code ~abbrev acc_exp;
      ]
  | MapFold (prim, var_arg, loc, body_exp, arg_exp, acc_exp) ->
    Exp.apply ~loc:(loc_of_loc loc)
      (Exp.ident (lid (LiquidTypes.string_of_map_fold_primitive prim)))
      [
        Nolabel, Exp.fun_ Nolabel None
          (pat_of_name ~loc var_arg)
          (convert_code ~abbrev body_exp);
        Nolabel, convert_code ~abbrev arg_exp;
        Nolabel, convert_code ~abbrev acc_exp;
      ]

  | Record (loc, fields) ->
    Exp.record ~loc:(loc_of_loc loc)
      (List.map (fun (name, exp) ->
           lid name, convert_code ~abbrev exp
         ) fields) None

  | MatchVariant (arg, loc, cases) ->
     Exp.match_ ~loc:(loc_of_loc loc) (convert_code ~abbrev arg)
       (List.map (function
            | CAny, exp ->
              Exp.case (Pat.any ()) (convert_code ~abbrev exp)
            | CConstr (constr, var_args), exp ->
              Exp.case
                (Pat.construct (lid constr)
                   (match var_args with
                    | [] -> None
                    | [var_arg] ->
                      Some (pat_of_name ~loc var_arg)
                    | var_args ->
                      Some
                        (Pat.tuple (List.map
                                      (fun var_arg ->
                                         pat_of_name ~loc var_arg
                                      ) var_args))
                   ))
                (convert_code ~abbrev exp)
          ) cases)

  | Constructor (loc, Constr id, { desc = Const (_loc', Tunit, CUnit) } ) ->
     Exp.construct ~loc:(loc_of_loc loc) (lid id) None
  | Constructor (loc, Constr id, arg) ->
     Exp.construct ~loc:(loc_of_loc loc) (lid id)
       (Some (convert_code ~abbrev arg))
  | Constructor (loc, Left right_ty, arg) ->
     Exp.constraint_ ~loc:(loc_of_loc loc)
       (Exp.construct (lid "Left")
                      (Some
                         (convert_code ~abbrev arg)))
       (Typ.constr (lid "variant")
                   [Typ.any (); convert_type ~abbrev right_ty])
  | Constructor (loc, Right left_ty, arg) ->
     Exp.constraint_ ~loc:(loc_of_loc loc)
       (Exp.construct (lid "Right")
                      (Some
                         (convert_code ~abbrev arg)))
       (Typ.constr (lid "variant")
                   [convert_type ~abbrev left_ty; Typ.any ()])

  | CreateContract (loc, [manager; delegate; spendable;
                          delegatable; init_balance; init_storage], contract) ->
    let restore_env = save_env () in
    let structure = structure_of_contract ~abbrev contract in
    restore_env ();
    (* let contract_struct_item =
     *   Str.module_
     *   (Mb.mk
     *      (id contract.contract_name)
     *      (Mod.structure structure)) in *)
    (* top_level_contracts := contract_struct_item :: !top_level_contracts; *)
    Exp.apply ~loc:(loc_of_loc loc)
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

  | ContractAt (loc, addr, ty) ->
    Exp.constraint_ ~loc:(loc_of_loc loc)
      (Exp.apply ~loc:(loc_of_loc loc)
         (Exp.ident (lid "Contract.at"))
         [ Nolabel, convert_code ~abbrev addr ])
      (convert_type ~abbrev (Toption ty))

  | Unpack (loc, e, ty) ->
    Exp.constraint_ ~loc:(loc_of_loc loc)
      (Exp.apply ~loc:(loc_of_loc loc)
         (Exp.ident (lid "Bytes.unpack"))
         [ Nolabel, convert_code ~abbrev e ])
      (convert_type ~abbrev (Toption ty))


and structure_item_of_entry ~abbrev storage_caml entry =
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
                    (pat_of_name entry.entry_sig.parameter_name)
                    (convert_type ~abbrev entry.entry_sig.parameter)
                 )
                 (Exp.fun_ Nolabel None
                    (Pat.constraint_
                       (pat_of_name entry.entry_sig.storage_name)
                       storage_caml
                    )
                    code
                 ))
          ]
      ])

and structure_of_contract
    ?(abbrev=true) ?type_annots ?(types=[]) contract =
  reset_env ();
  List.iter (fun (s, ty) ->
      if not (StringMap.mem s LiquidFromOCaml.predefined_types) then
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
  let storage_caml = add_abbrev "storage" contract.storage
      (TypeName (convert_type ~abbrev contract.storage)) in
  let version_caml = Str.extension (
      id "version",
      PStr [
        Str.eval
          (Exp.constant (Const.float output_version))
      ])
  in
  let entries =
    List.map (structure_item_of_entry ~abbrev storage_caml) contract.entries in
  let types_caml =
    let seen = ref StringSet.empty in
    list_caml_abbrevs_in_order ()
    |> List.map (fun (txt, kind, liq_ty) ->
        match kind with
        | TypeName manifest ->
          Str.type_ Recursive [
            match liq_ty with
            | Trecord (name, fields) when not @@ StringSet.mem name !seen ->
              seen := StringSet.add name !seen;
              Type.mk (id txt)
                ~kind:(Ptype_record (
                  List.map (fun (label, ty) ->
                      Type.field (id label) (convert_type ~abbrev:false ty)
                    ) fields))
            | Tsum (name, cstrs) when not @@ StringSet.mem name !seen ->
              seen := StringSet.add name !seen;
              Type.mk (id txt)
                ~kind:(Ptype_variant (
                  List.map (fun (cstr, ty) ->
                      Type.constructor (id cstr)
                        ~args:(Pcstr_tuple [(convert_type ~abbrev:false ty)])
                    ) cstrs))
            | _ ->
              Type.mk (id txt) ~manifest
          ]
        | ContractType typ -> Str.modtype (Mtd.mk (id txt) ~typ)
      )
  in
  [ version_caml ] @ types_caml @ List.rev !top_level_contracts @ entries


let structure_of_contract ?(abbrev=true) ?type_annots ?(types=[]) contract =
  reset_env ();
  structure_of_contract ~abbrev ?type_annots ~types contract

let string_of_structure = LiquidOCamlPrinter.string_of_structure

let translate_expression = convert_code ~abbrev:false

let string_of_expression = LiquidOCamlPrinter.string_of_expression

let convert_type ?(abbrev=true) ty = convert_type ~abbrev ty

let convert_code ?(abbrev=true) code = convert_code ~abbrev code
