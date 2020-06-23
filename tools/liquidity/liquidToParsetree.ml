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

(* The version that will be required to compile the generated files. *)
let output_version =
  match String.split_on_char '-' LiquidVersion.version with
  | x :: _ -> x
  | [] -> assert false

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

module HAbbrev = Hashtbl.Make (struct
    type t = datatype
    let rec erase_names ty = match ty with
      | Tvar { contents  = { contents = { tyo = Some ty }}} -> erase_names ty
      | Tunit|Tbool|Tint|Tnat|Ttez
      | Tstring|Tbytes|Ttimestamp|Tkey|Tkey_hash|Tsignature|Toperation|Taddress
      | Tfail | Tpartial _ | Tvar _ | Tchainid -> ty
      | Ttuple tyl -> Ttuple (List.map erase_names tyl)
      | Toption ty -> Toption (erase_names ty)
      | Tlist ty -> Tlist (erase_names ty)
      | Tset ty -> Tset (erase_names ty)
      | Tmap (ty1, ty2) -> Tmap (erase_names ty1, erase_names ty2)
      | Tbigmap (ty1, ty2) -> Tbigmap (erase_names ty1, erase_names ty2)
      | Tor (ty1, ty2) -> Tor (erase_names ty1, erase_names ty2)
      | Tlambda (ty1, ty2, u) ->
        Tlambda (erase_names ty1, erase_names ty2, u)
      | Tclosure ((ty1, ty2), ty3, u) ->
        Tclosure ((erase_names ty1, erase_names ty2), erase_names ty3, u)
      | Trecord (_rn, fl) ->
        Trecord ("", List.map (fun (fn, fty) -> (fn, erase_names fty)) fl)
      | Tsum (_sn, cl) ->
        Tsum (None, List.map (fun (cn, cty) -> (cn, erase_names cty)) cl)
      | Tcontract (entry, ty) -> Tcontract (entry, erase_names ty)
    let hash ty = Hashtbl.hash (erase_names ty)
    let equal ty1 ty2 = eq_types (erase_names ty1) (erase_names ty2)
  end)

let cpt_abbrev = ref 0
let abbrevs = HAbbrev.create 101
let out_abbrevs = HAbbrev.create 0
let rev_abbrevs = Hashtbl.create 101
let top_level_contracts = ref []

let get_abbrev ty =
  match HAbbrev.find abbrevs ty with
  | s, TypeName _, _ -> typ_constr s []
  | s, ContractType _, _ -> typ_constr (s ^ ".instance") []

let rec add_abbrev ?(ignore_out=false) s ty kind =
  try
    if ignore_out then raise Not_found;
    let _, kind, _ = HAbbrev.find out_abbrevs ty in
    add_abbrev ~ignore_out:true s Tunit kind
  with Not_found ->
  try
    match HAbbrev.find abbrevs ty with
    | s', kind', _ when s' <> s || kind' <> kind -> raise Not_found
    | _, TypeName _, _ -> typ_constr s []
    | _, ContractType _, _ -> typ_constr (s ^ ".instance") []
  with Not_found ->
    incr cpt_abbrev;
    let s =
      if Hashtbl.mem rev_abbrevs s then
        s ^ string_of_int !cpt_abbrev
      else s in
    HAbbrev.add abbrevs ty (s, kind, !cpt_abbrev);
    Hashtbl.replace rev_abbrevs s ty;
    match kind with
    | TypeName _ -> typ_constr s []
    | ContractType _ -> typ_constr (s ^ ".instance") []

let reset_env () =
  cpt_abbrev := 0;
  (* out_abbrevs := HAbbrev.create 0; *)
  HAbbrev.reset abbrevs;
  Hashtbl.reset rev_abbrevs;
  top_level_contracts := []

let save_env () =
  let cpt = !cpt_abbrev in
  let contracts = !top_level_contracts in
  let save_abbrevs = HAbbrev.copy abbrevs in
  let save_out_abbrevs = HAbbrev.copy out_abbrevs in
  HAbbrev.iter (HAbbrev.add out_abbrevs) abbrevs;
  let save_rev_abbrevs = Hashtbl.copy rev_abbrevs in
  (* reset_env (); *)
  (fun () ->
     reset_env ();
     cpt_abbrev := cpt;
     top_level_contracts := contracts;
     (* out_abbrevs := HAbbrev.create 0; *)
     HAbbrev.iter (HAbbrev.add out_abbrevs) save_out_abbrevs;
     HAbbrev.iter (HAbbrev.add abbrevs) save_abbrevs;
     Hashtbl.iter (Hashtbl.add rev_abbrevs) save_rev_abbrevs;
  )

let list_caml_abbrevs_in_order () =
  HAbbrev.fold (fun ty v l -> (ty, v) :: l) abbrevs []
  |> List.fast_sort (fun (_, (_, _, i1)) (_, (_, _, i2)) -> i1 - i2)
  |> List.map (fun (ty, (s, caml_ty, _)) -> s, caml_ty, ty)

let rec convert_type ~abbrev ?name ty =
  let params =
    LiquidTypes.free_tvars ty
    |> StringSet.elements
    |> List.map (fun v -> Typ.var v) in
  match ty with
  | Ttez -> typ_constr (LiquidOptions.amount_type ()) []
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
  | Tchainid -> typ_constr "chain_id" []
  | Tsum (None, constrs) ->
    let rows = List.map (fun (n, ty) ->
        let n = match n.[0] with
          | '`' -> String.sub n 1 (String.length n - 1)
          | _ -> n in
        let ty = convert_type ~abbrev ty in
        Rtag (id n, [], false, [ty])
      ) constrs in
    Typ.variant rows Closed None
  | Tsum (Some name, _)
  | Trecord (name, _) ->
    let args =
      try
        let known_ty = Hashtbl.find rev_abbrevs name in
        let subst = LiquidTypes.build_subst known_ty ty in
        List.map (fun (_, t) -> convert_type ~abbrev t)
          (StringMap.bindings subst)
      with Not_found -> params in
    typ_constr name args
  | Tcontract (_, ty) ->
    Typ.extension (id "handle", PTyp (convert_type ~abbrev ty))
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
        | Tfail | Trecord _ | Tsum _ | Tcontract _ | Tchainid -> assert false
        | Ttuple args ->
          Typ.tuple (List.map (convert_type ~abbrev) args), "pair_t"
        | Tor (x,y) ->
          typ_constr "variant" [convert_type ~abbrev x; convert_type ~abbrev y], "variant_t"
        | Tlambda (x,y, _) ->
          Typ.arrow Nolabel (convert_type ~abbrev x) (convert_type ~abbrev y), "lambda_t"
        | Tclosure ((x,e),r, _) ->
          Typ.arrow
            ~attrs:[id "closure", PTyp (convert_type ~abbrev e)]
            Nolabel
            (convert_type ~abbrev x) (convert_type ~abbrev r),
          "closure_t"
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

let convert_primitive prim args =
  match prim, args with
  | Prim_and, x :: _ when x.ty = Tnat -> "land"
  | Prim_or, x :: _ when x.ty = Tnat -> "lor"
  | Prim_xor, x :: _ when x.ty = Tnat -> "lxor"
  | Prim_not, [x] when x.ty = Tnat || x.ty = Tint -> "lnot"
  | _ -> LiquidTypes.string_of_primitive prim

let rec convert_const ~abbrev (expr : (datatype, 'a) exp const) =
  match expr with
  | CInt n ->
    Exp.constant (Const.integer (LiquidNumber.liq_of_integer n)),
    true
  | CNat n ->
    Exp.constant (Const.integer ~suffix:'p'
                    (LiquidNumber.liq_of_integer n)),
    true
  | CString s -> Exp.constant (Const.string s), true
  | CUnit -> Exp.construct (lid "()") None, true
  | CBool false -> Exp.construct (lid "false") None, true
  | CBool true -> Exp.construct (lid "true") None, true
  | CNone -> Exp.construct (lid "None") None, false
  | CSome x ->
    let x, inferable_type = convert_const ~abbrev x in
    Exp.construct (lid "Some") (Some x), inferable_type
  | CLeft x ->
    let x, inferable_type = convert_const ~abbrev x in
    Exp.construct (lid "Left") (Some x), inferable_type
  | CRight x ->
    let x, inferable_type = convert_const ~abbrev x in
    Exp.construct (lid "Right") (Some x), inferable_type
  | CConstr (c, CUnit) -> Exp.construct (lid c) None, true
  | CConstr (c, x) ->
    let x, inferable_type = convert_const ~abbrev x in
    Exp.construct (lid c) (Some x), inferable_type
  | CTuple args ->
    let args, inferable_type = List.fold_right (fun x (args, inferable_type) ->
        let x, infer = convert_const ~abbrev x in
        x :: args, inferable_type && infer
      ) args ([], true) in
    Exp.tuple args, inferable_type
  | CTez n ->
    begin match n.mutez with
      | None ->
        Exp.constant (Const.integer ~suffix:'\231' (LiquidNumber.liq_of_tez n)),
        true
      | Some _ ->
        Exp.constant (Const.float ~suffix:'\231' (LiquidNumber.liq_of_tez n)),
        true
    end
  | CTimestamp s ->
    let inferable_type = try ignore (int_of_string s); false with _ -> true in
    Exp.constant (Pconst_integer (s, Some '\232')), inferable_type
  | CKey_hash n ->
    let c = Exp.constant (Pconst_integer (n, Some '\233')) in
    if n.[0] <> '0' then c, true
    else Exp.constraint_ c (convert_type ~abbrev Tkey_hash), true
  | CKey n ->
    let c = Exp.constant (Pconst_integer (n, Some '\234')) in
    if n.[0] <> '0' then c, true
    else Exp.constraint_ c (convert_type ~abbrev Tkey), true
  | CSignature n ->
    let c = Exp.constant (Pconst_integer (n, Some '\235')) in
    if n.[0] <> '0' then c, true
    else Exp.constraint_ c (convert_type ~abbrev Tsignature), true
  | CContract (n, (None | Some "default")) ->
    Exp.constant (Pconst_integer (n, Some '\236')), false
  | CContract (n, Some entry) ->
    Exp.ident (lid (String.concat "%" [n; entry])), false
    (* Exp.apply
     *   (Exp.ident (lid "%"))
     *   [Nolabel, Exp.constant (Pconst_integer (n, Some '\236'));
     *    Nolabel, Exp.ident (lid entry)],
     * false *)
  | CBytes n -> Exp.constant (Pconst_integer (n, Some '\237')), true

  | CList [] -> Exp.construct (lid "[]") None, false
  | CList (head :: tail) ->
    let head, inferable_type_h = convert_const ~abbrev head in
    let tail, inferable_type_t = convert_const ~abbrev (CList tail) in
    let inferable_type = inferable_type_h || inferable_type_t in
    Exp.construct (lid "::") (Some (Exp.tuple [head; tail])), inferable_type
  | CSet [] ->
    Exp.construct (lid "Set") None, false
  | CSet list ->
    let list, inferable_type = convert_const ~abbrev (CList list) in
    Exp.construct (lid "Set") (Some list), inferable_type
  | CMap [] ->
    Exp.construct (lid "Map") None, false
  | CBigMap BMList [] ->
    Exp.construct (lid "BigMap") None, false
  | CBigMap BMId id ->
    let id = Exp.constant (Const.integer (LiquidNumber.liq_of_integer id)) in
    Exp.construct (lid "BigMap") (Some id), false
  | CMap list | CBigMap BMList list ->
    let list = List.map (fun (key, value) -> CTuple [key; value]) list in
    let list, inferable_type = convert_const ~abbrev (CList list) in
    let m = match expr with
      | CMap _ -> "Map"
      | CBigMap _ -> "BigMap"
      | _ -> assert false
    in
    Exp.construct (lid m) (Some list), inferable_type
  | CRecord labels ->
    Exp.record
      (List.map (fun (f, x) ->
           let x, _ = convert_const ~abbrev x in
           lid f, x) labels)
      None, true
  | CLambda { arg_name; arg_ty; body } ->
    Exp.fun_ Nolabel None
      (Pat.constraint_
         (pat_of_lname arg_name ~ty:arg_ty)
         (convert_type ~abbrev ~name:(arg_name.nname ^ "_t") arg_ty))
      (convert_code ~abbrev body), true


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

  | Const { ty; const } ->
      let const, inferable_type = convert_const ~abbrev const in
      if inferable_type then const
      else Exp.constraint_ ~loc const (convert_type ~abbrev ty)

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
          [convert_const ~abbrev cst |> fst]
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
  | Apply { prim =  Prim_exec _; args =  [f; x] } ->
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
    let contract_exp = convert_code ~abbrev contract in
    Exp.apply ~loc
      (Exp.ident (lid "Contract.call"))
      [
        Labelled "dest", contract_exp;
        Nolabel, convert_code ~abbrev arg;
        Labelled "amount", convert_code ~abbrev amount;
      ]

  | Call { contract; amount; entry = Some entry; arg } ->
    let contract_exp = convert_code ~abbrev contract in
    Exp.apply ~loc
      (Exp.field contract_exp (lid entry))
      [
        Nolabel, convert_code ~abbrev arg;
        Labelled "amount", convert_code ~abbrev amount;
      ]

  | Self { entry } ->
    Exp.extension (
      id ~loc "handle",
      PStr [
        Str.eval (Exp.ident (lid ("Self." ^ entry)))
      ])

  | SelfCall { amount; entry; arg } ->
    Exp.apply ~loc
      (Exp.ident (lid ("Self." ^ entry)))
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
               prim = Prim_exec _;
               args = [f; { desc = Var iter_arg }] }};
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
               prim = Prim_exec _;
               args = [f; { desc = Var iter_arg }] }};
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
              prim = Prim_exec _;
              args = [f; { desc = Var map_arg }] }};
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
                  prim = Prim_exec _;
                  args = [f; { desc = Var map_arg }] }};
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

  | CreateContract { args = [delegate; init_balance; init_storage];
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
      [Labelled "delegate", convert_code ~abbrev delegate;
       Labelled "amount", convert_code ~abbrev init_balance;
       Labelled "storage", convert_code ~abbrev init_storage;
       Nolabel, Exp.pack (Mod.structure structure)
       (* (Mod.ident (lid contract.contract_name)) *)]

  | CreateContract _ -> assert false

  | ContractAt { arg; entry; entry_param } ->
    Exp.apply ~loc
      (Exp.extension (
          id ~loc "handle",
          PSig [
            Sig.extension (
              id ~loc "entry",
              PSig [
                Sig.value
                  (Val.mk (id entry)
                     (convert_type ~abbrev entry_param))
              ])
          ]))
      [ Nolabel, convert_code ~abbrev arg ]

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
  let code = match entry.fee_code with
    | None -> code
    | Some fee_code ->
      let fee_code = convert_code ~abbrev fee_code in
      { code with
        pexp_attributes = code.pexp_attributes @ [
            id "fee", PStr [Str.eval fee_code]
          ] }
  in
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
    || (List.length contract.entries > 1 &&
        String.length s >= 8 && String.sub s 0 8 = "_entries") in
  List.iter (fun (s, ty) ->
      if not (ignore_type s) then
        ignore (add_abbrev s ty (TypeName (convert_type ~abbrev:false ty)))
    ) types;
  begin match type_annots with
    | Some type_annots ->
      Hashtbl.iter (fun ty s ->
          match ty with
          | Tlambda _ | Tclosure _ | Tor _ | Ttuple _
            when abbrev && not @@ HAbbrev.mem abbrevs ty ->
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
            | Tsum (Some name, cstrs) when not @@ StringSet.mem name !seen ->
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
  (if output_version = "inf" then [] else [ version_caml ] ) @
  types_caml @
  List.rev !top_level_contracts @ values @ entries


let structure_of_contract ?(abbrev=false) ?type_annots ?(types=[]) contract =
  (* if !LiquidOptions.verbosity > 0 then
   *   Format.eprintf "Contract %s to AST@."
   *     (LiquidNamespace.qual_contract_name contract); *)
  reset_env ();
  HAbbrev.reset out_abbrevs;
  structure_of_contract ~abbrev ?type_annots ~types contract

let translate_expression = convert_code ~abbrev:false

let convert_type ?(abbrev=false) ty = convert_type ~abbrev ty

let convert_code ?(abbrev=false) code = convert_code ~abbrev code

let convert_const  ?(abbrev=false) c = convert_const ~abbrev c |> fst
