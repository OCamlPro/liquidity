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

(* We use '\231' as suffix for integers and floats to encode the "tz"
    suffix.  *)

open LiquidTypes
open LiquidNamespace
open LiquidInfer

type 'a ast_elt =
  | Syn_value of 'a value
  | Syn_other_contract of 'a contract
  | Syn_sub_contract of 'a contract
  | Syn_sub_contract_alias of string * 'a contract
  | Syn_entry of 'a entry
  | Syn_init of 'a init

type 'a parsed_struct =
  | PaModule of 'a contract
  | PaContract of 'a contract

exception Stop of syntax_contract

let ident_counter = ref 0

(* The minimal version of liquidity files that are accepted by this compiler *)
let minimal_version = 0.9

(* The maximal version of liquidity files that are accepted by this compiler *)
let maximal_version = 1.042


open Asttypes
open Longident
open Parsetree
open LiquidTypes

let str_of_id id = String.concat "." (Longident.flatten id)

let loc_of_loc = LiquidLoc.loc_of_location


let ppf = Format.err_formatter

let mk_inner_env env contractname =
  let new_env = {
    types = StringMap.empty;
    contract_types = StringMap.empty;
    labels = StringMap.empty;
    constrs = StringMap.empty;
    ext_prims = StringMap.empty;
    filename = env.filename;
    top_env = Some env;
    others = env.others;
    contractname;
    path = env.path @ [ contractname ];
  } in
  env.others <- StringMap.add contractname (Direct new_env) env.others;
  new_env

let error_loc loc fmt =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc ("Unexpected syntax: "^^fmt^^"%!")

let error_version loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "version mismatch %s%!" msg

let unbound_type loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound type %S%!" ty

let unbound_contract_type loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound contract type %S%!" ty

let unbound_contract loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound contract %S%!" ty

let unbound_module loc ty =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unbound module %S%!" ty

let error_arg loc arg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Unexpected argument %S%!" arg

let todo_loc loc msg =
  let loc = loc_of_loc loc in
  LiquidLoc.raise_error ~loc "Syntax %S not yet implemented%!" msg

let has_stack typ = match typ.ptyp_desc with
  | Ptyp_extension ( { txt = "stack" }, _ ) -> true
  | _ -> false

let remove_stack typ = match typ.ptyp_desc with
  | Ptyp_extension ( { txt = "stack" }, PTyp  t ) -> t
  | _ -> typ

let rec translate_type env ?expected typ =
  if has_stack typ then
    error_loc typ.ptyp_loc "Attribute [%%stack] forbidden in this context";
  match typ with
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "unit" }, []) } -> Tunit
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bool" }, []) } -> Tbool
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "int" }, []) } -> Tint
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "nat" }, []) } -> Tnat
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "tez" }, []) } -> Ttez
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "string" }, []) } -> Tstring
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "bytes" }, []) } -> Tbytes
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "timestamp" }, []) } -> Ttimestamp
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key" }, []) } -> Tkey
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "key_hash" }, []) } -> Tkey_hash
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "signature" }, []) } -> Tsignature
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "operation" }, []) } -> Toperation
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "address" }, []) } -> Taddress

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "option" }, [param_type]) } ->
    let expected = match expected with
      | Some (Toption ty) -> Some ty
      | _ -> None
    in
    Toption (translate_type env ?expected param_type)
  | { ptyp_desc = Ptyp_constr ({ txt = Lident "list" }, [param_type]) } ->
    let expected = match expected with
      | Some (Tlist ty) -> Some ty
      | _ -> None
    in
    Tlist (translate_type env ?expected param_type)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "set" }, [param_type]);
      ptyp_loc } ->
    let expected = match expected with
      | Some (Tset ty) -> Some ty
      | _ -> None
    in
    let elt_type = translate_type env ?expected param_type in
    if not @@ comparable_type elt_type then
      error_loc ptyp_loc
        "type %S is not comparable, it cannot be used as element of a set"
        (LiquidPrinter.Liquid.string_of_type elt_type);
    Tset elt_type

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "variant" },
                               [left_type; right_type]) } ->
    let expected1, expected2 = match expected with
      | Some (Tor (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    Tor (translate_type env ?expected:expected1 left_type,
         translate_type env ?expected:expected2 right_type)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "map" },
                               [key_type; val_type]);
      ptyp_loc } ->
    let expected1, expected2 = match expected with
      | Some (Tmap (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    let key_type = translate_type env ?expected:expected1 key_type in
    if not @@ comparable_type key_type then
      error_loc ptyp_loc
        "type %S is not comparable, it cannot be used as key in a map"
        (LiquidPrinter.Liquid.string_of_type key_type);
    let val_type = translate_type env ?expected:expected2 val_type in
    Tmap (key_type, val_type)

  | { ptyp_desc = Ptyp_constr ({ txt = Lident "big_map" },
                               [key_type; val_type]);
      ptyp_loc } ->
    let expected1, expected2 = match expected with
      | Some (Tbigmap (ty1, ty2)) -> Some ty1, Some ty2
      | _ -> None, None
    in
    let key_type = translate_type env ?expected:expected1 key_type in
    if not @@ comparable_type key_type then
      error_loc ptyp_loc
        "type %S is not comparable, it cannot be used as key in a big map"
        (LiquidPrinter.Liquid.string_of_type key_type);
    let val_type = translate_type env ?expected:expected2 val_type in
    Tbigmap (key_type, val_type)

  | { ptyp_desc = Ptyp_arrow (_, parameter_type, return_type) } ->
    (* TODO ? *)
    (* let expected = match expected with
     *   | Some (Tcontract ty) -> Some ty
     *   | _ -> None
     * in *)
    Tlambda (translate_type env (* ?expected:expected *) parameter_type,
             translate_type env return_type,
             default_uncurry ())

  | { ptyp_desc = Ptyp_tuple [ty] } ->
    translate_type env ?expected ty

  | { ptyp_desc = Ptyp_tuple types } ->
    let expecteds = match expected with
      | Some (Ttuple tys) when List.length types = List.length tys ->
        List.map (fun ty -> Some ty) tys
      | _ -> List.map (fun _ -> None) types
    in
    Ttuple (List.map2 (fun ty expected -> translate_type env ?expected ty)
              types expecteds)

  | { ptyp_desc = Ptyp_constr ({ txt = ty_name ; loc }, []) }
    when Longident.last ty_name = "instance" ->
    let contract_type_name =
      match List.rev (Longident.flatten ty_name) with
      | _ :: rpath -> String.concat "." (List.rev rpath)
      | _ -> assert false
    in
    begin
      let loc = loc_of_loc loc in
      try Tcontract (find_contract_type ~loc contract_type_name env)
      with Not_found ->
        unbound_contract_type typ.ptyp_loc contract_type_name
    end

  | { ptyp_desc = Ptyp_constr ({ txt = ty_name; loc }, params); ptyp_loc } ->
    let ty_name = str_of_id ty_name in
    let loc = loc_of_loc loc in
    begin
      try
        find_type ~loc ty_name env (List.map (translate_type env) params)
      with
      | Invalid_argument _ -> error_loc ptyp_loc "too many type arguments"
      | Not_found -> unbound_type typ.ptyp_loc ty_name
    end

  | { ptyp_desc = Ptyp_any; ptyp_loc } ->
    begin match expected with
      | Some ty -> ty
      | None -> error_loc ptyp_loc "cannot infer type"
    end

  | { ptyp_desc = Ptyp_var id; ptyp_loc } -> mk_tvar id

  | { ptyp_loc } -> error_loc ptyp_loc "in type"

let translate_ext_type env typ =
  let rec aux noargs tvs atys typ = match typ with
    (* Type argument *)
    | { ptyp_desc = Ptyp_arrow (_, { ptyp_desc =
                                       Ptyp_extension ( { txt = "type" }, PTyp { ptyp_desc = Ptyp_var tv })
                                   }, return_type); ptyp_loc } ->
      if atys <> [] || noargs then
        error_loc ptyp_loc "Type arguments must come first";
      if List.mem tv tvs then
        error_loc ptyp_loc "Type variable '%s already used in this primitive" tv;
      aux false (tv :: tvs) atys return_type

    (* Argument *)
    | { ptyp_desc = Ptyp_arrow (_, parameter_type, return_type); ptyp_loc } ->
      let ty = translate_type env (remove_stack parameter_type) in
      let stack = has_stack parameter_type in
      if noargs then
        error_loc ptyp_loc "This primitive does not expect any argument"
      else if stack then
        aux false tvs (ty :: atys) return_type
      else if ty = Tunit && atys = [] then
        aux true tvs [] return_type
      else
        error_loc ptyp_loc "Attribute [%%stack] expected";

      (* Result : tuple *)
    | { ptyp_desc = Ptyp_tuple types; ptyp_loc } when atys <> [] || noargs ->
      let tys = List.map (fun ty -> translate_type env (remove_stack ty)) types in
      let stack = has_stack typ in
      if List.exists (fun t -> has_stack t = stack) types then
        error_loc ptyp_loc
          "[%%stack] must be either on the whole tuple or on ALL its components";
      let rtys = if not stack then tys else [Ttuple tys] in
      List.rev tvs, List.rev atys, rtys

    (* Result : any other type *)
    | { ptyp_loc } when atys <> [] || noargs ->
      let ty = translate_type env (remove_stack typ) in
      let stack = has_stack typ in
      if ty <> Tunit && not stack then
        error_loc ptyp_loc "Attribute [%%stack] expected on return value";
      let rtys = if not stack then [] else [ty] in
      List.rev tvs, List.rev atys, rtys

    (* Non-function type *)
    | { ptyp_loc } ->
      error_loc ptyp_loc "Primitives must be functions"
  in
  aux false [] [] typ

(* Prevent uncurring of lambda's in a type *)
let rec set_curry_flag ty = match ty with
  | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes | Ttimestamp | Tkey
  | Tkey_hash | Tsignature | Toperation | Taddress | Tfail -> ()
  | Ttuple tyl -> List.iter set_curry_flag tyl
  | Toption ty | Tlist ty | Tset ty -> set_curry_flag ty
  | Tmap (ty1, ty2) | Tbigmap (ty1, ty2) | Tor (ty1, ty2) ->
    set_curry_flag ty1; set_curry_flag ty2
  | Tlambda (ty1, ty2, u) ->
    set_curry_flag ty1; set_curry_flag ty2;
    !u := Some false;
  | Tclosure ((ty1, ty2), ty3, u) ->
    set_curry_flag ty1; set_curry_flag ty2; set_curry_flag ty3;
    !u := Some false;
  | Trecord (rn, fl) -> List.iter (fun (_, ty) -> set_curry_flag ty) fl
  | Tsum (sn, cl) -> List.iter (fun (_, ty) -> set_curry_flag ty) cl
  | Tcontract c ->
    List.iter (fun es -> set_curry_flag es.parameter) c.entries_sig
  | Tvar { contents = { contents = { tyo = Some ty }}} -> set_curry_flag ty
  | Tvar _ -> ()
  | Tpartial _ -> ()

let mk ~loc desc = mk ~loc desc ()

let vars_info_pat env pat =
  let rec vars_info_pat_aux acc indexes = function
    | { ppat_desc = Ppat_constraint (pat, ty) } ->
      let acc, _ = vars_info_pat_aux acc indexes pat in
      acc, translate_type env ty

    | { ppat_desc = Ppat_var { txt = var; loc } } ->
      (var, loc_of_loc loc, indexes) :: acc,
      fresh_tvar ()

    | { ppat_desc = Ppat_any; ppat_loc } ->
      ("_", loc_of_loc ppat_loc, indexes) :: acc,
      fresh_tvar ()

    | { ppat_desc = Ppat_construct ({ txt = Lident "()" }, None); ppat_loc } ->
      ("_", loc_of_loc ppat_loc, indexes) :: acc,
      Tunit

    | { ppat_desc = Ppat_tuple [pat] } ->
      vars_info_pat_aux acc indexes pat

    | { ppat_desc = Ppat_tuple pats } ->
      let _, acc, tys =
        List.fold_left (fun (i, acc, tys) pat ->
            let acc, ty = vars_info_pat_aux acc (i :: indexes) pat in
            i + 1, acc, ty :: tys
          ) (0, acc, []) pats
      in
      acc, Ttuple (List.rev tys)

    | { ppat_loc } ->
      error_loc ppat_loc "cannot deconstruct this pattern"
  in
  vars_info_pat_aux [] [] pat

let access_of_deconstruct var_name loc indexes =
  let a = mk ~loc (Var var_name) in
  List.fold_right (fun i a ->
      mk ~loc (Apply {
          prim = Prim_tuple_get;
          args = [
            a;
            mk ~loc (Const { ty = Tnat;
                             const = CNat (LiquidNumber.integer_of_int i) })
          ] })
    ) indexes a

let deconstruct_pat env pat e =
  let vars_infos, ty = vars_info_pat env pat in
  match vars_infos with
  | [] -> assert false
  | [nname, nloc, []] -> { nname; nloc }, ty, e
  | _ ->
    let var_name =
      String.concat "_" ( "" :: (List.rev_map (fun (v,_,_) -> v) vars_infos)) in
    let e =
      List.fold_left (fun e (v, loc, indexes) ->
          let access = access_of_deconstruct var_name loc indexes in
          mk ~loc (Let { bnd_var = { nname = v; nloc = loc };
                         inline = InAuto;
                         bnd_val = access; body = e })
        ) e vars_infos
    in
    let nloc = match vars_infos, List.rev vars_infos with
      | (_, first_loc, _) :: _, (_, last_loc, _) :: _ ->
        LiquidLoc.merge first_loc last_loc
      | _ -> assert false
    in
    ({ nname = var_name; nloc }, ty, e)

let order_labelled_args loc labels args =
  let labelled_exps, args =
    List.fold_left (fun (acc, args) label ->
        let exps, args =
          List.partition
            (function (Labelled l, _) -> l = label | _ -> false)
            args in
        (label, exps) :: acc, args
      ) ([], args) labels in
  let labelled_exps = List.rev labelled_exps in
  let args, extra_args = List.fold_left (fun (acc, args) labelled ->
      let exp, args = match labelled, args with
        | (_, [_, exp]), _ -> exp, args (* only one match *)
        | (_, []), (Nolabel, exp) :: args -> exp, args (* no match, take first one *)
        | (_, []), (Labelled label, exp) :: args ->
          error_loc loc "unknown label %s" label
        | (label, []), [] ->
          error_loc loc "missing argument %s" label
        | (label, _), _ ->
          error_loc loc "multiple arguments labelled with %s" label in
      (exp :: acc, args)
    ) ([], args) labelled_exps in
  match extra_args with
  | [] -> List.rev args
  | _ -> error_loc loc "too many arguments"

let add_type_alias ~loc ty_name params ty env =
  let pset, params = List.fold_left (fun (acc, params) -> function
      | { ptyp_desc = Ptyp_var alpha; ptyp_loc }, Invariant ->
        if StringSet.mem alpha acc then
          error_loc ptyp_loc "Type parameter '%s occurs several times" alpha;
          if ty_name = "storage" && alpha.[0] <> '_' then
            LiquidLoc.warn (loc_of_loc ptyp_loc) (WeakParam alpha);
        StringSet.add alpha acc, alpha :: params
      | { ptyp_loc }, _ ->
        error_loc ptyp_loc "Type parameter not allowed";
    ) (StringSet.empty, []) params in
  let params = List.rev params in
  if StringMap.mem ty_name env.types then
    error_loc loc "type %s already defined" ty_name;
  let tvars = free_tvars ty in
  StringSet.iter (fun v ->
      if not @@ StringSet.mem v pset then
        error_loc loc "Unbound type parameter '%s" v) tvars;
  let mk_ty pvals =
    let subst = make_subst params pvals in
    instantiate_to subst ty
  in
  env.types <- StringMap.add ty_name mk_ty env.types

let translate_record ~loc ty_name params labels env =
  let rtys = List.mapi
      (fun i pld ->
         let label = pld.pld_name.txt in
         if StringMap.mem label env.labels then
           error_loc pld.pld_loc "label %s already defined" label;
         let ty = translate_type env pld.pld_type in
         env.labels <- StringMap.add label (ty_name, i) env.labels;
         (label, ty)
      ) labels
  in
  let ty = Trecord (ty_name, rtys) in
  add_type_alias ~loc ty_name params ty env

let translate_variant ~loc ty_name params constrs env =
  let constrs = List.mapi
      (fun i pcd ->
         let constr = pcd.pcd_name.txt in
         try
           ignore (find_constr ~loc:(loc_of_loc loc) constr env);
           error_loc pcd.pcd_loc "constructor %s already defined" constr;
         with Not_found ->
           let ty = match pcd.pcd_args with
             | Pcstr_tuple [ ty ] -> translate_type env ty
             | Pcstr_tuple [] -> Tunit
             | Pcstr_tuple tys -> Ttuple (List.map (translate_type env) tys)
             | Pcstr_record _ -> error_loc pcd.pcd_loc "syntax error"
           in
           env.constrs <- StringMap.add constr (ty_name, i) env.constrs;
           (constr, ty)
      ) constrs
  in
  let ty = Tsum (ty_name, constrs) in
  add_type_alias ~loc ty_name params ty env

let check_version = function
  | { pexp_desc = Pexp_constant (Pconst_float (s, None)); pexp_loc } ->
    let req_version = float_of_string s in
    if req_version < minimal_version then
      Printf.kprintf (error_version pexp_loc)
        "(requires %F while compiler has minimal %F )"
        req_version minimal_version;
    if req_version > maximal_version then
      error_loc pexp_loc
        "(requires %F while compiler has maximal %F )"
        req_version maximal_version;
  | { pexp_loc } -> error_loc pexp_loc "version must be a floating point number"

let filter_contracts acc =
  List.fold_left (fun acc -> function
      | Syn_other_contract c | Syn_sub_contract c ->
        StringMap.add c.contract_name c acc
      | Syn_sub_contract_alias (alias, c) ->
        StringMap.add alias c acc
      | _ -> acc) StringMap.empty acc

let acc_for_subcontract acc =
  List.fold_left (fun acc -> function
      | Syn_init _ -> acc
      | Syn_sub_contract c | Syn_sub_contract_alias (_, c) ->
        Syn_other_contract c :: acc
      | a -> a :: acc
    ) [] acc
  |> List.rev

let acc_for_subcontract_as_main acc =
  List.fold_left (fun acc -> function
      | Syn_init _ -> acc
      | a -> a :: acc
    ) [] acc
  |> List.rev

let get_attributes loc attrs =
  List.iter (function
      | { txt = "inline" | "noinline" | "private" } , PStr [] -> ()
      | { txt = "ocaml.doc" } , _ -> () (* ignore ocamldoc *)
      | { txt; loc }, _ ->
        error_loc loc "Illegal attribute %s" txt
    ) attrs;
  let force_inline =
    List.exists (function { txt = "inline"} , PStr [] -> true | _ -> false)
      attrs in
  let no_inline =
    List.exists (function { txt = "noinline"} , PStr [] -> true | _ -> false)
      attrs in
  let inline = match force_inline, no_inline with
    | true, false -> InForced
    | false, true -> InDont
    | false, false -> InAuto
    | true, true ->
      error_loc loc
        "Value cannot have both attributes 'inline' and 'noinline'"
  in
  let val_private =
    List.exists (function { txt = "private"} , PStr [] -> true | _ -> false)
      attrs in
  inline, val_private


exception NotAConstant

(* Translate an expression, expecting a constant. Fails with [NotAConstant]
   if the expression is not a constant. *)
let rec translate_const env exp =
  match exp with
  | { pexp_desc = Pexp_construct ( { txt = Lident "()" }, None ) } ->
    CUnit, Some Tunit
  | { pexp_desc = Pexp_construct ( { txt = Lident "true" }, None ) } ->
    CBool true, Some Tbool
  | { pexp_desc = Pexp_construct ( { txt = Lident "false" }, None ) } ->
    CBool false, Some Tbool
  | { pexp_desc = Pexp_construct ( { txt = Lident "None" }, None ) } ->
    CNone, Some (Toption (fresh_tvar ()))
  | { pexp_desc = Pexp_constant (Pconst_integer (s,None)) } ->
    CInt (LiquidNumber.integer_of_liq s), Some Tint
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some 'p')) } ->
    CNat (LiquidNumber.integer_of_liq s), Some Tnat

  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\231')) } ->
    CTez (LiquidNumber.tez_of_liq s), Some Ttez
  | { pexp_desc = Pexp_constant (Pconst_float (s, Some '\231')) } ->
    CTez (LiquidNumber.tez_of_liq s), Some Ttez

  (* Timestamps *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\232')) } ->
    CTimestamp (ISO8601.of_string s), Some Ttimestamp

  (* Key_hash *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\233')) } ->
    CKey_hash s, Some Tkey_hash

  (* Address *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\236')) } ->
    CAddress s, Some Taddress

  (* Key *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\234')) } ->
    CKey s, Some Tkey

  (* Signature *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\235')) } ->
    CSignature s, Some Tsignature

  (* Bytes *)
  | { pexp_desc = Pexp_constant (Pconst_integer (s, Some '\237')) } ->
    CBytes s, Some Tbytes

  | { pexp_desc = Pexp_constant (Pconst_string (s, None)) } ->
    CString s, Some Tstring

  | { pexp_desc = Pexp_tuple [exp] } ->
    translate_const env exp

  | { pexp_desc = Pexp_tuple exps } ->
    let csts, tys = List.split (List.map (translate_const env) exps) in
    let tys =
      try
        Some (Ttuple (List.map (function
            | None -> raise Exit
            | Some ty -> ty) tys))
      with Exit -> None
    in
    CTuple csts, tys

  | { pexp_desc = Pexp_construct (
      { txt = Lident "[]" }, None) } ->
    CList [], Some (Tlist (fresh_tvar ()))

  | { pexp_desc = Pexp_construct (
      { txt = Lident "::" },
      Some { pexp_desc = Pexp_tuple [head; tail] }) } ->
    let head, head_ty = translate_const env head in
    let tail, tail_ty = translate_const env tail in
    begin
      match tail with
      | CList tail ->
        let cst = CList (head :: tail) in
        let ty =
          match head_ty, tail_ty with
          | Some head_ty, Some (Tlist tail_ty) ->
            let is_var = match tail_ty with Tvar _ -> true | _ -> false in
            if not is_var && not @@ eq_types head_ty tail_ty then
              error_loc exp.pexp_loc "inconsistent types in list";
            Some (Tlist head_ty)
          | Some head_ty, None ->
            Some (Tlist head_ty)
          | _ ->
            error_loc exp.pexp_loc "inconsistent types in list"
        in
        cst, ty
      | _ ->
        error_loc exp.pexp_loc "inconsistent types in list"
    end

  | { pexp_desc = Pexp_construct ({ txt = Lident "Map" }, None) } ->
    CMap [], Some (Tmap (fresh_tvar (), fresh_tvar ()))

  | { pexp_desc = Pexp_construct ({ txt = Lident "BigMap" }, None) } ->
    CBigMap [], Some (Tbigmap (fresh_tvar (), fresh_tvar ()))

  | { pexp_desc = Pexp_construct (
      { txt = Lident ("Map" | "BigMap" as map_kind) },
      Some pair_list) } ->
    let pair_list = translate_list pair_list in
    let pair_list = List.map translate_pair pair_list in
    let pair_list = List.map (fun (e1,e2) ->
        let cst1, ty1 = translate_const env e1 in
        let cst2, ty2 = translate_const env e2 in
        (cst1, cst2), (ty1, ty2)
      ) pair_list in
    let pair_list = List.sort compare pair_list in
    let csts, tys = List.split pair_list in
    let tys = match tys with
      | (Some ty1, Some ty2) :: tail ->

        List.iter (function
              (Some ty1', Some ty2') when ty1' = ty1 && ty2' = ty2
              -> ()
            | _ -> error_loc exp.pexp_loc
                     "inconsistent map types"
          )
          tail;
        begin match map_kind with
          | "Map" -> Some (Tmap (ty1, ty2))
          | "BigMap" -> Some (Tbigmap (ty1, ty2))
          | _ -> assert false
        end
      | _ -> (*None*)
        begin match map_kind with
          | "Map" -> Some (Tmap (fresh_tvar (), fresh_tvar ()))
          | "BigMap" -> Some (Tbigmap (fresh_tvar (), fresh_tvar ()))
          | _ -> assert false
        end
    in
    begin match map_kind with
      | "Map" -> CMap csts, tys
      | "BigMap" -> CBigMap csts, tys
      | _ -> assert false
    end

  | { pexp_desc = Pexp_construct (
      { txt = Lident "Set" }, None) } ->
    CSet [], Some (Tset (fresh_tvar ()))

  | { pexp_desc = Pexp_construct (
      { txt = Lident "Set" }, Some pair_list) } ->
    let list = translate_list pair_list in
    let list = List.map (fun e1 ->
        let cst1, ty1 = translate_const env e1 in
        cst1, ty1
      ) list in
    let list = List.sort compare list in
    let csts, tys = List.split list in
    let tys = match tys with
      | Some ty1 :: tail ->
        List.iter (function
              Some ty1' when ty1' = ty1 -> ()
            | _ -> error_loc exp.pexp_loc
                     "inconsistent set types"
          )
          tail;
        Some (Tset ty1)
      | _ -> None
    in
    CSet csts, tys


  | { pexp_desc = Pexp_construct (
      { txt = Lident "Some" }, Some arg) } ->
    let arg, ty = translate_const env arg in
    let ty = match ty with
      | None -> None
      | Some ty -> Some (Toption ty)
    in
    CSome arg, ty

  | { pexp_desc = Pexp_construct (
      { txt = Lident "Left" }, Some arg) } ->
    let arg, ty = translate_const env arg in
    let ty = match ty with
      | None -> None
      | Some ty -> Some (Tor (ty, fresh_tvar ()))
    in
    CLeft arg, ty

  | { pexp_desc = Pexp_construct (
      { txt = Lident "Right" }, Some arg) } ->
    let arg, ty = translate_const env arg in
    let ty = match ty with
      | None -> None
      | Some ty -> Some (Tor (fresh_tvar (), ty))
    in
    CRight arg, ty

  | { pexp_desc = Pexp_construct ({ txt = lid ; loc }, args) } ->
    let loc = loc_of_loc loc in
    let lid = str_of_id lid in
    let c =
      match args with
      | None -> CUnit
      | Some args ->
        let c, ty_opt = translate_const env args in
        c
    in
    let ty =
      try
        Some (fst (find_constr ~loc lid env))
      with Not_found -> raise NotAConstant (* None *)
    in
    CConstr (lid, c), ty

  | { pexp_desc = Pexp_record (lab_x_exp_list, None) } ->
    let lab_x_exp_list =
      List.map (fun ({ txt = label; loc }, exp) ->
          try
            let label = str_of_id label in
            let c, ty_opt = translate_const env exp in
            label, loc_of_loc loc, c
          with Not_found ->
            error_loc exp.pexp_loc "unknown label %s" (str_of_id label)
        ) lab_x_exp_list in
    let ty = match lab_x_exp_list with
      | [] -> error_loc exp.pexp_loc "empty record"
      | (label, loc, _) :: _ ->
        try
          Some (fst (find_label ~loc label env))
        with Not_found -> raise NotAConstant (* None *)
    in
    let fields = List.map (fun (f, _loc, c) -> f, c) lab_x_exp_list in
    CRecord fields, ty

  | { pexp_desc = Pexp_constraint (cst, ty) } ->
    let cst, tyo = translate_const env cst in
    let ty = translate_type env ?expected:tyo ty in
    cst, Some ty

  | { pexp_desc = Pexp_fun (Nolabel, None, pat, body_exp) } ->
    let body_exp, ret_ty = match body_exp.pexp_desc with
      | Pexp_constraint (body_exp, ret_ty) ->
        body_exp, translate_type env ret_ty
      | _ -> body_exp, fresh_tvar ()
    in
    let body_exp = translate_code StringMap.empty env body_exp in
    let arg_name, arg_ty, body = deconstruct_pat env pat body_exp in
    CLambda { arg_name; arg_ty; body; ret_ty; recursive = None },
    Some (Tlambda (arg_ty, ret_ty, default_uncurry ()))

  | _ -> raise NotAConstant

and translate_list exp =
  match exp.pexp_desc with
  | Pexp_tuple [exp] -> translate_list exp
  | Pexp_construct({ txt = Lident "[]" }, None) -> []

  | Pexp_construct({ txt = Lident "::" },
                   Some { pexp_desc = Pexp_tuple [e1; e2] }) ->
    e1 :: translate_list e2

  | _ -> error_loc exp.pexp_loc "list expected"

and translate_pair exp =
  match exp.pexp_desc with
  | Pexp_tuple [exp] -> translate_pair exp
  | Pexp_tuple [e1; e2] -> (e1, e2)
  | _ -> error_loc exp.pexp_loc "pair expected"


and translate_code contracts env exp =
  let loc = loc_of_loc exp.pexp_loc in
  let desc =
    match exp with
    | { pexp_desc = Pexp_extension ( { txt = "type" }, PTyp pty ) } ->
      let ty = translate_type env pty in
      Type ty

    | { pexp_desc = Pexp_ident ( { txt = var } ) } ->
      Var (str_of_id var)

    | { pexp_desc = Pexp_field (exp, { txt = label }) } ->
      let field = str_of_id label in
      let record = translate_code contracts env exp in
      Project { field; record }

    | { pexp_desc = Pexp_setfield (exp, { txt = label }, arg) } ->
      let field = str_of_id label in
      let record = translate_code contracts env exp in
      let set_val = translate_code contracts env arg in
      let rec set_field record loc field set_val =
        match record.desc with
        | Project p ->
          let set_val = mk ~loc (SetField { record; field; set_val }) in
          set_field p.record record.loc p.field set_val
        | _ -> SetField { record; field; set_val } in
      set_field record loc field set_val

    | { pexp_desc = Pexp_ifthenelse (e1, e2, None) } ->
      If { cond = translate_code contracts env e1;
           ifthen = translate_code contracts env e2;
           ifelse = mk ~loc (Const { ty = Tunit; const = CUnit }) }
    | { pexp_desc = Pexp_ifthenelse (e1, e2, Some e3) } ->
      If { cond = translate_code contracts env e1;
           ifthen = translate_code contracts env e2;
           ifelse = translate_code contracts env e3 }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Account", "transfer") } },
            ([_; _] as args));
        pexp_loc } ->
      let dest, amount =
        match order_labelled_args pexp_loc ["dest"; "amount"] args with
        | [d; a] -> d, a
        | _ -> assert false in
      Transfer { dest = translate_code contracts env dest;
                 amount = translate_code contracts env amount }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "call") } },
            ([_; _; _] as args));
        pexp_loc } ->
      let contract, amount, arg =
        match order_labelled_args pexp_loc
                ["dest"; "amount"; "parameter"] args
        with
        | [c; t; a] -> c, t, a
        | _ -> assert false in
      Call { contract = translate_code contracts env contract;
             amount = translate_code contracts env amount;
             entry = None;
             arg = translate_code contracts env arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "call") } },
            ([_; _; _; _] as args));
        pexp_loc } ->
      let contract, amount, entry, arg =
        match order_labelled_args pexp_loc
                ["dest"; "amount"; "entry"; "parameter"] args
        with
        | [c; t; { pexp_desc = Pexp_ident { txt = e }}; a] ->
          c, t, str_of_id e, a
        | _ -> error_loc pexp_loc "wrong arguments" in
      Call { contract = translate_code contracts env contract;
             amount = translate_code contracts env amount;
             entry = Some entry;
             arg = translate_code contracts env arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "create") } },
            ([_; _; _; _; _; _; _] as args));
        pexp_loc } ->
      let (manager_exp, delegate_exp, spendable_exp,
           delegatable_exp, amount_exp, storage_exp, contract) =
        match order_labelled_args pexp_loc
                ["manager"; "delegate"; "spendable"; "delegatable";
                 "amount"; "storage"; "code" ] args
        with
        | [d; m; de; s; a; st;
           { pexp_desc = Pexp_pack {
                 pmod_desc = Pmod_ident { txt = c };
                 pmod_loc } }] ->
          let c = str_of_id c in
          let c =
            try StringMap.find c contracts
            with Not_found -> unbound_contract pmod_loc c in
          (d, m, de, s, a, st, c)
        | [d; m; de; s; a; st;
           { pexp_desc = Pexp_pack {
                 pmod_desc = Pmod_structure structure;
                 pmod_loc } }] ->
          let inner_env = mk_inner_env env "_dummy_" in
          let inner_acc = StringMap.bindings contracts
                          |> List.map (fun (_, c) -> Syn_other_contract c) in
          let contract =
            translate_non_empty_contract inner_env inner_acc structure in
          (d, m, de, s, a, st, contract)
        | _ -> error_loc pexp_loc "wrong arguments for Contract.create" in
      CreateContract
        { args = [translate_code contracts env manager_exp;
                  translate_code contracts env delegate_exp;
                  translate_code contracts env spendable_exp;
                  translate_code contracts env delegatable_exp;
                  translate_code contracts env amount_exp;
                  translate_code contracts env storage_exp;
                 ];
          contract }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "create") } }, _);
        pexp_loc } ->
      error_loc pexp_loc
        "Wrong number or order of arguements for Contract.create.\n\
         Expected syntax is : Contract.create <manager> <delegate> \
         <delegatable> <spendable> <amount> <initial_storage> \
         (contract <ContractName>)"

    | { pexp_desc =
          Pexp_constraint (
            { pexp_desc =
                Pexp_apply (
                  { pexp_desc = Pexp_ident
                        { txt = Ldot(Lident "Contract", "at") } },
                  [
                    Nolabel, addr_exp;
                  ]) },
            pty) } ->

      let c_sig = match translate_type env pty with
        | Toption ((Tcontract csig)) -> csig
        | _ -> error_loc pty.ptyp_loc
                 "Contract.at type must be (contract C) option for some C"
      in
      ContractAt { arg =  translate_code contracts env addr_exp; c_sig }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(contract_id, "at") } },
            [
              Nolabel, addr_exp;
            ]) }
      when StringMap.mem (str_of_id contract_id) env.contract_types ->
      let c_sig = StringMap.find (str_of_id contract_id) env.contract_types in
      ContractAt { arg = translate_code contracts env addr_exp; c_sig }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident contract_name, "at") } },
            [
              Nolabel, addr_exp;
            ]) }
      when StringMap.mem contract_name contracts ->
      let c_sig = sig_of_contract (StringMap.find contract_name contracts) in
      ContractAt { arg =  translate_code contracts env addr_exp; c_sig }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Contract", "at") } },
            [
              Nolabel, addr_exp;
            ]);
        pexp_loc } ->
      (* let c_sig = Toption (Tvar (Ref.create
       *   { id = fresh_tv (); tyo = Some (Tpartial (Pcont [])) })) in
       * ContractAt { arg =  translate_code contracts env addr_exp; c_sig } *)
      error_loc pexp_loc
        "Contract.at must be annotated by the resulting contract type (option)"

    | { pexp_desc =
          Pexp_constraint (
            { pexp_desc =
                Pexp_apply (
                  { pexp_desc = Pexp_ident
                        { txt = Ldot(Lident "Bytes", "unpack") } },
                  [
                    Nolabel, exp;
                  ]) },
            pty) } ->

      let ty = match translate_type env pty with
        | Toption p -> p
        | _ -> error_loc pty.ptyp_loc
                 "Bytes.unpack type must be 'a option for some 'a"
      in
      Unpack { arg = translate_code contracts env exp; ty }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  { txt = Ldot(Lident "Bytes", "unpack") } },
            [
              Nolabel, exp;
            ]);
        pexp_loc } ->
      let ty = fresh_tvar () in
      Unpack { arg = translate_code contracts env exp; ty }

    | { pexp_desc = Pexp_let (Nonrecursive, [ {
        pvb_pat = pat;
        pvb_expr = var_exp;
        pvb_attributes = attrs;
        pvb_loc;
      } ], body) } ->
      let bnd_val = translate_code contracts env var_exp in
      let body = translate_code contracts env body in
      let bnd_var, ty, body = deconstruct_pat env pat body in
      let inline, val_private = get_attributes pvb_loc attrs in
      if val_private then
        error_loc pvb_loc "Value cannot be declared private here";
      let bnd_val =
        match pat.ppat_desc with
        | Ppat_constraint _ -> mk ~loc (TypeAnnot { e = bnd_val; ty = ty })
        | _ -> bnd_val
      in
      Let { bnd_var; inline; bnd_val; body }

    (* Special (limited) form for recursive functions:
       let rec f = fun ... -> ... f ... *)
    | { pexp_desc = Pexp_let (Recursive, [ {
        pvb_pat = { ppat_desc = Ppat_var { txt = fun_name; loc = name_loc } };
        pvb_expr = { pexp_desc = Pexp_fun (Nolabel, None, pat, ({
            pexp_desc = fun_expr_desc } as fun_expr));
            pexp_loc = fun_loc;
          };
        pvb_attributes = attrs;
        pvb_loc;
      } ], body) } ->
      let fun_body, ret_ty = match fun_expr_desc with
        | Pexp_constraint (fun_body, ret_ty) ->
          fun_body, translate_type env ret_ty
        | _ -> fun_expr, fresh_tvar ()
      in
      let fun_body = translate_code contracts env fun_body in
      let arg_name, arg_ty, fun_body = deconstruct_pat env pat fun_body in
      let is_rec =
        StringSet.mem fun_name
          (StringSet.remove arg_name.nname
             (LiquidBoundVariables.bv fun_body)) in
      if not is_rec then LiquidLoc.warn loc (NotRecursive fun_name);
      let recursive = if is_rec then Some fun_name else None in
      let lam = mk ~loc:(loc_of_loc fun_loc)
          (Lambda { arg_name; arg_ty; body = fun_body; ret_ty; recursive }) in
      let inline, val_private = get_attributes pvb_loc attrs in
      if val_private then
        error_loc pvb_loc "Value cannot be declared private here";
      let body = translate_code contracts env body in
      let bnd_var = { nname = fun_name; nloc = loc_of_loc name_loc } in
      Let { bnd_var; inline; bnd_val = lam; body }

    | { pexp_desc = Pexp_sequence (exp1, exp2) } ->
      Seq (translate_code contracts env exp1, translate_code contracts env exp2)

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident
                  ( { txt = (Lident "failwith"
                            | Ldot(Lident "Current", "failwith")) })},
            [
              Nolabel, err
            ]);
        pexp_loc
      } ->
      let arg = translate_code contracts env err in
      Failwith arg

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                    "loop");
                                         loc } ) },
            [
              Nolabel, { pexp_desc =
                           Pexp_fun (Nolabel,None,
                                     pat,
                                     body
                                    ) };
              Nolabel, arg
            ]) } ->
      let body = translate_code contracts env body in
      let arg = translate_code contracts env arg in
      let arg_name, _, body = deconstruct_pat env pat body in
      Loop { arg_name; body; arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                    "left");
                                         loc } ) },
            (Nolabel, { pexp_desc =
                          Pexp_fun (Nolabel,None,
                                    pat,
                                    body
                                   ) }) ::
            (Nolabel, arg) ::
            rest
          ) } ->
      let body = translate_code contracts env body in
      let arg = translate_code contracts env arg in
      let arg_name, _, body = deconstruct_pat env pat body in
      begin match rest with
        | [ Nolabel, acc ] ->
          let acc = translate_code contracts env acc in
          LoopLeft { arg_name; body; arg; acc = Some acc }
        | [] ->
          LoopLeft { arg_name; body; arg; acc = None }
        | _ -> error_loc loc "wrong number of arguments for Loop.left"
      end

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident "Loop",
                                                    "left");
                                         loc } ) }, _) } ->
      error_loc loc "wrong of arguments for Loop.left"

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "iter") } ) },
            [
              Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
              Nolabel, arg
            ]) }
      when LiquidTypes.is_fold_primitive (iter_coll^".iter") ->
      let body = translate_code contracts env body in
      let arg = translate_code contracts env arg in
      let arg_name, _, body = deconstruct_pat env pat body in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".iter") in
      let acc = mk ~loc (Const { ty = Tunit; const =  CUnit }) in
      Fold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "fold") } ) },
            [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
              Nolabel, arg;
              Nolabel, acc;
            ]) }
      when LiquidTypes.is_fold_primitive (iter_coll^".fold") ->
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let body = translate_code contracts env body in
      let arg_name, _, body = deconstruct_pat env pat body in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".fold") in
      Fold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_coll,
                                                    "map") } ) },
            [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
              Nolabel, arg;
            ]) }
      when LiquidTypes.is_map_primitive (map_coll^".map") ->
      let arg = translate_code contracts env arg in
      let body = translate_code contracts env body in
      let arg_name, _, body = deconstruct_pat env pat body in
      let prim = LiquidTypes.map_primitive_of_string (map_coll^".map") in
      Map { prim; arg_name; body; arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_fold_coll,
                                                    "map_fold") } ) },
            [ Nolabel, { pexp_desc = Pexp_fun (Nolabel,None, pat, body) };
              Nolabel, arg;
              Nolabel, acc;
            ]) }
      when LiquidTypes.is_map_fold_primitive (map_fold_coll^".map_fold") ->
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let body = translate_code contracts env body in
      let arg_name, _, body = deconstruct_pat env pat body in
      let prim = LiquidTypes.map_fold_primitive_of_string (map_fold_coll^".map_fold") in
      MapFold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "iter");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg
            ]) }
      when LiquidTypes.is_fold_primitive (iter_coll^".iter") ->
      let f = translate_code contracts env f_exp in
      let vloc = loc_of_loc vloc in
      let arg_name = "_iter_arg" in
      let arg_var = mk ~loc:vloc (Var arg_name) in
      let body =
        mk ~loc (Apply { prim = Prim_exec true; args = [f; arg_var] }) in
      let arg = translate_code contracts env arg in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".iter") in
      let acc = mk ~loc (Const { ty = Tunit; const =  CUnit }) in
      let arg_name = { nname = arg_name ; nloc = vloc } in
      Fold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident iter_coll,
                                                    "fold");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
              Nolabel, acc;
            ]) }
      when LiquidTypes.is_fold_primitive (iter_coll^".fold") ->
      let f = translate_code contracts env f_exp in
      let arg_name = "_fold_arg" in
      let vloc = loc_of_loc vloc in
      let arg_var = mk ~loc:vloc (Var arg_name) in
      let body =
        mk ~loc (Apply { prim = Prim_exec true; args = [f; arg_var] }) in
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let prim = LiquidTypes.fold_primitive_of_string (iter_coll^".fold") in
      let arg_name = { nname = arg_name ; nloc = vloc } in
      Fold { prim; arg_name; body; arg; acc }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_coll,
                                                    "map");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
            ]) }
      when LiquidTypes.is_map_primitive (map_coll^".map") ->
      let f = translate_code contracts env f_exp in
      let arg_name = "_map_arg" in
      let vloc = loc_of_loc vloc in
      let arg_var = mk ~loc:vloc (Var arg_name) in
      let body =
        mk ~loc (Apply { prim = Prim_exec true; args = [f; arg_var] }) in
      let arg = translate_code contracts env arg in
      let prim = LiquidTypes.map_primitive_of_string (map_coll^".map") in
      let arg_name = { nname = arg_name ; nloc = vloc } in
      Map { prim; arg_name; body; arg }

    | { pexp_desc =
          Pexp_apply (
            { pexp_desc = Pexp_ident ( { txt = Ldot(Lident map_fold_coll,
                                                    "map_fold");
                                         loc = vloc } ) },
            [
              Nolabel, f_exp;
              Nolabel, arg;
              Nolabel, acc;
            ]) }
      when LiquidTypes.is_map_fold_primitive (map_fold_coll^".map_fold") ->
      let f = translate_code contracts env f_exp in
      let arg_name = "_map_fold_arg" in
      let vloc = loc_of_loc vloc in
      let arg_var = mk ~loc:vloc (Var arg_name) in
      let body =
        mk ~loc (Apply { prim = Prim_exec true; args = [f; arg_var] }) in
      let arg = translate_code contracts env arg in
      let acc = translate_code contracts env acc in
      let prim =
        LiquidTypes.map_fold_primitive_of_string (map_fold_coll^".map_fold") in
      let arg_name = { nname = arg_name ; nloc = vloc } in
      MapFold { prim; arg_name; body; arg; acc }

    | { pexp_desc = Pexp_apply (
        { pexp_desc = Pexp_ident { txt = Ldot(Lident "Account", "create") } },
        args);
        pexp_loc } ->
      let manager, delegate, delegatable, amount =
        match order_labelled_args pexp_loc
                ["manager"; "delegate"; "delegatable"; "amount"] args
        with
        | [m; d; de; a] -> (m, d, de, a)
        | _ -> error_loc pexp_loc "wrong arguments for Account.create" in
      Apply { prim = Prim_create_account;
              args = [
                translate_code contracts env manager;
                translate_code contracts env delegate;
                translate_code contracts env delegatable;
                translate_code contracts env amount;
              ] }

    (* special case for contract call with contract.entry param ~amount *)
    | { pexp_desc = Pexp_apply (
        { pexp_desc = Pexp_field (contract, { txt = Lident entry }) },
        ( [Nolabel, param; Labelled "amount", amount]
        | [Labelled "amount", amount; Nolabel, param] )
      ) } ->
      Call { contract = translate_code contracts env contract;
             amount = translate_code contracts env amount;
             entry = Some entry;
             arg = translate_code contracts env param }

    (* f @@ x -> f x *)
    | { pexp_desc = Pexp_apply (
                { pexp_desc = Pexp_ident { txt = Lident "@@" } },
                (Nolabel, f) :: x) } ->
      let exp =
        translate_code contracts env
          { exp with pexp_desc = Pexp_apply (f, x) } in
      exp.desc

    (* x |> f -> f x *)
    | { pexp_desc = Pexp_apply (
                { pexp_desc = Pexp_ident { txt = Lident "|>" } },
                [x; Nolabel, f]) } ->
      let exp =
        translate_code contracts env
          { exp with pexp_desc = Pexp_apply (f, [x]) } in
      exp.desc

    (* f x1 x2 ... xn *)
    | { pexp_desc = Pexp_apply (
        { pexp_desc = Pexp_ident ( { txt = var } );
          pexp_loc = var_loc },
        args) } ->
      let f = str_of_id var in
      let args =
        List.map (function
            | (Nolabel, exp) -> translate_code contracts env exp
            | (_, { pexp_loc }) ->
              error_loc pexp_loc "cannot have labelled arguments"
          ) args in
      if is_primitive f then
        let prim = LiquidTypes.primitive_of_string f in
        Apply { prim; args }
      else if is_extprim f env then
        let eprim = find_extprim ~loc f env in
        let targs, args =
          List.fold_left (fun (targs, args) a ->
              match a.desc, targs with
              | Type ty, _ -> (ty :: targs, args)
              | _, [] -> (targs, a :: args)
              | _, _ -> error_loc exp.pexp_loc "Type arguments must come first"
            ) ([], []) (List.rev args) in
        let prim = Prim_extension
            (f, eprim.effect, targs, eprim.nb_arg, eprim.nb_ret, eprim.minst) in
        Apply { prim; args }
      else
        Apply { prim = Prim_exec true;
                args = mk ~loc:(loc_of_loc var_loc) (Var f) :: args }

    | { pexp_desc = Pexp_apply (exp, args) } ->
      let exp = translate_code contracts env exp in
      let args =
        List.map (function
            | (Nolabel, exp) -> translate_code contracts env exp
            | (_, { pexp_loc }) ->
              error_loc pexp_loc "cannot have labelled arguments"
          ) args in
      Apply { prim = Prim_exec true; args = exp :: args }

    | { pexp_desc = Pexp_extension (
        { txt = "nat" },
        PStr [{ pstr_desc = Pstr_eval (
            { pexp_desc = Pexp_match (e, cases); pexp_loc },
            [])
          }]
      )} ->
      let arg = translate_code contracts env e in
      let cases = List.map (translate_case contracts env) cases in
      let plus_name, ifplus, minus_name, ifminus = match cases with
        | [ PConstr ("Plus", [p]), ifplus, ploc;
            PConstr ("Minus", [m]), ifminus, mloc ]
        | [ PConstr ("Minus", [m]), ifminus, mloc;
            PConstr ("Plus", [p]), ifplus, ploc ] ->
          { nname = p; nloc = ploc}, ifplus, { nname = m; nloc = mloc }, ifminus
        | [ PConstr ("Plus", [p]), ifplus, ploc; PAny, ifminus, mloc ] ->
          { nname = p; nloc = ploc}, ifplus, { nname = "_"; nloc = mloc}, ifminus
        | [ PConstr ("Minus", [m]), ifminus, mloc; PAny, ifplus, ploc ] ->
          { nname = "_"; nloc = ploc}, ifplus, { nname = m; nloc = mloc }, ifminus
        | _ -> error_loc pexp_loc "match%%nat patterns are Plus _, Minus _"
      in
      MatchNat { arg; plus_name; ifplus; minus_name; ifminus }

    | { pexp_desc = Pexp_match (e, cases) } ->
      let arg = translate_code contracts env e in
      let cases = List.map (translate_case contracts env) cases in
      begin
        match cases with
        | [ PConstr ("None", []), ifnone, _; PConstr ("Some", [s]), ifsome, sloc ]
        | [ PConstr ("Some", [s]), ifsome, sloc; PConstr ("None", []), ifnone, _ ]
        | [ PConstr ("Some", [s]), ifsome, sloc; PAny, ifnone, _ ] ->
          MatchOption {arg; ifnone; some_name = { nname = s; nloc = sloc }; ifsome }
        | [ PConstr ("None", []), ifnone, _; PAny, ifsome, sloc ] ->
          MatchOption {arg; ifnone;
                       some_name = { nname = "_"; nloc = sloc }; ifsome }

        | [ PConstr ("[]", []), ifnil, _; PConstr ("::", [h; t]), ifcons, htloc ]
        | [ PConstr ("::", [h; t]), ifcons, htloc; PConstr ("[]", []), ifnil, _ ]
        | [ PConstr ("::", [h; t]), ifcons, htloc; PAny, ifnil, _ ] ->
          MatchList { arg;
                      head_name = { nname = h; nloc = htloc };
                      tail_name = { nname = t; nloc = htloc };
                      ifcons; ifnil }
        | [ PConstr ("[]", []), ifnil, _; PAny, ifcons, nloc ] ->
          MatchList { arg;
                      head_name = { nname = "_head"; nloc };
                      tail_name = { nname = "_tail"; nloc };
                      ifcons; ifnil }

        | _ ->
          let cases = List.map (fun (c, e, _) -> (c, e)) cases in
          MatchVariant { arg; cases }
      end

    | { pexp_desc = Pexp_fun (Nolabel, None, pat, body_exp) } ->
      let body_exp, ret_ty = match body_exp.pexp_desc with
        | Pexp_constraint (body_exp, ret_ty) ->
          body_exp, translate_type env ret_ty
        | _ -> body_exp, fresh_tvar ()
      in
      let body_exp = translate_code contracts env body_exp in
      let arg_name, arg_ty, body = deconstruct_pat env pat body_exp in
      Lambda { arg_name; arg_ty; body; ret_ty; recursive = None }

    | { pexp_desc = Pexp_record (lab_x_exp_list, None) } ->
      let fields =
        List.map (fun ({ txt = label }, exp) ->
            str_of_id label, translate_code contracts env exp
          ) lab_x_exp_list in
      Record fields

    | exp ->
      match translate_const env exp with
      | _, None -> error_loc exp.pexp_loc "constant needs a type annotation"
      | const, Some ty -> Const { ty; const }
      | exception NotAConstant ->
        match exp with
        | { pexp_desc = Pexp_construct (
            { txt = Lident "Some" }, Some args) } ->
          Apply { prim = Prim_Some;
                  args = [translate_code contracts env args] }

        (* Ok, this is a very special case. We accept
           not knowing the full type of the empty list because it will be infered
           from the head element. We use unit for that type. *)
        | { pexp_desc =
              Pexp_construct (
                { txt = Lident "::" },
                Some { pexp_desc =
                         Pexp_tuple
                           [a;
                            { pexp_desc =
                                Pexp_construct(
                                  { txt = Lident "[]" }, None) }]}) }
          ->
          Apply { prim = Prim_Cons;
                  args = [translate_code contracts env a;
                          mk ~loc (Const { ty = fresh_tvar (); const =  CList [] }) (* XXX ? *)
                         ] }

        | { pexp_desc = Pexp_construct (
            { txt = Lident "::" },
            Some { pexp_desc = Pexp_tuple [a;b]}) } ->
          Apply { prim = Prim_Cons;
                  args = [translate_code contracts env a;
                          translate_code contracts env b] }


        | { pexp_desc = Pexp_construct ({ txt = Lident "Left" }, args) } ->
          Constructor { constr = Left (fresh_tvar ());
                        arg = match args with
                          | None -> mk ~loc (Const { ty = Tunit; const = CUnit })
                          | Some arg -> translate_code contracts env arg }

        | { pexp_desc = Pexp_construct ({ txt = Lident "Right" }, args) } ->
          Constructor { constr = Right (fresh_tvar ());
                        arg = match args with
                          | None -> mk ~loc (Const { ty = Tunit; const = CUnit })
                          | Some arg -> translate_code contracts env arg }


        | { pexp_desc = Pexp_construct ({ txt = lid }, args) } ->
          let lid = str_of_id lid in
          begin
            try
              let _ty_name, _ = find_constr ~loc lid env in
              Constructor { constr = Constr lid;
                            arg = match args with
                              | None ->
                                mk ~loc (Const { ty = Tunit; const = CUnit })
                              | Some arg -> translate_code contracts env arg }
            with Not_found -> match lid with
              | "Map" | "Set" | "BigMap" ->
                error_loc exp.pexp_loc
                  "constructor %s can only be used with a constant argument"
                  lid
              | _ ->
                error_loc exp.pexp_loc "unknown constructor: %s" lid
          end

        | { pexp_desc =
              Pexp_constraint (
                { pexp_desc =
                    Pexp_construct (
                      { txt = Lident "Left" }, args) },
                pty
              ) } ->
          let right_ty = match pty with
            | { ptyp_desc = Ptyp_constr (
                { txt = Lident "variant" },
                [ left_ty; right_ty ])} ->
              translate_type env right_ty
            | _ -> match translate_type env pty with
              | Tor (_, right_ty) -> right_ty
              | _ -> error_loc pty.ptyp_loc
                       "Type of Left must be a variant"
          in
          Constructor { constr = Left right_ty;
                        arg = match args with
                          | None ->
                            mk ~loc (Const { ty = Tunit; const = CUnit })
                          | Some arg -> translate_code contracts env arg }

        | { pexp_desc =
              Pexp_constraint (
                { pexp_desc =
                    Pexp_construct (
                      { txt = Lident "Right" }, args) },
                pty
              ) } ->
          let left_ty = match pty with
            | { ptyp_desc = Ptyp_constr (
                { txt = Lident "variant" },
                [ left_ty; right_ty ])} ->
              translate_type env left_ty
            | _ -> match translate_type env pty with
              | Tor (left_ty, _) -> left_ty
              | _ -> error_loc pty.ptyp_loc
                       "Type of Right must be a variant"
          in
          Constructor { constr = Right left_ty;
                        arg = match args with
                          | None ->
                            mk ~loc (Const { ty = Tunit; const = CUnit })
                          | Some arg -> translate_code contracts env arg }

        (* TODO *)
        | { pexp_desc = Pexp_tuple [exp] } ->
          (translate_code contracts env exp).desc

        | { pexp_desc = Pexp_tuple exps } ->
          let exps = List.map (translate_code contracts env) exps in
          begin
            try
              let tys, csts = List.split (
                  List.map (
                    function
                    | { desc = Const { ty; const }} -> (ty, const)
                    | _ -> raise Exit
                  ) exps)
              in
              Const { ty = Ttuple tys; const = CTuple csts }
            with Exit ->
              Apply { prim = Prim_tuple; args = exps }
          end

        | { pexp_desc = Pexp_constraint (exp, ty); pexp_loc } ->
          TypeAnnot { e = translate_code contracts env exp;
                      ty = translate_type env ty }

        | { pexp_loc } ->
          error_loc pexp_loc
            "in expression %s"
            (LiquidPrinter.Syntax.string_of_expression exp)


  (*
    | { pexp_desc = Pexp_construct ({ txt = Lident id }, None) } ->
       todo_loc ("constructor " ^ id) exp.pexp_loc
   *)
  in
  mk ~loc desc

and translate_case contracts env case : (pattern * syntax_exp * location) =
  let var_of_pat = function
    | { ppat_desc = Ppat_var { txt = var } } -> var
    | { ppat_desc = Ppat_any } -> "_"
    | { ppat_desc = Ppat_construct _; ppat_loc } ->
      error_loc ppat_loc "cannot match deep"
    | { ppat_loc } ->
      error_loc ppat_loc "bad pattern"
  in
  match case.pc_guard with
  | Some e  ->
    error_loc e.pexp_loc "when forbidden"
  | None ->
    let e = case.pc_rhs in
    let e = translate_code contracts env e in
    match case.pc_lhs with
    | { ppat_desc = Ppat_any; ppat_loc} ->
      (PAny, e, loc_of_loc ppat_loc)
    | { ppat_desc =
          Ppat_construct (
            { txt = Lident ("Some" | "::" | "Plus" | "Minus" as c) },
            None);
        ppat_loc }  ->
      error_loc ppat_loc "Constructor %S takes arguments, was given 0" c
    | { ppat_desc =
          Ppat_construct ( { txt = Lident ("None" | "[]" as c) }, Some _);
        ppat_loc }  ->
      error_loc ppat_loc "Constructor %S takes no arguments" c
    | { ppat_desc = Ppat_construct ( { txt = name } , None); ppat_loc }  ->
      (PConstr (str_of_id name, []), e, loc_of_loc ppat_loc)
    | { ppat_desc =
          Ppat_construct (
            { txt = Lident "::" } ,
            Some { ppat_desc = Ppat_tuple [ p1; p2 ]}
          );
        ppat_loc }  ->
      (PConstr ("::", [var_of_pat p1; var_of_pat p2]), e, loc_of_loc ppat_loc)

    | { ppat_desc = Ppat_construct ({ txt = name } , Some pat); ppat_loc }  ->
      let var_name, _, e = deconstruct_pat env pat e in
      (PConstr (str_of_id name, [var_name.nname]), e, loc_of_loc ppat_loc)

    | { ppat_loc } ->
      error_loc ppat_loc "bad pattern"

and translate_entry name env contracts head_exp mk_parameter mk_storage =
  match head_exp with
  | { pexp_desc = Pexp_fun (Nolabel, None, param_pat, head_exp) }
    when mk_parameter = None ->
    let mk_param = deconstruct_pat env param_pat in
    translate_entry name env contracts head_exp (Some mk_param) mk_storage

  | { pexp_desc = Pexp_fun (Nolabel, None, storage_pat, head_exp);
      pexp_loc }
    when mk_storage = None && mk_parameter <> None ->
    let mk_storage code =
      let storage_name, storage_ty, code =
        deconstruct_pat env storage_pat code in
      match storage_pat.ppat_desc with
      | Ppat_constraint _ | Ppat_construct _ ->
        begin try
            let s = find_type ~loc:noloc "storage" env [] in
            if not @@ eq_types s storage_ty then
              LiquidLoc.raise_error ~loc:storage_name.nloc
                "storage argument %s for entry point %s must be the same type \
                 as contract storage" storage_name.nname name;
            (storage_name, code)
          with Not_found ->
            error_loc pexp_loc "type storage is required but not provided"
        end
      | _ -> (storage_name, code)
    in
    translate_entry name env contracts head_exp mk_parameter (Some mk_storage)

  | { pexp_desc = Pexp_fun (Nolabel, None, _, _);
      pexp_loc }  ->
    LiquidLoc.raise_error ~loc:(loc_of_loc pexp_loc)
      "entry point %s accepts only two arguments, \
       i.e. one parameter, one storage" name

  | { pexp_loc } when mk_parameter = None || mk_storage = None ->
    LiquidLoc.raise_error ~loc:(loc_of_loc pexp_loc)
      "entry point %s needs two arguments" name

  | { pexp_desc = Pexp_constraint (head_exp, return_type); pexp_loc } ->
    begin match translate_type env return_type with
      | Ttuple [ ret_ty; sto_ty ] ->
        let storage = find_type ~loc:noloc "storage" env [] in
        if not @@ eq_types sto_ty storage then
          error_loc pexp_loc
            "Second component of return type must be identical to storage type";
        if ret_ty <> Tlist Toperation then
          error_loc pexp_loc
            "First component of return type must be an operation list"
      | _ ->
        error_loc pexp_loc
          "return type must be a product of \"operation list\" \
           and the storage type"
    end;
    translate_entry name env contracts head_exp mk_parameter mk_storage

  | exp ->
    let code = translate_code contracts env exp in
    let parameter_name, parameter, code = match mk_parameter with
      | Some mk -> mk code
      | None -> assert false in
    let storage_name, code = match mk_storage with
      | Some mk -> mk code
      | None -> assert false in
    set_curry_flag parameter;
    {
      entry_sig = {
        entry_name = name;
        parameter;
        parameter_name = parameter_name.nname;
        storage_name = storage_name.nname;
      };
      code;
    }

and translate_initial_storage env init_name contracts exp args =
  match exp with
  | { pexp_desc =
        Pexp_fun (
          Nolabel, None,
          { ppat_desc =
              Ppat_constraint(
                { ppat_desc = Ppat_var { txt = arg; loc } },
                arg_type)
          },
          exp) } ->
    translate_initial_storage env init_name contracts exp
      ((arg, loc_of_loc loc, translate_type env arg_type) :: args)

  | { pexp_desc =
        Pexp_fun (
          Nolabel, None,
          { ppat_desc = Ppat_var { txt = arg; loc } }, exp) } ->
    translate_initial_storage env init_name contracts exp
      ((arg, loc_of_loc loc, (fresh_tvar ())) :: args)

  | _ ->
    let init_body = translate_code contracts env exp in
    { init_name;
      init_args = List.rev args;
      init_body }

and translate_signature contract_type_name env acc ast =
  match ast with
  | [] ->
    { sig_name = Some contract_type_name;
      entries_sig = List.rev acc }

  | { psig_desc = Psig_type (
      Recursive, [
        { ptype_name = { txt = ty_name; loc=name_loc };
          ptype_params = params;
          ptype_cstrs = [];
          ptype_private = Public;
          ptype_manifest = None;
          (* ptype_attributes = []; *)
          ptype_loc;
          ptype_kind; (* Ptype_record labels;*)
        }
      ]) } :: ast ->
    if StringMap.mem ty_name env.types then
      error_loc name_loc "type %s already defined" ty_name;
    begin match ptype_kind with
      | Ptype_record labels ->
        if List.length labels < 2 then begin
          error_loc ptype_loc "record must have at least two fields"
        end;
        translate_record ~loc:ptype_loc ty_name params labels env;
      | Ptype_abstract -> ()
      | Ptype_open -> error_loc ptype_loc "bad type definition"
      | Ptype_variant constrs ->
        if List.length constrs < 2 then begin
          error_loc ptype_loc "variant type must have at least two constructors"
        end;
        translate_variant ~loc:ptype_loc ty_name params constrs env;
    end;
    translate_signature contract_type_name env acc ast

  (* type alias *)
  | { psig_desc = Psig_type (
      Recursive,
      [
        { ptype_name = { txt = ty_name; loc=name_loc };
          ptype_params = params;
          ptype_cstrs = [];
          ptype_private = Public;
          ptype_manifest = Some ct;
          (* ptype_attributes = []; *)
          ptype_loc;
          ptype_kind;
        }
      ]) } :: ast ->
    let ty = translate_type env ct in
    add_type_alias ~loc:ptype_loc ty_name params ty env;
    translate_signature contract_type_name env acc ast

  | { psig_desc = Psig_extension (
      ({ txt = "entry" }, PSig [{
           psig_desc = Psig_value {
               pval_name = { txt = entry_name; loc = name_loc };
               pval_type = {
                 ptyp_desc = Ptyp_arrow (param_label, param_ty, ret_ty)
               };
               pval_prim = [];
               (* pval_attributes = []; *)
               pval_loc;
             }}
         ]), [])} :: ast ->
    if List.mem entry_name reserved_keywords then
      error_loc name_loc "entry point %S forbidden" entry_name;
    if List.exists (fun e -> e.entry_name = entry_name) acc then
      error_loc name_loc "entry point %S is already declared" entry_name;
    let parameter_name = match param_label with
      | Nolabel -> "parameter"
      | Optional _ -> error_loc pval_loc "cannot have optional parameter"
      | Labelled p -> p in
    let parameter = translate_type env param_ty in
    set_curry_flag parameter;
    let storage_name, ret_ty = match ret_ty.ptyp_desc with
      | Ptyp_arrow (Nolabel, stora_ty, ret_ty) ->
        "storage", ret_ty
      | Ptyp_arrow (Optional _, stora_ty, _) ->
        error_loc ret_ty.ptyp_loc "cannot have optional storage"
      | Ptyp_arrow (Labelled s, stora_ty, ret_ty) ->
        s, ret_ty
      | Ptyp_any ->
        "storage", ret_ty
      | _ ->
        error_loc ret_ty.ptyp_loc
          "must be an arrow type storage -> (operation list * storage)"
    in
    begin match ret_ty.ptyp_desc with
      | Ptyp_any -> ()
      | Ptyp_tuple [ ret_op ;
                     { ptyp_desc =
                         Ptyp_constr ({ txt = Lident "storage" }, []) }] ->
        begin match translate_type env ret_op with
          | Tlist Toperation -> ()
          | _ -> error_loc ret_op.ptyp_loc
                   "entry must return operation list as first component"
        end
      | _ -> error_loc ret_ty.ptyp_loc
               "entry must return (operation list * storage)"
    end;
    let entry = { entry_name; parameter; parameter_name; storage_name } in
    translate_signature contract_type_name env (entry :: acc) ast

  | { psig_desc = Psig_modtype
          {
            pmtd_name = { txt = contract_type_name };
            pmtd_type = Some {
                pmty_desc = Pmty_signature signature
              }
          };
      psig_loc;
    } :: ast ->
    let inner_env = mk_inner_env env contract_type_name in
    let contract_sig =
      translate_signature contract_type_name inner_env [] signature in
    if contract_sig.entries_sig = [] then
      error_loc psig_loc
        "Contract type %s has no entry points (use struct to define namespace)"
        contract_type_name;
    env.contract_types <-
      StringMap.add contract_type_name contract_sig env.contract_types;
    translate_signature contract_type_name env acc ast

  | { psig_loc } as ast :: _ ->
    if !LiquidOptions.verbosity > 0 then
      error_loc psig_loc "in signature:\n%a" Printast.interface [ast]
    else
      error_loc psig_loc "in signature"



and translate_structure env acc ast : syntax_exp parsed_struct =
  match ast with
  | { pstr_desc = Pstr_primitive {
      pval_name = { txt = prim_name; loc = prim_loc };
      pval_type = prim_type; pval_prim = [minst];
      pval_attributes = prim_attr } } :: ast ->
    if List.mem prim_name reserved_keywords then
      error_loc prim_loc "Primitive name %S forbidden" prim_name;
    if StringMap.mem prim_name env.ext_prims then
      error_loc prim_loc "Primitive %S already defined" prim_name;
    if List.exists (function
        | Syn_value v -> v.val_name = prim_name
        | _ -> false) acc
    then
      error_loc prim_loc "Top-level identifier %S already defined" prim_name;
    let tvs, atys, rtys = translate_ext_type env prim_type in
    let valid_in_external = function
      | Trecord _ | Tsum _ | Tclosure _ | Tfail | Ttuple (_ :: _ :: _ :: _) ->
        error_loc prim_loc
          "Primitive %S can only use standard Michelson types" prim_name
      | _ -> ()
    in
    List.iter valid_in_external atys;
    List.iter valid_in_external rtys;
    let effect =  List.exists (fun (a, _) -> a.txt = "effect") prim_attr in
    let nb_arg = List.length atys in
    let nb_ret = List.length rtys in
    let atys = if atys = [] then [Tunit] else atys in
    let rty = match rtys with
      | [] -> Tunit
      | [ty] -> ty
      | _ -> Ttuple rtys
    in
    env.ext_prims <- StringMap.add prim_name
        { tvs; atys; rty; effect; nb_arg; nb_ret; minst } env.ext_prims;
    translate_structure env acc ast

  | { pstr_desc =
        Pstr_extension
          (({ Asttypes.txt = "version" },
            PStr [{ pstr_desc = Pstr_eval (exp,[])}]),[])
    } :: ast ->
    check_version exp;
    translate_structure env acc ast

  | { pstr_desc =
        Pstr_extension
          (({ txt = "init" },
            PStr
              [{ pstr_desc =
                   Pstr_value (
                     Nonrecursive,
                     [ {
                       pvb_pat = { ppat_desc = Ppat_var { txt = init_name } };
                       pvb_expr = sto_exp;
                     }
                     ]) } ]
           ), []);
      pstr_loc } :: ast
    ->
    if List.exists (function Syn_init _ -> true | _ -> false) acc then
      error_loc pstr_loc "Initial storage already defined";
    let init =
      Syn_init (translate_initial_storage env init_name
                  (filter_contracts acc) sto_exp [])
    in
    translate_structure env (init :: acc) ast

  | { pstr_desc =
        Pstr_extension
          (({ txt = "entry" },
            PStr
              [{ pstr_desc =
                   Pstr_value (
                     Nonrecursive,
                     [ {
                       pvb_pat = { ppat_desc =
                                     Ppat_var { txt = name; loc = name_loc } };
                       pvb_expr = head_exp;
                     }
                     ]) } ]
           ), []) } :: ast
    ->
    if List.mem name reserved_keywords then
      error_loc name_loc "entry point %S forbidden" name;
    if List.exists (function
        | Syn_entry e -> e.entry_sig.entry_name = name
        | _ -> false) acc then
      error_loc name_loc "entry point %S is already defined" name;
    let entry =
      Syn_entry (translate_entry name env
                   (filter_contracts acc) head_exp None None) in
    translate_structure env (entry :: acc) ast

  | { pstr_desc = (
      Pstr_value (
        Nonrecursive,
        [ {
          pvb_pat = { ppat_desc = Ppat_var { txt = val_name; loc = name_loc } };
          pvb_expr = val_exp;
          pvb_attributes = attrs;
          pvb_loc;
        }
        ])); pstr_loc = f_loc } :: ast ->
    let val_exp = translate_code (filter_contracts acc) env val_exp in
    let inline, val_private = get_attributes pvb_loc attrs in
    if List.mem val_name reserved_keywords then
      error_loc name_loc "top-level value %S forbidden" val_name;
    if StringMap.mem val_name env.ext_prims then
      error_loc name_loc "Top-level identifier %S already defined" val_name;
    if List.exists (function
        | Syn_value v -> v.val_name = val_name
        | _ -> false) acc
    then
      error_loc name_loc "Top-level value %S already defined" val_name;
    let value = Syn_value { val_name; inline; val_private; val_exp } in
    translate_structure env (value :: acc) ast


  | { pstr_desc = (
      Pstr_value (
        Recursive,
        [ {
          pvb_pat = { ppat_desc = Ppat_var { txt = fun_name; loc = name_loc } };
          pvb_expr = { pexp_desc = Pexp_fun (Nolabel, None, pat, ({
              pexp_desc = fun_expr_desc } as fun_expr));
              pexp_loc = fun_loc;
            };
          pvb_attributes = attrs;
          pvb_loc;
        }
        ])); pstr_loc = f_loc } :: ast ->
    let contracts = filter_contracts acc in
    let fun_body, ret_ty = match fun_expr_desc with
      | Pexp_constraint (fun_body, ret_ty) ->
        fun_body, translate_type env ret_ty
      | _ -> fun_expr, fresh_tvar ()
    in
    (* let ret_ty = translate_type env ret_ty in *)
    let fun_body = translate_code contracts env fun_body in
    let arg_name, arg_ty, fun_body = deconstruct_pat env pat fun_body in
    let is_rec =
      StringSet.mem fun_name
        (StringSet.remove arg_name.nname
           (LiquidBoundVariables.bv fun_body)) in
    let loc = loc_of_loc fun_loc in
    if not is_rec then LiquidLoc.warn loc (NotRecursive fun_name);
    let recursive = if is_rec then Some fun_name else None in
    let lam = mk ~loc
        (Lambda { arg_name; arg_ty; body = fun_body; ret_ty; recursive }) in
    let inline, val_private = get_attributes pvb_loc attrs in
    if List.mem fun_name reserved_keywords then
      error_loc name_loc "top-level value %S forbidden" fun_name;
    if StringMap.mem fun_name env.ext_prims then
      error_loc name_loc "Top-level identifier %S already defined" fun_name;
    if List.exists (function
        | Syn_value v -> v.val_name = fun_name
        | _ -> false) acc
    then
      error_loc name_loc "Top-level value %S already defined" fun_name;
    let value =
      Syn_value { val_name = fun_name; inline; val_private; val_exp = lam } in
    translate_structure env (value :: acc) ast


  | { pstr_desc = Pstr_type (Recursive,
                             [
                               { ptype_name = { txt = ty_name; loc=name_loc };
                                 ptype_params = params;
                                 ptype_cstrs = [];
                                 ptype_private = Public;
                                 ptype_manifest = None;
                                 (* ptype_attributes = []; *)
                                 ptype_loc;
                                 ptype_kind; (* Ptype_record labels;*)
                               }
                             ]) } :: ast ->
    if StringMap.mem ty_name env.types then
      error_loc name_loc "type %s already defined" ty_name;
    begin match ptype_kind with
      | Ptype_record labels ->
        if List.length labels < 2 then begin
          error_loc ptype_loc "record must have at least two fields"
        end;
        translate_record ~loc:ptype_loc ty_name params labels env;
      | Ptype_abstract
      | Ptype_open -> error_loc ptype_loc "bad type definition"
      | Ptype_variant constrs ->
        if List.length constrs < 2 then begin
          error_loc ptype_loc "variant type must have at least two constructors"
        end;
        translate_variant ~loc:ptype_loc ty_name params constrs env;
    end;
    translate_structure env acc ast

  (* type alias *)
  | { pstr_desc = Pstr_type (Recursive,
                             [
                               { ptype_name = { txt = ty_name; loc=name_loc };
                                 ptype_params = params;
                                 ptype_cstrs = [];
                                 ptype_private = Public;
                                 ptype_manifest = Some ct;
                                 (* ptype_attributes = []; *)
                                 ptype_loc;
                                 ptype_kind;
                               }
                             ]) } :: ast ->
    let ty = translate_type env ct in
    add_type_alias ~loc:ptype_loc ty_name params ty env;
    translate_structure env acc ast

  (* contract types *)

  | { pstr_desc = Pstr_modtype
          {
            pmtd_name = { txt = contract_type_name };
            pmtd_type = Some {
                pmty_desc = Pmty_signature signature
              }
          };
      pstr_loc;
    } :: ast ->
    let inner_env = mk_inner_env env contract_type_name in
    let contract_sig =
      translate_signature contract_type_name inner_env [] signature in
    if contract_sig.entries_sig = [] then
      error_loc pstr_loc
        "Contract type %s has no entry points (use struct to define namespace)"
        contract_type_name;
    env.contract_types <-
      StringMap.add contract_type_name contract_sig env.contract_types;
    translate_structure env acc ast

  (* Deactivate aliases for signatures *)
  (*
  | { pstr_desc = Pstr_modtype
          {
            pmtd_name = { txt = contract_type_name };
            pmtd_type = Some {
                pmty_desc = Pmty_ident { txt = id };
                pmty_loc;
              }
          }
    } :: ast ->
    let contract_sig =
      try find_contract_type (str_of_id id) env
      with Not_found ->
        error_loc pmty_loc "No contract type named %s knwon" (str_of_id id)
    in
    renamespace env (str_of_id id) contract_type_name;
    env.contract_types <-
      StringMap.add contract_type_name contract_sig env.contract_types;
    translate_structure env acc ast

  | { pstr_desc = Pstr_modtype
          {
            pmtd_name = { txt = contract_type_name };
            pmtd_type = Some {
                pmty_desc = Pmty_typeof {
                    pmod_desc = Pmod_ident { txt = c_name };
                    pmod_loc;
                  }
              }
          }
    } :: ast ->
    let contract =
      try StringMap.find (str_of_id c_name) (filter_contracts acc)
      with Not_found ->
        error_loc pmod_loc "No contract named %s knwon" (str_of_id c_name)
    in
    let contract_sig = sig_of_contract contract in
    renamespace env (str_of_id c_name) contract_type_name;
    env.contract_types <-
      StringMap.add contract_type_name contract_sig env.contract_types;
    translate_structure env acc ast
  *)

  (* contracts *)

  (* contract alias *)
  | { pstr_desc = Pstr_module {
      pmb_name = { txt = contract_name };
      pmb_expr = {
        pmod_desc = Pmod_ident { txt = c_name; loc };
      }
    }
    } :: ast ->
    let c_path = Longident.flatten c_name in
    let c_name = str_of_id c_name in
    begin try
        let contract = find_contract ~loc:(loc_of_loc loc)
            c_name env (filter_contracts acc) in
        if is_only_module contract then raise Not_found;
        env.others <- StringMap.add contract_name (Alias c_path) env.others;
        translate_structure env acc ast
      with Not_found ->
        unbound_contract loc c_name
    end

  (* module alias *)
  | { pstr_desc =
        Pstr_extension
          (({ txt = "module" },
            PStr
              [{ pstr_desc = Pstr_module {
                   pmb_name = { txt = contract_name };
                   pmb_expr = {
                     pmod_desc = Pmod_ident { txt = m_name; loc };
                   }
                 };
                   pstr_loc }]
           ), [])} :: ast ->
    let mn = str_of_id m_name in
    let m_path = Longident.flatten m_name in
    begin try
        let contract = find_module ~loc:(loc_of_loc loc) m_path env
            ((StringMap.bindings (filter_contracts acc) |> List.map snd)) in
        if not @@ is_only_module contract then raise Not_found;
        env.others <- StringMap.add contract_name (Alias m_path) env.others;
        translate_structure env acc ast
      with Not_found ->
        unbound_module loc mn
    end

  | { pstr_desc = Pstr_module {
      pmb_name = { txt = contract_name };
      pmb_expr = {
        pmod_desc = Pmod_structure structure
      };
    }; pstr_loc } :: ast ->
    let inner_env = mk_inner_env env contract_name in
    begin
      match !LiquidOptions.main with
      | Some main when main = add_path_name env.path contract_name ->
        begin match translate_structure inner_env (acc_for_subcontract_as_main acc) structure with
          | PaModule contract ->
            error_loc pstr_loc
              "%s is a module and not a contract, \
               it has no entry points and cannot be used as main"
              contract_name;
          | PaContract contract ->
            raise (Stop contract)
        end
      | _ ->
        match translate_structure inner_env (acc_for_subcontract acc) structure with
        | PaModule contract ->
          error_loc pstr_loc
            "Contract %s has no entry points (use module instead)"
            contract_name;
        | PaContract contract ->
          let contract_sig = sig_of_contract contract in
          env.contract_types <-
            StringMap.add contract_name contract_sig env.contract_types;
          translate_structure env (Syn_sub_contract contract :: acc) ast
    end

  | { pstr_desc =
        Pstr_extension
          (({ txt = "module" },
            PStr
              [{ pstr_desc = Pstr_module {
                   pmb_name = { txt = contract_name };
                   pmb_expr = {
                     pmod_desc = Pmod_structure structure
                   };
                 };
                  pstr_loc }]
           ), [])} :: ast ->
    let inner_env = mk_inner_env env contract_name in
    begin
      match translate_structure inner_env (acc_for_subcontract acc) structure with
      | PaModule contract ->
        translate_structure env (Syn_sub_contract contract :: acc) ast
      | PaContract contract ->
        error_loc pstr_loc
          "Module %s cannot have entry points (use contract instead)"
          contract_name;
    end

  | [] -> pack_contract env (List.rev acc)

  | { pstr_loc = loc } as ast :: _ ->
    if !LiquidOptions.verbosity > 0 then
      error_loc loc "at toplevel:\n%a" Printast.implementation [ast]
    else
      error_loc loc "at toplevel"

and translate_non_empty_contract env acc ast =
  match translate_structure env acc ast with
  | PaModule _ ->
    LiquidLoc.raise_error ~loc:(LiquidLoc.loc_in_file env.filename)
      "No entry point found for contract %s%!"
      env.contractname
  | PaContract contract -> contract

and pack_contract env toplevels =
  let is_module =
    not (List.exists (function Syn_entry _ -> true | _ -> false) toplevels) in
  let storage =
    try StringMap.find "storage" env.types []
    with Not_found ->
      if is_module then Tunit
      else
        LiquidLoc.raise_error
          ~loc:(LiquidLoc.loc_in_file env.filename)
          "type storage is required but not provided"
  in
  let rec partition (contracts, values, entries, init) = function
    | Syn_value v :: r ->
      partition (contracts, v :: values, entries, init) r
    | Syn_sub_contract c :: r -> partition (c :: contracts, values, entries, init) r
    | Syn_sub_contract_alias (_, c) :: r ->
      partition (c :: contracts, values, entries, init) r
    | Syn_other_contract c :: r ->
      (* ignore *)
      partition (contracts, values, entries, init) r
    | Syn_entry e :: r -> partition (contracts, values, e :: entries, init) r
    | Syn_init i :: r -> partition (contracts, values, entries, Some i) r
    | [] -> (List.rev contracts, List.rev values, List.rev entries, init) in
  let contracts, values, entries, init =
    partition ([], [], [], None) toplevels in
  let c = {
    contract_name = env.contractname;
    storage;
    values;
    entries;
    c_init = init;
    subs = contracts;
    ty_env = env } in
  if is_module then PaModule c else PaContract c

let predefined_constructors =
  List.fold_left (fun acc (constr, info) ->
      StringMap.add constr info acc) StringMap.empty
    (* These constructors are never looked-up in this
       map, hence we can provide wrong info. However, they
       need to be there to prevent the user from overriding
       them. *)
    [
      "Some", ("'a option", 0);
      "None", ("'a option", 1);
      "::", ("'a list", 0);
      "[]", ("'a list", 1);
      "Left", ("('a, 'b) variant", 0);
      "Right", ("('a, 'b) variant", 1);
      "Map", ("('a, 'b) map", 0);
      "Set", ("'a set", 0);
    ]


let filename_to_contract filename =
  String.capitalize_ascii
    (LiquidMisc.string_replace
       Filename.(basename filename |> remove_extension)
       '.' '_')

let initial_env filename =
  {
    types = StringMap.map (fun ty -> fun _ -> ty) predefined_types;
    contract_types = predefined_contract_types;
    labels = StringMap.empty;
    constrs = predefined_constructors;
    ext_prims = StringMap.empty;
    filename;
    top_env = None;
    path = [];
    contractname = filename_to_contract filename;
    others = StringMap.empty;
  }

let translate_exn exn =
  match exn with
  | Location.Error err ->
    let loc = loc_of_loc Location.(err.loc) in
    LiquidLoc.raise_error ~loc "%s"  Location.(err.msg)

  (* Syntax errors *)
  | Syntaxerr.Error err  ->
    let (loc, msg) =
      match err with
      | Syntaxerr.Unclosed(opening_loc, opening, closing_loc, closing) ->
        closing_loc,
        Printf.sprintf "Syntax error: '%s' expected" closing
      | Syntaxerr.Expecting (loc, nonterm) ->
        loc,
        Printf.sprintf "Syntax error: %s expected." nonterm
      | Syntaxerr.Not_expecting (loc, nonterm) ->
        loc,
        Printf.sprintf "Syntax error: %s not expected." nonterm
      | Syntaxerr.Applicative_path loc ->
        loc,
        "Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."
      | Syntaxerr.Variable_in_scope (loc, var) ->
        loc,
        Printf.sprintf "In this scoped type, variable '%s \
                        is reserved for the local type %s."
          var var
      | Syntaxerr.Other loc ->
        loc, "Syntax error"
      | Syntaxerr.Ill_formed_ast (loc, s) ->
        loc,
        Printf.sprintf "broken invariant in parsetree: %s" s
      | Syntaxerr.Invalid_package_type (loc, s) ->
        loc,
        Printf.sprintf "invalid package type: %s" s
    in
    let loc = loc_of_loc loc in
    LiquidLoc.raise_error ~loc "%s" msg

  | LiquidOCamlLexer.Error (error, oloc) ->
    let loc = loc_of_loc oloc in
    LiquidLoc.raise_error ~loc "%s"
      (Location.error_of_printer
         oloc
         LiquidOCamlLexer.report_error error).Location.msg
      LiquidOCamlLexer.report_error ppf error
  | _ -> raise exn


let translate ~filename ast =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Parse file %s@." filename;
  let env = initial_env filename in
  try
    let contract = translate_non_empty_contract env [] ast in
    begin match !LiquidOptions.main with
      | Some main when main <> contract.contract_name ->
        Format.eprintf "No contract named %s.@." main;
        exit 2;
      | _ -> ()
    end;
    contract
  with
  | Stop contract -> contract
  | exn -> translate_exn exn

let mk_toplevel_env filename top_env =
  let contractname = filename_to_contract filename in
  let tenv = {
    types = StringMap.empty;
    contract_types = StringMap.empty;
    labels = StringMap.empty;
    constrs = StringMap.empty;
    ext_prims = StringMap.empty;
    filename;
    top_env = Some top_env;
    contractname;
    path = [contractname];
    others = top_env.others;
  } in
  top_env.others <- StringMap.add contractname (Direct tenv) top_env.others;
  tenv

let translate_multi l =
  match List.rev l with
  | [] ->
    Format.eprintf "No contracts@.";
    exit 2
  | (filename, ast) :: r_others ->
    if !LiquidOptions.verbosity > 0 then
      Format.eprintf "Parse file %s@." filename;
    let top_env = initial_env filename in
    try
      let acc =
        List.fold_left (fun acc (filename, ast) ->
            let env = mk_toplevel_env filename top_env in
            match !LiquidOptions.main with
            | Some main when main = env.contractname (* at toplevel *) ->
              begin match translate_structure env (acc_for_subcontract_as_main acc) ast with
                | PaModule contract ->
                  LiquidLoc.raise_error ~loc:(LiquidLoc.loc_in_file filename)
                    "%s is a module and not a contract, \
                     it has no entry points and cannot be used as main"
                    contract.contract_name;
                | PaContract contract ->
                  Format.eprintf "Main contract %s@." contract.contract_name;
                  raise (Stop contract)
              end
            | _ ->
              begin match translate_structure env (acc_for_subcontract acc) ast with
                | PaModule contract ->
                  Format.eprintf "Module %s@." contract.contract_name;
                  Syn_sub_contract contract :: acc
                | PaContract contract ->
                  Format.eprintf "Contract %s@." contract.contract_name;
                  let contract_sig = sig_of_contract contract in
                  top_env.contract_types <-
                    StringMap.add contract.contract_name contract_sig
                      top_env.contract_types;
                  Syn_sub_contract contract :: acc
              end
          ) [] (List.rev r_others)
      in
      let contract = translate_non_empty_contract top_env acc ast in
      Format.eprintf "Main contract %s@." contract.contract_name;
      begin match !LiquidOptions.main with
        | Some main when main <> contract.contract_name ->
          Format.eprintf "No contract named %s.@." main;
          exit 2;
        | _ -> ()
      end;
      contract
    with
    | Stop contract -> contract
    | exn -> translate_exn exn

let ocaml_of_file parser file =
  let ic = open_in file in
  try
    Location.input_name := file;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf file;
    let ast = parser lexbuf in
    close_in ic;
    ast
  with exn ->
    close_in ic;
    translate_exn exn

let read_file filename =
  let parse =
    if Filename.check_suffix filename ".reliq" then
      LiquidReasonParse.implementation
    else
      LiquidOCamlParse.implementation in
  try
    let ast, _comments =
      ocaml_of_file parse filename in
    ast
  with exn -> translate_exn exn

let translate_expression env expression =
  try
    translate_code StringMap.empty env expression
  with exn -> translate_exn exn

let ocaml_of_string ?(filename = "buffer") parser content =
  try
    Location.input_name := filename;
    let lexbuf = Lexing.from_string content in
    Location.init lexbuf filename;
    parser lexbuf
  with exn ->
    translate_exn exn

let structure_of_string ?filename impl =
  let str, _comments =
    ocaml_of_string ?filename LiquidParse.implementation impl in
  str

let expression_of_string ?filename s =
  ocaml_of_string ?filename LiquidParse.expression s

let translate_type env ty = translate_type env ty

let type_of_string ?filename s =
  ocaml_of_string ?filename LiquidParse.core_type s
