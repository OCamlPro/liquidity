(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017-2019                                             *)
(*    OCamlPro SAS <contact@ocamlpro.com>                                 *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

exception Unknown_namespace of string list * location

type 'a namespace_res =
  | Current_namespace
  | Contract_namespace of 'a contract * string list

(* Recursively look for value in (projection of) upper level contracts *)
let rec rec_find s env proj =
  try StringMap.find s (proj env), env
  with Not_found ->
  match env.top_env with
  | None -> raise Not_found
  | Some env -> rec_find s env proj

let unqualify s =
  match List.rev (String.split_on_char '.' s) with
  | [] -> assert false
  | s :: rpath -> List.rev rpath, s

let qualify_name path s = String.concat "." (path @ [snd @@ unqualify s])
let add_path_name path s = String.concat "." (path @ [s])


(* Find namespace in a tree of (sub-)contracts *)
let find_namespace ~loc fullpath subs =
  match fullpath with
  | [] -> Current_namespace
  | _ ->
    let rec find reached_path path subs =
      match path with
      | [] -> raise (Unknown_namespace (fullpath, loc))
      | [p] ->
        begin
          try Contract_namespace
                (List.find (fun c -> c.contract_name = p) subs,
                 List.rev reached_path)
          with Not_found -> raise (Unknown_namespace (fullpath, loc))
        end
      | p :: path ->
        try
          let c = List.find (fun c -> c.contract_name = p) subs in
          find (p :: reached_path) path c.subs
        with Not_found -> raise (Unknown_namespace (fullpath, loc))
    in
    find [] fullpath subs

(* Lookup for a qualified value in the corresponding
   namespace or upper levels *)
(* let rec_lookup ~loc env contracts (path, s) proj =
 *   match find_namespace ~loc path contracts with
 *   | Current_namespace -> rec_find s env proj
 *   | Contract_namespace (c, path) ->
 *     StringMap.find s (proj c.ty_env), c.ty_env *)

let rec find_env ~loc fullpath path env =
  match path with
  | [] -> env
  | p :: path ->
    let env =
      try StringMap.find p env.others
      with Not_found ->
        raise (Unknown_namespace (fullpath, loc))
    in
    find_env ~loc fullpath path env

let find_env ~loc path env = find_env ~loc path path env

let rec find ~loc s env proj =
  let path, s = unqualify s in
  let env = find_env ~loc path env in
  rec_find s env proj

let find_type ~loc s env subst =
  let mk, found_env = find ~loc s env (fun env -> env.types) in
  mk subst, found_env

let find_contract_type_aux ~loc s env =
  find ~loc s env (fun env -> env.contract_types)

let rec normalize_type env ty = match ty with
  | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes | Ttimestamp
  | Tkey | Tkey_hash | Tsignature | Toperation | Taddress | Tfail -> ty
  | Ttuple l -> Ttuple (List.map (normalize_type env) l)
  | Toption t -> Toption (normalize_type env t)
  | Tlist t -> Tlist (normalize_type env t)
  | Tset t -> Tset (normalize_type env t)
  | Tmap (t1, t2) -> Tmap (normalize_type env t1, normalize_type env t2)
  | Tbigmap (t1, t2) -> Tbigmap (normalize_type env t1, normalize_type env t2)
  | Tor (t1, t2) -> Tor (normalize_type env t1, normalize_type env t2)
  | Tlambda (t1, t2) -> Tlambda (normalize_type env t1, normalize_type env t2)
  | Tcontract c_sig -> Tcontract (normalize_contract_sig env c_sig)
  | Trecord (name, fields) ->
    let _, found_env = find_type ~loc:noloc name env [] in
    Trecord (qualify_name found_env.path name,
             List.map (fun (f, ty) ->
                 qualify_name found_env.path f,
                 normalize_type found_env ty) fields)
  | Tsum (name, constrs) ->
    let _, found_env = find_type ~loc:noloc name env [] in
    Tsum (qualify_name found_env.path name,
          List.map (fun (c, ty) ->
              qualify_name found_env.path c,
              normalize_type found_env ty) constrs)
  | Tclosure ((t1, t2), t3) ->
    Tclosure ((normalize_type env t1, normalize_type env t2),
              normalize_type env t3)
  | Tvar tvr ->
    let tv = Ref.get tvr in
    begin match tv.tyo with
      | None -> ty
      | Some ty ->
        (Ref.set tvr) { tv with tyo = Some (normalize_type env ty) };
        ty
    end
  | Tpartial _ -> raise (Invalid_argument "normalize_type")

and normalize_contract_sig env c_sig =
  match c_sig.sig_name with
  | None -> c_sig
  | Some s ->
    let _, found_env = find_contract_type_aux ~loc:noloc s env in
    { c_sig with sig_name = Some (qualify_name found_env.path s) }

let find_type ~loc s env subst =
  let ty, found_env = find_type ~loc s env subst in
  normalize_type found_env ty

let find_contract_type ~loc s env =
  let csig, found_env = find_contract_type_aux ~loc s env in
  normalize_contract_sig found_env csig

let find_label_ty_name ~loc s env =
  let (tn, i), found_env = find ~loc s env (fun env -> env.labels) in
  qualify_name found_env.path tn, i

let find_constr_ty_name ~loc s env =
  let (tn, i), found_env = find ~loc s env (fun env -> env.constrs) in
  qualify_name found_env.path tn, i

let find_label ~loc s env =
  let n, i = find_label_ty_name ~loc s env in
  let ty = find_type ~loc n env [] in
  let label_ty = match ty with
    | Trecord (_, l) -> snd (List.nth l i)
    | _ -> assert false in
  ty, (label_ty, i)

let find_constr ~loc s env =
  let n, i = find_constr_ty_name ~loc s env in
  let ty = find_type ~loc n env [] in
  let constr_ty = match ty with
    | Tsum (_, l) -> snd (List.nth l i)
    | _ -> assert false in
  ty, (constr_ty, i)

let find_extprim ~loc s env =
  let e, found_env = find ~loc s env (fun env -> env.ext_prims) in
  { e with atys = List.map (normalize_type found_env) e.atys;
           rty = normalize_type found_env e.rty }

let is_extprim s env =
  try find_extprim ~loc:noloc s env |> ignore; true
  with Not_found | Unknown_namespace _ -> false

(* ------------------------- *)

let lookup_global_value ~loc s env =
  let path, s = unqualify s in
  match find_namespace ~loc path env.visible_contracts with
  | Current_namespace -> raise Not_found
  | Contract_namespace (c, _)  ->
    List.find (fun v -> not v.val_private && v.val_name = s) c.values
