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

let common_path p1 p2 =
  let rec aux acc p1 p2 = match p1, p2 with
    | [], _ | _, [] -> List.rev acc, p1, p2
    | x1 :: p1, x2 :: p2 ->
      if x1 = x2 then aux (x1 :: acc) p1 p2
      else List.rev acc, x1 :: p1, x2 :: p2 in
  aux [] p1 p2

let qualify_name ?from_env ~at s =
  let from = match from_env with
    | Some e -> e.path
    | None -> [] (* top level *) in
  let _common, _remain_from, path = common_path from at in
  String.concat "." (path @ [snd @@ unqualify s])

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
    let oenv =
      try StringMap.find p env.others
      with Not_found ->
        raise (Unknown_namespace (fullpath, loc))
    in
    match oenv with
    | Direct env -> find_env ~loc fullpath path env
    | Alias apath -> find_env ~loc fullpath (apath @ path) env


let find_env ~loc path env = find_env ~loc path path env

let unalias path env =
  try
    let e = find_env ~loc:noloc path env in
    if e.path = env.path @ path then None
    else Some e.path
  with Unknown_namespace _ -> None

let rec find ~loc s env proj =
  let path, s = unqualify s in
  let env = find_env ~loc path env in
  if path = [] then
    rec_find s env proj
  else
    StringMap.find s (proj env), env

let find_type ~loc s env subst =
  let mk, found_env = find ~loc s env (fun env -> env.types) in
  mk subst, found_env

(* Necessary for encoding phases where some dependencies information
   is not kept *)
let rec find_type_loose ~loc s env subst =
  try find_type ~loc s env subst
  with (Not_found | Unknown_namespace _ as e) ->
    StringMap.fold (fun _ oenv acc ->
        match acc, oenv with
        | Some _, _ -> acc
        | _, Alias _ -> acc
        | _, Direct env ->
          try Some (find_type_loose ~loc s env subst)
          with Not_found | Unknown_namespace _ -> None
      ) env.others None
    |> function
    | Some res -> res
    | None -> match env.top_env with
      | None -> raise e
      | Some env -> find_type_loose ~loc s env subst

let rec find_contract_type_aux ~loc s env =
  let path, tn = unqualify s in
  let qs = match unalias (path @ [tn]) env with
    | None -> s
    | Some p -> String.concat "." p in
  try find ~loc qs env (fun env -> env.contract_types)
  with (Not_found | Unknown_namespace _ as e) ->
  match env.top_env with
  | None -> raise e
  | Some env -> find_contract_type_aux ~loc s env


let rec normalize_type ?from_env ~in_env ty =
  match ty with
  | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes | Ttimestamp
  | Tkey | Tkey_hash | Tsignature | Toperation | Taddress | Tfail -> ty
  | Ttuple l -> Ttuple (List.map (normalize_type ?from_env ~in_env) l)
  | Toption t -> Toption (normalize_type ?from_env ~in_env t)
  | Tlist t -> Tlist (normalize_type ?from_env ~in_env t)
  | Tset t -> Tset (normalize_type ?from_env ~in_env t)
  | Tmap (t1, t2) ->
    Tmap (normalize_type ?from_env ~in_env t1,
          normalize_type ?from_env ~in_env t2)
  | Tbigmap (t1, t2) ->
    Tbigmap (normalize_type ?from_env ~in_env t1,
             normalize_type ?from_env ~in_env t2)
  | Tor (t1, t2) ->
    Tor (normalize_type ?from_env ~in_env t1,
         normalize_type ?from_env ~in_env t2)
  | Tlambda (t1, t2, u) ->
    Tlambda (normalize_type ?from_env ~in_env t1,
             normalize_type ?from_env ~in_env t2, u)
  | Tcontract (e, ty) ->
    Tcontract (e, normalize_type ?from_env ~in_env ty)
  | Trecord (name, fields) ->
    let _, found_env =
      try find_type_loose ~loc:noloc name in_env []
      with Not_found | Unknown_namespace _ -> assert false in
    Trecord (qualify_name ?from_env ~at:found_env.path name,
             List.map (fun (f, ty) ->
                 qualify_name ?from_env ~at:found_env.path f,
                 normalize_type ?from_env ~in_env ty) fields)
  | Tsum (name, constrs) ->
    let _, found_env =
      try find_type_loose ~loc:noloc name in_env []
      with Not_found | Unknown_namespace _ -> assert false in
    Tsum (qualify_name ?from_env ~at:found_env.path name,
          List.map (fun (c, ty) ->
              qualify_name ?from_env ~at:found_env.path c,
              normalize_type ?from_env ~in_env ty) constrs)
  | Tclosure ((t1, t2), t3, u) ->
    Tclosure ((normalize_type ?from_env ~in_env t1,
               normalize_type ?from_env ~in_env t2),
              normalize_type ?from_env ~in_env t3, u)
  | Tvar tvr ->
    let tv = Ref.get tvr in
    begin match tv.tyo with
      | None -> ty
      | Some ty2 ->
        (Ref.set tvr)
          { tv with tyo = Some (normalize_type ?from_env ~in_env ty2) };
        ty
    end
  | Tpartial _ -> raise (Invalid_argument "normalize_type")

and normalize_contract_sig ?from_env ~in_env ~build_sig_env c_sig =
  match c_sig.sig_name with
  | None -> c_sig (* TODO *)
  | Some s ->
    let rec env_of_contract_type_name s env =
      try find_contract_type_aux ~loc:noloc s env |> snd
      with (Not_found | Unknown_namespace _ as e) ->
        StringMap.fold (fun _ oenv acc ->
            match acc, oenv with
            | Some _, _ -> acc
            | _, Alias _ -> acc
            | _, Direct env ->
              try Some (env_of_contract_type_name s env)
              with Not_found | Unknown_namespace _ -> None
          ) env.others None
        |> function
        | None -> raise e
        | Some env -> env in
    let found_env = env_of_contract_type_name s in_env in
    let sig_env =
      if not build_sig_env then in_env
      else
        try find_env ~loc:noloc ( [unqualify s |> snd]) found_env
        with Not_found | Unknown_namespace _ ->
          (* for built-in signatures *)
          found_env
    in
    { sig_name = Some (qualify_name ?from_env ~at:found_env.path s);
      entries_sig =
        List.map (fun e ->
            { e with parameter =
                       normalize_type ?from_env ~in_env:sig_env e.parameter }
          ) c_sig.entries_sig }

let find_type ~loc s env subst =
  let ty, found_env = find_type ~loc s env subst in
  normalize_type ~from_env:env ~in_env:found_env ty

let find_contract_type ~loc s env =
  let csig, found_env = find_contract_type_aux ~loc s env in
  normalize_contract_sig ~from_env:env ~in_env:found_env ~build_sig_env:true csig

let find_label_ty_name ~loc s env =
  let (tn, i), found_env = find ~loc s env (fun env -> env.labels) in
  qualify_name ~from_env:env ~at:found_env.path tn, i

let find_constr_ty_name ~loc s env =
  let (tn, i), found_env = find ~loc s env (fun env -> env.constrs) in
  qualify_name ~from_env:env ~at:found_env.path tn, i

let find_label ~loc s env =
  let n, i = find_label_ty_name ~loc s env in
  let ty = find_type ~loc n env [] in
  let label_name, label_ty = match ty with
    | Trecord (_, l) -> List.nth l i
    | _ -> assert false in
  ty, (label_name, label_ty, i)

let find_constr ~loc s env =
  let n, i = find_constr_ty_name ~loc s env in
  let ty = find_type ~loc n env [] in
  let constr_name, constr_ty = match ty with
    | Tsum (_, l) -> List.nth l i
    | _ -> assert false in
  ty, (constr_name, constr_ty, i)

let find_extprim ~loc s env =
  let e, found_env = find ~loc s env (fun env -> env.ext_prims) in
  { e with
    atys = List.map (normalize_type ~from_env:env ~in_env:found_env) e.atys;
    rty = normalize_type ~from_env:env ~in_env:found_env e.rty }

let is_extprim s env =
  try find_extprim ~loc:noloc s env |> ignore; true
  with Not_found | Unknown_namespace _ -> false

(* ------------------------- *)

let lookup_global_value ~loc s env =
  let path, s = unqualify s in
  let path = match unalias path env.env with
    | None -> path
    | Some path -> path in
  match find_namespace ~loc path env.visible_contracts with
  | Current_namespace -> raise Not_found
  | Contract_namespace (c, _)  ->
    let v = List.find (fun v -> not v.val_private && v.val_name = s) c.values in
    Format.eprintf "Normalize %s@."
      (LiquidPrinter.Liquid.string_of_type v.val_exp.ty);
    let ty = normalize_type
        ~from_env:env.env
        ~in_env:c.ty_env v.val_exp.ty in
    { v with val_exp = { v.val_exp with ty } }
  | exception Unknown_namespace (
      ["Current" | "Account" | "Map" | "Set" | "List" | "Contract"
      | "Crypto" | "Bytes" | "String" ], _ ) ->
    (* Better error messages for typos *)
    raise Not_found


let find_contract ~loc s env contracts =
  let path, s = unqualify s in
  let path = match unalias path env with
    | None -> path
    | Some path -> path in
  match find_namespace ~loc path (StringMap.bindings contracts |> List.map snd) with
  | Current_namespace ->
    StringMap.find s contracts
  | Contract_namespace (c, _)  ->
    List.find (fun c -> c.contract_name = s) c.subs

let find_module ~loc path env contracts =
  let path = match unalias path env with
    | None -> path
    | Some path -> path in
  match find_namespace ~loc path contracts with
  | Current_namespace -> raise Not_found
  | Contract_namespace (c, _)  -> c

let qual_contract_name c =
  match c.ty_env.path with
  | [] -> c.contract_name
  | p -> String.concat "." p

let unalias_name s env =
  let path, s' = unqualify s in
  match unalias path env with
  | None -> s
  | Some path -> add_path_name path s'
