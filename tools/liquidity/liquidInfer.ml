(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2020 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                             Steven De Oliveira                           *)
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
open LiquidPrinter.Liquid

module QualMap = struct
  open LiquidNamespace
  include Map.Make (struct
    type t = string list
    let compare = Pervasives.compare
  end)
  let add path s v m =
    let p', s = unqualify s in
    add (path @ p' @ [s]) v m
  let find path s m =
    let p', s = unqualify s in
    find (path @ p' @ [s]) m
  let mem path s m =
    let p', s = unqualify s in
    mem (path @ p' @ [s]) m
end

let error loc msg =
  LiquidLoc.raise_error ~loc ("Type error:  " ^^ msg ^^ "%!")

let fresh_tv =
  let cnt = ref 0 in
  let a = Char.code 'a' in
  fun () ->
    let q = !cnt / 26 in
    let r = !cnt mod 26 in
    cnt := !cnt + 1;
    (Char.escaped (Char.chr (a + r)))
    ^ (if q > 0 then string_of_int q else "")

let mk_tvar id =
  Tvar (Ref.create { id; tyo = None })

let fresh_tvar () =
  mk_tvar (fresh_tv ())

let wrap_tvar ty =
  Tvar (Ref.create { id = fresh_tv (); tyo = Some ty })

let rec has_tvar = function
  | Ttuple tyl -> List.exists has_tvar tyl
  | Toption ty | Tlist ty | Tset ty | Tcontract_handle (_, ty) -> has_tvar ty
  | Tmap (ty1, ty2) | Tbigmap (ty1, ty2) | Tor (ty1, ty2)
  | Tcontract_view (_, ty1, ty2)
  | Tlambda (ty1, ty2, _) -> has_tvar ty1 || has_tvar ty2
  | Tclosure ((ty1, ty2), ty3, _) -> has_tvar ty1 || has_tvar ty2 || has_tvar ty3
  | Trecord (_, fl) | Tsum (_, fl) ->List.exists (fun (_, ty) -> has_tvar ty) fl
  | Tvar _ -> true
  | Tpartial _ -> failwith "Anomaly : has_tvar Tpartial should not happen"
  | _ -> false

let rec occurs id = function
  | Ttuple tyl -> List.exists (fun ty -> occurs id ty) tyl
  | Toption ty | Tlist ty | Tset ty | Tcontract_handle (_, ty) -> occurs id ty
  | Tmap (ty1, ty2) | Tbigmap (ty1, ty2) | Tor (ty1, ty2)
  | Tcontract_view (_, ty1, ty2)
  | Tlambda (ty1, ty2, _) -> occurs id ty1 || occurs id ty2
  | Tclosure ((ty1, ty2), ty3, _) -> occurs id ty1 || occurs id ty2 ||occurs id ty3
  | Trecord (_, fl) | Tsum (_, fl)->List.exists (fun (_, ty) -> occurs id ty) fl
  | Tvar tvr ->
    let tv = Ref.get tvr in
    tv == id || (match tv.tyo with Some ty -> occurs id ty | _ -> false)
  | Tpartial (Peqn (el,_)) ->
    List.exists (fun (cl, rty) -> occurs id rty ||
                                  List.exists (fun (ty1, ty2) -> occurs id ty1 || occurs id ty2) cl) el
  | Tpartial (Ptup al) -> List.exists (fun (_, ty) -> occurs id ty) al
  | Tpartial (Pmap (ty1, ty2)) -> occurs id ty1 || occurs id ty2
  | Tpartial (Pcont (Some (_, ty))) -> occurs id ty
  | _ -> false

let merge_lists l1 l2 =
  List.fold_left (fun l e ->
      if List.mem e l then l else e :: l
    ) [] (l1 @ l2)

let rec make_subst l1 l2 = match l1, l2 with
  | [], [] -> []
  | _, [] -> List.map (fun v -> v, fresh_tvar ()) l1
  | [], _ -> invalid_arg "too many type arguments"
  | x1 :: l1, x2 :: l2 -> (x1, x2) :: make_subst l1 l2

(* let print_loc loc =
 *   match loc.loc_pos with
 *   | Some ( (begin_line, begin_char) , (end_line, end_char) ) ->
 *     Printf.printf "%d.%d-%d.%d"
 *       begin_line begin_char
 *       end_line end_char
 *   | None ->
 *     Printf.printf "%s" loc.loc_file *)

let rec generalize tyx1 tyx2 =

  (* Generalize the types wrt one another *)
  match tyx1, tyx2 with

  | Tvar tvr1, Tvar tvr2 when (Ref.get tvr1).id = (Ref.get tvr2).id -> ()

  | Tvar ({ contents = { contents = { tyo = Some tyz1 }}} as tvr1),
    Tvar ({ contents = { contents = { tyo = Some tyz2 }}} as tvr2) ->
    if not @@ eq_types tyz1 tyz2 then begin
      Ref.set tvr1 { id = fresh_tv (); tyo = None };
      Ref.set tvr2 { id = fresh_tv (); tyo = None };
    end

  | Tvar tvr, tyx | tyx, Tvar tvr ->
    begin match (Ref.get tvr).tyo with
      | None -> ()
      | Some ty ->
        generalize ty tyx;
        if not @@ eq_types ty tyx then
          Ref.set tvr { id = fresh_tv (); tyo = None }
    end

  | Tpartial _, Tpartial _ ->
    failwith "Anomaly : cannot generalize Tpartial"


  | Ttuple tl1, Ttuple tl2 when List.compare_lengths tl1 tl2 = 0 ->
    List.iter2 generalize tl1 tl2

  | Toption ty1, Toption ty2
  | Tlist ty1, Tlist ty2
  | Tset ty1, Tset ty2
  | Tcontract_handle (_, ty1), Tcontract_handle (_, ty2) ->
    generalize ty1 ty2

  | Tmap (k_ty1, v_ty1), Tmap (k_ty2, v_ty2)
  | Tbigmap (k_ty1, v_ty1), Tbigmap (k_ty2, v_ty2) ->
    generalize k_ty1 k_ty2;
    generalize v_ty1 v_ty2

  | Tor (l_ty1, r_ty1), Tor (l_ty2, r_ty2) ->
    generalize l_ty1 l_ty2;
    generalize r_ty1 r_ty2

  | Tlambda (from_ty1, to_ty1, _), Tlambda (from_ty2, to_ty2, _) ->
    generalize from_ty1 from_ty2;
    generalize to_ty1 to_ty2

  | Tcontract_view (_, from_ty1, to_ty1), Tcontract_view (_, from_ty2, to_ty2) ->
    generalize from_ty1 from_ty2;
    generalize to_ty1 to_ty2

  | Tclosure ((from_ty1, env_ty1), to_ty1, _),
    Tclosure ((from_ty2, env_ty2), to_ty2, _) ->
    generalize from_ty1 from_ty2;
    generalize env_ty1 env_ty2;
    generalize to_ty1 to_ty2

  | Trecord (_, fl1), Trecord (_, fl2)
  | Tsum (_, fl1), Tsum (_, fl2)
    when List.compare_lengths fl1 fl2 = 0 ->
    List.iter2 (fun (_, ty1) (_, ty2) ->
        generalize ty1 ty2;
      ) fl1 fl2

  | _ , _ ->
    if not (eq_types tyx1 tyx2) then
      error noloc "Types %s and %s are not compatible\n"
        (string_of_type tyx1)
        (string_of_type tyx2)

let unify_uncurry_flags u1 u2 =
  let u = match !(!u1), !(!u2) with
    | None, None -> None
    | None, Some u | Some u, None -> Some u
    | Some u1, Some u2 -> Some (u1 || u2) in
  !u1 := u;
  !u2 := u;
  u2 := !u1

let rec unify loc ty1 ty2 =

  match ty1, ty2 with
  | Tpartial _, Tpartial _ ->
    failwith "Anomaly : both Tpartial outside Tvar"
  | _, _ -> ();

    let unify = unify loc in

    (* Expand tvars *)
    let tyx1 = expand ty1 in
    let tyx2 = expand ty2 in

    (* print_loc loc;
     * Printf.printf ": Unify %s " (string_of_type ty1);
     * Printf.printf "| %s\n%!" (string_of_type ty2); *)

    (* Unify the types *)
    let tyx, to_unify = match tyx1, tyx2 with

      | Tvar tvr1, Tvar tvr2 when (Ref.get tvr1).id = (Ref.get tvr2).id ->
        tyx1, []

      | Tvar tvr, tyx | tyx, Tvar tvr -> (* (Ref.get tvr).tyo = None *)
        if occurs (Ref.get tvr) tyx then
          failwith ("Cyclic vars '" ^ (Ref.get tvr).id);
        tyx, []


      | Tpartial Peqn (el1, l1), Tpartial Peqn (el2, l2) ->
        let el = List.fold_left (fun el (cl1, rty1) ->
            List.fold_left (fun el (cl2, rty2) ->
                if not (eq_types rty1 rty2) then el (* eqn do not contain tvars *)
                else (merge_lists cl1 cl2, rty1) :: el (*might duplicate constraints*)
              ) el el2
          ) [] el1 in
        Tpartial (Peqn (el, l1)) |> finalize_eqn loc

      | Tpartial Peqn (el, l), ty | ty, Tpartial Peqn (el, l) ->
        let el = List.filter (fun (_, rty) -> eq_types ty rty) el in
        Tpartial (Peqn (el, l)) |> finalize_eqn loc


      | Tpartial Ptup pl1, Tpartial Ptup pl2 ->
        let pl = List.fold_left (fun pl (n, ty2) ->
            try let ty1 = List.assoc n pl in unify ty1 ty2; pl
            with Not_found -> (n, ty2) :: pl
          ) pl1 pl2 in
        Tpartial (Ptup pl), []

      | Tpartial (Ptup pl), ty | ty, Tpartial (Ptup pl) ->
        begin match ty with
          | Ttuple tuple ->
            begin try
                List.iter (fun (n, ty) -> unify (List.nth tuple n) ty) pl;
                Ttuple tuple, []
              with
              | Invalid_argument _ ->
                error loc "Index must be positive or zero"
              | Failure _ ->
                error loc "Expecting a tuple of arity >= %d" (List.length tuple)
            end
          | Trecord (rn, fl) ->
            begin try
                List.iter (fun (n, ty) -> unify (snd (List.nth fl n)) ty) pl;
                Trecord (rn, fl), []
              with
              | Invalid_argument _ ->
                error loc "Index must be positive or zero"
              | Failure _ ->
                error loc "Expecting a record of arity >= %d" (List.length fl)
            end
          | _ ->
            error loc "Partial tuple incompatible with %S"
              (string_of_type ty)
        end


      | Tpartial (Pmap (k_ty1, v_ty1)), Tpartial (Pmap (k_ty2, v_ty2)) ->
        unify k_ty1 k_ty2; unify v_ty1 v_ty2;
        Tpartial (Pmap (k_ty1, v_ty1)), []

      | Tpartial (Pmap (k_ty1, v_ty1)), ty
      | ty, Tpartial (Pmap (k_ty1, v_ty1)) ->
        begin match ty with
          | Tmap (k_ty2, v_ty2) | Tbigmap (k_ty2, v_ty2) ->
            unify k_ty1 k_ty2; unify v_ty1 v_ty2;
            ty, []
          | _ -> error loc "Undetermined map incompatible with %S"
                   (string_of_type ty)
        end


      | Tpartial (Pcont None), Tpartial (Pcont None) ->
        Tpartial (Pcont None), []
      | Tpartial (Pcont (Some (e, ty))), Tpartial (Pcont None)
      | Tpartial (Pcont None), Tpartial (Pcont (Some (e, ty))) ->
        Tpartial (Pcont (Some (e, ty))), []
      | Tpartial (Pcont (Some (e1, ty1))), Tpartial (Pcont (Some (e2, ty2))) ->
        if e1 <> e2 then
          error loc "Handles for different entry points (%s and %s)"
            e1 e2;
        unify ty1 ty2;
        tyx1, []

      | Tpartial (Pcont c), ty | ty, Tpartial (Pcont c) ->
        begin match ty, c with
          | Tcontract_handle (e, ty), None ->
            Tcontract_handle (e, ty), []
          | Tcontract_handle (Some e, ty), Some (ep, typ) ->
            unify typ ty;
            let e = if e <> ep then None else Some e in
            Tcontract_handle (e, ty), []
          | Tcontract_handle (None, ty), Some (ep, typ) ->
            Tcontract_handle (Some ep, ty), []
          | _, _ -> error loc "Partial contract incompatible with %S"
                   (string_of_type ty)
        end


      | Tpartial (Ppar), Tpartial (Ppar) ->
        Tpartial (Ppar), []

      | Tpartial (Ppar), ty | ty, Tpartial (Ppar) ->
        ty, []


      | Ttuple tl1, Ttuple tl2 ->
        begin try List.iter2 unify tl1 tl2;
          with Invalid_argument _ ->
            error loc "Tuples %S and %S have different arities"
              (string_of_type ty1)
              (string_of_type ty2)
        end;
        tyx1, []

      | Toption ty1, Toption ty2
      | Tlist ty1, Tlist ty2 ->
        unify ty1 ty2; tyx1, []

      | Tset ty1, Tset ty2 ->
        unify ty1 ty2;
        if not @@ comparable_type ty1 then
          error loc "Elements of set are of a non comparable type %s"
            (string_of_type ty1);
        tyx1, []

      | Tmap (k_ty1, v_ty1), Tmap (k_ty2, v_ty2)
      | Tbigmap (k_ty1, v_ty1), Tbigmap (k_ty2, v_ty2) ->
        unify k_ty1 k_ty2;
        if not @@ comparable_type k_ty1 then
          error loc "Keys of map are of a non comparable type %s"
            (string_of_type k_ty1);
        unify v_ty1 v_ty2;
        tyx1, []

      | Tor (l_ty1, r_ty1), Tor (l_ty2, r_ty2) ->
        unify l_ty1 l_ty2; unify r_ty1 r_ty2; tyx1, []

      | Tlambda (from_ty1, to_ty1, u1), Tlambda (from_ty2, to_ty2, u2) ->
        unify from_ty1 from_ty2; unify to_ty1 to_ty2;
        unify_uncurry_flags u1 u2;
        tyx1, []

      | Tclosure ((from_ty1, env_ty1), to_ty1, u1),
        Tclosure ((from_ty2, env_ty2), to_ty2, u2) ->
        unify from_ty1 from_ty2;
        unify env_ty1 env_ty2;
        unify to_ty1 to_ty2;
        unify_uncurry_flags u1 u2;
        tyx1, []

      | Trecord (_, fl1), Trecord (_, fl2)
      | Tsum (_, fl1), Tsum (_, fl2) ->
        begin try
            List.iter2 (fun (_, ty1) (_, ty2) -> unify ty1 ty2) fl1 fl2;
          with Invalid_argument _ ->
            error loc "Types %S and %S have different arities"
              (string_of_type ty1)
              (string_of_type ty2)
        end;
        tyx1, []

      | Tcontract_handle (e1, ty1), Tcontract_handle (e2, ty2) ->
        (match e1, e2 with
         | Some e1, Some e2 when e1 <> e2 ->
           error loc "Handles for different entry points (%s and %s)"
             e1 e2
         | _ -> ()
        );
        unify ty1 ty2;
        tyx1, []

      | Tcontract_view (v1, tp1, tr1), Tcontract_view (v2, tp2, tr2) ->
        if v1 <> v2 then
          error loc "Handles for different views (%s and %s)" v1 v2;
        unify tp1 tp2;
        unify tr1 tr2;
        tyx1, []

      | _, _ ->
        if not (eq_types tyx1 tyx2) then
          error loc "Types %s and %s are not compatible\n"
            (string_of_type tyx1)
            (string_of_type tyx2);
        tyx1, []
    in

    (* Update the type variables *)
    begin match ty1, ty2, tyx with
      | Tvar tvr1, Tvar tvr2, Tvar tvr ->
        if (Ref.get tvr).tyo <> None then failwith "Tvar after unify"
        else Ref.merge_set tvr1 tvr2 { (Ref.get tvr1) with tyo = None }
      | Tvar tvr1, Tvar tvr2, _ ->
        Ref.merge_set tvr1 tvr2 { (Ref.get tvr1) with tyo = Some tyx }
      | Tvar tvr, _, _ | _, Tvar tvr, _ ->
        Ref.set tvr { (Ref.get tvr) with tyo = Some tyx }
      | _ -> () end;

    (* Printf.printf "After unify %s | %s\n\n"
     *   (string_of_type ty1)
     *   (string_of_type ty2); *)

    (* Unify LHS of equations *)
    unify_list loc to_unify

and unify_list loc to_unify =
  List.iter (fun (ty1, ty2) ->
      unify loc ty1 ty2) to_unify

and finalize_eqn loc = function
  | Tpartial (Peqn ([], _)) -> error loc "No suitable overload\n"
  | Tpartial (Peqn ([(cl, rty)], _)) -> rty, cl
  (* | Tpartial (Peqn ((_, rty) :: el, _)) when
   *     List.for_all (fun (_, rty') -> eq_types rty rty') el ->
   *   rty, [] *)
  | tyx -> tyx, []

and resolve loc ty =
  let rec aux ty = match ty with
    | Tpartial (Peqn (el, l)) ->
      let el = List.fold_left (fun el (cl, rty) ->
          let cl, unsat = List.fold_left (fun (cl, unsat) (tv, ty) ->
              if unsat then (cl, unsat) else
                let tv, to_unify = match expand tv with
                  | Tpartial (Peqn _) as ty -> aux ty
                  | _ -> tv, [] in
                unify_list loc to_unify;
                match expand tv with
                | Tpartial _ | Tvar _ -> ((tv, ty) :: cl, unsat)
                | ty' -> if eq_types ty ty' then (cl, unsat) else ([], true)
            ) ([], false) cl in
          if unsat then el else (cl, rty) :: el
        ) [] el in
      Tpartial (Peqn (el, loc)) |> finalize_eqn loc
    | _ -> ty, []
  in
  aux ty

let rec compat_types ty1 ty2 =
  match expand ty1, expand ty2 with
  | Tvar _, _ | _, Tvar _ -> true
  | Tpartial Ppar, _ | _, Tpartial Ppar -> true
  | Tpartial (Peqn (el1, _)), Tpartial (Peqn (el2, _)) ->
    List.exists (fun (_, rty1) ->
        List.exists (fun (_, rty2) ->
            compat_types rty1 rty2) el2
      ) el1
  | Tpartial (Peqn (el, _)), ty | ty, Tpartial (Peqn (el, _)) ->
    List.exists (fun (_, rty) -> compat_types rty ty) el
  | _, _ -> eq_types ty1 ty2

let make_type_eqn loc overloads params =
  let el = List.fold_left (fun eqn (opl, ort) ->
      let cl, unsat = List.fold_left2 (fun (cl, unsat) op p ->
          if unsat || eq_types op p then (cl, unsat)
          else match p with (* if expand, then Tvar below does not work*)
            | Tvar tv1 when
                compat_types op p &&
                List.for_all (function
                    | Tvar tv2, oty when (Ref.get tv1).id = (Ref.get tv2).id ->
                      compat_types op oty
                    | _ -> true) cl
              ->
              ((p, op) :: cl, unsat)
            | _ -> ([], true)
        ) ([], false) opl params in
      if unsat then eqn else (cl, ort) :: eqn
    ) [] overloads in
  let ty = Tpartial (Peqn (el, loc)) in
  let ty, to_unify = resolve loc ty in
  let ty = match ty with
    | Tpartial (Peqn (_, _)) ->
      Tvar (Ref.create { id = fresh_tv (); tyo = Some ty })
    | Tpartial _ -> failwith "Bad return type"
    | _ -> ty (* what if compound ? *)
  in
  unify_list loc to_unify;
  ty

let rec find_variant_type ~loc env = function
  | [] -> None
  | (PAny, _) :: cases -> find_variant_type ~loc env cases
  | (PConstr (("Left"|"Right"), _), _) :: _ ->
    Some (Tor (fresh_tvar (), fresh_tvar ()))
  | (PConstr (c, _), _) :: _ ->
    try Some (fst (LiquidNamespace.find_constr ~loc c env))
    with Not_found -> None



(* Monomorphisation *)

let instantiate_to s ty =
  let rec aux ty = match ty with
    | Ttuple tyl -> Ttuple (List.map aux tyl)
    | Toption ty -> Toption (aux ty)
    | Tlist ty -> Tlist (aux ty)
    | Tset ty -> Tset (aux ty)
    | Tmap (ty1, ty2) -> Tmap (aux ty1, aux ty2)
    | Tbigmap (ty1, ty2) -> Tbigmap (aux ty1, aux ty2)
    | Tor (ty1, ty2) -> Tor (aux ty1, aux ty2)
    | Tlambda (ty1, ty2, u) -> Tlambda (aux ty1, aux ty2, u)
    | Tclosure ((ty1, ty2), ty3, u) -> Tclosure ((aux ty1, aux ty2), aux ty3, u)
    | Trecord (rn, fl) ->
      Trecord (rn, List.map (fun (fn, fty) -> (fn, aux fty)) fl)
    | Tsum (sn, cl) ->
      Tsum (sn, List.map (fun (cn, cty) -> (cn, aux cty)) cl)
    | Tcontract_handle (e, ty) -> Tcontract_handle (e, aux ty)
    | Tcontract_view (v, t1, t2) -> Tcontract_view (v, aux t1, aux t2)
    | Tvar tvr ->
      let tv = Ref.get tvr in
      begin match tv.tyo with
        | None -> begin try List.assoc tv.id s with Not_found -> ty end
        | Some (Tpartial _ as ty') ->
          let ty'' = aux ty' in
          if not @@ eq_types ty' ty'' then
            Ref.set tvr { tv with tyo = Some ty'' };
          ty
        | Some ty -> aux ty (* a substitution should not exist for tv.id *)
      end
    | Tpartial (Peqn (el, loc)) ->
      let el = List.map (fun (cl, rty) ->
          let rty = aux rty in
          let cl = List.map (fun (x, y) -> aux x, aux y) cl in
          (cl, rty)) el in
      let el = List.filter (fun (cl, _) ->
          List.for_all (fun (x, y) -> compat_types x y) cl
        ) el in
      Tpartial (Peqn (el, loc))
    | _ -> ty
  in
  aux ty

let get_type env loc ty =
  let rec aux ty = match ty with
    | Ttuple tyl -> Ttuple (List.map aux tyl)
    | Toption ty -> Toption (aux ty)
    | Tlist ty -> Tlist (aux ty)
    | Tset ty ->
      let ty = aux ty in
      if not @@ comparable_type ty then
        error loc "Elements of set are of a non comparable type %s"
          (string_of_type ty);
      Tset ty
    | Tmap (ty1, ty2) ->
      let ty1 = aux ty1 in
      if not @@ comparable_type ty1 then
        error loc "Keys of map are of a non comparable type %s"
          (string_of_type ty1);
      Tmap (ty1, aux ty2)
    | Tbigmap (ty1, ty2) ->
      let ty1 = aux ty1 in
      if not @@ comparable_type ty1 then
        error loc "Keys of big map are of a non comparable type %s"
          (string_of_type ty1);
      Tbigmap (ty1, aux ty2)
    | Tor (ty1, ty2) -> Tor (aux ty1, aux ty2)
    | Tlambda (ty1, ty2, u) -> Tlambda (aux ty1, aux ty2, u)
    | Tclosure ((ty1, ty2), ty3, u) ->
      Tclosure ((aux ty1, aux ty2), aux ty3, u)
    | Trecord (rn, fl) ->
      Trecord (rn, List.map (fun (f, ty) -> (f, aux ty)) fl)
    | Tsum (sn, cl) ->
      Tsum (sn, List.map (fun (c, ty) -> (c, aux ty)) cl)
    | Tcontract_handle (e, ty) -> Tcontract_handle (e, aux ty)
    | Tcontract_view (v, t1, t2) -> Tcontract_view (v, aux t1, aux t2)
    | Tvar tvr when (Ref.get tvr).tyo = None -> ty
    | Tvar tvr ->
      let tv = Ref.get tvr in
      let ty = match tv.tyo with Some ty -> ty | _ -> assert false in
      begin match ty with
        | Tpartial (Ptup pl) ->
          let pl = List.sort (fun (i1, p1) (i2, p2) ->
              Pervasives.compare i1 i2) pl in
          let _, pl = List.fold_left (fun (l, pl) (i, p) ->
              if l = i then (l + 1, (aux p) :: pl)
              else begin
                let plr = ref pl in
                for c = l to i-1 do
                  plr := (fresh_tvar ()) :: !plr
                done;
                i + 1, (aux p) :: !plr
              end
            ) (0, []) pl in
          let ty = match List.rev pl with
            | [ty] -> Ttuple [ty; fresh_tvar ()]
            | l -> Ttuple l in
          Ref.set tvr { tv with tyo = Some ty }; ty
        (* | Tpartial (Pmap (k_ty, v_ty)) ->
         * TODO might be bigmap also
         *   let ty = Tmap (aux k_ty, aux v_ty) in
         *   Ref.set tvr { tv with tyo = Some ty }; ty *)
        | Tpartial (Pcont c) ->
          let tyo = match c with
            | None -> unit_contract_ty
            | Some (e, ty) -> Tcontract_handle (Some e, ty) in
          Ref.set tvr { tv with tyo = Some tyo }; ty

        | Tpartial (Peqn _) as ty ->
          let ty, to_unify = resolve loc ty in
          Ref.set tvr { tv with tyo = Some ty };
          unify_list loc to_unify;
          ty
        | Tpartial (Ppar) ->
          error loc "Parameter type can't be inferred, add an annotation"
        | ty -> aux ty
      end
    | _ -> ty
  in
  aux ty

module Hty = Hashtbl.Make (struct
    type t = datatype
    let equal = eq_types
    let hash = Hashtbl.hash
  end)

(* Create unique nunmber for a type associated to a name *)
let type_nb =
  let name_cpts = Hashtbl.create 17 in
  fun name ty ->
    let ntypes, max_cpt =
      try Hashtbl.find name_cpts name
      with Not_found ->
        let ts = Hty.create 9 in
        Hashtbl.add name_cpts name (ts, 0);
        ts, 0 in
    try Hty.find ntypes ty
    with Not_found ->
      let cpt = max_cpt + 1 in
      Hty.add ntypes ty cpt;
      Hashtbl.add name_cpts name (ntypes, cpt);
      cpt

let mk_typed_name s tn =
  if tn = 1 then s
  else Printf.sprintf "%s__%d_" s tn

let rec vars_to_unit ?loc ~keep_tvars ty =
  let vars_to_unit = vars_to_unit ~keep_tvars in
  match ty with
  | Ttuple tyl -> Ttuple (List.map (vars_to_unit ?loc) tyl)
  | Toption ty -> Toption (vars_to_unit ?loc ty)
  | Tlist ty -> Tlist (vars_to_unit ?loc ty)
  | Tset ty -> Tset (vars_to_unit ?loc ty)
  | Tmap (ty1, ty2) -> Tmap (vars_to_unit ?loc ty1, vars_to_unit ?loc ty2)
  | Tbigmap (ty1, ty2) -> Tbigmap (vars_to_unit ?loc ty1, vars_to_unit ?loc ty2)
  | Tor (ty1, ty2) -> Tor (vars_to_unit ?loc ty1, vars_to_unit ?loc ty2)
  | Tlambda (ty1, ty2, u) -> Tlambda (vars_to_unit ?loc ty1, vars_to_unit ?loc ty2, u)
  | Tclosure ((ty1, ty2), ty3, u) ->
    Tclosure ((vars_to_unit ?loc ty1, vars_to_unit ?loc ty2),
              vars_to_unit ?loc ty3, u)
  | Trecord (rn, fl) ->
    Trecord (rn, List.map (fun (fn, fty) -> (fn, vars_to_unit ?loc fty)) fl)
  | Tsum (sn, cl) ->
    Tsum (sn, List.map (fun (cn, cty) -> (cn, vars_to_unit ?loc cty)) cl)
  | Tcontract_handle (e, ty) -> Tcontract_handle (e, vars_to_unit ?loc ty)
  | Tcontract_view (e, t1, t2) ->
    Tcontract_view (e, vars_to_unit ?loc t1, vars_to_unit ?loc t2)
  | Tvar { contents = { contents = { tyo = Some ty }}} -> vars_to_unit ?loc ty
  | Tvar _ when keep_tvars -> ty
  | Tvar _ ->
    (* Remaining vars correspond to unused arguments *)
    (* unify (match loc with None -> noloc | Some loc -> loc)
     *   ty Tunit; *)
    Tunit
  | Tpartial (Peqn (_, eqloc)) as ty ->
    let loc = match loc with None -> eqloc | Some loc -> loc in
    let ty, to_unify = resolve loc ty in
    begin match ty with
      | Tpartial (Peqn (el, l)) -> (* pick first ?*)
        error loc "Unresolved overload, add annotations"
      | _ ->
        unify_list loc to_unify;
        ty
    end
  | Tpartial _ ->
    error (match loc with None -> noloc | Some loc -> loc)
      "Type cannot be inferred, please add an annotation : %s"
      (string_of_type ty)
  | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes | Ttimestamp | Tkey
  | Tkey_hash | Tsignature | Toperation | Taddress | Tfail | Tchainid -> ty

let sig_vars_to_unit ?loc ~keep_tvars c =
  { c with entries_sig =
             List.map (fun es ->
                 { es with parameter = vars_to_unit ?loc ~keep_tvars es.parameter }
               ) c.entries_sig
  }

let rec has_unresolved = function
  | Ttuple tyl -> List.exists has_unresolved tyl
  | Toption ty | Tlist ty | Tset ty | Tcontract_handle (_, ty) -> has_unresolved ty
  | Tmap (ty1, ty2) | Tbigmap (ty1, ty2) | Tor (ty1, ty2)
  | Tcontract_view (_, ty1, ty2)
  | Tlambda (ty1, ty2, _) -> has_unresolved ty1 || has_unresolved ty2
  | Tclosure ((ty1, ty2), ty3, _) ->
    has_unresolved ty1 || has_unresolved ty2 || has_unresolved ty3
  | Trecord (_, fl) | Tsum (_, fl) ->
    List.exists (fun (_, ty) -> has_unresolved ty) fl
  | Tvar { contents = { contents = { tyo = Some t }} } -> has_unresolved t
  | Tvar _ -> true
  | Tpartial _ -> true
  | _ -> false

let vars_to_unit ?(warn=false) ?(err=false) ?loc ~keep_tvars ty =
  let vars_to_unit = vars_to_unit ~keep_tvars in
  if not (warn || err) then vars_to_unit ?loc ty
  else
    let warning = has_unresolved ty in
    let tvars = free_tvars ty in
    let str_ty = (string_of_type ty) in
    let ty' = vars_to_unit ?loc ty in
    let warning = if keep_tvars then has_unresolved ty' else warning in
    if warning then begin
      let loc = match loc with None -> noloc | Some loc -> loc in
      if err then error loc "Unresolved type %s, add annotation" str_ty
      else
        let msg =
          Printf.sprintf
            "Unresolved type %s. Type variables '%s were replaced by unit."
            str_ty (String.concat ", '" (StringSet.elements tvars)) in
        LiquidLoc.warn loc (WOther msg)
    end;
    ty'

let rec tvars_to_unit ?(err=false) ~keep_tvars ({ desc; ty; loc } as e) =
  let tvars_to_unit = tvars_to_unit ~keep_tvars in
  let vars_to_unit = vars_to_unit ~keep_tvars in
  let const_tvars_to_unit = const_tvars_to_unit ~keep_tvars in
  let contract_tvars_to_unit = contract_tvars_to_unit ~keep_tvars in
  let desc = match desc with
    | Var _ -> desc
    | Let { bnd_var; inline; bnd_val; body } ->
      Let { bnd_var; inline;
            bnd_val = tvars_to_unit bnd_val;
            body = tvars_to_unit body }
    | SetField { record; field; set_val } ->
      SetField { field;
                 record = tvars_to_unit record;
                 set_val = tvars_to_unit set_val }
    | Project { field; record } ->
      Project { field; record = tvars_to_unit record }
    | Const { ty; const } ->
      let const = const_tvars_to_unit ~loc const in
      (* if has_unresolved ty then
       *   error loc "Unresolved constant type %S, add annotation. Constant:\n  %s"
       *     (string_of_type ty) (string_of_const const); *)
      Const { ty = vars_to_unit ~err ~warn:true ~loc ty; const }
    | Apply { prim = Prim_extension (prim_name, effect, targs,
                                     nb_arg, nb_ret, minst); args } ->
      List.iter (fun ty ->
          if has_unresolved ty then
            error loc "Unresolved type parameter %S for %s, add annotation"
              (string_of_type ty) prim_name
        ) targs;
      let targs = List.map (vars_to_unit ~loc) targs in
      let args = List.map tvars_to_unit args in
      Apply { prim = Prim_extension (prim_name, effect, targs,
                                     nb_arg, nb_ret, minst); args }

    | Apply { prim; args } ->
      Apply { prim; args = List.map tvars_to_unit args }
    | If { cond; ifthen; ifelse } ->
      If { cond = tvars_to_unit cond;
           ifthen = tvars_to_unit ifthen;
           ifelse = tvars_to_unit ifelse }
    | Seq (e1, e2) ->
      Seq (tvars_to_unit e1, tvars_to_unit e2)
    | Transfer { dest; amount } ->
      Transfer { dest = tvars_to_unit dest;
                 amount = tvars_to_unit amount }
    | Call { contract; amount; entry; arg } ->
      let contract = match contract with
        | DSelf -> DSelf
        | DContract contract -> DContract (tvars_to_unit ~err:true contract) in
      let amount = match amount with
        | None -> None
        | Some amount -> Some (tvars_to_unit amount) in
      Call { contract; amount; entry;
             arg = tvars_to_unit ~err:true arg }
    | Self { entry } -> Self { entry }
    | SelfCall { amount; entry; arg } ->
      SelfCall { amount = tvars_to_unit amount;
                 entry;
                 arg = tvars_to_unit arg }
    | MatchOption { arg; ifnone; some_name; ifsome } ->
      MatchOption { arg = tvars_to_unit arg;
                    ifnone = tvars_to_unit ifnone;
                    some_name;
                    ifsome = tvars_to_unit ifsome }
    | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
      MatchList { arg = tvars_to_unit arg;
                  head_name; tail_name;
                  ifcons = tvars_to_unit ifcons;
                  ifnil = tvars_to_unit ifnil }
    | Loop { arg_name; body; arg } ->
      Loop { arg_name;
             body = tvars_to_unit body;
             arg = tvars_to_unit arg }
    | LoopLeft { arg_name; body; arg; acc } ->
      LoopLeft { arg_name;
                 body = tvars_to_unit body;
                 arg = tvars_to_unit arg;
                 acc = match acc with
                   | None -> None
                   | Some acc -> Some (tvars_to_unit acc) }
    | Fold { prim; arg_name; body; arg; acc } ->
      Fold { prim; arg_name;
             body = tvars_to_unit body;
             arg = tvars_to_unit arg;
             acc = tvars_to_unit acc }
    | Map { prim; arg_name; body; arg } ->
      Map { prim; arg_name;
            body = tvars_to_unit body;
            arg = tvars_to_unit arg }
    | MapFold { prim; arg_name; body; arg; acc } ->
      MapFold { prim; arg_name;
                body = tvars_to_unit body;
                arg = tvars_to_unit arg;
                acc = tvars_to_unit acc }
    | Lambda { arg_name; recursive; arg_ty; body; ret_ty } ->
      Lambda { arg_name; recursive;
               arg_ty = vars_to_unit ~err ~warn:true ~loc:arg_name.nloc arg_ty ;
               body = tvars_to_unit body;
               ret_ty = vars_to_unit ~err ~warn:true ~loc:body.loc ret_ty }
    | Closure { arg_name; arg_ty; call_env; body; ret_ty } ->
      Closure { arg_name;
                arg_ty = vars_to_unit ~err ~warn:true ~loc:arg_name.nloc arg_ty;
                call_env =
                  List.map (fun (v, e) -> (v, tvars_to_unit e)) call_env;
                body = tvars_to_unit body;
                ret_ty = vars_to_unit ~err ~warn:true ~loc:arg_name.nloc ret_ty }
    | Record l ->
      Record (List.map (fun (f, e) -> (f, tvars_to_unit e)) l)
    | Constructor { constr; arg } ->
      let constr = match constr with
        | Constr _ -> constr
        | Left ty -> Left (vars_to_unit ~loc ty)
        | Right ty -> Right (vars_to_unit ~loc ty) in
      Constructor { constr; arg = tvars_to_unit arg }
    | MatchVariant { arg; cases } ->
      MatchVariant { arg = tvars_to_unit arg;
                     cases =
                       List.map (fun (p, e) -> (p, tvars_to_unit e)) cases }
    | MatchNat { plus_name; minus_name; arg; ifplus; ifminus } ->
      MatchNat { plus_name; minus_name;
                 arg = tvars_to_unit arg;
                 ifplus = tvars_to_unit ifplus;
                 ifminus = tvars_to_unit ifminus }
    | Failwith e -> Failwith (tvars_to_unit e)
    | CreateContract { args; contract } ->
      CreateContract { args = List.map tvars_to_unit args;
                       contract = contract_tvars_to_unit contract }
    | ContractAt { arg; entry; entry_param } ->
      ContractAt { arg = tvars_to_unit ~err:true arg;
                   entry;
                   entry_param = vars_to_unit ~err:true ~loc entry_param }
    | Unpack { arg; ty } ->
      if has_unresolved ty then
        error loc "Unresolved unpack type %S, add annotation"
          (string_of_type ty) ;
      Unpack { arg = tvars_to_unit arg;
               ty = vars_to_unit ~warn:true ~loc:arg.loc ty }
    | TypeAnnot _ -> assert false (* Removed during typechecking *)
    | Type _ -> assert false (* Removed during typechecking*)
  in
  { e with desc; ty = vars_to_unit ~err ~loc ty }

and const_tvars_to_unit ~loc ~keep_tvars c =
  let const_tvars_to_unit = const_tvars_to_unit ~keep_tvars in
  let vars_to_unit = vars_to_unit ~keep_tvars in
  let tvars_to_unit = tvars_to_unit ~keep_tvars in
  match c with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CSignature _ | CNone  | CKey_hash _
    | CContract _) as c -> c
  | CSome x -> CSome (const_tvars_to_unit ~loc x)
  | CLeft x -> CLeft (const_tvars_to_unit ~loc x)
  | CRight x -> CRight (const_tvars_to_unit ~loc x)
  | CTuple xs -> CTuple (List.map (const_tvars_to_unit ~loc) xs)
  | CList xs -> CList (List.map (const_tvars_to_unit ~loc) xs)
  | CSet xs -> CSet (List.map (const_tvars_to_unit ~loc) xs)
  | CMap l ->
    CMap (List.map (fun (x,y) ->
        const_tvars_to_unit ~loc x, const_tvars_to_unit ~loc y) l)
  | CBigMap BMList l ->
    CBigMap (BMList (List.map (fun (x,y) ->
        const_tvars_to_unit ~loc x, const_tvars_to_unit ~loc y) l))
  | CBigMap BMId _ as c -> c
  | CRecord labels ->
    CRecord (List.map (fun (f, x) ->
        f, const_tvars_to_unit ~loc x) labels)
  | CConstr (constr, x) ->
    CConstr (constr, const_tvars_to_unit ~loc x)
  | CLambda { arg_name; arg_ty; body; ret_ty; recursive } ->
    CLambda { arg_name; recursive;
              arg_ty = vars_to_unit ~err:true ~loc:arg_name.nloc arg_ty ;
              body = tvars_to_unit body;
              ret_ty = vars_to_unit ~err:true ~loc:body.loc ret_ty }

and contract_tvars_to_unit ~keep_tvars (contract : typed_contract) =
  let contract_tvars_to_unit = contract_tvars_to_unit ~keep_tvars in
  let tvars_to_unit = tvars_to_unit ~keep_tvars in
  let vars_to_unit = vars_to_unit ~keep_tvars in
  let sig_vars_to_unit = sig_vars_to_unit ~keep_tvars in
  let subs = List.map contract_tvars_to_unit contract.subs in
  let values = List.map (fun v ->
      { v with val_exp = tvars_to_unit v.val_exp }
    ) contract.values in
  let c_init = match contract.c_init with
    | None -> None
    | Some { init_name; init_args; init_body } ->
      Some { init_name;
             init_args = List.map (fun (x, loc, ty) ->
                 x, loc, vars_to_unit ~loc ty) init_args;
             init_body = tvars_to_unit init_body } in
  let entries = List.map (fun { entry_sig; code; fee_code; view } ->
      { entry_sig = { entry_sig with
                      parameter =
                        vars_to_unit ~loc:(code : typed_exp).loc
                          entry_sig.parameter };
        code = tvars_to_unit code;
        view;
        fee_code = match fee_code with
          | None -> None
          | Some fee_code -> Some (tvars_to_unit fee_code);
      }) contract.entries in
  let rec env_tvars_to_unit ty_env = {
    ty_env with
    types = StringMap.map (fun (mk, id) ->
        (fun p -> vars_to_unit (mk p)), id
      ) ty_env.types;
    contract_types = StringMap.map sig_vars_to_unit ty_env.contract_types;
    top_env = match ty_env.top_env with
      | None -> None
      | Some env -> Some (env_tvars_to_unit env);
  } in
  let ty_env = env_tvars_to_unit contract.ty_env in
  { contract with values; c_init; entries; ty_env ; subs }

let rec mono_exp env subst vtys (e:typed_exp) =
  let mono_exp = mono_exp env in
  let mono_const = mono_const env in
  let instantiate ty = instantiate_to subst (get_type env e.loc ty) in
  (* Printf.printf "Exp %s : %s ->>" (string_of_code e) (string_of_type e.ty); *)
  let ty = instantiate e.ty in
  (* Printf.printf " %s\n" (string_of_type ty); *)
  let desc = match e.desc with
    (* TODO check with tests if this is needed *)

    | Let ({ bnd_val = { ty = Tlambda _ | Tclosure _ as bvty }} as lb)
      when not (StringSet.is_empty (free_tvars bvty)) ->
    (* Printf.printf "let %s = ... in E\n" lb.bnd_var.nname; *)
    let vtys' = QualMap.add env.path lb.bnd_var.nname (ref []) vtys in
    let body = mono_exp subst vtys' lb.body in
    let vty = QualMap.find env.path lb.bnd_var.nname vtys' in
    let substs = List.fold_left (fun ss (tn, ty) ->
        (tn, ty, build_subst lb.bnd_val.ty ty) :: ss) [] !vty in
    (* Printf.printf "Raw type of %s : %s\n" lb.bnd_var.nname
     *   (string_of_type lb.bnd_val.ty);
     * List.iter (fun (tn, ty, s) ->
     *   Printf.printf "%s\n%s\n" tn (string_of_type ty);
     *   StringMap.iter (fun id ty ->
     *     Printf.printf "%s -> %s  " id
     *       (string_of_type ty)
     *     ) s;
     *   Printf.printf "\n"
     * ) substs;
     * Printf.printf "   let %s = E in ...\n" lb.bnd_var.nname; *)
    if substs = [] then begin (* when the bound var is unused *)
      let bnd_val = mono_exp subst vtys lb.bnd_val in
      Let { lb with bnd_val; body }
    end else begin
      List.fold_left (fun body (tn, ty, s) ->
          let bnd_var = { lb.bnd_var with
                          nname = mk_typed_name lb.bnd_var.nname tn } in
          let bnd_val =
            mono_exp ((StringMap.bindings s) @ subst) vtys lb.bnd_val in
          { e with desc = Let { lb with bnd_var; bnd_val; body } }
        ) body substs
    end.desc
    | Var s ->
      begin try
          let vty = QualMap.find env.path s vtys in
          let tn =
            match List.find_opt (fun (tn, ty') -> eq_types ty ty') !vty with
            | None ->
              let tn = type_nb s ty in
              vty := (tn, ty) :: !vty;
              tn
            | Some (tn, _) -> tn in
          Var (mk_typed_name s tn)
        with Not_found -> Var s
      end
    | Lambda ({ recursive = Some f } as l) ->
      let recursive =
        if QualMap.mem env.path f vtys then
          Some (mk_typed_name f (type_nb f ty))
        else Some f in
      let arg_ty = instantiate l.arg_ty in
      let ret_ty = instantiate l.ret_ty in
      let body = mono_exp subst vtys l.body in
      Lambda { arg_name = l.arg_name; arg_ty; body; ret_ty; recursive }
    | Let lb ->
      let bnd_val = mono_exp subst vtys lb.bnd_val in
      let body = mono_exp subst vtys lb.body in
      Let { bnd_var = lb.bnd_var; inline = lb.inline; bnd_val; body }
    | SetField sf -> SetField { field = sf.field;
                                record = mono_exp subst vtys sf.record;
                                set_val = mono_exp subst vtys sf.set_val }
    | Project p -> Project { field = p.field;
                             record = mono_exp subst vtys p.record }
    | Const c ->
      let const = mono_const subst vtys c.const in
      Const { const; ty = instantiate c.ty }
    | Apply { prim = Prim_extension (prim_name, effect, targs,
                                     nb_arg, nb_ret, minst); args } ->
      let targs = List.map instantiate targs in
      let args = List.map (mono_exp subst vtys) args in
      Apply { prim = Prim_extension (prim_name, effect, targs,
                                     nb_arg, nb_ret, minst); args }
    | Apply app ->
      Apply { prim = app.prim;
              args = List.map (mono_exp subst vtys) app.args }
    | If ite -> If { cond = mono_exp subst vtys ite.cond;
                     ifthen = mono_exp subst vtys ite.ifthen;
                     ifelse = mono_exp subst vtys ite.ifelse }
    | Seq (e1, e2) -> Seq (mono_exp subst vtys e1, mono_exp subst vtys e2)
    | Transfer tr -> Transfer { dest = mono_exp subst vtys tr.dest;
                                amount = mono_exp subst vtys tr.amount }
    | Call c ->
      let contract = match c.contract with
        | DSelf -> DSelf
        | DContract c -> DContract (mono_exp subst vtys c) in
      let amount = match c.amount with
        | None -> None
        | Some a -> Some (mono_exp subst vtys a) in
      Call { entry = c.entry;
             contract; amount;
             arg = mono_exp subst vtys c.arg }
    | Self { entry } -> Self { entry }
    | SelfCall c -> SelfCall { entry = c.entry;
                               amount = mono_exp subst vtys c.amount;
                               arg = mono_exp subst vtys c.arg }
    | MatchOption mo -> MatchOption { some_name = mo.some_name;
                                      arg = mono_exp subst vtys mo.arg;
                                      ifnone = mono_exp subst vtys mo.ifnone;
                                      ifsome = mono_exp subst vtys mo.ifsome }
    | MatchList ml -> MatchList { head_name = ml.head_name;
                                  tail_name = ml.tail_name;
                                  arg = mono_exp subst vtys ml.arg;
                                  ifcons = mono_exp subst vtys ml.ifcons;
                                  ifnil = mono_exp subst vtys ml.ifnil }
    | Loop l -> Loop { arg_name = l.arg_name;
                       body = mono_exp subst vtys l.body;
                       arg = mono_exp subst vtys l.arg  }
    | LoopLeft ll -> LoopLeft { arg_name = ll.arg_name;
                                body = mono_exp subst vtys ll.body;
                                arg = mono_exp subst vtys ll.arg;
                                acc = match ll.acc with
                                  | Some e -> Some (mono_exp subst vtys e)
                                  | _ -> ll.acc }
    | Fold f -> Fold { prim = f.prim; arg_name = f.arg_name;
                       body = mono_exp subst vtys f.body;
                       arg = mono_exp subst vtys f.arg;
                       acc = mono_exp subst vtys f.acc }
    | Map m -> Map { prim = m.prim; arg_name = m.arg_name;
                     body = mono_exp subst vtys m.body ;
                     arg = mono_exp subst vtys m.arg }
    | MapFold mf -> MapFold { mf with body = mono_exp subst vtys mf.body;
                                      arg = mono_exp subst vtys mf.arg;
                                      acc = mono_exp subst vtys mf.acc }
    | Lambda l -> Lambda { recursive = l.recursive;
                           arg_name = l.arg_name;
                           arg_ty = instantiate l.arg_ty ;
                           body = mono_exp subst vtys l.body;
                           ret_ty = instantiate l.ret_ty }
    | Closure c -> Closure { arg_name = c.arg_name;
                             arg_ty = instantiate c.arg_ty ;
                             call_env = List.map (fun (s, e) ->
                                 s, mono_exp subst vtys e) c.call_env ;
                             body = mono_exp subst vtys c.body;
                             ret_ty = instantiate c.ret_ty }
    | Record r -> Record (List.map (fun (s, e) -> s, mono_exp subst vtys e) r)
    | Constructor c ->
      Constructor {
        constr = begin match c.constr with
          | Left ty -> Left (instantiate ty)
          | Right ty -> Right (instantiate ty)
          | _ -> c.constr end;
        arg = mono_exp subst vtys c.arg }
    | MatchVariant mv -> MatchVariant { arg = mono_exp subst vtys mv.arg;
                                        cases = List.map (fun (p, e) ->
                                            p, mono_exp subst vtys e) mv.cases }
    | MatchNat mn -> MatchNat { mn with arg = mono_exp subst vtys mn.arg;
                                        ifplus = mono_exp subst vtys mn.ifplus;
                                        ifminus = mono_exp subst vtys mn.ifminus }
    | Failwith e -> Failwith (mono_exp subst vtys e)
    | CreateContract cc ->
      CreateContract { args = List.map (mono_exp subst vtys) cc.args;
                       contract = fst @@ mono_contract vtys cc.contract }
    | ContractAt ca ->
      ContractAt { arg = mono_exp subst vtys ca.arg;
                   entry = ca.entry;
                   entry_param = instantiate ca.entry_param }
    | Unpack up ->
      Unpack { arg = mono_exp subst vtys up.arg;
               ty = instantiate up.ty }
    | TypeAnnot _ -> assert false (* Removed during typechecking *)
    | Type _ -> assert false (* Removed during typechecking*)
  in
  { e with desc; ty }

and mono_const env subst vtys (c : typed_const) = match c with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CSignature _ | CNone  | CKey_hash _
    | CContract _) as c -> c
  | CSome x -> CSome (mono_const env subst vtys x)
  | CLeft x -> CLeft (mono_const env subst vtys x)
  | CRight x -> CRight (mono_const env subst vtys x)
  | CTuple xs -> CTuple (List.map (mono_const env subst vtys) xs)
  | CList xs -> CList (List.map (mono_const env subst vtys) xs)
  | CSet xs -> CSet (List.map (mono_const env subst vtys) xs)
  | CMap l ->
    CMap (List.map (fun (x,y) ->
        mono_const env subst vtys x, mono_const env subst vtys y) l)
  | CBigMap BMList l ->
    CBigMap (BMList (List.map (fun (x,y) ->
        mono_const env subst vtys x, mono_const env subst vtys y) l))
  | CBigMap BMId _ as c -> c
  | CRecord labels ->
    CRecord (List.map (fun (f, x) -> f, mono_const env subst vtys x) labels)
  | CConstr (constr, x) ->
    CConstr (constr, mono_const env subst vtys x)
  | CLambda { arg_name; arg_ty; body; ret_ty; recursive } ->
    let instantiate ty = instantiate_to subst (get_type env body.loc ty) in
    let arg_ty = instantiate arg_ty in
    let ret_ty = instantiate ret_ty in
    let body = mono_exp env subst vtys body in
    CLambda { arg_name; arg_ty; body; ret_ty; recursive }


and register_global_vtys vtys c =
  let env = c.ty_env in
  let vtys =
    List.fold_left (fun vtys v ->
        if StringSet.is_empty (free_tvars v.val_exp.ty) then vtys
        else QualMap.add env.path v.val_name (ref []) vtys
      ) vtys c.values in
  List.fold_left register_global_vtys vtys c.subs

and mono_contract vtys c =
  let env = c.ty_env in
  let entries = List.map (fun e ->
      let code = mono_exp env [] vtys e.code in
      let fee_code = match e.fee_code with
        | None -> None
        | Some fee_code -> Some (mono_exp env [] vtys fee_code) in
      let pty = get_type env code.loc e.entry_sig.parameter in
      if has_unresolved pty then
        error e.code.loc
          "Parameter type for entry %s can't be inferred (%s), \
           add an annotation"
          e.entry_sig.entry_name
          (string_of_type pty);
      { code;
        fee_code;
        view = e.view;
        entry_sig = { e.entry_sig with parameter = pty } }
    ) c.entries in
  let c_init = match c.c_init with
    | Some init ->
      let init_body = mono_exp env [] vtys init.init_body in
      let init_args = List.map (fun (arg, loc, arg_ty) ->
          (arg, loc, get_type env loc arg_ty)) init.init_args in
      Some { init_name = init.init_name;
             init_body;
             init_args }
    | None -> None
  in
  let loc = LiquidLoc.loc_in_file env.filename in
  let storage = get_type env loc c.storage in
  if not @@ StringSet.is_empty @@ free_tvars storage then
    error loc
      "Storage type for contract %s cannot be inferred (%s), \
       add an annotation or specialize type"
      c.contract_name
      (string_of_type storage);
  let subs, vtys = List.fold_left (fun (subs, vtys) c ->
      let c, vtys = mono_contract vtys c in
      c :: subs, vtys
    ) ([], vtys) (List.rev c.subs) in

  let contract = { c with storage; entries; c_init; subs } in
  (* delay monomorphisation of global values *)
  contract, vtys


and mono_values top vtys c =
  let env = c.ty_env in
  let values = List.fold_left (fun cval v ->
      let vty =
        try QualMap.find env.path v.val_name vtys
        with Not_found -> ref [] in
      let substs = List.fold_left (fun ss (tn, ty) ->
          (tn, ty, build_subst v.val_exp.ty ty) :: ss) [] !vty in
      if substs = [] then begin
        { v with val_exp = mono_exp env [] vtys v.val_exp } :: cval
      end else begin
        List.fold_left (fun cval (tn, ty, s) ->
            { v with
              val_name = mk_typed_name v.val_name tn;
              val_exp = mono_exp env (StringMap.bindings s) vtys v.val_exp;
            } :: cval
          ) cval substs
      end
    ) [] (List.rev c.values) in
  let subs = List.rev_map (mono_values top vtys) (List.rev c.subs) in
  { c with values; subs }


let mono_contract ?(keep_tvars=false) c =
  let vtys = register_global_vtys QualMap.empty c in
  let c, vtys = mono_contract vtys c in
  let c = mono_values c vtys c in
  contract_tvars_to_unit ~keep_tvars c

let copy_ty ty =
  let refs = Hashtbl.create 17 in
  let rec copy_ty ty = match ty with
    |Tunit|Tbool|Tint|Tnat|Ttez|
     Tstring|Tbytes|Ttimestamp|Tkey|Tkey_hash|Tsignature|Toperation|Taddress|
     Tfail | Tchainid -> ty
    | Ttuple tyl -> Ttuple (List.map copy_ty tyl)
    | Toption ty -> Toption (copy_ty ty)
    | Tlist ty -> Tlist (copy_ty ty)
    | Tset ty -> Tset (copy_ty ty)
    | Tmap (ty1, ty2) -> Tmap (copy_ty ty1, copy_ty ty2)
    | Tbigmap (ty1, ty2) -> Tbigmap (copy_ty ty1, copy_ty ty2)
    | Tor (ty1, ty2) -> Tor (copy_ty ty1, copy_ty ty2)
    | Tlambda (ty1, ty2, u) ->
      Tlambda (copy_ty ty1, copy_ty ty2, ref !u)
    | Tclosure ((ty1, ty2), ty3, u) ->
      Tclosure ((copy_ty ty1, copy_ty ty2), copy_ty ty3, ref !u)
    | Trecord (rn, fl) ->
      Trecord (rn, List.map (fun (fn, fty) -> (fn, copy_ty fty)) fl)
    | Tsum (sn, cl) ->
      Tsum (sn, List.map (fun (cn, cty) -> (cn, copy_ty cty)) cl)
    | Tcontract_handle (e, ty) -> Tcontract_handle (e, copy_ty ty)
    | Tcontract_view (v, t1, t2) -> Tcontract_view (v, copy_ty t1, copy_ty t2)
    | Tvar tvr ->
      let tv = Ref.get tvr in
      let tvr =
        try Hashtbl.find refs tv.id
        with Not_found ->
          let tyo = match tv.tyo with
            | None -> None
            | Some tyo -> Some (copy_ty tyo) in
          let tvr = Ref.create { tv with tyo } in
          Hashtbl.add refs tv.id tvr;
          tvr in
      Tvar tvr
    | Tpartial (Peqn (el, loc)) ->
      let el = List.map (fun (cl, rty) ->
          let rty = copy_ty rty in
          let cl = List.map (fun (x, y) -> copy_ty x, copy_ty y) cl in
          (cl, rty)) el in
      let el = List.filter (fun (cl, _) ->
          List.for_all (fun (x, y) -> compat_types x y) cl
        ) el in
      Tpartial (Peqn (el, loc))
    | Tpartial Ppar -> ty
    | Tpartial Ptup l ->
      Tpartial (Ptup (List.map (fun (s, t) -> s, copy_ty t) l))
    | Tpartial Pmap (t1, t2) ->
      Tpartial (Pmap (copy_ty t1, copy_ty t2))
    | Tpartial Pcont None -> ty
    | Tpartial Pcont Some (e, ty) ->
      Tpartial (Pcont (Some (e, copy_ty ty))) in
  copy_ty ty

let instantiate_to subst ty =
  let ty = if subst = [] then ty else copy_ty ty in
  instantiate_to subst ty

let instantiate (tvars, ty) =
  let s = StringSet.fold (fun id s ->
      StringMap.add id (fresh_tvar ()) s
    ) tvars StringMap.empty in
  instantiate_to (StringMap.bindings s) ty
