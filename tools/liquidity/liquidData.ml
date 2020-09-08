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

(* TODO: We could use a simplify pass to propagate the no-effect
   defining expression of once-used variables to their uniq use
   site. It could dramatically decrease the size of the stack.  *)


open LiquidTypes


let default_key_hash () =
  match !LiquidOptions.network with
  | LiquidOptions.Dune_network -> "dn1UqnHgHFe8ezEgsoow4hERctPssuWiw9h8"
  | LiquidOptions.Tezos_network -> "tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc"

let rec default_const = function
  | Tunit -> CUnit
  | Tbool -> CBool false
  | Tint -> CInt (LiquidNumber.integer_of_int 0)
  | Tnat -> CNat (LiquidNumber.integer_of_int 0)
  | Ttez -> CTez (LiquidNumber.tez_of_liq "0")
  | Tstring -> CString ""
  | Tbytes -> CBytes "0x"
  | Ttimestamp -> CTimestamp "1970-01-01T00:00:00Z"
  | Tkey -> CKey "edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN"
  | Tkey_hash -> CKey_hash (default_key_hash ())
  | Tsignature ->
    CSignature
      "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk\
       68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7"
  | Taddress | Tcontract_handle ((None | Some "default"), _) | Tcontract_view _ ->
    CContract ("KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi", None)
  | Tcontract_handle (e, _) ->
    CContract ("KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi", e)
  | Tchainid -> CString "NetXwhYbWGa82xo"
  | Ttuple l ->
    CTuple (List.map default_const l)
  | Toption ty -> CSome (default_const ty)
  | Tlist ty -> CList [default_const ty]
  | Tset ty -> CSet [default_const ty]
  | Tmap (ty1, ty2) ->
    CMap [default_const ty1, default_const ty2]
  | Tbigmap (ty1, ty2) ->
    CBigMap (BMList [default_const ty1, default_const ty2])
  | Tor (ty, _) -> CLeft (default_const ty)
  | Trecord (_, fields) ->
    CRecord (
      List.map (fun (name, ty) ->
          name, default_const ty) fields
    )
  | Tsum (_, (c, ty) :: _) ->
    CConstr (c, default_const ty)

  | Tlambda (arg_ty, ret_ty, _) as ty ->
    CLambda { arg_ty; ret_ty; recursive = None;
              arg_name = { nname = "_"; nloc = noloc };
              body = mk ~loc:noloc
                  (Const { ty = ret_ty; const = default_const ret_ty }) ty }

  | Tvar tv ->
    (match (Ref.get tv).tyo with
     | None -> raise Not_found
     | Some ty -> default_const ty)

  | Tsum (_, [])
  | Tfail
  | Tclosure _
  | Toperation -> raise Not_found
  | Tpartial _ -> raise Not_found

let rec default_empty_const = function
  | Tunit -> CUnit
  | Tbool -> CBool false
  | Tint -> CInt (LiquidNumber.integer_of_int 0)
  | Tnat -> CNat (LiquidNumber.integer_of_int 0)
  | Ttez -> CTez (LiquidNumber.tez_of_liq "0")
  | Tstring -> CString ""
  | Tbytes -> CBytes "0x"
  | Ttimestamp -> CTimestamp "1970-01-01T00:00:00Z"
  | Tkey -> CKey "edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN"
  | Tkey_hash -> CKey_hash (default_key_hash ())
  | Tsignature ->
    CSignature
      "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk\
       68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7"
  | Taddress | Tcontract_handle ((None | Some "default"), _) | Tcontract_view _ ->
    CContract ("KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi", None)
  | Tcontract_handle (e, _) ->
    CContract ("KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi", e)
  | Tchainid -> CString "NetXwhYbWGa82xo"
  | Ttuple l ->
    CTuple (List.map default_empty_const l)
  | Toption ty -> CNone
  | Tlist ty -> CList []
  | Tset ty -> CSet []
  | Tmap (ty1, ty2) -> CMap []
  | Tbigmap  (ty1, ty2) -> CBigMap (BMList [])
  | Tor (ty, _) -> CLeft (default_empty_const ty)
  | Trecord (_, fields) ->
    CRecord (
      List.map (fun (name, ty) ->
          name, default_empty_const ty) fields
    )
  | Tsum (_, (c, ty) :: _) ->
    CConstr (c, default_empty_const ty)


  | Tlambda (arg_ty, ret_ty, _) as ty ->
    CLambda { arg_ty; ret_ty; recursive = None;
              arg_name = { nname = "_"; nloc = noloc };
              body = mk ~loc:noloc
                  (Failwith
                     (mk ~loc:noloc (Const { ty = ret_ty; const = CUnit })
                        Tunit
                     )) ty }

  | Tvar tv ->
    (match (Ref.get tv).tyo with
     | None -> raise Not_found
     | Some ty -> default_empty_const ty)

  | Tsum (_, [])
  | Tfail
  | Tclosure _
  | Toperation -> raise Not_found
  | Tpartial _ -> raise Not_found

let rec default_empty_untyped_const = function
  | Tunit -> CUnit
  | Tbool -> CBool false
  | Tint -> CInt (LiquidNumber.integer_of_int 0)
  | Tnat -> CNat (LiquidNumber.integer_of_int 0)
  | Ttez -> CTez (LiquidNumber.tez_of_liq "0")
  | Tstring -> CString ""
  | Tbytes -> CBytes "0x"
  | Ttimestamp -> CTimestamp "1970-01-01T00:00:00Z"
  | Tkey -> CKey "edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN"
  | Tkey_hash -> CKey_hash (default_key_hash ())
  | Tsignature ->
    CSignature
      "edsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk\
       68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7"
  | Taddress | Tcontract_handle ((None | Some "default"), _) | Tcontract_view _ ->
    CContract ("KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi", None)
  | Tcontract_handle (e, _) ->
    CContract ("KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi", e)
  | Tchainid -> CString "NetXwhYbWGa82xo"
  | Ttuple l ->
    CTuple (List.map default_empty_untyped_const l)
  | Toption ty -> CNone
  | Tlist ty -> CList []
  | Tset ty -> CSet []
  | Tmap (ty1, ty2) -> CMap []
  | Tbigmap  (ty1, ty2) -> CBigMap (BMList [])
  | Tor (ty, _) -> CLeft (default_empty_untyped_const ty)
  | Trecord (_, fields) ->
    CRecord (
      List.map (fun (name, ty) ->
          name, default_empty_untyped_const ty) fields
    )
  | Tsum (_, (c, ty) :: _) ->
    CConstr (c, default_empty_untyped_const ty)


  | Tlambda (arg_ty, ret_ty, _) ->
    CLambda { arg_ty; ret_ty; recursive = None;
              arg_name = { nname = "_"; nloc = noloc };
              body = mk ~loc:noloc
                  (Failwith
                     (mk ~loc:noloc (Const { ty = ret_ty; const = CUnit })
                        ()
                     )) () }

  | Tvar tv ->
    (match (Ref.get tv).tyo with
     | None -> raise Not_found
     | Some ty -> default_empty_untyped_const ty)

  | Tsum (_, [])
  | Tfail
  | Tclosure _
  | Toperation -> raise Not_found
  | Tpartial _ -> raise Not_found

let rec translate_const_exp (exp : ('a, 'b) exp) =
  let loc = exp.loc in
  match exp.desc with
  | Let _ ->
    LiquidLoc.raise_error ~loc "'let' forbidden in constant"
  | Const { const } -> const

  | Record fields ->
    CRecord (List.map (fun (f, e) -> (f, translate_const_exp e)) fields)

  | Constructor { constr = Constr c; arg } ->
    CConstr (c, translate_const_exp arg)
  | Constructor { constr = Left _; arg } -> CLeft (translate_const_exp arg)
  | Constructor { constr = Right _; arg } -> CRight (translate_const_exp arg)

  | Apply { prim = Prim_Left; args = [x] } -> CLeft (translate_const_exp x)
  | Apply { prim = Prim_Right; args = [x] } -> CRight (translate_const_exp  x)
  | Apply { prim = Prim_Some; args = [x] } -> CSome (translate_const_exp x)
  | Apply { prim = Prim_Cons; args } ->
    CList (List.map translate_const_exp args)
  | Apply { prim = Prim_tuple; args } ->
    CTuple (List.map translate_const_exp args)
  | Apply { prim = Prim_big_map_create } ->
    CBigMap (BMList [])

  | TypeAnnot { e } -> translate_const_exp e

  | Lambda lam -> CLambda lam

  | Apply _
  | Var _
  | SetField _
  | Project _
  | If _
  | Seq _
  | Transfer _
  | Call _
  | Self _
  | SelfCall _
  | MatchOption _
  | MatchNat _
  | MatchList _
  | Loop _
  | LoopLeft _
  | Fold _
  | Map _
  | MapFold _
  | Closure _
  | MatchVariant _
  | Failwith _
  | CreateContract _
  | HandleAt _
  | Unpack _
  | Type _
    ->
    LiquidLoc.raise_error ~loc "non-constant expression: %s"
      (LiquidPrinter.LiquidDebug.string_of_code exp)


let translate env contract_sig s ty =
  let ml_exp =
    LiquidFromParsetree.expression_of_string ~filename:env.filename s in
  (* hackish: add type annotation for constants *)
  let ml_ty = LiquidToParsetree.convert_type ~abbrev:false ty in
  let ml_exp = Ast_helper.Exp.constraint_
      ~loc:(Location.in_file env.filename) ml_exp ml_ty in
  let sy_exp = LiquidFromParsetree.translate_expression env ml_exp in
  let tenv = empty_typecheck_env ~warnings:true contract_sig env in
  sy_exp
  |> LiquidCheck.typecheck_code tenv ~expected_ty:ty
  |> translate_const_exp
  |> LiquidEncode.encode_const env contract_sig
