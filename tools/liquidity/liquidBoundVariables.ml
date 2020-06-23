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


let rec bv code =
  match code.desc with
  | If { cond; ifthen; ifelse} ->
    StringSet.union (bv cond)
      (StringSet.union (bv ifthen) (bv ifelse))
  | Seq (x, y) -> StringSet.union (bv x) (bv y)
  | Const { ty; const } ->  StringSet.empty

  | Failwith arg -> bv arg

  | Apply { prim; args } ->
    List.fold_left (fun set arg ->
        StringSet.union set (bv arg)
      ) StringSet.empty args

  | Let { bnd_var; inline; bnd_val; body } ->
    StringSet.union (bv bnd_val)
      (StringSet.remove bnd_var.nname (bv body))

  | Lambda lam -> bv_lambda lam

  | Closure { arg_name; arg_ty; call_env; body; ret_ty } ->
    bv body
    |> StringSet.remove arg_name.nname
    |> List.fold_right (fun (_, e) -> StringSet.union (bv e)) call_env

  | Var name -> StringSet.add name StringSet.empty

  | SetField { record; field; set_val } ->
    StringSet.union (bv record) (bv set_val)

  | Project { field; record } -> (bv record)

  | MatchOption { arg; ifnone; some_name; ifsome } ->
    StringSet.union (bv arg)
      (StringSet.union (bv ifnone)
         (StringSet.remove some_name.nname (bv ifsome)))

  | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
    StringSet.union (bv arg)
      (StringSet.union
         (StringSet.remove minus_name.nname (bv ifminus))
         (StringSet.remove plus_name.nname (bv ifplus)))

  | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
    StringSet.union
      (bv arg)
      (StringSet.union (bv ifnil)
         (StringSet.remove head_name.nname
            (StringSet.remove tail_name.nname
               (bv ifcons))))

  | Self _ -> StringSet.empty

  | Transfer { dest; amount } -> StringSet.union (bv dest) (bv amount)

  | SelfCall { amount; arg } -> StringSet.union (bv arg) (bv amount)

  | Call { contract; amount; entry; arg } ->
    List.fold_left (fun set exp ->
        StringSet.union set (bv exp)
      ) StringSet.empty [contract; amount; arg]

  | Loop { arg_name; body; arg }
  | LoopLeft { arg_name; body; arg; acc = None }
  | Map { arg_name; body; arg } ->
    StringSet.union (bv arg)
      (StringSet.remove arg_name.nname (bv body))

  | MapFold { arg_name; body; arg; acc }
  | Fold { arg_name; body; arg; acc }
  | LoopLeft { arg_name; body; arg; acc = Some acc } ->
    StringSet.union (bv acc)
      (StringSet.union (bv arg)
         (StringSet.remove arg_name.nname (bv body)))

  | Record fields ->
    List.fold_left (fun set (_,exp) ->
        StringSet.union set (bv exp)
      ) StringSet.empty fields

  | Constructor { arg } -> bv arg

  | MatchVariant { arg; cases } ->
    StringSet.union (bv arg)
      (List.fold_left (fun set (pat, exp) ->
           let bv_exp = bv exp in
           let bv_case = match pat with
             | PConstr (_constr, var_args) ->
               List.fold_left (fun set var_arg ->
                   StringSet.remove var_arg set
                 ) bv_exp var_args
             | PAny -> bv_exp
           in
           StringSet.union set bv_case
         ) StringSet.empty cases)

  | CreateContract { args; contract } ->
    let bc = bv_contract contract in
    List.fold_left (fun set arg ->
        StringSet.union set (bv arg)
      ) bc args

  | ContractAt { arg }
  | Unpack { arg } -> bv arg

  | TypeAnnot { e } -> bv e

  | Type _ -> StringSet.empty

and bv_const const =
  match const with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CSignature _ | CNone  | CKey_hash _
    | CContract _ ) -> StringSet.empty
  | CSome x | CLeft x | CRight x | CConstr (_, x) -> bv_const x
  | CTuple xs | CList xs | CSet xs ->
    List.fold_left
      (fun acc x -> StringSet.union acc (bv_const x)) StringSet.empty xs
  | CMap l | CBigMap BMList l ->
    List.fold_left
      (fun acc (x, y) -> StringSet.union acc
          (StringSet.union (bv_const x) (bv_const y))) StringSet.empty l
  | CBigMap BMId _ -> StringSet.empty
  | CRecord labels ->
    List.fold_left
      (fun acc (_, x) -> StringSet.union acc (bv_const x))
      StringSet.empty labels
  | CLambda lam -> bv_lambda lam

and bv_lambda { arg_name; arg_ty; body; ret_ty; recursive } =
  bv body
  |> StringSet.remove arg_name.nname
  |> fun bv -> match recursive with
  | None -> bv
  | Some f -> StringSet.remove f bv

and bv_entry acc e =
  bv e.code
  |> StringSet.remove e.entry_sig.parameter_name
  |> StringSet.remove e.entry_sig.storage_name
  |> StringSet.union acc

and bv_contract contract =
    let be = List.fold_left bv_entry StringSet.empty contract.entries in
    List.fold_right (fun v acc ->
        StringSet.union (bv v.val_exp)
          (StringSet.remove v.val_name acc)
      ) contract.values be

let mk desc exp bv = { exp with desc; bv }

let rec bound code =
  match code.desc with

  | If { cond; ifthen; ifelse } ->
    let cond = bound cond in
    let ifthen = bound ifthen in
    let ifelse = bound ifelse in
    let bv = StringSet.union cond.bv
        (StringSet.union ifthen.bv ifelse.bv) in
    let desc = If { cond; ifthen; ifelse } in
    mk desc code bv

  | Seq (x, y) ->
    let x = bound x in
    let y = bound y in
    let bv = StringSet.union x.bv y.bv in
    let desc = Seq(x,y) in
    mk desc code bv

  | Const { ty; const } ->
    let bv = bv_const const in
    let const = bound_const const in
    let desc = Const { ty; const } in
    mk desc code bv

  | Failwith arg ->
    let arg = bound arg in
    let bv = arg.bv in
    let desc = Failwith arg in
    mk desc code bv

  | Apply { prim; args } ->
    let args = List.map bound args in
    let bv =
      List.fold_left (fun set arg ->
          StringSet.union set arg.bv
        ) StringSet.empty args
    in
    let desc = Apply { prim; args } in
    mk desc code bv

  | Let { bnd_var; inline; bnd_val; body } ->
    let bnd_val = bound bnd_val in
    let body = bound body in
    let bv = StringSet.union bnd_val.bv
        (StringSet.remove bnd_var.nname body.bv) in
    let desc = Let { bnd_var; inline; bnd_val; body } in
    mk desc code bv

  | Lambda lam ->
    let lam, bv = bound_lambda lam in
    let desc = Lambda lam in
    mk desc code bv

  | Closure { arg_name; arg_ty; call_env; body; ret_ty } ->
    let call_env = List.map (fun (name, t) -> name, bound t) call_env in
    let body = bound body in
    let bv =
      body.bv
      |> StringSet.remove arg_name.nname
      |> List.fold_right (fun (_, e) -> StringSet.union e.bv) call_env
    in
    let desc = Closure { arg_name; arg_ty; call_env; body; ret_ty } in
    mk desc code bv

  | Var name ->
    let bv = StringSet.add name StringSet.empty in
    let desc = Var name in
    mk desc code bv

  | SetField { record; field; set_val } ->
    let record = bound record in
    let set_val = bound set_val in
    let bv = StringSet.union record.bv set_val.bv in
    let desc = SetField { record; field; set_val } in
    mk desc code bv

  | Project { field; record } ->
    let record = bound record in
    let desc = Project { field; record } in
    mk desc code record.bv

  | MatchOption { arg; ifnone; some_name; ifsome } ->
    let arg = bound arg in
    let ifnone = bound ifnone in
    let ifsome = bound ifsome in
    let bv =
      StringSet.union arg.bv
        (StringSet.union ifnone.bv
           (StringSet.remove some_name.nname ifsome.bv))
    in
    let desc = MatchOption { arg; ifnone; some_name; ifsome } in
    mk desc code bv


  | MatchNat { arg; plus_name; ifplus; minus_name; ifminus } ->
    let arg = bound arg in
    let ifplus = bound ifplus in
    let ifminus = bound ifminus in
    let bv =
      StringSet.union arg.bv
        (StringSet.union
           (StringSet.remove minus_name.nname ifminus.bv)
           (StringSet.remove plus_name.nname ifplus.bv))
    in
    let desc = MatchNat { arg; plus_name; ifplus; minus_name; ifminus } in
    mk desc code bv

  | MatchList { arg; head_name; tail_name; ifcons; ifnil } ->
    let arg = bound arg in
    let ifnil = bound ifnil in
    let ifcons = bound ifcons in
    let bv =
      StringSet.union
        arg.bv
        (StringSet.union ifnil.bv
           (StringSet.remove head_name.nname
              (StringSet.remove tail_name.nname
                 ifcons.bv)))
    in
    let desc = MatchList { arg; head_name; tail_name; ifcons; ifnil } in
    mk desc code bv

  | Self { entry } ->
    let desc = Self { entry } in
    mk desc code StringSet.empty

  | Transfer { dest; amount } ->
    let dest = bound dest in
    let amount = bound amount in
    let bv = StringSet.union dest.bv amount.bv in
    let desc = Transfer { dest; amount } in
    mk desc code bv

  | SelfCall { amount; entry; arg } ->
    let amount = bound amount in
    let arg = bound arg in
    let bv = StringSet.union arg.bv amount.bv in
    let desc = SelfCall { amount; entry; arg } in
    mk desc code bv

  | Call { contract; amount; entry; arg } ->
    let contract = bound contract in
    let amount = bound amount in
    let arg = bound arg in
    let bv =
      List.fold_left (fun set exp ->
          StringSet.union set (exp.bv)
        ) StringSet.empty [contract; amount; arg]
    in
    let desc = Call {contract; amount; entry; arg } in
    mk desc code bv

  | Loop { arg_name; body; arg }
  | LoopLeft { arg_name; body; arg; acc = None } ->
    let arg = bound arg in
    let body = bound body in
    let bv = StringSet.union arg.bv (StringSet.remove arg_name.nname body.bv)
    in
    let desc = match code.desc with
      | Loop _ -> Loop { arg_name; body; arg }
      | LoopLeft _ -> LoopLeft { arg_name; body; arg; acc = None }
      | _ -> assert false in
    mk desc code bv

  | Fold { arg_name; body; arg; acc }
  | MapFold { arg_name; body; arg; acc }
  | LoopLeft { arg_name; body; arg; acc = Some acc } ->
    let acc = bound acc in
    let arg = bound arg in
    let body = bound body in
    let bv =
      StringSet.union acc.bv
        (StringSet.union arg.bv
           (StringSet.remove arg_name.nname body.bv))
    in
    let desc = match code.desc with
      | Fold { prim } ->
        Fold { prim; arg_name; body; arg; acc }
      | MapFold { prim } ->
        MapFold  { prim; arg_name; body; arg; acc }
      | LoopLeft _ ->
        LoopLeft { arg_name; body; arg; acc = Some acc }
      | _ -> assert false
    in
    mk desc code bv

  | Map { prim; arg_name; body; arg } ->
    let arg = bound arg in
    let body = bound body in
    let bv = StringSet.union arg.bv (StringSet.remove arg_name.nname body.bv)
    in
    let desc = Map  { prim; arg_name; body; arg } in
    mk desc code bv

  | Record fields ->
    let fields = List.map (fun (l, exp) -> (l,bound exp)) fields in
    let bv =
      List.fold_left (fun set (_,exp) ->
          StringSet.union set (exp.bv)
        ) StringSet.empty fields
    in
    let desc = Record fields in
    mk desc code bv

  | Constructor { constr; arg } ->
    let arg = bound arg in
    let desc = Constructor { constr; arg } in
    mk desc code arg.bv

  | MatchVariant { arg; cases } ->
    let arg = bound arg in
    let cases =
      List.map (fun (pat, exp) ->
          (pat, bound exp)
        ) cases in
    let bv = List.fold_left (fun set (pat, exp) ->
        let bv_exp = bv exp in
        let bv_case = match pat with
          | PConstr (_constr, var_args) ->
            List.fold_left (fun set var_arg ->
                StringSet.remove var_arg set
              ) bv_exp var_args
          | PAny -> bv_exp
        in
        StringSet.union set bv_case
      ) StringSet.empty cases
    in
    let bv = StringSet.union arg.bv bv in
    let desc = MatchVariant { arg; cases } in
    mk desc code bv

  | CreateContract { args; contract } ->
    let args = List.map bound args in
    let contract, bv = bound_contract contract in
    let bv =
      List.fold_left (fun set arg ->
          StringSet.union set arg.bv
        ) bv args
    in
    let desc = CreateContract { args; contract } in
    mk desc code bv

  | ContractAt { arg; entry; entry_param } ->
    let arg = bound arg in
    let desc = ContractAt { arg; entry; entry_param } in
    mk desc code arg.bv

  | Unpack { arg; ty } ->
    let arg = bound arg in
    let desc = Unpack { arg; ty } in
    mk desc code arg.bv

  | TypeAnnot { e; ty } ->
    let e = bound e in
    let bv = e.bv in
    let desc = TypeAnnot { e; ty } in
    mk desc code bv

  | Type _ ->
    mk code.desc code StringSet.empty

and bound_const = function
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CSignature _ | CNone  | CKey_hash _
    | CContract _) as c -> c
  | CSome x -> CSome (bound_const x)
  | CLeft x -> CLeft (bound_const x)
  | CRight x -> CRight (bound_const x)
  | CTuple xs -> CTuple (List.map (bound_const) xs)
  | CList xs -> CList (List.map (bound_const) xs)
  | CSet xs -> CSet (List.map (bound_const) xs)
  | CMap l ->
    CMap (List.map (fun (x,y) -> bound_const x, bound_const y) l)
  | CBigMap BMList l ->
    CBigMap (BMList (List.map (fun (x,y) -> bound_const x, bound_const y) l))
  | CBigMap BMId _ as c -> c
  | CRecord labels ->
    CRecord (List.map (fun (f, x) -> f, bound_const x) labels)
  | CConstr (constr, x) ->
    CConstr (constr, bound_const x)
  | CLambda lam ->
    CLambda (fst @@ bound_lambda lam)

and bound_lambda { arg_name; arg_ty; body; ret_ty; recursive } =
  let body = bound body in
  let lam = { arg_name; arg_ty; body; ret_ty; recursive } in
  let bv = bv body |> StringSet.remove arg_name.nname in
  let bv = match recursive with
    | None -> bv
    | Some f -> StringSet.remove f bv in
  lam, bv

and bound_entry entry =
  let c = bound entry.code in
  let f = match entry.fee_code with
    | None -> None
    | Some fee_code -> Some (bound fee_code) in
  assert (StringSet.equal c.bv (bv entry.code));
  { entry with code = c; fee_code = f }

and bound_contract contract =
  let values = List.map (fun v ->
      { v with val_exp = bound v.val_exp }) contract.values in
  let entries = List.map bound_entry contract.entries in
  let bv_entry acc e =
    StringSet.union e.code.bv
      (match e.fee_code with
       | None -> StringSet.empty
       | Some fee_code -> fee_code.bv)
    |> StringSet.remove e.entry_sig.parameter_name
    |> StringSet.remove e.entry_sig.storage_name
    |> StringSet.union acc in
  let bv = List.fold_left bv_entry StringSet.empty contract.entries in
  let bv =
    List.fold_left (fun bv v -> StringSet.remove v.val_name bv) bv values in
  { contract with values; entries }, bv

let bound_contract contract =
  let c, _ = bound_contract contract in
  c
