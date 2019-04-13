(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

  | Lambda { arg_name; arg_ty; body; ret_ty; recursive } ->
    bv body
    |> StringSet.remove arg_name.nname
    |> fun bv -> begin match recursive with
      | None -> bv
      | Some f -> StringSet.remove f bv
    end

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

  | Transfer { dest; amount } -> StringSet.union (bv dest) (bv amount)

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
    mk code.desc code StringSet.empty

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

  | Lambda { arg_name; arg_ty; body; ret_ty; recursive } ->
    let body = bound body in
    let desc = Lambda { arg_name; arg_ty; body; ret_ty; recursive } in
    let bv = bv body |> StringSet.remove arg_name.nname in
    let bv = match recursive with
      | None -> bv
      | Some f -> StringSet.remove f bv in
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

  | Transfer { dest; amount } ->
    let dest = bound dest in
    let amount = bound amount in
    let bv = StringSet.union dest.bv amount.bv in
    let desc = Transfer { dest; amount } in
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

  | ContractAt { arg; c_sig } ->
    let arg = bound arg in
    let desc = ContractAt { arg; c_sig } in
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

and bound_entry entry =
  let c = bound entry.code in
  assert (StringSet.equal c.bv (bv entry.code));
  { entry with code = c }

and bound_contract contract =
  let values = List.map (fun v ->
      { v with val_exp = bound v.val_exp }) contract.values in
  let entries = List.map bound_entry contract.entries in
  let bv_entry acc e =
    e.code.bv
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
