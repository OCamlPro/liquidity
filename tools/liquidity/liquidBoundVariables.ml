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
  | Const { loc; ty; const } ->  StringSet.empty

  | Failwith { arg; loc } -> bv arg

  | Apply { prim = Prim_unknown;
            args = ({ desc = Var { name } } :: args) } ->
    let set =
      try
        LiquidTypes.primitive_of_string name |> ignore;
        StringSet.empty
      with Not_found -> StringSet.singleton name
    in
    List.fold_left (fun set arg ->
        StringSet.union set (bv arg)
      ) set args

  | Apply { prim; loc; args } ->
     List.fold_left (fun set arg ->
         StringSet.union set (bv arg)
       ) StringSet.empty args

  | Let { bnd_var; inline; loc; bnd_val; body } ->
     StringSet.union (bv bnd_val)
                     (StringSet.remove bnd_var (bv body))

  | Lambda { arg_name; arg_ty; loc; body; ret_ty } ->
    bv body
    |> StringSet.remove arg_name

  | Closure { arg_name; arg_ty; loc; call_env; body; ret_ty } ->
    bv body
    |> StringSet.remove arg_name
    |> List.fold_right (fun (_, e) -> StringSet.union (bv e)) call_env

  | Var { name; loc } -> StringSet.add name StringSet.empty

  | SetField { record; loc; field; set_val } ->
     StringSet.union (bv record) (bv set_val)

  | Project { loc; field; record } -> (bv record)

  | MatchOption { arg; loc; ifnone; some_name; ifsome } ->
     StringSet.union (bv arg)
                     (StringSet.union (bv ifnone)
                                      (StringSet.remove some_name (bv ifsome)))

  | MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus } ->
     StringSet.union (bv arg)
       (StringSet.union
          (StringSet.remove minus_name (bv ifminus))
          (StringSet.remove plus_name (bv ifplus)))

  | MatchList { arg; loc; head_name; tail_name; ifcons; ifnil } ->
     StringSet.union
       (bv arg)
       (StringSet.union (bv ifnil)
                        (StringSet.remove head_name
                                          (StringSet.remove tail_name
                                                            (bv ifcons))))
  | Transfer { loc; contract; amount; entry; arg } ->
     List.fold_left (fun set exp ->
         StringSet.union set (bv exp)
       ) StringSet.empty [contract; amount; arg]

  | Loop { arg_name; loc; body; arg }
  | Map { arg_name; loc; body; arg } ->
    StringSet.union (bv arg)
      (StringSet.remove arg_name (bv body))

  | MapFold { arg_name; loc; body; arg; acc }
  | Fold { arg_name; loc; body; arg; acc } ->
    StringSet.union (bv acc)
      (StringSet.union (bv arg)
         (StringSet.remove arg_name (bv body)))

  | Record { fields } ->
     List.fold_left (fun set (_,exp) ->
         StringSet.union set (bv exp)
       ) StringSet.empty fields

  | Constructor { arg } -> bv arg

  | MatchVariant { arg; cases } ->
    StringSet.union (bv arg)
      (List.fold_left (fun set (pat, exp) ->
           let bv_exp = bv exp in
           let bv_case = match pat with
             | CConstr (_constr, var_args) ->
               List.fold_left (fun set var_arg ->
                 StringSet.remove var_arg set
                 ) bv_exp var_args
             | CAny -> bv_exp
           in
           StringSet.union set bv_case
         ) StringSet.empty cases)

  | CreateContract { loc; args; contract } ->
    let bv_entry acc e =
      bv e.code
      |> StringSet.remove e.entry_sig.parameter_name
      |> StringSet.remove e.entry_sig.storage_name
      |> StringSet.union acc in
    let bc = List.fold_left bv_entry StringSet.empty contract.entries in
    List.fold_left (fun set arg ->
        StringSet.union set (bv arg)
      ) bc args

  | ContractAt { arg }
  | Unpack { arg } -> bv arg


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

  | Const { loc; ty; const } ->
     mk code.desc code StringSet.empty

  | Failwith { arg; loc } ->
    let arg = bound arg in
    let bv = arg.bv in
    let desc = Failwith { arg; loc } in
    mk desc code bv

  | Apply { prim = Prim_unknown; loc;
            args = { desc = Var { name; loc = varloc }} :: args } ->
    let args = List.map bound args in
    let bv =
      try
        LiquidTypes.primitive_of_string name |> ignore;
        StringSet.empty
      with Not_found -> StringSet.singleton name
    in
    let v = mk (Var { name; loc = varloc }) code bv in
    let bv =
      List.fold_left (fun set arg ->
        StringSet.union set arg.bv
        ) bv args
    in
    mk (Apply { prim = Prim_unknown; loc; args = v :: args }) code bv

  | Apply { prim; loc; args } ->
     let args = List.map bound args in
     let bv =
       List.fold_left (fun set arg ->
           StringSet.union set arg.bv
         ) StringSet.empty args
     in
     let desc = Apply { prim; loc; args } in
     mk desc code bv

  | Let { bnd_var; inline; loc; bnd_val; body } ->
     let bnd_val = bound bnd_val in
     let body = bound body in
     let bv = StringSet.union bnd_val.bv
                              (StringSet.remove bnd_var body.bv) in
     let desc = Let { bnd_var; inline; loc; bnd_val; body } in
     mk desc code bv

  | Lambda { arg_name; arg_ty; loc; body; ret_ty } ->
     let body = bound body in
     let desc = Lambda { arg_name; arg_ty; loc; body; ret_ty } in
     let bv = bv body |> StringSet.remove arg_name in
     mk desc code bv

  | Closure { arg_name; arg_ty; loc; call_env; body; ret_ty } ->
     let call_env = List.map (fun (name, t) -> name, bound t) call_env in
     let body = bound body in
     let bv =
       body.bv
       |> StringSet.remove arg_name
       |> List.fold_right (fun (_, e) -> StringSet.union e.bv) call_env
     in
     let desc = Closure { arg_name; arg_ty; loc; call_env; body; ret_ty } in
     mk desc code bv

  | Var { name; loc } ->
     let bv = StringSet.add name StringSet.empty in
     let desc = Var { name; loc } in
     mk desc code bv

  | SetField { record; loc; field; set_val } ->
     let record = bound record in
     let set_val = bound set_val in
     let bv = StringSet.union record.bv set_val.bv in
     let desc = SetField { record; loc; field; set_val } in
     mk desc code bv

  | Project { loc; field; record } ->
     let record = bound record in
     let desc = Project { loc; field; record } in
     mk desc code record.bv

  | MatchOption { arg; loc; ifnone; some_name; ifsome } ->
    let arg = bound arg in
    let ifnone = bound ifnone in
    let ifsome = bound ifsome in
    let bv =
      StringSet.union arg.bv
        (StringSet.union ifnone.bv
           (StringSet.remove some_name ifsome.bv))
    in
    let desc = MatchOption { arg; loc; ifnone; some_name; ifsome } in
    mk desc code bv


  | MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus } ->
    let arg = bound arg in
    let ifplus = bound ifplus in
    let ifminus = bound ifminus in
    let bv =
      StringSet.union arg.bv
        (StringSet.union
           (StringSet.remove minus_name ifminus.bv)
           (StringSet.remove plus_name ifplus.bv))
    in
    let desc = MatchNat { arg; loc; plus_name; ifplus; minus_name; ifminus } in
    mk desc code bv

  | MatchList { arg; loc; head_name; tail_name; ifcons; ifnil } ->
     let arg = bound arg in
     let ifnil = bound ifnil in
     let ifcons = bound ifcons in
     let bv =
       StringSet.union
         arg.bv
         (StringSet.union ifnil.bv
            (StringSet.remove head_name
               (StringSet.remove tail_name
                  ifcons.bv)))
     in
     let desc = MatchList { arg; loc; head_name; tail_name; ifcons; ifnil } in
     mk desc code bv

  | Transfer { loc; contract; amount; entry; arg } ->
     let contract = bound contract in
     let amount = bound amount in
     let arg = bound arg in
     let bv =
       List.fold_left (fun set exp ->
           StringSet.union set (exp.bv)
         ) StringSet.empty [contract; amount; arg]
     in
     let desc = Transfer {loc; contract; amount; entry; arg } in
     mk desc code bv

  | Loop { arg_name; loc; body; arg } ->
     let arg = bound arg in
     let body = bound body in
     let bv = StringSet.union arg.bv (StringSet.remove arg_name body.bv)
     in
     let desc = Loop { arg_name; loc; body; arg } in
     mk desc code bv

  | Fold { arg_name; loc; body; arg; acc }
  | MapFold { arg_name; loc; body; arg; acc } ->
     let acc = bound acc in
     let arg = bound arg in
     let body = bound body in
     let bv =
       StringSet.union acc.bv
         (StringSet.union arg.bv
            (StringSet.remove arg_name body.bv))
     in
     let desc = match code.desc with
       | Fold { prim } ->
         Fold { prim; arg_name; loc; body; arg; acc }
       | MapFold { prim } ->
         MapFold  { prim; arg_name; loc; body; arg; acc }
       | _ -> assert false
     in
     mk desc code bv

  | Map { prim; arg_name; loc; body; arg } ->
     let arg = bound arg in
     let body = bound body in
     let bv = StringSet.union arg.bv (StringSet.remove arg_name body.bv)
     in
     let desc = Map  { prim; arg_name; loc; body; arg } in
     mk desc code bv

  | Record { loc; fields } ->
     let fields = List.map (fun (l, exp) -> (l,bound exp)) fields in
     let bv =
       List.fold_left (fun set (_,exp) ->
           StringSet.union set (exp.bv)
         ) StringSet.empty fields
     in
     let desc = Record { loc; fields } in
     mk desc code bv

  | Constructor { loc; constr; arg } ->
     let arg = bound arg in
     let desc = Constructor { loc; constr; arg } in
     mk desc code arg.bv

  | MatchVariant { arg; loc; cases } ->
     let arg = bound arg in
     let cases =
       List.map (fun (pat, exp) ->
           (pat, bound exp)
         ) cases in
     let bv = List.fold_left (fun set (pat, exp) ->
          let bv_exp = bv exp in
          let bv_case = match pat with
            | CConstr (_constr, var_args) ->
              List.fold_left (fun set var_arg ->
                  StringSet.remove var_arg set
                ) bv_exp var_args
            | CAny -> bv_exp
          in
          StringSet.union set bv_case
       ) StringSet.empty cases
     in
     let bv = StringSet.union arg.bv bv in
     let desc = MatchVariant { arg; loc; cases } in
     mk desc code bv

  | CreateContract { loc; args; contract } ->
    let args = List.map bound args in
    let contract = bound_contract contract in
    let bv_entry acc e =
      e.code.bv
      |> StringSet.remove e.entry_sig.parameter_name
      |> StringSet.remove e.entry_sig.storage_name
      |> StringSet.union acc in
    let bv = List.fold_left bv_entry StringSet.empty contract.entries in
    let bv =
      List.fold_left (fun set arg ->
          StringSet.union set arg.bv
        ) bv args
    in
    let desc = CreateContract { loc; args; contract } in
    mk desc code bv

  | ContractAt { loc; arg; c_sig } ->
     let arg = bound arg in
     let desc = ContractAt { loc; arg; c_sig } in
     mk desc code arg.bv

  | Unpack { loc; arg; ty } ->
     let arg = bound arg in
     let desc = Unpack { loc; arg; ty } in
     mk desc code arg.bv

and bound_entry entry =
  let c = bound entry.code in
  assert (StringSet.equal c.bv (bv entry.code));
  { entry with code = c }

and bound_contract contract =
  { contract with
    values = List.map (fun (v, i, body) -> (v, i, bound body)) contract.values;
    entries = List.map bound_entry contract.entries }
