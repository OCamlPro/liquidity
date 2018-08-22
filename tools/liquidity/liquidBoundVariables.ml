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
  | If (cond, ifthen, ifelse) ->
     StringSet.union (bv cond)
                     (StringSet.union (bv ifthen) (bv ifelse))
  | Seq (x, y) -> StringSet.union (bv x) (bv y)
  | Const (loc, ty, cst) ->  StringSet.empty

  | Failwith (err, loc) -> bv err

  | Apply (Prim_unknown, _,
           ({ desc = Var (name, _)} :: args)) ->
    let set =
      try
        LiquidTypes.primitive_of_string name |> ignore;
        StringSet.empty
      with Not_found -> StringSet.singleton name
    in
    List.fold_left (fun set arg ->
        StringSet.union set (bv arg)
      ) set args

  | Apply (prim, loc, args) ->
     List.fold_left (fun set arg ->
         StringSet.union set (bv arg)
       ) StringSet.empty args

  | Let (var, inline, loc, exp, body) ->
     StringSet.union (bv exp)
                     (StringSet.remove var (bv body))

  | Lambda (arg_name, arg_type, loc, body, res_type) ->
    bv body
    |> StringSet.remove arg_name

  | Closure (arg_name, arg_type, loc, call_env, body, res_type) ->
    bv body
    |> StringSet.remove arg_name
    |> List.fold_right (fun (_, e) -> StringSet.union (bv e)) call_env

  | Var (name, loc) -> StringSet.add name StringSet.empty

  | SetField (arg, loc, fields, exp) ->
     StringSet.union (bv arg) (bv exp)

  | Project (loc, field, exp) -> (bv exp)

  | MatchOption (exp, loc, ifnone, some_pat, ifsome) ->
     StringSet.union (bv exp)
                     (StringSet.union (bv ifnone)
                                      (StringSet.remove some_pat (bv ifsome)))

  | MatchNat (exp, loc, p, ifplus, m, ifminus) ->
     StringSet.union (bv exp)
       (StringSet.union
          (StringSet.remove m (bv ifminus))
          (StringSet.remove p (bv ifplus)))

  | MatchList (exp, loc, head_pat, tail_pat, ifcons, ifnil) ->
     StringSet.union
       (bv exp)
       (StringSet.union (bv ifnil)
                        (StringSet.remove head_pat
                                          (StringSet.remove tail_pat
                                                            (bv ifcons))))
  | Transfer (loc, contract_exp, amount_exp, _entry, arg_exp) ->
     List.fold_left (fun set exp ->
         StringSet.union set (bv exp)
       ) StringSet.empty [contract_exp;
                          amount_exp;
                          arg_exp]


  | Loop (var_arg, loc, body_exp, arg_exp) ->
      StringSet.union (bv arg_exp)
      (StringSet.remove var_arg
         (bv body_exp))

  | Map (_, var_arg, loc, body_exp, arg_exp) ->
    StringSet.union (bv arg_exp)
      (StringSet.remove var_arg
         (bv body_exp))

  | MapFold (_, var_arg, loc, body_exp, arg_exp, acc_exp)
  | Fold (_, var_arg, loc, body_exp, arg_exp, acc_exp) ->
    StringSet.union (bv acc_exp)
      (StringSet.union (bv arg_exp)
         (StringSet.remove var_arg
            (bv body_exp)))

  | Record (_loc, labels) ->
     List.fold_left (fun set (_,exp) ->
         StringSet.union set (bv exp)
       ) StringSet.empty labels

  | Constructor (_loc, _lid, arg) ->
     bv arg

  | MatchVariant (exp, _loc, args) ->
    StringSet.union (bv exp)
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
         ) StringSet.empty args)

  | CreateContract (loc, args, contract) ->
    let bv_entry acc e =
      bv e.code
      |> StringSet.remove e.entry_sig.parameter_name
      |> StringSet.remove e.entry_sig.storage_name
      |> StringSet.union acc in
    let bc = List.fold_left bv_entry StringSet.empty contract.entries in
    List.fold_left (fun set arg ->
        StringSet.union set (bv arg)
      ) bc args

  | ContractAt (loc, addr, ty) -> bv addr

  | Unpack (loc, e, ty) -> bv e


let mk desc exp bv = { exp with desc; bv }

let rec bound code =
  match code.desc with

  | If (cond, ifthen, ifelse) ->
     let cond = bound cond in
     let ifthen = bound ifthen in
     let ifelse = bound ifelse in
     let bv = StringSet.union cond.bv
                              (StringSet.union ifthen.bv ifelse.bv) in
     let desc = If(cond, ifthen, ifelse) in
     mk desc code bv

  | Seq (x, y) ->
     let x = bound x in
     let y = bound y in
     let bv = StringSet.union x.bv y.bv in
     let desc = Seq(x,y) in
     mk desc code bv

  | Const (loc, ty, cst) ->
     mk code.desc code StringSet.empty

  | Failwith (err, loc) ->
    let err = bound err in
    let bv = err.bv in
    let desc = Failwith (err, loc) in
    mk desc code bv

  | Apply (Prim_unknown, loc,
           ({ desc = Var (name, varloc)} :: args)) ->
    let args = List.map bound args in
    let bv =
      try
        LiquidTypes.primitive_of_string name |> ignore;
        StringSet.empty
      with Not_found -> StringSet.singleton name
    in
    let v = mk (Var (name, varloc)) code bv in
    let bv =
      List.fold_left (fun set arg ->
        StringSet.union set arg.bv
        ) bv args
    in
    mk (Apply(Prim_unknown, loc, v :: args)) code bv

  | Apply (prim, loc, args) ->
     let args = List.map bound args in
     let bv =
       List.fold_left (fun set arg ->
           StringSet.union set arg.bv
         ) StringSet.empty args
     in
     let desc = Apply(prim,loc,args) in
     mk desc code bv

  | Let (var, inline, loc, exp, body) ->
     let exp = bound exp in
     let body = bound body in
     let bv = StringSet.union exp.bv
                              (StringSet.remove var body.bv) in
     let desc = Let(var, inline, loc, exp, body) in
     mk desc code bv

  | Lambda (arg_name, arg_type, loc, body, res_type) ->
     let body = bound body in
     let desc = Lambda(arg_name, arg_type, loc, body, res_type) in
     let bv = bv body |> StringSet.remove arg_name in
     mk desc code bv

  | Closure (arg_name, arg_type, loc, call_env, body, res_type) ->
     let call_env = List.map (fun (name, t) -> name, bound t) call_env in
     let body = bound body in
     let bv =
       body.bv
       |> StringSet.remove arg_name
       |> List.fold_right (fun (_, e) -> StringSet.union e.bv) call_env
     in
     let desc = Closure(arg_name, arg_type, loc, call_env, body, res_type) in
     mk desc code bv

  | Var (name, loc) ->
     let bv = StringSet.add name StringSet.empty in
     let desc = Var(name, loc) in
     mk desc code bv

  | SetField (arg, loc, fields, exp) ->
     let arg = bound arg in
     let exp = bound exp in
     let bv = StringSet.union arg.bv exp.bv in
     let desc = SetField(arg, loc, fields, exp) in
     mk desc code bv

  | Project (loc, field, exp) ->
     let exp = bound exp in
     let desc = Project (loc, field, exp) in
     mk desc code exp.bv

  | MatchOption (exp, loc, ifnone, some_pat, ifsome) ->
     let exp = bound exp in
     let ifnone = bound ifnone in
     let ifsome = bound ifsome in
     let bv =
       StringSet.union exp.bv
                       (StringSet.union ifnone.bv
                                        (StringSet.remove some_pat ifsome.bv))
     in
     let desc = MatchOption(exp,loc,ifnone, some_pat, ifsome) in
     mk desc code bv


  | MatchNat (exp, loc, p, ifplus, m, ifminus) ->
    let exp = bound exp in
    let ifplus = bound ifplus in
    let ifminus = bound ifminus in
    let bv =
      StringSet.union exp.bv
        (StringSet.union
           (StringSet.remove m ifminus.bv)
           (StringSet.remove p ifplus.bv))
    in
    let desc = MatchNat (exp, loc, p, ifplus, m, ifminus) in
    mk desc code bv

  | MatchList (exp, loc, head_pat, tail_pat, ifcons, ifnil) ->
     let exp = bound exp in
     let ifnil = bound ifnil in
     let ifcons = bound ifcons in
     let bv =
       StringSet.union
         (exp.bv)
         (StringSet.union (ifnil.bv)
                          (StringSet.remove head_pat
                                            (StringSet.remove tail_pat
                                                              (ifcons.bv))))
     in
     let desc = MatchList(exp, loc, head_pat, tail_pat, ifcons, ifnil) in
     mk desc code bv

  | Transfer (loc, contract_exp, amount_exp, entry, arg_exp) ->
     let contract_exp = bound contract_exp in
     let amount_exp = bound amount_exp in
     let arg_exp = bound arg_exp in
     let bv =
       List.fold_left (fun set exp ->
           StringSet.union set (exp.bv)
         ) StringSet.empty [contract_exp;
                            amount_exp;
                            arg_exp]
     in
     let desc = Transfer (loc, contract_exp, amount_exp, entry, arg_exp) in
     mk desc code bv

  | Loop (var_arg, loc, body_exp, arg_exp) ->
     let arg_exp = bound arg_exp in
     let body_exp = bound body_exp in
     let bv =
       StringSet.union (arg_exp.bv)
                       (StringSet.remove var_arg
                                         (body_exp.bv))
     in
     let desc = Loop (var_arg, loc, body_exp, arg_exp) in
     mk desc code bv

  | Fold (_, var_arg, loc, body_exp, arg_exp, acc_exp)
  | MapFold (_, var_arg, loc, body_exp, arg_exp, acc_exp) ->
     let acc_exp = bound acc_exp in
     let arg_exp = bound arg_exp in
     let body_exp = bound body_exp in
     let bv =
       StringSet.union (acc_exp.bv)
         (StringSet.union (arg_exp.bv)
            (StringSet.remove var_arg
               (body_exp.bv)))
     in
     let desc = match code.desc with
       | Fold (prim, _, _, _, _, _) ->
         Fold (prim, var_arg, loc, body_exp, arg_exp, acc_exp)
       | MapFold (prim, _, _, _, _, _) ->
         MapFold (prim, var_arg, loc, body_exp, arg_exp, acc_exp)
       | _ -> assert false
     in
     mk desc code bv

  | Map (prim, var_arg, loc, body_exp, arg_exp) ->
     let arg_exp = bound arg_exp in
     let body_exp = bound body_exp in
     let bv =
       StringSet.union (arg_exp.bv)
         (StringSet.remove var_arg
            (body_exp.bv))
     in
     let desc = Map (prim, var_arg, loc, body_exp, arg_exp) in
     mk desc code bv

  | Record (loc, labels) ->
     let labels = List.map (fun (l, exp) -> (l,bound exp)) labels in
     let bv =
       List.fold_left (fun set (_,exp) ->
           StringSet.union set (exp.bv)
         ) StringSet.empty labels
     in
     let desc = Record(loc,labels) in
     mk desc code bv

  | Constructor (loc, lid, arg) ->
     let arg = bound arg in
     let desc = Constructor(loc,lid,arg) in
     mk desc code arg.bv

  | MatchVariant (exp, loc, args) ->
     let exp = bound exp in
     let args =
       List.map (fun (pat, exp) ->
           (pat, bound exp)
         ) args in
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
       ) StringSet.empty args
     in
     let bv = StringSet.union (exp.bv) bv in
     let desc = MatchVariant(exp,loc,args) in
     mk desc code bv

  | CreateContract (loc, args, contract) ->
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
    let desc = CreateContract (loc, args, contract) in
    mk desc code bv

  | ContractAt (loc, addr, ty) ->
     let addr = bound addr in
     let desc = ContractAt (loc, addr, ty) in
     mk desc code addr.bv

  | Unpack (loc, e, ty) ->
     let e = bound e in
     let desc = Unpack (loc, e, ty) in
     mk desc code e.bv

and bound_entry entry =
  let c = bound entry.code in
  assert (StringSet.equal c.bv (bv entry.code));
  { entry with code = c }

and bound_contract contract =
  { contract with
    values = List.map (fun (v, i, body) -> (v, i, bound body)) contract.values;
    entries = List.map bound_entry contract.entries }
