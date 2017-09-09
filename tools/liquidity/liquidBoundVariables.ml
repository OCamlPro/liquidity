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
  | Const (ty, cst) ->  StringSet.empty
  | Apply (prim, loc, args) ->
     List.fold_left (fun set arg ->
         StringSet.union set (bv arg)
       ) StringSet.empty args

  | Let (var, loc, exp, body) ->
     StringSet.union (bv exp)
                     (StringSet.remove var (bv body))

  | Lambda (arg_name, arg_type, loc, body, res_type) -> StringSet.empty

  | Var (name, loc, fields) -> StringSet.add name StringSet.empty

  | SetVar (name, loc, fields, exp) ->
     StringSet.add name (bv exp)

  | MatchOption (exp, loc, ifnone, some_pat, ifsome) ->
     StringSet.union (bv exp)
                     (StringSet.union (bv ifnone)
                                      (StringSet.remove some_pat (bv ifsome)))

  | MatchList (exp, loc, head_pat, tail_pat, ifcons, ifnil) ->
     StringSet.union
       (bv exp)
       (StringSet.union (bv ifnil)
                        (StringSet.remove head_pat
                                          (StringSet.remove tail_pat
                                                            (bv ifcons))))
  | LetTransfer ( var_storage, var_result,
                  loc,
                  contract_exp,
                  amount_exp,
                  storage_exp,
                  arg_exp,
                  body_exp) ->
     List.fold_left (fun set exp ->
         StringSet.union set (bv exp)
       ) StringSet.empty [contract_exp;
                          amount_exp;
                          storage_exp;
                          arg_exp]


  | Loop (var_arg, loc, body_exp, arg_exp) ->
     StringSet.union (bv arg_exp)
                     (StringSet.remove var_arg
                                       (bv body_exp))

  | Record (_loc, labels) ->
     List.fold_left (fun set (_,exp) ->
         StringSet.union set (bv exp)
       ) StringSet.empty labels

  | Constructor (_loc, _lid, arg) ->
     bv arg

  | MatchVariant (exp, _loc, args) ->
     StringSet.union (bv exp)
                     (List.fold_left (fun set (_constr, var_args, exp) ->
                          StringSet.union set (
                                            List.fold_left (fun set var_arg ->
                                                StringSet.remove var_arg set
                                              )  (bv exp) var_args)
                        ) StringSet.empty args)

let mk desc exp bv = { desc; ty = exp.ty; bv; fail = exp.fail }

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

  | Const (ty, cst) ->
     mk code.desc code StringSet.empty

  | Apply (prim, loc, args) ->
     let args = List.map bound args in
     let bv =
       List.fold_left (fun set arg ->
           StringSet.union set arg.bv
         ) StringSet.empty args
     in
     let desc = Apply(prim,loc,args) in
     mk desc code bv

  | Let (var, loc, exp, body) ->
     let exp = bound exp in
     let body = bound body in
     let bv = StringSet.union exp.bv
                              (StringSet.remove var body.bv) in
     let desc = Let(var, loc, exp, body) in
     mk desc code bv

  | Lambda (arg_name, arg_type, loc, body, res_type) ->
     let body = bound body in
     let desc = Lambda(arg_name, arg_type, loc, body, res_type) in
     mk desc code StringSet.empty

  | Var (name, loc, fields) ->
     let bv = StringSet.add name StringSet.empty in
     let desc = Var(name, loc, fields) in
     mk desc code bv

  | SetVar (name, loc, fields, exp) ->
     let exp = bound exp in
     let bv = StringSet.add name exp.bv in
     let desc = SetVar(name, loc, fields,exp) in
     mk desc code bv

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

  | LetTransfer ( var_storage, var_result,
                  loc,
                  contract_exp,
                  amount_exp,
                  storage_exp,
                  arg_exp,
                  body_exp) ->
     let contract_exp = bound contract_exp in
     let amount_exp = bound amount_exp in
     let storage_exp = bound storage_exp in
     let arg_exp = bound arg_exp in
     let body_exp = bound body_exp in

     let bv =
       List.fold_left (fun set exp ->
           StringSet.union set (exp.bv)
         ) StringSet.empty [contract_exp;
                            amount_exp;
                            storage_exp;
                            arg_exp]
     in
     let desc = LetTransfer(var_storage, var_result,
                            loc,
                            contract_exp,
                            amount_exp,
                            storage_exp,
                            arg_exp,
                            body_exp) in
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
       List.map (fun (constr, var_args, exp) ->
           (constr, var_args, bound exp)
         ) args in
     let bv =
       StringSet.union (exp.bv)
                       (List.fold_left (fun set (_constr, var_args, exp) ->
                            StringSet.union set (
                                              List.fold_left (fun set var_arg ->
                                                  StringSet.remove var_arg set
                                                )  exp.bv var_args)
                          ) StringSet.empty args)
     in
     let desc = MatchVariant(exp,loc,args) in
     mk desc code bv

let bound_contract contract =
  let c = bound contract.code in
  assert (StringSet.equal c.bv (bv contract.code));
  { contract with code = c }
