(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let rec compute decompile code to_inline =

  let old_to_inline = to_inline in
  let to_inline = ref (if decompile then StringMap.empty else to_inline) in

  (* Do not inline terms larger than this value when decompiling *)
  let inline_treshold_low = 100 in

  let rec size exp =
    match exp.desc with
    | Const _ | Var _ | SetVar _ -> 1

    | Failwith (e, _)
    | ContractAt (_, e, _)
    | Unpack (_, e, _)
    | Constructor (_, _, e) -> size e

    | Seq (e1, e2) -> size e1 + size e2

    | Let (_, _, _, e1, e2) -> size e1

    | Loop (_, _, e1, e2) -> 30 + size e1 + size e2

    | If (e1, e2, e3)
    | MatchNat (e1, _, _, e2, _, e3) -> 40 + size e1 + size e2 + size e3

    | MatchList (e1, _, _, _, e2, e3)
    | MatchOption (e1, _, e2, _, e3) -> 40 + size e1 + size e2 + size e3

    | Map (_, _, _, e1, e2) -> 30 + size e1 + size e2
    | Fold (_, _, _, e1, e2, e3)
    | MapFold (_, _, _, e1, e2, e3) -> 30 + size e1 + size e2 + size e3
    | Transfer (_, e1, e2, _, e3) -> 1 + size e1 + size e2 + size e3

    | Apply (prim, _, l) ->
      List.fold_left (fun acc e -> acc + size e) 1 l

    | Lambda (_, _, _, e, _)
    | Closure (_, _, _, _, e, _) -> 70 + size e

    | Record (_, labels) ->
      List.fold_left (fun acc (_, e) -> acc + size e) 1 labels

    | MatchVariant (e, _, cases) ->
      List.fold_left (fun acc (_, e) -> acc + size e) (size e + 40) cases

    | CreateContract (_, l, _) ->
      List.fold_left (fun acc e -> acc + size e) 1 l
  in


  let rec iter exp =
    match exp.desc with
    | Const _ -> exp
    | Var (name, _loc, []) ->
       begin
         try
           let v = StringMap.find name !to_inline in
           iter v
         with Not_found -> exp
       end
    | Var (name, _loc, _::_) -> assert false
    | SetVar (name, _loc, _, _) -> assert false
    | Let (name, _inline, loc, v, _body) when v.ty = Tfail ->
      iter v
    | Let (name, _inline, loc, v, { desc = Var (vname, _loc, []) })
      when vname = name -> (* special case for let x = e in x *)
      iter v
    | Let (name, _inline, loc, ({ ty = Ttuple tys} as v),
           { desc = Apply (Prim_tuple, _, tuple) })
      when
        let len, ok =
          List.fold_left (fun (i, ok) t -> match t.desc with
            | Apply (Prim_tuple_get, _, [
                { desc = Var (vname, _, []) };
                { desc = Const (_, _, (CInt n | CNat n)) }]) ->
              let ok = ok && vname = name &&
                       LiquidPrinter.int_of_integer n = i in
              (i + 1, ok)
            | _ -> (i + 1, false)
            ) (0, true) tuple in
        ok && List.length tys = len
      ->
      (* special case for let x = v in (x.(0), x.(1)) *)
      iter v
    | Let (name, inline, loc, v, body) ->
      if decompile && v.name = None && size v <= inline_treshold_low &&
         (StringMap.mem name old_to_inline ||
          match v.desc with
          | Var _ | Apply (Prim_tuple_get, _, _) -> true
          | _ -> false)
      then
        to_inline := StringMap.add name v !to_inline;
      (* let obody = body in *)
      let body = iter body in
      (* if body <> obody then iter { exp with desc = Let (name, loc, v, body) } else *)
      if StringMap.mem name !to_inline then
        body
      else
        let v = iter v in
        begin
          try
            if StringSet.mem name (LiquidBoundVariables.bv body) then
              raise Exit;
            if not v.fail then
              body
            else
            if v.ty <> Tunit then raise Exit
            else
              { exp with desc = Seq(v, body); name = None }
          with Exit ->
            { exp with desc = Let(name, inline, loc, v, body) }
        end

    | MatchOption(arg, loc, ifnone, name, ifsome) ->
       let arg = iter arg in
       let ifnone = iter ifnone in
       let ifsome = iter ifsome in
       { exp with desc = MatchOption(arg,loc,ifnone,name,ifsome) }

    | MatchNat(arg, loc, p, ifplus, m, ifminus) ->
       let arg = iter arg in
       let ifplus = iter ifplus in
       let ifminus = iter ifminus in
       { exp with desc = MatchNat(arg, loc, p, ifplus, m, ifminus) }

    | MatchList(arg, loc, head_name, tail_name, ifcons, ifnil) ->
       let arg = iter arg in
       let ifcons = iter ifcons in
       let ifnil = iter ifnil in
       { exp with desc = MatchList(arg,loc,
                                   head_name, tail_name, ifcons,
                                   ifnil) }

    | MatchVariant(arg, loc, cases) ->
       let arg = iter arg in
       let cases = List.map (fun (pat, e) -> pat, iter e) cases in
       { exp with desc = MatchVariant(arg,loc, cases) }

    | Loop(name, loc, body, arg) ->
       let body = iter body in
       let arg = iter arg in
       { exp with desc = Loop(name, loc, body, arg) }

    | Fold(prim, name, loc, body, arg, acc) ->
       let body = iter body in
       let arg = iter arg in
       let acc = iter acc in
       { exp with desc = Fold(prim, name, loc, body, arg, acc) }

    | Map(prim, name, loc, body, arg) ->
       let body = iter body in
       let arg = iter arg in
       { exp with desc = Map(prim, name, loc, body, arg) }

    | MapFold(prim, name, loc, body, arg, acc) ->
       let body = iter body in
       let arg = iter arg in
       let acc = iter acc in
       { exp with desc = MapFold(prim, name, loc, body, arg, acc) }

    | Seq(e1, e2) ->
       let e1 = iter e1 in
       let e2 = iter e2 in
       if e1.ty = Tfail (* e1 always fails *)
       then e1
       else if not e1.fail && not e1.transfer (* no side-effects *)
       then e2
       else { exp with desc = Seq(e1,e2) }

    | If(cond, ifthen, ifelse) ->
       let cond = iter cond in
       let ifthen = iter ifthen in
       let ifelse = iter ifelse in
       { exp with desc = If(cond, ifthen, ifelse) }

    | Apply(Prim_exec, loc, [x; f]) ->
       (* inline body of lambda *)
       let x = iter x in
       let f = iter f in
       begin match f.desc with
         | Lambda (arg, _, _, body, _) ->
           iter { exp with desc = Let (arg, false, loc, x, body) }
         | _ ->
           { exp with desc = Apply(Prim_exec, loc, [x; f]) }
       end

    | Apply(prim, loc, args) ->
       let args = List.map iter args in
       { exp with desc = Apply(prim, loc, args) }

    | Transfer (loc, contract_exp, tez_exp, entry, arg_exp) ->
       let contract_exp = iter contract_exp in
       let tez_exp = iter tez_exp in
       let arg_exp = iter arg_exp in
       { exp with desc = Transfer (loc, contract_exp, tez_exp, entry, arg_exp) }

    | Lambda (arg_name, arg_type, loc, body_exp, res_type) ->
      let body_exp = iter body_exp in
      { exp with
        desc = Lambda (arg_name, arg_type, loc, body_exp, res_type) }

    | Closure (arg_name, arg_type, loc, call_env, body_exp, res_type) ->
      let body_exp = iter body_exp in
      let call_env = List.map (fun (name, t) -> name, iter t) call_env in
      { exp with
        desc = Closure (arg_name, arg_type, loc, call_env, body_exp, res_type) }

    | Record (loc, lab_x_exp_list) ->
       let lab_x_exp_list = List.map (fun (label, exp) -> label, iter exp)
                                     lab_x_exp_list in
       { exp with desc = Record(loc, lab_x_exp_list) }

    | Failwith (err, loc) ->
      { exp with desc = Failwith (iter err, loc) }

    | CreateContract (loc, args, contract) ->
      let args = List.map iter args in
      (* contract is already simplified *)
      { exp with desc = CreateContract(loc, args, contract) }

    | ContractAt (loc, addr, ty) ->
      let addr = iter addr in
      { exp with desc = ContractAt (loc, addr, ty) }

    | Unpack (loc, e, ty) ->
      let e = iter e in
      { exp with desc = Unpack (loc, e, ty) }

    | Constructor _ -> assert false (* never found in typed_exp *)
  in

  let rec fixpoint code =
    let c = iter code in
    if c <> code then fixpoint c else c
  in

  fixpoint code

  (* iter code *)

and simplify_contract ?(decompile_annoted=false) contract to_inline =
  match contract.entries with
  | [{ entry_sig = { entry_name = "main" };
       code } as entry ] ->
    { contract with
      entries = [{ entry with code = compute decompile_annoted code to_inline }]
    }
  | _ -> assert false
