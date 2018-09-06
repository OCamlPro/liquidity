(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let noloc = LiquidLoc.noloc

let mk ?name ~loc desc = mk ?name ~loc desc ()

let const_name_of_datatype = function
  | Tunit -> "u"
  | Tbool -> "b"
  | Tint -> "i"
  | Tnat -> "n"
  | Ttez -> "amount"
  | Tstring -> "s"
  | Tbytes -> "by"
  | Ttimestamp -> "time"
  | Tkey -> "key"
  | Tkey_hash -> "key_hash"
  | Tsignature -> "sig"
  | Ttuple _ -> "tuple"
  | Toption _ -> "opt"
  | Trecord _ -> "record"
  | Tsum _ -> "sum"
  | Tlist _ -> "l"
  | Tset _ -> "s"
  | Tmap _ -> "map"
  | Tbigmap _ -> "bigmap"
  | Tcontract _ -> "contrat"
  | Tor _ -> "or"
  | Tlambda _ | Tclosure _  -> "fun"
  | Tfail -> "fail"
  | Toperation -> "op"
  | Taddress -> "addr"


let vars_nums = Hashtbl.create 101

let rec var_of node =
  match node.kind with
  | N_VAR name -> name
  | _ -> match node.node_name with

    | Some name -> 
      begin try
          if Hashtbl.find vars_nums name = node.num then name
          else Printf.sprintf "%s%d" name node.num
        with Not_found -> name
      end
    | None -> match node.kind with

      (* | N_VAR name -> name *)
      | N_PRIM "CAR" ->
        begin match node.args with
          | [n] -> Printf.sprintf "%s_fst%d" (var_of n) node.num
          | _ -> assert false
        end
      | N_PRIM "CDR" ->
        begin match node.args with
          | [n] -> Printf.sprintf "%s_snd%d" (var_of n) node.num
          | _ -> assert false
        end
      | N_PRIM p -> Printf.sprintf "%s%d" (String.lowercase_ascii p) node.num
      | N_IF _ -> Printf.sprintf "res%d" node.num
      | N_IF_THEN _ -> Printf.sprintf "then%d" node.num
      | N_IF_ELSE _ -> Printf.sprintf "else%d" node.num
      | N_IF_NIL _ -> Printf.sprintf "if_nil%d" node.num
      | N_IF_CONS _ -> Printf.sprintf "if_cons%d" node.num
      | N_IF_LEFT _ -> Printf.sprintf "if_left%d" node.num
      | N_IF_RIGHT _ -> Printf.sprintf "if_right%d" node.num
      | N_IF_PLUS _ -> Printf.sprintf "if_plus%d" node.num
      | N_IF_MINUS _ -> Printf.sprintf "if_minus%d" node.num
      | N_LEFT _ -> Printf.sprintf "left%d" node.num
      | N_CONTRACT _ -> Printf.sprintf "contract%d" node.num
      | N_UNPACK _ -> Printf.sprintf "unpacked%d" node.num
      | N_RIGHT _ -> Printf.sprintf "right%d" node.num
      | N_TRANSFER -> Printf.sprintf "transfer%d" node.num
      | N_IF_RESULT _ | N_IF_END_RESULT _ | N_LOOP_RESULT _ ->
        Printf.sprintf "var%d" node.num
      | N_FAILWITH -> Printf.sprintf "fail%d" node.num
      | N_LOOP _ -> Printf.sprintf "loop%d" node.num
      | N_LAMBDA _ -> Printf.sprintf "fun%d" node.num
      | N_LAMBDA_BEGIN | N_LOOP_BEGIN _ | N_FOLD_BEGIN _ ->
        Printf.sprintf "arg%d" node.num
      | N_CONST (ty, _) ->
        Printf.sprintf "%s%d" (const_name_of_datatype ty) node.num
      | _ -> Printf.sprintf "exp%d" node.num


let var_of node =
  let name = var_of node in
  Hashtbl.add vars_nums name node.num;
  name

let lvar_of node =
  let name = var_of node in
  Hashtbl.add vars_nums name node.num;
  { nname = name; nloc = node.loc }


let nat_n ~loc n =
  mk ~loc (Const { ty = Tnat; const = CNat (LiquidPrinter.integer_of_int n) })
let nat_zero = nat_n 0
let nat_one = nat_n 1

let int_n ~loc n =
  mk ~loc (Const { ty = Tint; const = CInt (LiquidPrinter.integer_of_int n) })
let int_zero = int_n 0
let int_one = int_n 1

let unit ~loc = mk ~loc (Const { ty =  Tunit; const = CUnit })

let mk_get ~loc ?name arg n =
  mk ?name ~loc
    (Apply { prim = Prim_tuple_get; args = [arg ; nat_n ~loc n ] })



let rec arg_of node =
  let loc = node.loc in
  match node.kind with
  | N_IF_END_RESULT ({ kind = N_IF_END (if_node, _);
                       args }, _end_else_node, pos ) ->
     begin
       match pos, List.length args with
       | 0, 1 -> arg_of if_node
       | _ -> mk_get ~loc (arg_of if_node) pos
     end
  | N_ARG ({ kind = N_LOOP_BEGIN ( _); args } as begin_node, pos ) ->
     begin
       match pos, List.length args with
       | 0, 1 -> arg_of begin_node
       | _ -> mk_get ~loc (arg_of begin_node) pos
     end
  | N_LOOP_RESULT (loop_node, begin_node, pos ) ->
     begin
       match pos, List.length begin_node.args with
       | 0, 1 -> arg_of loop_node
       | _ -> mk_get ~loc (arg_of loop_node) pos
     end
  | N_ARG ({ kind = N_FOLD_BEGIN ( _); args = acc } as begin_node, pos ) ->
    let x_acc = arg_of begin_node in
    begin
       match pos, acc with
       | 0, [] -> x_acc
       | 0, [ { kind = N_CONST (_, CUnit)}] -> (* iter *) x_acc
       | 0, _ -> (* arg is iterated element *)
         mk_get ?name:node.node_name ~loc x_acc 0
       | 1, [_] -> (* arg is accumulator *)
         mk_get ?name:node.node_name ~loc x_acc 1
       | _, _ when pos > 0 -> (* arg in accumulator *)
         let acc_liq = mk_get ~loc x_acc 1 in
         mk_get ?name:node.node_name ~loc acc_liq (pos - 1)
       | _ -> assert false
     end
  | N_FOLD_RESULT (fold_node, end_node, pos ) ->
    begin
       match pos, List.length end_node.args with
       | 0, 1 -> arg_of fold_node
       | _ -> mk_get ~loc (arg_of fold_node) pos
     end

  | N_ARG ({ kind = N_MAP_BEGIN ( _); args = [] } as begin_node, 0 ) ->
    (* .map *)
    arg_of begin_node

  | N_ARG ({ kind = N_MAP_BEGIN ( _); args = acc } as begin_node, pos ) ->
    (* .map_fold *)
    let x_acc = arg_of begin_node in
    begin
       match pos, acc with
       | _, [] -> assert false
       | 0, _ -> (* arg is map element *)
         mk_get ?name:node.node_name ~loc x_acc 0
       | 1, [_] -> (* arg is accumulator *)
         mk_get ?name:node.node_name ~loc x_acc 1
       | _, _ when pos > 0 -> (* arg in accumulator *)
         let acc_liq = mk_get ~loc x_acc 1 in
         mk_get ?name:node.node_name ~loc acc_liq (pos - 1)
       | _ -> assert false
     end

  | N_MAP_RESULT (map_node, end_node, pos ) ->
    begin
       match pos, end_node.args with
       | 0, [] -> arg_of map_node
       | _ -> mk_get ~loc (arg_of map_node) pos
     end

  | N_RESULT (node, pos) -> mk_get ~loc (arg_of node) pos

  | N_CONST (ty, ((
                   CUnit | CBool _ | CInt _ | CNat _ | CTez _
             ) as const)) ->
    mk ~loc (Const { ty; const })

  | _ -> mk ~loc (Var (var_of node))


let rec decompile contract =

  let rec decompile_next node =
    let loc = node.loc in
    match node.next with
    | None -> assert false
    | Some node ->
       match node.kind, node.args with
       | N_PRIM "MOD", [arg1; arg2] ->
         mklet node (MatchOption {
             arg = mk ~loc (Apply { prim = Prim_ediv;
                                    args = [arg_of arg1; arg_of arg2] });
             ifnone = mk ~loc (Failwith (unit ~loc));
             some_name = lvar_of node;
             ifsome = mk_get ~loc (mk ~loc (Var (var_of node))) 1;
           })
       | N_PRIM "DIV", [arg1; arg2] ->
         mklet node (MatchOption {
             arg = mk ~loc (Apply { prim = Prim_ediv;
                                    args = [arg_of arg1; arg_of arg2] });
             ifnone = mk ~loc (Failwith (unit ~loc));
             some_name = lvar_of node;
             ifsome = mk_get ~loc (mk ~loc (Var (var_of node))) 0;
           })

       (* ABS : int -> int *)
       | N_ABS, [arg] ->
         mklet node (Apply { prim = Prim_abs; args = [arg_of arg] })

       (* ABS as match%nat *)
       | N_PRIM "ABS", [arg] ->
         let name = var_of arg in
         let v = mk ~loc (Var name) in
         mklet node ( MatchNat {
             arg = arg_of arg;
             plus_name = { nname = name; nloc = arg.loc };
             ifplus = v;
             minus_name = { nname = name; nloc = arg.loc };
             ifminus = v })

       (* UPDATE true -> Set.add *)
       | N_PRIM "UPDATE", [arg1;
                           { kind = N_CONST (_, CBool true) };
                           arg3] ->
         let arg1, arg3 = arg_of arg1, arg_of arg3 in
         mklet node (Apply { prim = Prim_set_add; args = [arg1; arg3] })

       (* UPDATE false -> Set.remove *)
       | N_PRIM "UPDATE", [arg1;
                           { kind = N_CONST (_, CBool false) };
                           arg3] ->
         let arg1, arg3 = arg_of arg1, arg_of arg3 in
         mklet node (Apply { prim = Prim_set_remove; args = [arg1; arg3] })

       (* UPDATE None -> Map.remove *)
       | N_PRIM "UPDATE", [arg1;
                           { kind = N_CONST (_, CNone) };
                           arg3] ->
         let arg1, arg3 = arg_of arg1, arg_of arg3 in
         mklet node (Apply { prim = Prim_map_remove; args = [arg1; arg3] })

       (* UPDATE Some -> Map.add *)
       | N_PRIM "UPDATE", [arg1;
                           { kind = N_CONST (cty, CSome c) };
                           arg3] ->
         let arg1, arg3 = arg_of arg1, arg_of arg3 in
         let v = mk ~loc (Const { ty = cty; const = c }) in
         mklet node (Apply { prim = Prim_map_add; args = [arg1; v; arg3] })

       (* UPDATE Some -> Map.add *)
       | N_PRIM "UPDATE", [arg1;
                           { kind = N_PRIM "SOME"; args = [arg2]};
                           arg3] ->
         let arg1, arg2, arg3 = arg_of arg1, arg_of arg2, arg_of arg3 in
         mklet node (Apply { prim = Prim_map_add;
                             args = [arg1; arg2; arg3] })

       | N_PRIM "PAIR", [{ kind = N_LAMBDA _ } as f; env]  ->
         begin match f.node_name with
         | None -> f.node_name <-node.node_name
         | Some _ -> ()
         end;
         let f = arg_of f in
         let env = arg_of env in
         mklet node (Apply { prim = Prim_tuple; args =  [f; env] })

       | N_PRIM prim, _ ->
          let prim, args =
            match prim, node.args with
            | "CDR", [arg] -> Prim_tuple_get, [arg_of arg; nat_one ~loc]
            | "CAR", [arg] -> Prim_tuple_get, [arg_of arg; nat_zero ~loc]
            | "NEQ", [arg] -> Prim_neq, [arg_of arg; int_zero ~loc]
            | "EQ", [arg] -> Prim_eq, [arg_of arg; int_zero ~loc]
            | "GE", [arg] -> Prim_ge, [arg_of arg; int_zero ~loc]
            | "GT", [arg] -> Prim_gt, [arg_of arg; int_zero ~loc]
            | "LE", [arg] -> Prim_le, [arg_of arg; int_zero ~loc]
            | "LT", [arg] -> Prim_lt, [arg_of arg; int_zero ~loc]
            | "NEQ", [arg1;arg2] -> Prim_neq, [arg_of arg1; arg_of arg2]
            | "EQ", [arg1;arg2] -> Prim_eq, [arg_of arg1; arg_of arg2]
            | "GE", [arg1;arg2] -> Prim_ge, [arg_of arg1; arg_of arg2]
            | "GT", [arg1;arg2] -> Prim_gt, [arg_of arg1; arg_of arg2]
            | "LE", [arg1;arg2] -> Prim_le, [arg_of arg1; arg_of arg2]
            | "LT", [arg1;arg2] -> Prim_lt, [arg_of arg1; arg_of arg2]
            | "NOW", [] -> Prim_now, [unit ~loc]
            | "BALANCE", [] -> Prim_balance, [unit ~loc]
            | "AMOUNT",[] -> Prim_amount, [unit ~loc]
            | "STEPS_TO_QUOTA",[] -> Prim_gas, [unit ~loc]
            | "SOURCE",[] -> Prim_source, [unit ~loc]
            | "SENDER",[] -> Prim_sender, [unit ~loc]
            | "SELF",[] -> Prim_self, [unit ~loc]
            | prim, args ->
               let prim =
                 match prim with
                 | "GET" -> Prim_map_find
                 | "PAIR" -> Prim_tuple
                 | "COMPARE" -> Prim_compare
                 | "CONCAT" -> Prim_concat
                 | "SLICE" -> Prim_slice
                 | "UPDATE" -> Prim_coll_update
                 | "MEM" -> Prim_coll_mem
                 | "SIZE" -> Prim_coll_size
                 | "CONS" -> Prim_Cons
                 | "SUB" -> Prim_sub
                 | "ADD" -> Prim_add
                 | "MUL" -> Prim_mul
                 | "EDIV" -> Prim_ediv
                 | "NEG" -> Prim_neg
                 | "EXEC" -> Prim_exec
                 | "INT" -> Prim_int
                 | "ISNAT" -> Prim_is_nat
                 | "PACK" -> Prim_pack
                 | "BLAKE2B" -> Prim_blake2b
                 | "SHA256" -> Prim_sha256
                 | "SHA512" -> Prim_sha512
                 | "HASH_KEY" -> Prim_hash_key
                 | "CHECK_SIGNATURE" -> Prim_check
                 | "CREATE_ACCOUNT" -> Prim_create_account
                 | "ADDRESS" -> Prim_address
                 | "XOR" -> Prim_xor
                 | "NOT" -> Prim_not
                 | "OR" -> Prim_or
                 | "AND" -> Prim_and
                 | "LSR" -> Prim_lsr
                 | "LSL" -> Prim_lsl
                 | "IMPLICIT_ACCOUNT" -> Prim_default_account
                 | "SET_DELEGATE" -> Prim_set_delegate
                 | "SOME" -> Prim_Some
                 | ins ->
                    LiquidLoc.raise_error
                      "Error: unknown instruction %S"
                      ins
               in
               (prim, List.map arg_of args)
          in
          mklet node (Apply { prim; args })

       | N_PROJ field, [arg] ->
         mklet node (Project { field; record = arg_of arg })

       | N_RECORD labels, args ->
         let fields = try
             List.map2 (fun label arg ->
                 label, arg_of arg
               ) labels args
           with Invalid_argument _ ->
             LiquidLoc.raise_error
               "Error: Annotatated record construct has %d fields, \
                given %d arguments" (List.length labels) (List.length args)
         in
         mklet node (Record fields)

       | N_SETFIELD field, [x; y] ->
         let set_val = arg_of x in
         let record = arg_of y in
         mklet node (SetField { record; field; set_val })

       | N_LEFT right_ty, [arg] ->
         mklet node
           (Constructor {constr = Left right_ty; arg = arg_of arg })

       | N_RIGHT left_ty, [arg] ->
         mklet node
           (Constructor {constr = Right left_ty; arg = arg_of arg })

       | N_CONSTR c, [arg] ->
         mklet node (Constructor {constr = Constr c; arg = arg_of arg })

       | N_CONTRACT ty, [arg] ->
         mklet node (ContractAt { arg = arg_of arg;
                                  c_sig = contract_sig_of_param ty })

       | N_UNPACK ty, [arg] ->
         mklet node (Unpack { arg = arg_of arg; ty })

       | N_END, [ arg ] -> arg_of arg

       | N_FAILWITH, [ arg ] ->
         mk ~loc (Failwith (arg_of arg))

       | N_CONST (ty, cst), [] ->
         let to_tez s = LiquidPrinter.tez_of_mic_mutez @@ Z.of_string s in
         let const = LiquidCheck.check_const_type ~from_mic:true ~to_tez
             loc ty cst
         in
         mklet node (Const { ty; const })

       | N_IF ({ kind = N_IF_END (_, then_node) },
               { kind = N_IF_END (_, else_node) }), [arg] ->
          let desc =
            match then_node.kind, else_node.kind with
            | N_IF_THEN (_), N_IF_ELSE (_) ->
              If { cond = arg_of arg;
                   ifthen = decompile_next then_node;
                   ifelse = decompile_next else_node }
            | N_IF_CONS (_, var0, var1), N_IF_NIL (_) ->
              MatchList { arg = arg_of arg;
                          head_name = lvar_of var0;
                          tail_name = lvar_of var1;
                          ifcons = decompile_next then_node;
                          ifnil = decompile_next else_node }
            | N_IF_NONE (_), N_IF_SOME (_,var0) ->
              MatchOption { arg = arg_of arg;
                            ifnone = decompile_next then_node;
                            some_name = lvar_of var0;
                            ifsome = decompile_next else_node }
            | N_IF_PLUS (_, var0), N_IF_MINUS (_,var1) ->
              MatchNat { arg = arg_of arg;
                         plus_name = lvar_of var0;
                         ifplus = decompile_next then_node;
                         minus_name = lvar_of var1;
                         ifminus = decompile_next else_node }
            | N_IF_LEFT (_, var0), N_IF_RIGHT (_,var1) ->
              MatchVariant { arg = arg_of arg;
                             cases = [
                               CConstr ("Left", [var_of var0]),
                               decompile_next then_node;
                               CConstr ("Right", [var_of var1]),
                               decompile_next else_node
                             ] }
            | _ ->
              LiquidLoc.raise_error
                "Error: not implemented at IF node %s%!"
                (LiquidPrinter.string_of_node then_node)
          in
          mklet node desc
       | N_IF_END _, args -> value_of_args ~loc args

       | N_LOOP (begin_node, end_node), [cond] ->
         let cond_e = arg_of cond in
         let loop_e =
           mk ~loc
             (Loop { arg_name = lvar_of begin_node;
                     body = decompile_next begin_node;
                     arg = value_of_args ~loc begin_node.args })
         in
         let desc = match cond_e.desc with
           | Const { const = CBool true } ->
             loop_e.desc
           | _ ->
             If { cond = arg_of cond;
                  ifthen = loop_e;
                  ifelse = value_of_args ~loc begin_node.args }
          in
          mklet node desc

       | N_LOOP_END (_,_,final_cond), args ->
         mk ~loc (Apply { prim = Prim_tuple;
                          args = [arg_of final_cond;
                                  value_of_args ~loc args] })

       | N_FOLD ({args = rargs} as begin_node, end_node), [arg] ->
         let acc = value_of_args ~loc rargs in
         let desc = match acc.desc with
           | Const { const = CUnit } ->
             Fold { prim = Prim_coll_iter;
                    arg_name = lvar_of begin_node;
                    body = decompile_next begin_node;
                    arg = arg_of arg;
                    acc }
           | _ ->
             Fold { prim = Prim_coll_fold;
                    arg_name = lvar_of begin_node;
                    body = decompile_next begin_node;
                    arg = arg_of arg;
                    acc }
         in
         mklet node desc

       | N_FOLD_END (_,_,_), args -> value_of_args ~loc args

       | N_MAP ({args = []} as begin_node, end_node), [arg] ->
         let desc =
           Map { prim = Prim_coll_map;
                 arg_name = lvar_of begin_node;
                 body = decompile_next begin_node;
                 arg = arg_of arg }
         in
         mklet node desc

       | N_MAP ({args = rargs} as begin_node, end_node), [arg] ->
         let acc = value_of_args ~loc rargs in
         let desc =
           MapFold { prim = Prim_coll_map_fold;
                     arg_name = lvar_of begin_node;
                     body = decompile_next begin_node;
                     arg = arg_of arg;
                     acc }
         in
         mklet node desc

       | N_MAP_END (_,_, res), [] ->
         (* result of .map body *)
         arg_of res

       | N_MAP_END (_,_, res), args ->
         (* result of .map_fold body *)
         mk ~loc (Apply { prim = Prim_tuple;
                          args = [arg_of res; value_of_args ~loc args] })

       | N_LAMBDA (begin_node, end_node, arg_ty, res_ty), [] ->
          let desc = Lambda { arg_name = lvar_of begin_node;
                              arg_ty;
                              body = decompile_next begin_node;
                              ret_ty = Tunit (* res_ty, not yet inferred *) }
          in
          mklet node desc
       | N_LAMBDA_END _, [arg] -> arg_of arg

       | N_TRANSFER, [contract; amount; arg] ->
         mklet node
           (Transfer { contract = arg_of contract;
                       amount = arg_of amount;
                       entry = None;
                       arg = arg_of arg })
       (* TODO *)

       | N_CREATE_CONTRACT contract, args ->
         (* Hack: using annotation to represent contract name *)
         let contract_name =
           match node.node_name with
           | None -> "Contract" ^ string_of_int node.num
           | Some s ->
             try contract_name_of_annot s
             with _ -> "Contract" ^ string_of_int node.num in
         let contract = { (decompile contract) with contract_name } in
         mklet node
           (CreateContract { args = List.map arg_of args; contract })

       | (
         N_LAMBDA_END _
       | N_LAMBDA _
       | N_TRANSFER
       | N_LOOP _
       | N_FOLD _
       | N_MAP _
       | N_IF _
       | N_CONST _
       | N_END
       | N_UNKNOWN _
       | N_LEFT _
       | N_RIGHT _
       | N_CONTRACT _
       | N_UNPACK _
       | N_ABS
       | N_START
       | N_LAMBDA_BEGIN
       | N_VAR _
       | N_FAILWITH
       | N_IF_RESULT (_, _)
       | N_IF_THEN _
       | N_IF_ELSE _
       | N_IF_END_RESULT (_, _, _)
       | N_IF_NONE _
       | N_IF_SOME (_, _)
       | N_IF_NIL _
       | N_IF_CONS (_, _, _)
       | N_IF_LEFT (_, _)
       | N_IF_RIGHT (_, _)
       | N_IF_PLUS (_, _)
       | N_IF_MINUS (_, _)
       | N_LOOP_BEGIN _
       | N_ARG (_, _)
       | N_LOOP_RESULT (_, _, _)
       | N_FOLD_BEGIN _
       | N_FOLD_RESULT (_, _, _)
       | N_MAP_BEGIN _
       | N_MAP_RESULT (_, _, _)
       | N_RESULT (_, _)
       | N_PROJ _
       | N_CONSTR _
       | N_SETFIELD _
       ), _->
         LiquidLoc.raise_error
           "not implemented at node %s%!"
           (LiquidPrinter.string_of_node node)


  and value_of_args ~loc args =
    match args with
    | [] -> mk ~loc (Const { ty = Tunit; const = CUnit })
    | [arg] -> arg_of arg
    | args ->
       mk ~loc (Apply { prim = Prim_tuple; args = List.map arg_of args })

  and mklet node desc =
    let node_liq = mk ?name:node.node_name ~loc:node.loc desc in
    mk ~loc:node.loc
      (Let { bnd_var = lvar_of node;
             inline = false;
             bnd_val = node_liq;
             body = decompile_next node })

  in
  let (begin_node, end_node) = contract.mic_code in
  let code = decompile_next begin_node in
  (*  let code =
    mk (Let ("exp1", noloc,
             mk (Apply(Prim_tuple, noloc,
                       [
                         mk (Apply(Prim_tuple, noloc,
                                   [
                                     mk (Var ("amount", noloc, []));
                                     mk (Var ("parameter", noloc, []));
                            ]));
                         mk (Var ("storage", noloc, []))
                ])), code))
  in
  *)
  { contract_name = "_dummy_";
    storage = contract.mic_storage;
    values = [];
    entries = [{ entry_sig = { entry_name = "main";
                               parameter = contract.mic_parameter;
                               parameter_name = "parameter";
                               storage_name = "storage" };
                 code }]
  }


let decompile contract =
  Hashtbl.reset vars_nums;
  decompile contract
