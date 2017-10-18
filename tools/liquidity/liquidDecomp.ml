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

let mk desc = { desc; ty = (); bv = StringSet.empty; fail = false }

let const_name_of_datatype = function
  | Tunit -> "u"
  | Tbool -> "b"
  | Tint -> "i"
  | Tnat -> "n"
  | Ttez -> "amount"
  | Tstring -> "s"
  | Ttimestamp -> "time"
  | Tkey -> "key"
  | Tkey_hash -> "key_hash"
  | Tsignature -> "sig"
  | Ttuple _ -> "tuple"
  | Toption _ -> "opt"
  | Tlist _ -> "l"
  | Tset _ -> "s"
  | Tmap _ -> "map"
  | Tcontract _ -> "contrat"
  | Tor _ -> "or"
  | Tlambda _ | Tclosure _  -> "fun"
  | Tfail -> "fail"
  | Ttype _ -> "ty"


let rec var_of node =
  match node.kind with
  | N_VAR name -> name
  | N_PRIM p -> Printf.sprintf "%s%d" (String.lowercase_ascii p) node.num
  | N_IF _ -> Printf.sprintf "branch%d" node.num
  | N_IF_THEN _ -> Printf.sprintf "then%d" node.num
  | N_IF_ELSE _ -> Printf.sprintf "else%d" node.num
  | N_IF_NIL _ -> Printf.sprintf "if_nil%d" node.num
  | N_IF_CONS _ -> Printf.sprintf "if_cons%d" node.num
  | N_IF_LEFT _ -> Printf.sprintf "if_left%d" node.num
  | N_IF_RIGHT _ -> Printf.sprintf "if_right%d" node.num
  | N_IF_PLUS _ -> Printf.sprintf "if_plus%d" node.num
  | N_IF_MINUS _ -> Printf.sprintf "if_minus%d" node.num
  | N_LEFT _ -> Printf.sprintf "left%d" node.num
  | N_RIGHT _ -> Printf.sprintf "right%d" node.num
  | N_TRANSFER _ -> Printf.sprintf "transfer%d" node.num
  | N_TRANSFER_RESULT _ -> Printf.sprintf "transfer_res%d" node.num
  | N_IF_RESULT _ | N_IF_END_RESULT _ | N_LOOP_RESULT _ ->
    Printf.sprintf "res%d" node.num
  | N_SOURCE _ -> Printf.sprintf "source%d" node.num
  | N_FAIL -> Printf.sprintf "fail%d" node.num
  | N_LOOP _ -> Printf.sprintf "loop%d" node.num
  | N_LAMBDA _ -> Printf.sprintf "fun%d" node.num
  | N_LAMBDA_BEGIN -> Printf.sprintf "arg%d" node.num
  | N_LOOP_BEGIN _ ->  Printf.sprintf "loop_arg%d" node.num
  | N_CONST (ty, _) ->
    Printf.sprintf "%s%d" (const_name_of_datatype ty) node.num
  | _ -> Printf.sprintf "exp%d" node.num


let nat_n n = mk (Const (Tnat,CNat (LiquidPrinter.integer_of_int n)))
let nat_zero = nat_n 0
let nat_one = nat_n 1



let rec arg_of node =
  match node.kind with
  | N_IF_END_RESULT ({ kind = N_IF_END (if_node, _);
                       args }, _end_else_node, pos ) ->
     begin
       match pos, List.length args with
       | 0, 1 -> arg_of if_node
       | _ ->
          mk (Apply (Prim_tuple_get, noloc, [ arg_of if_node; nat_n pos ]))
     end
  | N_LOOP_ARG ({ kind = N_LOOP_BEGIN ( _); args } as begin_node, pos ) ->
     begin
       match pos, List.length args with
       | 0, 1 -> arg_of begin_node
       | _ ->
          mk (Apply (Prim_tuple_get, noloc, [ arg_of begin_node; nat_n pos ]))
     end
  | N_LOOP_RESULT (loop_node, begin_node, pos ) ->
     begin
       match pos, List.length begin_node.args with
       | 0, 1 -> arg_of loop_node
       | _ ->
          mk (Apply (Prim_tuple_get, noloc, [ arg_of loop_node; nat_n pos ]))
     end
  | N_CONST (ty, ((
                   CUnit | CBool _ | CInt _ | CNat _ | CTez _
             ) as cst)) ->
     mk (Const (ty, cst))

  | _ ->
       mk (Var (var_of node, noloc, []))


let int_n n = mk (Const (Tint,CInt (LiquidPrinter.integer_of_int n)))
let int_zero = int_n 0
let int_one = int_n 1

let unit = mk (Const (Tunit, CUnit))

let decompile contract =

  let rec decompile_next node =
    match node.next with
    | None -> assert false
    | Some node ->
       match node.kind, node.args with
       | N_PRIM "MOD", [arg1; arg2] ->
          mklet node (MatchOption(
                          mk(Apply(Prim_ediv,noloc,[arg_of arg1;arg_of arg2])),
                          noloc,
                          mk(Apply(Prim_fail,noloc, [unit])),
                          var_of node,
                          mk(Apply(Prim_tuple_get,noloc,[
                                       mk(Var(var_of node,noloc,[]));
                                       int_one]))))
       | N_PRIM "DIV", [arg1; arg2] ->
          mklet node (MatchOption(
                          mk(Apply(Prim_ediv,noloc,[arg_of arg1;arg_of arg2])),
                          noloc,
                          mk(Apply(Prim_fail,noloc, [unit])),
                          var_of node,
                          mk(Apply(Prim_tuple_get,noloc,[
                                       mk(Var(var_of node,noloc,[]));
                                       int_zero]))))

       (* ABS as match%nat *)
       | N_PRIM "ABS", [arg] ->
         let x = var_of arg in
         let vx = mk (Var (x, noloc, [])) in
         mklet node (MatchNat(arg_of arg, noloc, x, vx, x, vx))

       | N_PRIM prim, _ ->
          let prim, args =
            match prim, node.args with
            | "CDR", [arg] -> Prim_tuple_get, [arg_of arg; nat_one]
            | "CAR", [arg] -> Prim_tuple_get, [arg_of arg; nat_zero]
            | "NEQ", [arg] -> Prim_neq, [arg_of arg; int_zero]
            | "EQ", [arg] -> Prim_eq, [arg_of arg; int_zero]
            | "GE", [arg] -> Prim_ge, [arg_of arg; int_zero]
            | "GT", [arg] -> Prim_gt, [arg_of arg; int_zero]
            | "LE", [arg] -> Prim_le, [arg_of arg; int_zero]
            | "LT", [arg] -> Prim_lt, [arg_of arg; int_zero]
            | "NEQ", [arg1;arg2] -> Prim_neq, [arg_of arg1; arg_of arg2]
            | "EQ", [arg1;arg2] -> Prim_eq, [arg_of arg1; arg_of arg2]
            | "GE", [arg1;arg2] -> Prim_ge, [arg_of arg1; arg_of arg2]
            | "GT", [arg1;arg2] -> Prim_gt, [arg_of arg1; arg_of arg2]
            | "LE", [arg1;arg2] -> Prim_le, [arg_of arg1; arg_of arg2]
            | "LT", [arg1;arg2] -> Prim_lt, [arg_of arg1; arg_of arg2]
            | "NOW", [] -> Prim_now, [unit]
            | "BALANCE", [] -> Prim_balance, [unit]
            | "AMOUNT",[] -> Prim_amount, [unit]
            | "STEPS_TO_QUOTA",[] -> Prim_gas, [unit]
            | prim, args ->
               let prim =
                 match prim with
                 | "GET" -> Prim_map_find
                 | "PAIR" -> Prim_tuple
                 | "COMPARE" -> Prim_compare
                 | "CONCAT" -> Prim_concat
                 | "UPDATE" -> Prim_coll_update
                 | "MEM" -> Prim_coll_mem
                 | "MAP" -> Prim_coll_map
                 | "REDUCE" -> Prim_coll_reduce
                 | "SIZE" -> Prim_coll_size
                 | "CONS" -> Prim_Cons
                 | "SUB" -> Prim_sub
                 | "ADD" -> Prim_add
                 | "MUL" -> Prim_mul
                 | "EDIV" -> Prim_ediv
                 | "EXEC" -> Prim_exec
                 | "INT" -> Prim_int
                 | "ABS" -> Prim_abs
                 | "H" -> Prim_hash
                 | "HASH_KEY" -> Prim_hash_key
                 | "CHECK_SIGNATURE" -> Prim_check
                 | "CREATE_ACCOUNT" -> Prim_create_account
                 | "CREATE_CONTRACT" -> Prim_create_contract
                 | "MANAGER" -> Prim_manager
                 | "XOR" -> Prim_xor
                 | "NOT" -> Prim_not
                 | "OR" -> Prim_or
                 | "AND" -> Prim_and
                 | "LSR" -> Prim_lsr
                 | "LSL" -> Prim_lsl
                 | "DEFAULT_ACCOUNT" -> Prim_default_account
                 | "SOME" -> Prim_Some
                 | ins ->
                    LiquidLoc.raise_error
                      "Error: unknown instruction %S"
                      ins
               in
               (prim, List.map arg_of args)
          in
          mklet node (Apply (prim, noloc, args))


       | N_LEFT right_ty, [arg] ->
          mklet node (Constructor(noloc, Left right_ty, arg_of arg))
       | N_RIGHT left_ty, [arg] ->
          mklet node (Constructor(noloc, Right left_ty, arg_of arg))
       | N_SOURCE (from_ty, to_ty), [] ->
          mklet node (Constructor(noloc, Source (from_ty, to_ty), unit))

       | N_END, [ arg ] -> arg_of arg

       | N_FAIL, _ ->
          mk (Apply (Prim_fail, noloc, [unit]))
       | N_CONST (ty, cst), [] ->
          let cst = LiquidCheck.check_const_type
                      ~to_tez:LiquidPrinter.tez_of_mic noloc ty cst
          in
          mklet node (Const (ty, cst))

       | N_IF ({ kind = N_IF_END (_, then_node) },
               { kind = N_IF_END (_, else_node) }), [arg] ->
          let desc =
            match then_node.kind, else_node.kind with
            | N_IF_THEN (_), N_IF_ELSE (_) ->
               If (arg_of arg,
                   decompile_next then_node,
                   decompile_next else_node)
            | N_IF_CONS (_, var0, var1), N_IF_NIL (_) ->
               MatchList(arg_of arg, noloc,
                         var_of var0, var_of var1,
                         decompile_next then_node,
                         decompile_next else_node)
            | N_IF_NONE (_), N_IF_SOME (_,var0) ->
               MatchOption(arg_of arg, noloc,
                           decompile_next then_node,
                           var_of var0,
                           decompile_next else_node)
            | N_IF_PLUS (_, var0), N_IF_MINUS (_,var1) ->
               MatchNat(arg_of arg, noloc,
                        var_of var0,
                        decompile_next then_node,
                        var_of var1,
                        decompile_next else_node)
            | N_IF_LEFT (_, var0), N_IF_RIGHT (_,var1) ->
               MatchVariant(arg_of arg, noloc,
                            [
                              CConstr ("Left", [var_of var0]),
                              decompile_next then_node;
                              CConstr ("Right", [var_of var1]),
                              decompile_next else_node
                           ])
            | _ ->
              LiquidLoc.raise_error
                "Error: not implemented at IF node %s%!"
                (LiquidPrinter.string_of_node then_node)
          in
          mklet node desc
       | N_IF_END _, args -> value_of_args args

       | N_LOOP (begin_node, end_node), [cond] ->
          let desc =
            If (arg_of cond,
                mk (Loop
                      (var_of begin_node, noloc,
                       decompile_next begin_node,
                       value_of_args begin_node.args
                   )),
                value_of_args begin_node.args
               )
          in
          mklet node desc

       | N_LOOP_END (_,_,final_cond), args ->
          mk (Apply(Prim_tuple, noloc,
                    [arg_of final_cond;
                     value_of_args args]))

       | N_LAMBDA (begin_node, end_node, arg_ty, res_ty), [] ->
          let desc = Lambda (var_of begin_node,
                             arg_ty,
                             noloc,
                             decompile_next begin_node,
                             Tunit (* res_ty, not yet inferred *))
          in
          mklet node desc
       | N_LAMBDA_END _, [arg] -> arg_of arg

       | N_TRANSFER (res_storage, result),
         [contract; amount; arg_storage; arg] ->
          mk
            (LetTransfer (var_of res_storage, var_of result,
                          noloc,
                          arg_of contract,
                          arg_of amount,
                          arg_of arg_storage,
                          arg_of arg,
                          decompile_next node))

       | (
         N_LAMBDA_END _
         | N_LAMBDA _
       | N_TRANSFER _
       | N_LOOP _
       | N_IF _
       | N_CONST _
       | N_END
       | N_UNKNOWN _
       | N_SOURCE _
       | N_LEFT _
       | N_RIGHT _

       | N_START
       | N_LAMBDA_BEGIN
       | N_VAR _
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
       | N_TRANSFER_RESULT _
       | N_LOOP_BEGIN _
       | N_LOOP_ARG (_, _)
       | N_LOOP_RESULT (_, _, _)
       ), _->
         LiquidLoc.raise_error
           "not implemented at node %s%!"
           (LiquidPrinter.string_of_node node)


  and value_of_args args =
    match args with
    | [] -> mk (Const (Tunit, CUnit))
    | [arg] -> arg_of arg
    | args ->
       mk (Apply (Prim_tuple, noloc, List.map arg_of args))

  and mklet node desc =
    mk (Let (var_of node, noloc,
             mk desc, decompile_next node))

  in
  let (begin_node, end_node) = contract.code in
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
  { contract with code }
