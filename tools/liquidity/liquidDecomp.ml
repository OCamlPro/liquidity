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

let rec var_of node =
  match node.kind with
  | N_VAR name -> name
  | _ -> Printf.sprintf "exp%d" node.num


let nat_n n = mk (Const (Tnat,CNat (string_of_int n)))
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
          mk (Apply ("Array.get", noloc, [ arg_of if_node; nat_n pos ]))
     end
  | N_LOOP_ARG ({ kind = N_LOOP_BEGIN ( _); args } as begin_node, pos ) ->
     begin
       match pos, List.length args with
       | 0, 1 -> arg_of begin_node
       | _ ->
          mk (Apply ("Array.get", noloc, [ arg_of begin_node; nat_n pos ]))
     end
  | N_LOOP_RESULT (loop_node, begin_node, pos ) ->
     begin
       match pos, List.length begin_node.args with
       | 0, 1 -> arg_of loop_node
       | _ ->
          mk (Apply ("Array.get", noloc, [ arg_of loop_node; nat_n pos ]))
     end
  | N_CONST (ty, ((
                   CUnit | CBool _ | CInt _ | CNat _ | CTez _
             ) as cst)) ->
     mk (Const (ty, cst))

  | _ ->
       mk (Var (var_of node, noloc, []))


let int_zero = mk (Const (Tint,CInt "0"))
let int_one = mk (Const (Tint,CInt "1"))

let unit = mk (Const (Tunit, CUnit))

let decompile contract =

  let rec decompile_next node =
    match node.next with
    | None -> assert false
    | Some node ->
       match node.kind, node.args with
       | N_PRIM "MOD", [arg1; arg2] ->
          mklet node (MatchOption(
                          mk(Apply("/",noloc,[arg_of arg1;arg_of arg2])),
                          noloc,
                          mk(Apply("Current.fail",noloc, [unit])),
                          var_of node,
                          mk(Apply("Array.get",noloc,[
                                       mk(Var(var_of node,noloc,[]));
                                       int_one]))))
       | N_PRIM "DIV", [arg1; arg2] ->
          mklet node (MatchOption(
                          mk(Apply("/",noloc,[arg_of arg1;arg_of arg2])),
                          noloc,
                          mk(Apply("Current.fail",noloc, [unit])),
                          var_of node,
                          mk(Apply("Array.get",noloc,[
                                       mk(Var(var_of node,noloc,[]));
                                       int_zero]))))

       | N_PRIM prim, _ ->
          let prim, args =
            match prim, node.args with
            | "CDR", [arg] -> "Array.get", [arg_of arg; nat_one]
            | "CAR", [arg] -> "Array.get", [arg_of arg; nat_zero]
            | "SOME", [arg] -> "Some", [arg_of arg]
            | "NEQ", [arg] -> "<>", [arg_of arg; int_zero]
            | "EQ", [arg] -> "=", [arg_of arg; int_zero]
            | "GE", [arg] -> ">=", [arg_of arg; int_zero]
            | "GT", [arg] -> ">", [arg_of arg; int_zero]
            | "LE", [arg] -> "<=", [arg_of arg; int_zero]
            | "LT", [arg] -> "<", [arg_of arg; int_zero]
            | "NEQ", [arg1;arg2] -> "<>", [arg_of arg1; arg_of arg2]
            | "EQ", [arg1;arg2] -> "=", [arg_of arg1;  arg_of arg2]
            | "GE", [arg1;arg2] -> ">=", [arg_of arg1; arg_of arg2]
            | "GT", [arg1;arg2] -> ">", [arg_of arg1;  arg_of arg2]
            | "LE", [arg1;arg2] -> "<=", [arg_of arg1; arg_of arg2]
            | "LT", [arg1;arg2] -> "<", [arg_of arg1; arg_of arg2 ]
            | "NOW", [] -> "Current.time", [unit]
            | "BALANCE", [] -> "Current.balance", [unit]
            | "AMOUNT",[] -> "Current.amount", [unit]
            | "STEPS_TO_QUOTA",[] -> "Current.gas", [unit]
            | prim, args ->
               let prim =
                 match prim with
                 | "GET" -> "Map.find"
                 | "PAIR" -> "tuple"
                 | "COMPARE" -> "compare"
                 | "CONCAT" -> "@"
                 | "UPDATE" -> "Coll.update"
                 | "MEM" -> "Coll.mem"
                 | "MAP" -> "Coll.map"
                 | "REDUCE" -> "Coll.reduce"
                 | "SIZE" -> "Coll.size"
                 | "CONS" -> "::"
                 | "SUB" -> "-"
                 | "ADD" -> "+"
                 | "MUL" -> "*"
                 | "EDIV" -> "/"
                 | "EXEC" -> "|>"
                 | "INT" -> "int"
                 | "ABS" -> "abs"
                 | "H" -> "Crypto.hash"
                 | "CHECK_SIGNATURE" -> "Crypto.check"
                 | "CREATE_ACCOUNT" -> "Account.create"
                 | "CREATE_CONTRACT" -> "Contract.create"
                 | "MANAGER" -> "Contract.manager"
                 | "XOR" -> "xor"
                 | "NOT" -> "not"
                 | "OR" -> "or"
                 | "AND" -> "&"
                 | "LSR" -> ">>"
                 | "LSL" -> "<<"
                 | "DEFAULT_ACCOUNT" -> "Account.default"
                 | _ -> prim
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
          mk (Apply ("Current.fail", noloc, [unit]))
       | N_CONST (ty, cst), [] ->
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
            | N_IF_LEFT (_, var0), N_IF_RIGHT (_,var1) ->
               MatchVariant(arg_of arg, noloc,
                            [
                              "Left", [var_of var0],
                              decompile_next then_node;
                              "Right", [var_of var1],
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
          mk (Apply("tuple", noloc,
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
       mk (Apply ("tuple", noloc, List.map arg_of args))

  and mklet node desc =
    mk (Let (var_of node, noloc,
             mk desc, decompile_next node))

  in
  let (begin_node, end_node) = contract.code in
  let code = decompile_next begin_node in
  (*  let code =
    mk (Let ("exp1", noloc,
             mk (Apply("tuple", noloc,
                       [
                         mk (Apply("tuple", noloc,
                                   [
                                     mk (Var ("amount", noloc, []));
                                     mk (Var ("parameter", noloc, []));
                            ]));
                         mk (Var ("storage", noloc, []))
                ])), code))
  in
   *)
  { contract with code }
