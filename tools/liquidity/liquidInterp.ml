(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes

let counter = ref 0
let nodes = Hashtbl.create 1000
let node kind args prevs =
  incr counter;
  let num = !counter in
  let node = { num; kind; args; next = None; prevs } in
  List.iter (fun prev ->
    match prev.next with
    | None -> prev.next <- Some node
    | Some _ -> assert false
    ) prevs;
  Hashtbl.add nodes num node;
  node

let bv = StringSet.empty
let mk desc = { desc ; ty = (); bv; fail = false }

let eprint_stack msg stack =
  Printf.eprintf "Stack %s:\n" msg;
  List.iter (fun node ->
      Printf.eprintf "  %s\n" (LiquidPrinter.string_of_node node)
    ) stack;
  Printf.eprintf "%!"

let rec uniformize_stack if_stack stack =
  let if_stack_length = List.length if_stack in
  let stack_length = List.length stack in
  if stack_length = if_stack_length then
    if_stack
  else
    if stack_length < if_stack_length then
      LiquidMisc.list_remove (if_stack_length - stack_length) if_stack
    else
      LiquidMisc.list_make (stack_length - if_stack_length)
                { num = -1; kind = N_UNKNOWN "STACK"; args = [];
                  next = None; prevs = [] }
                @ if_stack

let rec merge_stacks if_stack end_node1 end_node2 stack1 stack2 =
  match stack1, stack2 with
  | [], [] -> []
  | { kind = N_FAIL } :: _, { kind = N_FAIL } :: _ -> stack1
  | { kind = N_FAIL } :: _, _ ->
     begin
       match end_node2 with
       | None -> assert false
       | Some end_node2 ->
          merge_stacks if_stack end_node2 None
                       stack2 (uniformize_stack if_stack stack2)
     end
  | _, { kind = N_FAIL } :: _ ->
     merge_stacks if_stack end_node1 None
                  stack1 (uniformize_stack if_stack stack1)
  | _ ->
     try
       let rec merge i stack1 stack2 =
         if stack1 == stack2 then stack1
         else
           match stack1, stack2 with
           | [], [] -> []
           | s1 :: stack1, s2 :: stack2 ->
              if s1 == s2 then
                s1 :: (merge i stack1 stack2)
              else
                let arg = node (N_IF_END_RESULT (end_node1, end_node2, i))
                               [] [] in

                end_node1.args <- s1 :: end_node1.args;
                (match end_node2 with
                   None -> ()
                 | Some node -> node.args <- s2 :: node.args);
                arg :: (merge (i+1) stack1 stack2)
           | _ -> raise Exit

       in
       let stack = merge 0 stack1 stack2 in
       end_node1.args <- List.rev end_node1.args;
       (match end_node2 with
        | None -> ()
        | Some node -> node.args <- List.rev node.args);
       stack
     with Exit ->
       Printf.eprintf "interp error merging stacks:\n";
       eprint_stack "stack1 " stack1;
       eprint_stack "stack2 " stack2;
       raise Error

let interp contract =

  counter := 0;
  Hashtbl.clear nodes;

  let rec decompile_seq stack (seq : node) code =
    match code with
    | [] -> (stack, seq)
    | ins :: code ->
       let stack, seq = decompile stack seq ins in
       decompile_seq stack seq code

  and decompile stack (seq : node) ins =
    match ins, stack with
    | SEQ exprs, _ ->
       decompile_seq stack seq exprs

(* If-expressions followed by FAIL: we create IF statements where the
   FAIL is the neighbour of an empty sequence, so that the comparison
   of stacks is meaningless. Of course, this assume that a sequence
   of instructions finishing with a FAIL is forbidden. Be careful to
   avoid infinite loops here...
*)

    | IF (SEQ [FAIL], SEQ ( (_ :: _) as ifelse )),_ ->
       decompile stack seq
                 (SEQ (IF (SEQ [FAIL], SEQ []) :: ifelse))
    | IF (SEQ ( (_ :: _) as ifthen ), SEQ [FAIL]),_ ->
       decompile stack seq
                 (SEQ (IF (SEQ [], SEQ [FAIL]) :: ifthen))
    | IF_NONE (SEQ ( (_::_) as ifthen), SEQ [FAIL]),_ ->
       decompile stack seq
                 (SEQ (IF_NONE (SEQ [], SEQ [FAIL]) :: ifthen))
    | IF_CONS (SEQ [FAIL], SEQ ( (_::_) as ifelse ) ),_ ->
       decompile stack seq
                 (SEQ (IF_CONS (SEQ [FAIL], SEQ []) :: ifelse))

    | IF (ifthen, ifelse), x :: stack ->
       let if_node = node (N_UNKNOWN "IF") [x] [seq] in

       let then_node = node (N_IF_THEN if_node) [] [] in
       let then_stack = stack in

       let else_node = node (N_IF_ELSE if_node) [] [] in
       let else_stack = stack in

       decompile_if if_node stack
                    ifthen then_node then_stack
                    ifelse else_node else_stack

    | IF_NONE (ifthen, ifelse), x :: stack ->
       let if_node = node (N_UNKNOWN "IF_NONE") [x] [seq] in

       let then_node = node (N_IF_NONE if_node) [] [] in
       let then_stack = stack in

       let var0 = node (N_IF_RESULT (if_node, 0)) [] [] in
       let else_node = node (N_IF_SOME (if_node, var0)) [] [] in
       let else_stack = var0 :: stack in

       decompile_if if_node stack
                    ifthen then_node then_stack
                    ifelse else_node else_stack

    | IF_LEFT (ifthen, ifelse), x :: stack ->
       let if_node = node (N_UNKNOWN "IF_LEFT") [x] [seq] in

       let var0 = node (N_IF_RESULT (if_node,0)) [] [] in
       let then_node = node (N_IF_LEFT (if_node, var0)) [] [] in
       let then_stack = var0 :: stack in

       let var0 = node (N_IF_RESULT (if_node,0)) [] [] in
       let else_node = node (N_IF_RIGHT (if_node, var0)) [] [] in
       let else_stack = var0 :: stack in

       decompile_if if_node stack
                    ifthen then_node then_stack
                    ifelse else_node else_stack


    | IF_CONS (ifthen, ifelse), x :: stack ->
       let if_node = node (N_UNKNOWN "IF_CONS") [x] [seq] in

       let var0 = node (N_IF_RESULT (if_node,0)) [] [] in
       let var1 = node (N_IF_RESULT (if_node, 1)) [] [] in
       let then_node = node (N_IF_CONS (if_node, var0, var1)) [] [] in
       let then_stack = var0 :: var1 :: stack in

       let else_node = node (N_IF_NIL if_node) [] [] in
       let else_stack = stack in

       decompile_if if_node stack
                    ifthen then_node then_stack
                    ifelse else_node else_stack


    | LOOP code, x :: prev_stack ->
       let loop_node = node (N_UNKNOWN "LOOP") [x] [seq] in
       let begin_node = node (N_LOOP_BEGIN loop_node) [] [] in

       let pseudo_node = node (N_UNKNOWN "LOOP") [] [] in
       begin match decompile prev_stack pseudo_node code with
       | [],_ -> assert false
       | x :: end_stack,_ ->

          let rec merge i stack1 stack2 =
            if stack1 == stack2 then stack1
            else
              match stack1, stack2 with
              | [], [] -> []
              | { kind = N_FAIL } :: _, _ -> stack2
              | _, { kind = N_FAIL } :: _ -> stack1
              | s1 :: stack1, s2 :: stack2 ->
                 if s1 == s2 then
                   s1 :: (merge i stack1 stack2)
                 else
                   let arg = node (N_LOOP_ARG (begin_node,i)) [] [] in
                   begin_node.args <- s1 :: begin_node.args;
                   arg :: (merge (i+1) stack1 stack2)
              | _ -> assert false
          in
          let stack = merge 0 prev_stack end_stack in

          begin_node.args <- List.rev begin_node.args;

          match decompile stack begin_node code with
          | [], _ -> assert false
          | x :: end_stack, loop_seq ->

             let end_node = node (N_LOOP_END (loop_node, begin_node, x)) []
                                 [ loop_seq ] in

             let rec merge i stack1 stack2 =
               if stack1 == stack2 then stack1
               else
                 match stack1, stack2 with
                 | [], [] -> []
                 | { kind = N_FAIL } :: _, _ -> stack2
                 | _, { kind = N_FAIL } :: _ -> stack1
                 | { kind = N_LOOP_ARG (n, i) }:: stack1, s2 :: stack2
                      when n == begin_node
                   ->
                    let arg = node (N_LOOP_RESULT (loop_node, begin_node, i)) [] [] in
                    end_node.args <- s2 :: end_node.args;
                    arg :: merge (i+1) stack1 stack2
                 | s1 :: stack1, s2 :: stack2 ->
                    assert (s1 == s2);
                    s1 :: (merge i stack1 stack2)
                 | _ -> assert false
             in

             end_node.args <- List.rev end_node.args;
             loop_node.kind <- N_LOOP (begin_node, end_node);
             let stack = merge 0 stack end_stack in
             stack, loop_node
       end


    | LAMBDA (arg_ty, res_ty, code), stack ->

       let begin_node = node N_LAMBDA_BEGIN [] [] in
       let lambda_stack, lambda_seq =
         decompile [ begin_node ] begin_node code in
       begin match lambda_stack with
       | [] | _ :: _ :: _ -> assert false
       | [res_node] ->
          let end_node = node (N_LAMBDA_END begin_node) [res_node]
                              [lambda_seq] in
          let lambda_node = node (N_LAMBDA (begin_node, end_node, arg_ty, res_ty)) [] [seq]
          in
          lambda_node :: stack, lambda_node
       end

    (* Stack modifications *)
    | DUP 1, v :: _ ->
       v :: stack, seq
    | DROP, _ :: stack ->
       stack, seq
    | DIP (1, code), x :: stack ->
       let stack, seq = decompile stack seq code in
       x :: stack, seq
    | SWAP, x :: y :: stack ->
       y :: x :: stack, seq



    (* Primitives *)

    | PUSH (ty, cst), stack ->
       let x = node (N_CONST (ty, cst)) [] [seq] in
       x :: stack, x
    | FAIL, _ ->
       let x = node N_FAIL [] [seq] in
       [x], x
    | TRANSFER_TOKENS,
      arg :: amount :: contract :: arg_storage :: [] ->
       let res_storage = node (N_VAR "storage") [] [] in
       let result = node (N_TRANSFER_RESULT 1) [] [] in
       let x = node (N_TRANSFER (res_storage, result))
                    [contract; amount; arg_storage; arg ] [seq] in
       [ result ; res_storage ], x

    | SOURCE (arg_ty, res_ty), stack -> (* TODO : keep types too ! *)
       let x = node (N_SOURCE (arg_ty, res_ty)) [] [seq] in
       x :: stack, x
    | NOW, stack ->
       let x = node (N_PRIM "NOW") [] [seq] in
       x :: stack, x
    | BALANCE, stack ->
       let x = node (N_PRIM "BALANCE") [] [seq] in
       x :: stack, x
    | AMOUNT, stack ->
       let x = node (N_PRIM "AMOUNT") [] [seq] in
       x :: stack, x

    | DEFAULT_ACCOUNT, key :: stack ->
       let x = node (N_PRIM "DEFAULT_ACCOUNT") [key] [seq] in
       x :: stack, x


    | MANAGER, x :: stack ->
       let x = node (N_PRIM "MANAGER") [x] [seq] in
       x :: stack, x
    | H, x :: stack ->
       let x = node (N_PRIM "H") [x] [seq] in
       x :: stack, x
    | SOME, x :: stack ->
       let x = node (N_PRIM "SOME") [x] [seq] in
       x :: stack, x
    | LEFT right_ty, x :: stack -> (* TODO : keep types too ! *)
       let x = node (N_LEFT right_ty) [x] [seq] in
       x :: stack, x
    | RIGHT left_ty, x :: stack -> (* TODO : keep types too ! *)
       let x = node (N_RIGHT left_ty) [x] [seq] in
       x :: stack, x
    | INT, x :: stack ->
       let x = node (N_PRIM "INT") [x] [seq] in
       x :: stack, x
    | ABS, x :: stack ->
       let x = node (N_PRIM "ABS") [x] [seq] in
       x :: stack, x
    | CAR, { kind = N_PRIM "PAIR"; args = [x;_] } :: stack ->
       x :: stack, seq
    | CDR, { kind = N_PRIM "PAIR"; args = [_;x] } :: stack ->
       x :: stack, seq
    | CAR, x :: stack ->
       let x = node (N_PRIM "CAR") [x] [seq] in
       x :: stack, x
    | CDR, x :: stack ->
       let x = node (N_PRIM "CDR") [x] [seq] in
       x :: stack, x

    | NEQ, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node (N_PRIM "NEQ") [x;y] [seq] in
       x :: stack, x
    | NEQ, x :: stack ->
       let x = node (N_PRIM "NEQ") [x] [seq] in
       x :: stack, x
    | EQ, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node (N_PRIM "EQ") [x;y] [seq] in
       x :: stack, x
    | EQ, x :: stack ->
       let x = node (N_PRIM "EQ") [x] [seq] in
       x :: stack, x
    | LE, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node (N_PRIM "LE") [x;y] [seq] in
       x :: stack, x
    | LE, x :: stack ->
       let x = node (N_PRIM "LE") [x] [seq] in
       x :: stack, x
    | LT, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node (N_PRIM "LT") [x;y] [seq] in
       x :: stack, x
    | LT, x :: stack ->
       let x = node (N_PRIM "LT") [x] [seq] in
       x :: stack, x
    | GE, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node (N_PRIM "GE") [x;y] [seq] in
       x :: stack, x
    | GE, x :: stack ->
       let x = node (N_PRIM "GE") [x] [seq] in
       x :: stack, x
    | GT, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node (N_PRIM "GT") [x;y] [seq] in
       x :: stack, x
    | GT, x :: stack ->
       let x = node (N_PRIM "GT") [x] [seq] in
       x :: stack, x

    | CHECK_SIGNATURE, x :: y :: stack ->
       let x = node (N_PRIM "CHECK_SIGNATURE") [x; y] [seq] in
       x :: stack, x
    | MOD, x :: y :: stack ->
       let x = node (N_PRIM "MOD") [x; y] [seq] in
       x :: stack, x
    | DIV, x :: y :: stack ->
       let x = node (N_PRIM "DIV") [x; y] [seq] in
       x :: stack, x
    | CONS, x :: y :: stack ->
       let x = node (N_PRIM "CONS") [x; y] [seq] in
       x :: stack, x
    | MEM, x :: y :: stack ->
       let x = node (N_PRIM "MEM") [x;y] [seq] in
       x :: stack, x
    | CONCAT, x :: y :: stack ->
       let x = node (N_PRIM "CONCAT") [x;y] [seq] in
       x :: stack, x
    | OR, x :: y :: stack ->
       let x = node (N_PRIM "OR") [x;y] [seq] in
       x :: stack, x
    | MUL, x :: y :: stack ->
       let x = node (N_PRIM "MUL") [x;y] [seq] in
       x :: stack, x
    | EDIV, x :: y :: stack ->
       let x = node (N_PRIM "EDIV") [x;y] [seq] in
       x :: stack, x
    | ADD, x :: y :: stack ->
       let x = node (N_PRIM "ADD") [x;y] [seq] in
       x :: stack, x
    | SUB, x :: y :: stack ->
       let x = node (N_PRIM "SUB") [x;y] [seq] in
       x :: stack, x
    | PAIR, x :: y :: stack ->
       let x = node (N_PRIM "PAIR") [x;y] [seq] in
       x :: stack, x

    | COMPARE, x ::  { kind = N_CONST (Tint,CInt "0")} :: stack->
       x :: stack, seq

    | COMPARE, x :: y :: stack ->
       let x = node (N_PRIM "COMPARE") [x;y] [seq] in
       x :: stack, x
    | GET, x :: y :: stack ->
       let x = node (N_PRIM "GET") [x;y] [seq] in
       x :: stack, x
    | EXEC, x :: y :: stack ->
       let x = node (N_PRIM "EXEC") [x;y] [seq] in
       x :: stack, x

    | UPDATE, x :: y :: z :: stack ->
       let x = node (N_PRIM "UPDATE") [x;y;z] [seq] in
       x :: stack, x

    | REDUCE, x :: y :: z :: stack ->
       let x = node (N_PRIM "REDUCE") [x;y;z] [seq] in
       x :: stack, x

    | _ ->
       let ins = LiquidEmit.emit_code ins in
       let s = LiquidPrinter.Michelson.string_of_code ins in
       Printf.eprintf "Error while decompiling:\n %s\n%!" s;
       raise Error


  and decompile_if if_node if_stack
                   ifthen then_node then_stack
                   ifelse else_node else_stack =

    let then_stack, then_seq =
      decompile then_stack then_node ifthen
    in
    let else_stack, else_seq =
      decompile else_stack else_node ifelse
    in

    let then_end_node = node (N_IF_END (if_node, then_node)) []
                             [ then_seq ] in
    let else_end_node = node (N_IF_END (if_node, else_node)) []
                             [ else_seq ] in
    let stack = merge_stacks if_stack
                  then_end_node (Some else_end_node)
                  then_stack else_stack in
    if_node.kind <- N_IF (then_end_node, else_end_node);

    stack, if_node

  in
  let start_node = node N_START [] [] in

  let initial_code = match contract.code with
    | SEQ code -> SEQ (PAIR :: code)
    | _ -> assert false
  in

  let initial_stack = [
    node (N_VAR "parameter") [] [];
    node (N_VAR "storage") [] [];
    ] in

  let stack, seq = decompile initial_stack start_node initial_code in
  let end_node = match stack with
      [ arg ] -> node N_END [arg] [seq]
    | _ -> assert false
  in
  let code = (start_node, end_node) in
  { contract with code }
