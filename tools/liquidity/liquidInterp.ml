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
let node loc kind args prevs =
  incr counter;
  let num = !counter in
  let node = { num; loc; kind; args; next = None; prevs } in
  List.iter (fun prev ->
    match prev.next with
    | None -> prev.next <- Some node
    | Some _ -> assert false
    ) prevs;
  Hashtbl.add nodes num node;
  node

let fprint_stack msg fmt stack =
  Format.fprintf fmt "Stack %s:\n" msg;
  List.iter (fun node ->
      Format.fprintf fmt "  %s\n" (LiquidPrinter.string_of_node node)
    ) stack;
  Format.fprintf fmt "%!"

let eprintf_stack msg = fprint_stack msg Format.err_formatter

let sprint_stack msg stack =
  let fmt = Format.str_formatter in
  fprint_stack msg fmt stack;
  Format.flush_str_formatter ()

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
        { num = -1; loc = LiquidLoc.noloc;
          kind = N_UNKNOWN "STACK"; args = [];
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
                let arg = node end_node1.loc
                    (N_IF_END_RESULT (end_node1, end_node2, i))
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
       LiquidLoc.raise_error ~loc:end_node1.loc
         "interp error merging stacks:\n%a%a"
         (fprint_stack "stack1 ") stack1
         (fprint_stack "stack2 ") stack2

let interp contract =

  counter := 0;
  Hashtbl.clear nodes;

  let rec decompile_seq stack (seq : node) code =
    match code, stack with
    | [], _ -> (stack, seq)

    (* Special case for abs *)
    | {ins=ABS; loc} :: {ins=INT} :: code, x :: stack ->
      let n = node loc N_ABS [x] [seq] in
      let stack, seq = n :: stack, n in
      decompile_seq stack seq code

    (* Special case for match%nat *)
    | {ins=DUP 1; loc} ::
      {ins=ABS} ::
      {ins=SWAP} ::
      {ins=GE} ::
      {ins=IF(ifplus, ifminus)} ::
      code,
      x :: stack ->
      let if_node = node loc (N_UNKNOWN "IF_PLUS") [x] [seq] in

      let var0 = node ifplus.loc (N_IF_RESULT (if_node, 0)) [] [] in
      let then_node = node ifplus.loc (N_IF_PLUS (if_node, var0)) [] [] in
      let then_stack = var0 :: stack in

      let var0 = node ifminus.loc (N_IF_RESULT (if_node, 0)) [] [] in
      let else_node = node ifminus.loc (N_IF_MINUS (if_node, var0)) [] [] in
      let else_stack = var0 :: stack in

      let stack, seq =
        decompile_if if_node stack
          ifplus then_node then_stack
          ifminus else_node else_stack
      in
      decompile_seq stack seq code

    | ins :: code, _ ->
       let stack, seq = decompile stack seq ins in
       decompile_seq stack seq code

  and decompile stack (seq : node) ins =
    match ins.ins, stack with
    | SEQ exprs, _ ->
       decompile_seq stack seq exprs

(* If-expressions followed by FAIL: we create IF statements where the
   FAIL is the neighbour of an empty sequence, so that the comparison
   of stacks is meaningless. Of course, this assume that a sequence
   of instructions finishing with a FAIL is forbidden. Be careful to
   avoid infinite loops here...
*)

    | IF ({ins=SEQ [{ins=FAIL}]} as seq_fail,
          {ins=SEQ ( (_ :: _) as ifelse )}),_ ->
      decompile stack seq
        (mic_loc ins.loc
           (SEQ (mic_loc ins.loc
                   (IF (seq_fail,
                        mic_loc ins.loc (SEQ []))) :: ifelse)))
    | IF ({ins=SEQ ( (_ :: _) as ifthen )},
          ({ins=SEQ [{ins=FAIL}]} as seq_fail)),_ ->
      decompile stack seq
        (mic_loc ins.loc
           (SEQ (mic_loc ins.loc
                   (IF (mic_loc ins.loc (SEQ []),
                        seq_fail)) :: ifthen)))
    | IF_NONE ({ins=SEQ ( (_::_) as ifthen)},
               ({ins=SEQ [{ins=FAIL}]} as seq_fail)),_ ->
      decompile stack seq
        (mic_loc ins.loc
           (SEQ (mic_loc ins.loc
                   (IF_NONE (mic_loc ins.loc (SEQ []),
                             seq_fail)) :: ifthen)))
    | IF_CONS ({ins=SEQ [{ins=FAIL}]} as seq_fail,
               {ins=SEQ ( (_::_) as ifelse )} ),_ ->
      decompile stack seq
        (mic_loc ins.loc
           (SEQ (mic_loc ins.loc
                   (IF_CONS (seq_fail,
                             mic_loc ins.loc (SEQ []))) :: ifelse)))

    | IF (ifthen, ifelse), x :: stack ->
       let if_node = node ins.loc (N_UNKNOWN "IF") [x] [seq] in

       let then_node = node ifthen.loc (N_IF_THEN if_node) [] [] in
       let then_stack = stack in

       let else_node = node ifelse.loc (N_IF_ELSE if_node) [] [] in
       let else_stack = stack in

       decompile_if if_node stack
                    ifthen then_node then_stack
                    ifelse else_node else_stack

    | IF_NONE (ifthen, ifelse), x :: stack ->
       let if_node = node ins.loc (N_UNKNOWN "IF_NONE") [x] [seq] in

       let then_node = node ifthen.loc (N_IF_NONE if_node) [] [] in
       let then_stack = stack in

       let var0 = node ifelse.loc (N_IF_RESULT (if_node, 0)) [] [] in
       let else_node = node ifelse.loc (N_IF_SOME (if_node, var0)) [] [] in
       let else_stack = var0 :: stack in

       decompile_if if_node stack
                    ifthen then_node then_stack
                    ifelse else_node else_stack

    | IF_LEFT (ifthen, ifelse), x :: stack ->
       let if_node = node ins.loc (N_UNKNOWN "IF_LEFT") [x] [seq] in

       let var0 = node ifthen.loc (N_IF_RESULT (if_node,0)) [] [] in
       let then_node = node ifthen.loc (N_IF_LEFT (if_node, var0)) [] [] in
       let then_stack = var0 :: stack in

       let var0 = node ifelse.loc (N_IF_RESULT (if_node,0)) [] [] in
       let else_node = node ifelse.loc (N_IF_RIGHT (if_node, var0)) [] [] in
       let else_stack = var0 :: stack in

       decompile_if if_node stack
                    ifthen then_node then_stack
                    ifelse else_node else_stack


    | IF_CONS (ifthen, ifelse), x :: stack ->
       let if_node = node ins.loc (N_UNKNOWN "IF_CONS") [x] [seq] in

       let var0 = node ifthen.loc (N_IF_RESULT (if_node,0)) [] [] in
       let var1 = node ifthen.loc (N_IF_RESULT (if_node, 1)) [] [] in
       let then_node =
         node ifthen.loc (N_IF_CONS (if_node, var0, var1)) [] [] in
       let then_stack = var0 :: var1 :: stack in

       let else_node = node ifelse.loc (N_IF_NIL if_node) [] [] in
       let else_stack = stack in

       decompile_if if_node stack
                    ifthen then_node then_stack
                    ifelse else_node else_stack


    | LOOP code, x :: prev_stack ->
       let loop_node = node ins.loc (N_UNKNOWN "LOOP") [x] [seq] in
       let begin_node = node code.loc (N_LOOP_BEGIN loop_node) [] [] in

       let pseudo_node = node ins.loc (N_UNKNOWN "LOOP") [] [] in
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
                   let arg = node code.loc (N_LOOP_ARG (begin_node,i)) [] [] in
                   begin_node.args <- s1 :: begin_node.args;
                   arg :: (merge (i+1) stack1 stack2)
              | _ -> assert false
          in
          let stack = merge 0 prev_stack end_stack in

          begin_node.args <- List.rev begin_node.args;

          match decompile stack begin_node code with
          | [], _ -> assert false
          | x :: end_stack, loop_seq ->

             let end_node = node x.loc
                 (N_LOOP_END (loop_node, begin_node, x)) []
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
                   let arg = node ins.loc
                       (N_LOOP_RESULT (loop_node, begin_node, i)) [] [] in
                   end_node.args <- s2 :: end_node.args;
                   arg :: merge (i+1) stack1 stack2
                 | s1 :: stack1, s2 :: stack2 ->
                   assert (s1 == s2);
                   s1 :: (merge i stack1 stack2)
                 | _ -> assert false
             in

             let stack = merge 0 stack end_stack in
             end_node.args <- List.rev end_node.args;
             loop_node.kind <- N_LOOP (begin_node, end_node);
             stack, loop_node
       end


    | LAMBDA (arg_ty, res_ty, code), stack ->

       let begin_node = node ins.loc N_LAMBDA_BEGIN [] [] in
       let lambda_stack, lambda_seq =
         decompile [ begin_node ] begin_node code in
       begin match lambda_stack with
       | [] | _ :: _ :: _ -> assert false
       | [res_node] ->
          let end_node = node res_node.loc (N_LAMBDA_END begin_node) [res_node]
                              [lambda_seq] in
          let lambda_node = node ins.loc
              (N_LAMBDA (begin_node, end_node, arg_ty, res_ty)) [] [seq]
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
       let x = node ins.loc (N_CONST (ty, cst)) [] [seq] in
       x :: stack, x
    | FAIL, _ ->
       let x = node ins.loc N_FAIL [] [seq] in
       [x], x
    | TRANSFER_TOKENS,
      arg :: amount :: contract :: arg_storage :: [] ->
       let res_storage = node ins.loc (N_VAR "storage") [] [] in
       let result = node ins.loc (N_TRANSFER_RESULT 1) [] [] in
       let x = node ins.loc (N_TRANSFER (res_storage, result))
                    [contract; amount; arg_storage; arg ] [seq] in
       [ result ; res_storage ], x

    | SOURCE (arg_ty, res_ty), stack -> (* TODO : keep types too ! *)
       let x = node ins.loc (N_SOURCE (arg_ty, res_ty)) [] [seq] in
       x :: stack, x
    | NOW, stack ->
       let x = node ins.loc (N_PRIM "NOW") [] [seq] in
       x :: stack, x
    | BALANCE, stack ->
       let x = node ins.loc (N_PRIM "BALANCE") [] [seq] in
       x :: stack, x
    | AMOUNT, stack ->
       let x = node ins.loc (N_PRIM "AMOUNT") [] [seq] in
       x :: stack, x

    | DEFAULT_ACCOUNT, key :: stack ->
       let x = node ins.loc (N_PRIM "DEFAULT_ACCOUNT") [key] [seq] in
       x :: stack, x

    | MANAGER, x :: stack ->
       let x = node ins.loc (N_PRIM "MANAGER") [x] [seq] in
       x :: stack, x
    | H, x :: stack ->
       let x = node ins.loc (N_PRIM "H") [x] [seq] in
       x :: stack, x
    | HASH_KEY, x :: stack ->
       let x = node ins.loc (N_PRIM "HASH_KEY") [x] [seq] in
       x :: stack, x
    | SOME, x :: stack ->
       let x = node ins.loc (N_PRIM "SOME") [x] [seq] in
       x :: stack, x
    | LEFT right_ty, x :: stack -> (* TODO : keep types too ! *)
       let x = node ins.loc (N_LEFT right_ty) [x] [seq] in
       x :: stack, x
    | RIGHT left_ty, x :: stack -> (* TODO : keep types too ! *)
       let x = node ins.loc (N_RIGHT left_ty) [x] [seq] in
       x :: stack, x
    | INT, x :: stack ->
       let x = node ins.loc (N_PRIM "INT") [x] [seq] in
       x :: stack, x
    | ABS, x :: stack ->
       let x = node ins.loc (N_PRIM "ABS") [x] [seq] in
       x :: stack, x
    | CAR, { kind = N_PRIM "PAIR"; args = [x;_] } :: stack ->
       x :: stack, seq
    | CDR, { kind = N_PRIM "PAIR"; args = [_;x] } :: stack ->
       x :: stack, seq
    | CAR, x :: stack ->
       let x = node ins.loc (N_PRIM "CAR") [x] [seq] in
       x :: stack, x
    | CDR, x :: stack ->
       let x = node ins.loc (N_PRIM "CDR") [x] [seq] in
       x :: stack, x

    | NEQ, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node ins.loc (N_PRIM "NEQ") [x;y] [seq] in
       x :: stack, x
    | NEQ, x :: stack ->
       let x = node ins.loc (N_PRIM "NEQ") [x] [seq] in
       x :: stack, x
    | EQ, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node ins.loc (N_PRIM "EQ") [x;y] [seq] in
       x :: stack, x
    | EQ, x :: stack ->
       let x = node ins.loc (N_PRIM "EQ") [x] [seq] in
       x :: stack, x
    | LE, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node ins.loc (N_PRIM "LE") [x;y] [seq] in
       x :: stack, x
    | LE, x :: stack ->
       let x = node ins.loc (N_PRIM "LE") [x] [seq] in
       x :: stack, x
    | LT, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node ins.loc (N_PRIM "LT") [x;y] [seq] in
       x :: stack, x
    | LT, x :: stack ->
       let x = node ins.loc (N_PRIM "LT") [x] [seq] in
       x :: stack, x
    | GE, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node ins.loc (N_PRIM "GE") [x;y] [seq] in
       x :: stack, x
    | GE, x :: stack ->
       let x = node ins.loc (N_PRIM "GE") [x] [seq] in
       x :: stack, x
    | GT, { kind = N_PRIM "COMPARE"; args = [x;y] } :: stack->
       let x = node ins.loc (N_PRIM "GT") [x;y] [seq] in
       x :: stack, x
    | GT, x :: stack ->
       let x = node ins.loc (N_PRIM "GT") [x] [seq] in
       x :: stack, x

    | CHECK_SIGNATURE, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "CHECK_SIGNATURE") [x; y] [seq] in
       x :: stack, x
    | MOD, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "MOD") [x; y] [seq] in
       x :: stack, x
    | DIV, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "DIV") [x; y] [seq] in
       x :: stack, x
    | CONS, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "CONS") [x; y] [seq] in
       x :: stack, x
    | MEM, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "MEM") [x;y] [seq] in
       x :: stack, x
    | CONCAT, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "CONCAT") [x;y] [seq] in
       x :: stack, x
    | OR, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "OR") [x;y] [seq] in
       x :: stack, x
    | MUL, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "MUL") [x;y] [seq] in
       x :: stack, x
    | EDIV, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "EDIV") [x;y] [seq] in
       x :: stack, x
    | ADD, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "ADD") [x;y] [seq] in
       x :: stack, x
    | SUB, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "SUB") [x;y] [seq] in
       x :: stack, x
    | PAIR, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "PAIR") [x;y] [seq] in
       x :: stack, x

    | COMPARE, x ::  { kind = N_CONST (Tint,CInt n)} :: stack
         when LiquidPrinter.int_of_integer n = 0
      ->
       x :: stack, seq

    | COMPARE, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "COMPARE") [x;y] [seq] in
       x :: stack, x
    | GET, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "GET") [x;y] [seq] in
       x :: stack, x
    | EXEC, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "EXEC") [x;y] [seq] in
       x :: stack, x

    | UPDATE, x :: y :: z :: stack ->
       let x = node ins.loc (N_PRIM "UPDATE") [x;y;z] [seq] in
       x :: stack, x

    | REDUCE, x :: y :: z :: stack ->
       let x = node ins.loc (N_PRIM "REDUCE") [x;y;z] [seq] in
       x :: stack, x

    | MAP, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "MAP") [x;y] [seq] in
       x :: stack, x

    | SIZE, x :: stack ->
       let x = node ins.loc (N_PRIM "SIZE") [x] [seq] in
       x :: stack, x

    | AND, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "AND") [x; y] [seq] in
       x :: stack, x

    | XOR, x :: y :: stack ->
       let x = node ins.loc (N_PRIM "XOR") [x; y] [seq] in
       x :: stack, x

    | NOT, x :: stack ->
       let x = node ins.loc (N_PRIM "NOT") [x] [seq] in
       x :: stack, x

    | STEPS_TO_QUOTA, stack ->
       let x = node ins.loc (N_PRIM "STEPS_TO_QUOTA") [] [seq] in
       x :: stack, x

    | CREATE_ACCOUNT, manager :: delegate :: delegatable :: amount :: stack ->
       let x = node ins.loc (N_PRIM "CREATE_ACCOUNT")
                    [manager; delegate; delegatable; amount] [seq] in
       x :: stack, x

    | CREATE_CONTRACT, manager :: delegate ::
                         spendable :: delegatable ::
                           amount :: contract :: storage :: stack ->
       let x = node ins.loc (N_PRIM "CREATE_CONTRACT")
                    [manager; delegate;
                     spendable; delegatable;
                     amount; contract; storage] [seq] in
       x :: stack, x

    | _ ->
      (* let ins = LiquidEmit.emit_code ins in *)
      let s = LiquidPrinter.Michelson.string_of_loc_michelson ins in
      LiquidLoc.raise_error ~loc:ins.loc "Error while decompiling:\n %s%!" s

  and decompile_if if_node if_stack
                   ifthen then_node then_stack
                   ifelse else_node else_stack =

    let then_stack, then_seq =
      decompile then_stack then_node ifthen
    in
    let else_stack, else_seq =
      decompile else_stack else_node ifelse
    in

    let then_end_node = node ifthen.loc (N_IF_END (if_node, then_node)) []
                             [ then_seq ] in
    let else_end_node = node ifelse.loc (N_IF_END (if_node, else_node)) []
                             [ else_seq ] in
    let stack = merge_stacks if_stack
                  then_end_node (Some else_end_node)
                  then_stack else_stack in
    if_node.kind <- N_IF (then_end_node, else_end_node);

    stack, if_node

  in

  let initial_code = match contract.code.ins with
    | SEQ code -> mic_loc contract.code.loc
                    (SEQ (mic_loc contract.code.loc PAIR :: code))
    | _ -> assert false
  in

  let start_node = node contract.code.loc N_START [] [] in

  let initial_stack = [
    node contract.code.loc (N_VAR "parameter") [] [];
    node contract.code.loc (N_VAR "storage") [] [];
    ] in

  let stack, seq = decompile initial_stack start_node initial_code in
  let end_node = match stack with
      [ arg ] -> node arg.loc N_END [arg] [seq]
    | _ -> assert false
  in
  let code = (start_node, end_node) in
  { contract with code }
