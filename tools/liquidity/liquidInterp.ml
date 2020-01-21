(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)

open LiquidTypes

let counter = ref 0
let nodes = Hashtbl.create 1000
let node loc kind args prevs =
  incr counter;
  let num = !counter in
  let node = { num; loc; node_name = None; kind; args; next = None; prevs } in
  List.iter (fun prev ->
      match prev.next with
      | None -> prev.next <- Some node
      | Some s -> assert false
    ) prevs;
  Hashtbl.add nodes num node;
  node

let mic_loc loc ins = { loc; ins; loc_name = None }

let unsanitize_gen_name s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec aux s i =
    if i >= len then Buffer.contents b
    else
      let next () =
        Buffer.add_char b s.[i];
        aux s (i+1)
      in
      if s.[i] = '.' then begin
        Buffer.add_char b '_';
        aux s (i+1)
      end
      else
      if s.[i] = '_' && len - i >= 6 then
        if s.[i+1] = 'p' && s.[i+2] = 'r' && s.[i+3] = 'i'
           && s.[i+4] = 'm' && s.[i+5] = '_' then begin
          Buffer.add_char b '\'';
          aux s (i+6)
        end
        else if len >= 7 then
          if s.[i+1] = 's' then
            (*
            if s.[i+2] = 'h' && s.[i+3] = 'a' && s.[i+4] = 'r'
               && s.[i+5] = 'p'  && s.[i+6] = '_' then begin
              Buffer.add_char b '#';
              aux s (i+7)
            end
            else *)
            if s.[i+2] = 'l' && s.[i+3] = 'a' && s.[i+4] = 's'
               && s.[i+5] = 'h'  && s.[i+6] = '_' then begin
              Buffer.add_char b '/';
              aux s (i+7)
            end else next ()
          else next ()
        else next ()
      else next ()
  in
  aux s 0

let unsanitize_name s =
  let s = unsanitize_gen_name s in
  if List.mem s reserved_keywords || has_reserved_prefix s then
    Some (s ^ "_")
  else if String.length s > 0 then
    match s.[0] with
    | 'A' .. 'Z' | '0' .. '9' -> Some ("_" ^ s)
    | '_' ->
      if String.length s > 1 then
        if s.[1] = '/' then None
        else Some s
      else None
    | _ -> Some s
  else Some s

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
        node_name = None;
        kind = N_UNKNOWN "STACK"; args = [];
        next = None; prevs = [] }
    @ if_stack

let rec fail_stack = function
  | { kind = N_FAILWITH } :: _ -> true
  | { kind = N_IF_END_RESULT ({ args }, _, _) } :: _ -> fail_stack args
  | _ -> false

let rec merge_stacks if_stack end_node1 end_node2 stack1 stack2 =
  match stack1, stack2 with
  | [], [] -> []
  | _ when
      fail_stack stack1 &&
      not @@ fail_stack stack2 ->
    begin
      match end_node2 with
      | None -> assert false
      | Some end_node2 ->
        merge_stacks if_stack end_node2 None
          stack2 (uniformize_stack if_stack stack2)
    end
  |  _ when
      fail_stack stack2 &&
      not @@ fail_stack stack1 ->
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
      let loc = match end_node2 with
        | None -> end_node1.loc
        | Some end_node2 -> LiquidLoc.merge end_node1.loc end_node2.loc in
      LiquidLoc.raise_error ~loc
        "interp error merging stacks:\n%a%a"
        (fprint_stack "stack1 ") stack1
        (fprint_stack "stack2 ") stack2

(*
let merge_stacks if_stack end_node1 end_node2 stack1 stack2 =
  Format.eprintf
    "\nmerging stacks:\n%a%a"
    (fprint_stack "stack1 ") stack1
    (fprint_stack "stack2 ") stack2;
  let s = merge_stacks if_stack end_node1 end_node2 stack1 stack2 in
  Format.eprintf
    "\n==>\n%a@."
    (fprint_stack "result ") s;
  s
*)

let add_name stack seq name =
  match stack with
  | x :: _ ->
    let name = unsanitize_name name in
    if x.node_name = None then x.node_name <- name;
    begin match x.kind, seq.kind with
      | N_IF_END_RESULT _, N_IF _
      | N_LOOP_RESULT _, N_LOOP _
      | N_LOOP_LEFT_RESULT _, N_LOOP_LEFT _
      | N_FOLD_RESULT _, N_FOLD _ ->
        if seq.node_name = None then seq.node_name <- name;
      | _ -> ()
    end;
    begin match x with
      (* recover closures names *)
      | { kind = N_PRIM "PAIR"; args = [ { kind = N_LAMBDA _ } as x; _] } ->
        if x.node_name = None then x.node_name <- name;
      | _ -> ()
    end;
  | [] -> ()

let remove_name stack =
  match stack with
  | x :: _ -> x.node_name <- None
  | [] -> ()

let add_name_to_ins stack seq ins =
  match ins.loc_name, ins.ins, stack with
  | Some _, FAILWITH, _ -> ()
  | Some name, _, x :: _ -> add_name stack seq name
  | _, _, _ -> ()

let rec short_circuit node ~from =
  match node.node_name with
  | Some n  -> ()
  | None ->
    List.iter (fun n ->
        begin match n.next with
          | Some k when k.num = node.num ->
            n.next <- from.next
          | _ -> ()
        end;
        short_circuit node ~from:n;
      ) from.prevs

let rec undo_cdr acc node =
  match node with
  | { kind = N_PRIM "CDR"; args = [x]; node_name = None } ->
    undo_cdr (node :: acc) x
  | _ -> acc, node

let rec constrlabel_is_in_type c = function
  | Tunit | Tbool | Tint | Tnat | Ttez | Tstring | Tbytes | Ttimestamp
  | Tkey | Tkey_hash | Tsignature | Toperation | Taddress | Tfail ->
    false
  | Ttuple tys -> List.exists (constrlabel_is_in_type c) tys
  | Toption ty | Tlist ty | Tset ty | Tcontract (_, ty) -> constrlabel_is_in_type c ty
  | Tmap (t1, t2) | Tbigmap (t1, t2) | Tor (t1, t2) | Tlambda (t1, t2, _) ->
    constrlabel_is_in_type c t1 || constrlabel_is_in_type c t2
  | Tclosure ((t1, t2), t3, _) ->
    constrlabel_is_in_type c t1 ||
    constrlabel_is_in_type c t2 ||
    constrlabel_is_in_type c t3
  | Trecord (_, l) | Tsum (_, l) ->
    List.exists (fun (c', _) -> c' = c) l ||
    List.exists (fun (_, t) -> constrlabel_is_in_type c t) l
  | Tvar { contents = { contents = { tyo = Some ty }}} ->
    constrlabel_is_in_type c ty
  | Tvar _ | Tpartial _ -> (* assert *) false

let rec constrlabel_is_in_code c code =
  match code.ins with
  | RENAME _ | EXTENSION _
  | EXEC | DUP _ | DIP_DROP _ | DROP _ | CAR _ | CDR _ | CDAR _ | CDDR _
  | PAIR | RECORD _ | COMPARE | LE | LT | GE | GT | NEQ | EQ | FAILWITH
  | NOW | TRANSFER_TOKENS | ADD | SUB | BALANCE | SWAP | GET | UPDATE | SOME
  | CONCAT | MEM | SLICE | SELF _ | AMOUNT | STEPS_TO_QUOTA
  | BLAKE2B | SHA256 | SHA512 | HASH_KEY | CHECK_SIGNATURE | ADDRESS | CONS
  | OR | XOR | AND | NOT | INT | ABS | ISNAT | NEG | MUL | EDIV | LSL | LSR
  | SOURCE | SENDER | SIZE | IMPLICIT_ACCOUNT | SET_DELEGATE | PACK | MOD | DIV
  | BLOCK_LEVEL | IS_IMPLICIT | COLLECT_CALL | GET_BALANCE | EMPTY_BIG_MAP _
    -> false
  | UNPACK ty
  | PUSH (ty, _)
  | LEFT (ty, _)
  | RIGHT (ty, _)
  | CONTRACT (_, ty) -> constrlabel_is_in_type c ty
  | CREATE_CONTRACT contract -> constrlabel_is_in_contract c contract
  | LAMBDA (ty1, ty2, code) ->
    constrlabel_is_in_type c ty1 ||
    constrlabel_is_in_type c ty2 ||
    constrlabel_is_in_code c code
  | DIP (_, code)
  | LOOP code
  | LOOP_LEFT code
  | ITER code
  | MAP code ->
    constrlabel_is_in_code c code
  | IF (code1, code2)
  | IF_NONE (code1, code2)
  | IF_CONS (code1, code2)
  | IF_LEFT (code1, code2) ->
    constrlabel_is_in_code c code1 ||
    constrlabel_is_in_code c code2
  | SEQ seq -> List.exists (constrlabel_is_in_code c) seq

and constrlabel_is_in_contract c contract =
  constrlabel_is_in_type c contract.mic_parameter ||
  constrlabel_is_in_type c contract.mic_storage ||
  constrlabel_is_in_code c contract.mic_code


let curr_contract = ref { mic_parameter = Tunit;
                          mic_storage = Tunit;
                          mic_code = { loc = noloc;
                                       loc_name = None;
                                       ins = SEQ [] };
                          mic_fee_code = None }

let known_constrlabel c =
  constrlabel_is_in_contract c !curr_contract


let rec decompile_seq stack (seq : node) code =
  match code, stack with
  | [], _ -> (stack, seq)

  (* Special case for abs *)
  | {ins=ABS; loc} :: {ins=INT} :: code, x :: stack ->
    let n = node loc N_ABS [x] [seq] in
    let stack, seq = n :: stack, n in
    decompile_seq stack seq code

  (* Special case for failwith, annot is before *)
  | {ins=RENAME name} :: ({ins=FAILWITH} as fail) :: code, _ ->
    fail.loc_name <- name;
    let stack, seq = decompile stack seq fail in
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

and decompile stack seq ins =
  let stack, seq = decompile_aux stack seq ins in
  add_name_to_ins stack seq ins;
  stack, seq

and decompile_aux stack (seq : node) ins =
  match ins.ins, stack with
  | RENAME (Some name), _ ->
    add_name stack seq name;
    stack, seq

  | RENAME None, _ ->
    remove_name stack;
    stack, seq

  | SEQ exprs, _ ->
    decompile_seq stack seq exprs

  (* If-expressions followed by FAIL: we create IF statements where the
     FAIL is the neighbour of an empty sequence, so that the comparison
     of stacks is meaningless. Of course, this assume that a sequence
     of instructions finishing with a FAIL is forbidden. Be careful to
     avoid infinite loops here...
  *)

  | IF ({ins=SEQ [{ins=FAILWITH}]} as seq_fail,
        {ins=SEQ ( (_ :: _) as ifelse )}),_ ->
    decompile stack seq
      (mic_loc ins.loc
         (SEQ (mic_loc ins.loc
                 (IF (seq_fail,
                      mic_loc ins.loc (SEQ []))) :: ifelse)))
  | IF ({ins=SEQ ( (_ :: _) as ifthen )},
        ({ins=SEQ [{ins=FAILWITH}]} as seq_fail)),_ ->
    decompile stack seq
      (mic_loc ins.loc
         (SEQ (mic_loc ins.loc
                 (IF (mic_loc ins.loc (SEQ []),
                      seq_fail)) :: ifthen)))
  | IF_NONE ({ins=SEQ ( (_::_) as ifthen)},
             ({ins=SEQ [{ins=FAILWITH}]} as seq_fail)),_ ->
    decompile stack seq
      (mic_loc ins.loc
         (SEQ (mic_loc ins.loc
                 (IF_NONE (mic_loc ins.loc (SEQ []),
                           seq_fail)) :: ifthen)))
  | IF_CONS ({ins=SEQ [{ins=FAILWITH}]} as seq_fail,
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
            | { kind = N_FAILWITH } :: _, _ -> stack2
            | _, { kind = N_FAILWITH } :: _ -> stack1
            | s1 :: stack1, s2 :: stack2 ->
              if s1 == s2 then
                s1 :: (merge i stack1 stack2)
              else
                let arg = node code.loc (N_ARG (begin_node,i)) [] [] in
                begin_node.args <- s1 :: begin_node.args;
                arg :: (merge (i+1) stack1 stack2)
            | _ ->
              LiquidLoc.raise_error ~loc:ins.loc
                "Bad stack size in LOOP body%!"
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
              | { kind = N_FAILWITH } :: _, _ -> stack2
              | _, { kind = N_FAILWITH } :: _ -> stack1
              | { kind = N_ARG (n, i); node_name }:: stack1, s2 :: stack2
                when n == begin_node
                ->
                if i = 0 then begin_node.node_name <- node_name;
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


  | LOOP_LEFT code, x :: prev_stack ->
    let loopleft_node = node ins.loc (N_UNKNOWN "LOOP_LEFT") [x] [seq] in
    let begin_node = node code.loc (N_LOOP_LEFT_BEGIN loopleft_node) [] [] in
    let arg = node code.loc (N_ARG (begin_node, 0)) [] [] in

    let pseudo_node = node ins.loc (N_UNKNOWN "LOOP_LEFT") [] [] in
    begin match decompile (arg :: prev_stack) pseudo_node code with
      | [],_ -> assert false
      | x :: end_stack,_ ->

        let rec merge i stack1 stack2 =
          if stack1 == stack2 then stack1
          else
            match stack1, stack2 with
            | [], [] -> []
            | { kind = N_FAILWITH } :: _, _ -> stack2
            | _, { kind = N_FAILWITH } :: _  -> stack1
            | s1 :: stack1, s2 :: stack2 ->
              if s1 == s2 then
                s1 :: (merge i stack1 stack2)
              else
                let arg = node code.loc (N_ARG (begin_node,i)) [] [] in
                begin_node.args <- s1 :: begin_node.args;
                arg :: (merge (i+1) stack1 stack2)
            | _ ->
              LiquidLoc.raise_error ~loc:ins.loc
                "Bad stack size in LOOP_LEFT body%!"
        in
        let stack = merge 1 prev_stack end_stack in

        begin_node.args <- List.rev begin_node.args;

        match decompile (arg :: stack) begin_node code with
        | [], _ -> assert false
        | x :: end_stack, loopleft_seq ->

          let end_node = node x.loc
              (N_LOOP_LEFT_END (loopleft_node, begin_node, x)) []
              [ loopleft_seq ] in

          let rec merge i stack1 stack2 =
            if stack1 == stack2 then stack1
            else
              match stack1, stack2 with
              | [], [] -> []
              | { kind = N_FAILWITH } :: _, _ -> stack2
              | _, { kind = N_FAILWITH } :: _ -> stack1
              | { kind = N_ARG (n, i); node_name }:: stack1, s2 :: stack2
                when n == begin_node
                ->
                let arg = node ins.loc
                    (N_LOOP_LEFT_RESULT (loopleft_node, end_node, i)) [] [] in
                end_node.args <- s2 :: end_node.args;
                arg :: merge (i+1) stack1 stack2
              | s1 :: stack1, s2 :: stack2 ->
                assert (s1 == s2);
                s1 :: (merge i stack1 stack2)
              | _ -> assert false
          in

          let stack = merge 1 stack end_stack in
          let stack =
            node ins.loc (N_LOOP_LEFT_RESULT (loopleft_node, end_node, 0)) [] [] ::
            stack in
          end_node.args <- List.rev end_node.args;
          begin_node.node_name <- arg.node_name;
          loopleft_node.kind <- N_LOOP_LEFT (begin_node, end_node);
          stack, loopleft_node
    end

  | ITER code, x :: prev_stack ->
    let fold_node = node ins.loc (N_UNKNOWN "FOLD") [x] [seq] in
    let begin_node = node code.loc (N_FOLD_BEGIN fold_node) [] [] in

    let pseudo_node = node ins.loc (N_UNKNOWN "FOLD") [] [] in
    let arg = node code.loc (N_ARG (begin_node,0)) [] [] in

    begin match decompile (arg :: prev_stack) pseudo_node code with
      | end_stack, fold_seq ->

        let rec merge i stack1 stack2 =
          if stack1 == stack2 then stack1
          else
            match stack1, stack2 with
            | [], [] -> []
            | { kind = N_FAILWITH } :: _, _ -> stack2
            | _, { kind = N_FAILWITH } :: _ -> stack1
            | s1 :: stack1, s2 :: stack2 ->
              if s1 == s2 then
                s1 :: (merge i stack1 stack2)
              else
                let arg = node code.loc (N_ARG (begin_node,i)) [] [] in
                begin_node.args <- s1 :: begin_node.args;
                arg :: (merge (i+1) stack1 stack2)
            | _ ->
              LiquidLoc.raise_error ~loc:ins.loc
                "Bad stack size in ITER body%!"
        in
        let stack = merge 1 prev_stack end_stack in
        begin_node.args <- List.rev begin_node.args;

        match decompile ( arg :: stack) begin_node code with
        | end_stack, fold_seq ->

          let end_node = node x.loc
              (N_FOLD_END (fold_node, begin_node, begin_node)) [  ]
              [ fold_seq ] in

          let rec merge i stack1 stack2 =
            if stack1 == stack2 then stack1
            else
              match stack1, stack2 with
              | [], [] -> []
              | { kind = N_FAILWITH } :: _, _ -> stack2
              | _, { kind = N_FAILWITH } :: _ -> stack1
              | { kind = N_ARG (n, _) }:: stack1, s2 :: stack2
                when n == begin_node
                ->
                let arg = node ins.loc
                    (N_FOLD_RESULT (fold_node, end_node, i)) [] [] in
                end_node.args <- s2 :: end_node.args;
                arg :: merge (i+1) stack1 stack2
              | s1 :: stack1, s2 :: stack2 ->
                assert (s1 == s2);
                s1 :: (merge i stack1 stack2)
              | _ -> assert false
          in

          let stack = merge 0 stack end_stack in
          end_node.args <- List.rev end_node.args;
          begin match begin_node.next with
            | Some { node_name = Some name } ->
              begin_node.node_name <- Some name
            | _ ->
              begin_node.node_name <- arg.node_name
          end;
          fold_node.kind <- N_FOLD (begin_node, end_node);
          stack, fold_node
    end

  | MAP code, x :: prev_stack ->
    let map_node = node ins.loc (N_UNKNOWN "MAP") [x] [seq] in
    let begin_node = node code.loc (N_MAP_BEGIN map_node) [] [] in

    let pseudo_node = node ins.loc (N_UNKNOWN "MAP") [] [] in
    let arg = node code.loc (N_ARG (begin_node, 0)) [] [] in

    (* Format.eprintf "map start stack : %d@." (List.length prev_stack + 1); *)
    begin match decompile (arg :: prev_stack) pseudo_node code with
      | [], _ -> assert false
      | x :: end_stack, _ ->

        let rec merge i stack1 stack2 =
          if stack1 == stack2 then stack1
          else
            match stack1, stack2 with
            | [], [] -> []
            | { kind = N_FAILWITH } :: _, _ -> stack2
            | _, { kind = N_FAILWITH } :: _ -> stack1
            | s1 :: stack1, s2 :: stack2 ->
              if s1 == s2 then
                s1 :: (merge i stack1 stack2)
              else
                let arg = node code.loc (N_ARG (begin_node,i)) [] [] in
                begin_node.args <- s1 :: begin_node.args;
                arg :: (merge (i+1) stack1 stack2)
            | _ ->
              LiquidLoc.raise_error ~loc:ins.loc
                "Bad stack size in MAP body%!"
        in
        let stack = merge 1 prev_stack end_stack in
        begin_node.args <- List.rev begin_node.args;

        match decompile ( arg :: stack) begin_node code with
        | [], _ -> assert false
        | x :: end_stack, map_seq ->

          let end_node = node x.loc
              (N_MAP_END (map_node, begin_node, x)) [  ] (* ? *)
              [ map_seq ] in

          let rec merge i stack1 stack2 =
            if stack1 == stack2 then stack1
            else
              match stack1, stack2 with
              | [], [] -> []
              | { kind = N_FAILWITH } :: _, _ -> stack2
              | _, { kind = N_FAILWITH } :: _ -> stack1
              | { kind = N_ARG (n, _) }:: stack1, s2 :: stack2
                when n == begin_node
                ->
                let arg = node ins.loc
                    (N_MAP_RESULT (map_node, end_node, i)) [] [] in
                end_node.args <- s2 :: end_node.args;
                arg :: merge (i+1) stack1 stack2
              | s1 :: stack1, s2 :: stack2 ->
                assert (s1 == s2);
                s1 :: (merge i stack1 stack2)
              | [], [s2] ->
                assert false
              (* [s2] *)
              | _ -> assert false
          in

          let stack = merge 1 stack end_stack in
          let stack =
            node ins.loc (N_MAP_RESULT (map_node, end_node, 0)) [] [] ::
            stack in
          end_node.args <- List.rev end_node.args;
          begin_node.node_name <- arg.node_name;
          map_node.kind <- N_MAP (begin_node, end_node);
          (* Format.eprintf "map end stack : %d@." (List.length stack); *)
          stack, map_node
    end

  | LAMBDA (arg_ty, res_ty, code), stack ->
    let (begin_node, end_node) =
      decompile_lambda ins.loc arg_ty res_ty code in
    let lambda_node = node ins.loc
        (N_LAMBDA (begin_node, end_node, arg_ty, res_ty)) [] [seq]
    in
    lambda_node :: stack, lambda_node

  (* Stack modifications *)
  | DUP 0, stack -> stack, seq
  | DUP n, stack when n <= List.length stack ->
    let rec dup n stack = match n, stack with
      | 1, v :: _ -> v
      | _, _ :: stack -> dup (n - 1) stack
      | _, [] -> assert false in
    let v = dup n stack in
    v :: stack, seq
  | DROP n, stack when n <= List.length stack ->
    let rec drop n stack = match n, stack with
      | 0, _ -> stack
      | _, _ :: stack -> drop (n - 1) stack
      | _, [] -> assert false in
    drop n stack, seq
  | DIP (n, code), stack when n <= List.length stack ->
    let rec dip n acc stack = match n, stack with
      | 0, _ -> List.rev acc, stack
      | _, x :: stack -> dip (n - 1) (x :: acc) stack
      | _, [] -> assert false in
    let top, stack = dip n [] stack in
    let stack, seq = decompile stack seq code in
    top @ stack, seq
  | SWAP, x :: y :: stack ->
    y :: x :: stack, seq



  (* Primitives *)

  | PUSH (ty, cst), stack ->
    let cst = decompile_const ins.loc cst in
    let x = node ins.loc (N_CONST (ty, cst)) [] [seq] in
    x :: stack, x

  | FAILWITH, arg :: _ ->
    let x = node ins.loc N_FAILWITH [arg] [seq] in
    [x], x

  | TRANSFER_TOKENS,
    { kind = N_CONST (Tunit, CUnit) } :: amount ::
    { kind = N_PRIM "IMPLICIT_ACCOUNT"; args = [ key ] } :: stack ->
    let x = node ins.loc N_TRANSFER [ key; amount ] [seq] in
    x :: stack, x

  | TRANSFER_TOKENS, arg :: amount :: contract :: stack ->
    let x = node ins.loc N_CALL [contract; amount; arg] [seq] in
    x :: stack, x

  | SOURCE, stack ->
    let x = node ins.loc (N_PRIM "SOURCE") [] [seq] in
    x :: stack, x
  | SENDER, stack ->
    let x = node ins.loc (N_PRIM "SENDER") [] [seq] in
    x :: stack, x
  | SELF entry, stack ->
    let x = node ins.loc (N_SELF entry) [] [seq] in
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

  | IMPLICIT_ACCOUNT, key :: stack ->
    let x = node ins.loc (N_PRIM "IMPLICIT_ACCOUNT") [key] [seq] in
    x :: stack, x

  | SET_DELEGATE, key :: stack ->
    let x = node ins.loc (N_PRIM "SET_DELEGATE") [key] [seq] in
    x :: stack, x

  | ADDRESS, x :: stack ->
    let x = node ins.loc (N_PRIM "ADDRESS") [x] [seq] in
    x :: stack, x
  | PACK, x :: stack ->
    let x = node ins.loc (N_PRIM "PACK") [x] [seq] in
    x :: stack, x
  | BLAKE2B, x :: stack ->
    let x = node ins.loc (N_PRIM "BLAKE2B") [x] [seq] in
    x :: stack, x
  | SHA256, x :: stack ->
    let x = node ins.loc (N_PRIM "SHA256") [x] [seq] in
    x :: stack, x
  | SHA512, x :: stack ->
    let x = node ins.loc (N_PRIM "SHA512") [x] [seq] in
    x :: stack, x
  | HASH_KEY, x :: stack ->
    let x = node ins.loc (N_PRIM "HASH_KEY") [x] [seq] in
    x :: stack, x
  | SOME, x :: stack ->
    let x = node ins.loc (N_PRIM "SOME") [x] [seq] in
    x :: stack, x

  | RIGHT (right_ty, Some "_"), ({ kind = N_CONSTR c; } as x) :: stack ->
    (* special case decoding nested (Right .. (Right (Left x))) *)
    x :: stack, seq
  | LEFT (_, Some constr), x :: stack when known_constrlabel constr ->
    let x = node ins.loc (N_CONSTR constr) [x] [seq] in
    x :: stack, x
  | RIGHT (_, Some constr), x :: stack when known_constrlabel constr ->
    let x = node ins.loc (N_CONSTR constr) [x] [seq] in
    x :: stack, x

  | LEFT (right_ty, _), x :: stack ->
    let x = node ins.loc (N_LEFT right_ty) [x] [seq] in
    x :: stack, x
  | RIGHT (left_ty, _), x :: stack ->
    let x = node ins.loc (N_RIGHT left_ty) [x] [seq] in
    x :: stack, x

  | CONTRACT (entry, ty), x :: stack -> (* TODO : keep types too ! *)
    let x = node ins.loc (N_CONTRACT (entry, ty)) [x] [seq] in
    x :: stack, x
  | UNPACK ty, x :: stack -> (* TODO : keep types too ! *)
    let x = node ins.loc (N_UNPACK ty) [x] [seq] in
    x :: stack, x
  | INT, x :: stack ->
    let x = node ins.loc (N_PRIM "INT") [x] [seq] in
    x :: stack, x
  | ISNAT, x :: stack ->
    let x = node ins.loc (N_PRIM "ISNAT") [x] [seq] in
    x :: stack, x
  | ABS, x :: stack ->
    let x = node ins.loc (N_PRIM "ABS") [x] [seq] in
    x :: stack, x

  | CAR (Some f), { kind = N_RECORD (f' :: _); args = x :: _ } :: stack
    when f = f'->
    x :: stack, seq
  | (CAR (Some field) | CDR (Some field)), x :: stack
    when known_constrlabel field ->
    let to_remove, x = undo_cdr [] x in
    let x = node ins.loc (N_PROJ field) [x] [seq] in
    List.iter (short_circuit ~from:x) to_remove;
    x :: stack, x

  | CAR _, { kind = N_PRIM "PAIR"; args = [x;_] } :: stack ->
    x :: stack, seq
  | CDR _, { kind = N_PRIM "PAIR"; args = [_;x] } :: stack ->
    x :: stack, seq
  | CAR _, x :: stack ->
    let x = node ins.loc (N_PRIM "CAR") [x] [seq] in
    x :: stack, x
  | CDR _, x :: stack ->
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

  | CHECK_SIGNATURE, x :: y :: z :: stack ->
    let x = node ins.loc (N_PRIM "CHECK_SIGNATURE") [x; y; z] [seq] in
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
  | CONCAT, x :: stack ->
    let x = node ins.loc (N_PRIM "CONCAT") [x] [seq] in
    x :: stack, x
  | SLICE, x :: y :: z :: stack ->
    let x = node ins.loc (N_PRIM "SLICE") [x;y;z] [seq] in
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
  | NEG, x :: stack ->
    let x = node ins.loc (N_PRIM "NEG") [x] [seq] in
    x :: stack, x

  | PAIR, { kind = N_PRIM "CAR"; args = [p] } ::
          { kind = N_PRIM "CDR"; args = [p'] } :: stack when p.num = p'.num ->
    p :: stack, seq

  | PAIR, x :: y :: stack ->
    let x = node ins.loc (N_PRIM "PAIR") [x;y] [seq] in
    x :: stack, x

  | RECORD (label_x, None), x :: y :: stack
    when not @@ known_constrlabel label_x ->
    let x = node ins.loc (N_PRIM "PAIR") [x;y] [seq] in
    x :: stack, x

  | RECORD (label_x, Some label_y), x :: y :: stack
    when not (known_constrlabel label_x && known_constrlabel label_y) ->
    let x = node ins.loc (N_PRIM "PAIR") [x;y] [seq] in
    x :: stack, x

  | RECORD (label_x, Some label_y), x :: y :: stack ->
    let x = node ins.loc (N_RECORD [label_x; label_y]) [x;y] [seq] in
    x :: stack, x

  | RECORD (label_x, None),
    x :: ({ kind = N_RECORD y_labels; args } as y) :: stack ->
    let x =
      node ins.loc (N_RECORD (label_x :: y_labels)) (x :: args) [seq] in
    short_circuit ~from:x y;
    x :: stack, x

  | RECORD (label_x, None),
    x :: ({ kind = N_PRIM "CDR"; args = [_] } as y) :: stack ->
    let to_remove, y = undo_cdr [] y in
    let x =
      node ins.loc (N_SETFIELD label_x) [x; y] [seq] in
    List.iter (short_circuit ~from:x) to_remove;
    x :: stack, x

  | RECORD (label_x, None),
    x :: ({ kind = N_SETFIELD _ } as y) :: stack ->
    let x =
      node ins.loc (N_SETFIELD label_x) [x; y] [seq] in
    x :: stack, x


  | RECORD (label_x, None), x :: y :: stack ->
    let x = node ins.loc (N_RECORD [label_x; "_"]) [x;y] [seq] in
    x :: stack, x

  | COMPARE, x ::  { kind = N_CONST (Tint,CInt n)} :: stack
    when LiquidNumber.int_of_integer n = 0
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

  | LSL, x :: y :: stack ->
    let x = node ins.loc (N_PRIM "LSL") [x; y] [seq] in
    x :: stack, x

  | LSR, x :: y :: stack ->
    let x = node ins.loc (N_PRIM "LSR") [x; y] [seq] in
    x :: stack, x

  | STEPS_TO_QUOTA, stack ->
    let x = node ins.loc (N_PRIM "STEPS_TO_QUOTA") [] [seq] in
    x :: stack, x

  | CREATE_CONTRACT contract, delegate :: amount :: storage :: stack ->
    let contract = interp contract in
    let x = node ins.loc (N_CREATE_CONTRACT contract)
        [delegate; amount; storage] [seq] in
    let res_op = node ins.loc (N_RESULT (x, 0)) [] [] in
    let res_addr = node ins.loc (N_RESULT (x, 1)) [] [] in
    res_op :: res_addr :: stack, x

  | BLOCK_LEVEL, stack ->
    let x = node ins.loc (N_PRIM "BLOCK_LEVEL") [] [seq] in
    x :: stack, x

  | COLLECT_CALL, stack ->
    let x = node ins.loc (N_PRIM "COLLECT_CALL") [] [seq] in
    x :: stack, x

  | GET_BALANCE, x :: stack ->
    let x = node ins.loc (N_PRIM "GET_BALANCE") [x] [seq] in
    x :: stack, x

  | IS_IMPLICIT, x :: stack ->
    let x = node ins.loc (N_PRIM "IS_IMPLICIT") [x] [seq] in
    x :: stack, x

  | EMPTY_BIG_MAP (k, v), stack ->
    let x = node ins.loc (N_CONST (Tbigmap (k, v), CBigMap (BMList []))) []  [seq] in
    x :: stack, x

  | _ ->
    (* let ins = LiquidEmit.emit_code ins in *)
    let s = LiquidPrinter.Michelson.string_of_loc_michelson ins in
    LiquidLoc.raise_error ~loc:ins.loc "Error while decompiling:\n %s%!" s

and decompile_lambda loc arg_ty res_ty code =
  let begin_node = node loc N_LAMBDA_BEGIN [] [] in
  let lambda_stack, lambda_seq =
    decompile [ begin_node ] begin_node code in
  match lambda_stack with
  | [] | _ :: _ :: _ -> assert false
  | [res_node] ->
    let end_node = node res_node.loc (N_LAMBDA_END begin_node) [res_node]
        [lambda_seq] in
    (begin_node, end_node)

and decompile_const loc cst = match cst with
  | ( CUnit | CBool _ | CInt _ | CNat _ | CTez _ | CTimestamp _ | CString _
    | CBytes _ | CKey _ | CSignature _ | CNone  | CKey_hash _
    | CAddress _ ) as c -> c
  | CSome x -> CSome (decompile_const loc x)
  | CLeft x -> CLeft (decompile_const loc x)
  | CRight x -> CRight (decompile_const loc x)
  | CTuple xs -> CTuple (List.map (decompile_const loc) xs)
  | CList xs -> CList (List.map (decompile_const loc) xs)
  | CSet xs -> CSet (List.map (decompile_const loc) xs)
  | CMap l ->
    CMap (List.map (fun (x,y) -> decompile_const loc x, decompile_const loc y) l)
  | CBigMap BMId _ as c -> c
  | CBigMap BMList l ->
    CBigMap
      (BMList (List.map (fun (x,y) -> decompile_const loc x, decompile_const loc y) l))
  | CRecord labels ->
    CRecord (List.map (fun (f, x) -> f, decompile_const loc x) labels)
  | CConstr (constr, x) ->
    CConstr (constr, decompile_const loc x)
  | CLambda lam ->
    let nodes =
      decompile_lambda loc lam.arg_ty lam.ret_ty lam.body in
    CLambda { lam with body = nodes }


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


and interp_code mic_code =

  let initial_code = match mic_code.ins with
    | SEQ code -> mic_loc mic_code.loc
                    (SEQ (mic_loc mic_code.loc PAIR :: code))
    | _ ->
      LiquidLoc.raise_error ~loc:mic_code.loc
        "Code of contract must be a sequence%!"
  in

  let start_node = node mic_code.loc N_START [] [] in

  let initial_stack = [
    node mic_code.loc (N_VAR "parameter") [] [];
    node mic_code.loc (N_VAR "storage") [] [];
  ] in

  let stack, seq = decompile initial_stack start_node initial_code in
  let end_node = match stack with
    | [ arg ] -> node arg.loc N_END [arg] [seq]
    | _ ->
      LiquidLoc.raise_error ~loc:mic_code.loc
        "Final stack of contract must have a single element%!"
  in
  (start_node, end_node)

and interp contract =
  let mic_code = interp_code contract.mic_code in
  counter := 0;
  Hashtbl.clear nodes;
  let mic_fee_code = match contract.mic_fee_code with
    | None -> None
    | Some mic_fee -> Some (interp_code mic_fee) in
  { contract with mic_code ; mic_fee_code}


let interp contract =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Symbolic execution of Michelson contract@.";
  counter := 0;
  curr_contract := contract;
  Hashtbl.clear nodes;
  interp contract
