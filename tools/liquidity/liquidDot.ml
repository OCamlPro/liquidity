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
open Ocamldot.TYPES

let subgraph_counter = ref 0

let rec to_dot ~sub_contract_of contract =

  incr subgraph_counter;

  let g, nodes = match sub_contract_of with
    | None ->
      Ocamldot.create "Contract" [],
      Hashtbl.create 1000
    | Some (g, nodes) ->
      Ocamldot.cluster g
        ("Contract_sub_" ^ (string_of_int !subgraph_counter)) [],
      nodes
  in

  let node_of node =
    try
      Hashtbl.find nodes node.num
    with Not_found ->
      let name =
        Printf.sprintf "%d:%s%s" node.num
          (match node.node_name with
           | Some name -> Printf.sprintf "var:%s =\n" name
           | None -> ""
          )
          (match node.kind with
           | N_PRIM prim -> prim
           | N_VAR var -> Printf.sprintf "var:%s" var
           | _ -> LiquidPrinter.string_of_node node)
      in
      let n = Ocamldot.node g name [] in
      Hashtbl.add nodes node.num n;
      n
  in

  let add_edge n1 n2 attrs =
    Ocamldot.add_edge (node_of n1) (node_of n2) attrs
  in

  let done_set = Hashtbl.create 1000 in

  let rec iter_next node =
    match node.next with
    | None -> ()
    | Some next ->
      add_edge node next [ EdgeStyle Bold ];
      iter next

  and iter node =
    if not (Hashtbl.mem done_set node.num) then begin
      Hashtbl.add done_set node.num true;
      let add_edge_deps deps =
        List.iter (fun arg ->
            add_edge node arg [ ]
          ) deps
      in
      begin match node.kind with
        | N_UNKNOWN _
        | N_START
        | N_FAILWITH
        | N_LAMBDA_BEGIN
        | N_END
        | N_VAR _
        | N_CONST (_, _)
        | N_PRIM _
        | N_LEFT _
        | N_RIGHT _
        | N_ABS
        | N_TRANSFER
        | N_CALL
        | N_CONTRACT _
        | N_UNPACK _
        | N_PROJ _
        | N_RECORD _
        | N_CONSTR _
        | N_SETFIELD _
        | N_SELF _
          -> ()

        | N_LOOP_END (x,y,z)
        | N_FOLD_END (x,y,z)
        | N_MAP_END (x,y,z)
        | N_IF_CONS (x, y, z)
        | N_LOOP_LEFT_END (x, y, z) ->
          add_edge_deps [x;y;z]

        | N_LOOP_RESULT (x,y,_)
        | N_LOOP_LEFT_RESULT (x,y,_)
        | N_FOLD_RESULT (x,y,_)
        | N_MAP_RESULT (x,y,_)
        | N_IF_SOME (x,y)
        | N_IF_LEFT (x,y)
        | N_IF_RIGHT (x,y)
        | N_IF_PLUS (x,y)
        | N_IF_MINUS (x,y)
        | N_IF_END_RESULT (x,Some y,_)
        | N_IF_END (x,y)
        | N_IF (x,y)
        | N_LOOP (x,y)
        | N_FOLD (x,y)
        | N_MAP (x,y)
        | N_LAMBDA (x,y,_,_)
        | N_LOOP_LEFT (x, y) ->
          add_edge_deps [x;y]

        | N_IF_END_RESULT (x,None,_)
        | N_IF_RESULT (x,_)
        | N_IF_THEN x
        | N_IF_ELSE x
        | N_IF_NONE x
        | N_IF_NIL x
        | N_LOOP_BEGIN x
        | N_LOOP_LEFT_BEGIN x
        | N_ARG (x,_)
        | N_FOLD_BEGIN x
        | N_MAP_BEGIN x
        | N_LAMBDA_END x
        | N_RESULT (x, _) ->
          add_edge_deps [x]

        | N_CREATE_CONTRACT c ->
          let _cg = to_dot ~sub_contract_of:(Some (g, nodes)) c in
          let (begin_c, _) = c.mic_code in
          add_edge_deps [begin_c];
      end;
      List.iter (fun arg ->
          add_edge node arg [  EdgeStyle Dotted; EdgeLabel "\"; constraint=\"false" ]
        ) node.args;
      List.iter iter node.args;
      iter_next node
    end
  in

  let (begin_node, end_node) = contract.mic_code in
  Ocamldot.add_node_attrs (node_of begin_node)
    [NodeColor "green"; NodeStyle Filled];
  Ocamldot.add_node_attrs (node_of end_node)
    [NodeColor "red"; NodeStyle Filled];
  iter begin_node;

  begin match contract.mic_fee_code with
    | None -> ()
    | Some (begin_fee_node, end_fee_node) ->
      Ocamldot.add_node_attrs (node_of begin_fee_node)
        [NodeColor "turquoise"; NodeStyle Filled];
      Ocamldot.add_node_attrs (node_of end_fee_node)
        [NodeColor "purple"; NodeStyle Filled];
      iter begin_fee_node;
  end;

  (* Return graph *)
  g

let to_string contract =
  if !LiquidOptions.verbosity > 0 then
    Format.eprintf "Produce decompilation graph as graphviz file@.";
  subgraph_counter := 0;
  Ocamldot.to_string (to_dot ~sub_contract_of:None contract)
