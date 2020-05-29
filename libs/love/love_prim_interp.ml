(**************************************************************************)
(*                             Dune Network                               *)
(*                                                                        *)
(*  Copyright 2019 Origin-Labs                                            *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  any later version.                                                    *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(**************************************************************************)

open Error_monad
open Alpha_context
open Love_pervasives
open Exceptions
open Love_context
open Love_primitive
open Love_value
open Value
open Collections

let generic_primitives =
  Array.make (Love_primitive.max_possible_prim_id + 1)
    (Gas.step_cost 1,  fun _ -> assert false)

let register_primitive p gas f =
  Love_primitive.add_primitive p;
  generic_primitives.(p.prim_id) <- ( gas , f )

let consume_gas ctxt gas =
  Lwt.return (Alpha_context.Gas.consume ctxt.actxt gas
              >|? fun actxt -> { ctxt with actxt })

let sig_of_sigref ctxt env = 
  let open Love_type in
  function
  | Anonymous s -> return (ctxt, s)
  | Named id ->
      Love_env.find_sig_opt ctxt env id >>|? fun (ctxt, res) ->
      match res with
      | Some s -> ctxt, s
      | None -> raise (InvariantBroken "Signature not found")


let hash_data ctxt k =
  match Data_encoding.Binary.to_bytes Love_encoding.Value.encoding k with
  | None -> raise (InvariantBroken "Can't encode value")
  | Some res ->
      consume_gas ctxt (Love_gas.Cost_of.hash res Script_expr_hash.size)
      >>=? fun ctxt -> return (ctxt, Script_expr_hash.hash_bytes [res])

let empty_big_map tk tv =
  { id = None; diff = Love_value.ValueMap.empty;
    key_type = tk; value_type = tv }

let big_map_mem (ctxt : Love_context.t) { id; diff; _ } key =
  match Love_value.ValueMap.find_opt key diff, id with
  | None, None -> return (ctxt, false)
  | None, Some id ->
      hash_data ctxt key >>=? fun (ctxt, hash) ->
      Alpha_context.Big_map.mem ctxt.actxt id hash
      >>=? fun (actxt, res) -> return ({ctxt with actxt = actxt}, res)
  | Some None, _ -> return (ctxt, false)
  | Some (Some _), _ -> return (ctxt, true)

let big_map_get (ctxt : Love_context.t) { id; diff; _ } key =
  match Love_value.ValueMap.find_opt key diff, id with
  | Some (Some res), _ -> return (ctxt, Value.some res)
  | Some None, _ -> return (ctxt, Value.none ())
  | None, None -> return (ctxt, Value.none ())
  | None, Some id ->
      begin hash_data ctxt key >>=? fun (ctxt, hash) ->
        Alpha_context.Big_map.get_opt ctxt.actxt id hash
        >>=? fun (actxt, res) -> match res with
        | None -> return ({ ctxt with actxt = actxt }, Value.none ())
        | Some (Script_all.Dune_code _) -> assert false
        | Some (Dune_expr e) ->
            begin match Love_repr.is_const e with
              | None -> return (ctxt, Value.none ())
              | Some (Value value) -> return (ctxt, Value.some value)
              | Some (Type _) -> assert false
            end
        | Some (Michelson_expr _) -> assert false
      end

let big_map_add ctxt bm key bnd =
(* No inlining of key as it does not contain lambdas *)
(* No inlining of bnd : this will be done when passing
   to another contract or when storing the bigmap *)
  (* let bnd = Love_translator.inline_value ctxt bnd in *)
  let new_bm = { bm with diff = ValueMap.add key (Some bnd) bm.diff } in
  return (ctxt, new_bm)

let big_map_del ctxt bm key =
  let new_bm = { bm with diff = ValueMap.add key None bm.diff} in
  return (ctxt, new_bm)


let diff_of_big_map ctxt fresh ~ids Value.{ id; key_type; value_type; diff } =
  let open Collections in
  consume_gas ctxt (Love_gas.Cost_of.construct (ValueMap.cardinal diff))
  >>=? fun ctxt ->
  begin match id with
    | Some id ->
        if ZSet.mem id ids then
          fresh ctxt >>=? fun (ctxt, duplicate) ->
          return (ctxt, [ Op.Copy (id, duplicate) ], duplicate)
        else
          (* The first occurence encountered of a big_map reuses the
             ID. This way, the payer is only charged for the diff.
             For this to work, this diff has to be put at the end of
             the global diff, otherwise the duplicates will use the
             updated version as a base. This is true because we add
             this diff first in the accumulator of
             `extract_big_map_updates`, and this accumulator is not
             reversed before being flattened. *)
          return (ctxt, [], id)
    | None ->
        fresh ctxt >>=? fun (ctxt, id) ->
        return (ctxt, [ Op.Alloc { big_map = id; key_type; value_type } ], id)
  end >>=? fun (ctxt, init, big_map) ->
  let pairs =
    ValueMap.fold (fun key value acc -> (key, value) :: acc) diff [] in
  fold_left_s (fun (acc, ctxt) (diff_key, (diff_value : Value.t option)) ->
      consume_gas ctxt Love_gas.Cost_of.cycle >>=? fun ctxt ->
      hash_data ctxt diff_key >>=? fun (ctxt, diff_key_hash) ->
      let diff_item =
        Op.Update { big_map; diff_key; diff_key_hash; diff_value } in
      return (diff_item :: acc, ctxt))
    ([], ctxt) pairs >>=? fun (diff, ctxt) ->
  return (init @ diff, big_map, ctxt)

let extract_big_map_updates ctxt fresh ids acc v =
  let open Value in
  let open Collections in
  let rec aux_list ctxt ids acc vl =
    fold_left_s (fun (ctxt, vl, ids, acc) v ->
        consume_gas ctxt Love_gas.Cost_of.cycle >>=? fun ctxt ->
        aux ctxt fresh ids acc v >>|? fun (ctxt, v, ids, acc) ->
        (ctxt, v :: vl, ids, acc)
      ) (ctxt, [], ids, acc) vl
  and aux ctxt fresh ids acc v =
    match Value.unrec_closure v with
    | VBigMap vbm ->
        diff_of_big_map ctxt fresh ids vbm >>=? fun (diff, id, ctxt) ->
        let vbm = { vbm with diff = ValueMap.empty; id = Some id } in
        return (ctxt, VBigMap vbm, ZSet.add id ids, diff :: acc)

    | VTuple vl ->
        aux_list ctxt ids acc vl >>|? fun (ctxt, vl, ids, acc) ->
        (ctxt, VTuple (List.rev vl), ids, acc)

    | VList vl ->
        aux_list ctxt ids acc vl >>|? fun (ctxt, vl, ids, acc) ->
        (ctxt, VList (List.rev vl), ids, acc)

    | VConstr (cstr, vl) ->
        aux_list ctxt ids acc vl >>|? fun (ctxt, vl, ids, acc) ->
        (ctxt, VConstr (cstr, List.rev vl), ids, acc)

    | VPrimitive (prim, xl, vl) ->
        aux_list ctxt ids acc vl >>|? fun (ctxt, vl, ids, acc) ->
        (ctxt, VPrimitive (prim, xl, List.rev vl), ids, acc)

    | VRecord fvl ->
        fold_left_s (fun (ctxt, fvl, ids, acc) (f, v) ->
          consume_gas ctxt Love_gas.Cost_of.cycle >>=? fun ctxt ->
          aux ctxt fresh ids acc v >>|? fun (ctxt, v, ids, acc) ->
          (ctxt, (f, v) :: fvl, ids, acc)
        ) (ctxt, [], ids, acc) fvl >>|? fun (ctxt, fvl, ids, acc) ->
        (ctxt, VRecord (List.rev fvl), ids, acc)

    | VMap vm -> (* Key should not contain bigmaps *)
        fold_left_s (fun (ctxt, bl, ids, acc) (k, v) ->
          consume_gas ctxt Love_gas.Cost_of.cycle >>=? fun ctxt ->
          aux ctxt fresh ids acc k >>=? fun (ctxt, k, ids, acc) ->
          aux ctxt fresh ids acc v >>|? fun (ctxt, v, ids, acc) ->
          (ctxt, (k, v) :: bl, ids, acc)
        ) (ctxt, [], ids, acc) (ValueMap.bindings vm)
        >>|? fun (ctxt, bl, ids, acc) ->
        let vm = Utils.bindings_to_map ValueMap.add ValueMap.empty bl in
        (ctxt, VMap vm, ids, acc)

    | VClosure ({ call_env = { values; _ } as call_env; lambda }) ->
        fold_left_s (fun (ctxt, vcl, ids, acc) (id, vc) ->
          consume_gas ctxt Love_gas.Cost_of.cycle >>=? fun ctxt ->
          match vc with
          | Local v ->
              aux ctxt fresh ids acc v >>|? fun (ctxt, v, ids, acc) ->
              (ctxt, (id, Local v) :: vcl, ids, acc)
          | Global (Inlined (_, _)) (* no big_map at top-level *)
          | Global (Pointer _) -> return (ctxt, vcl, ids, acc)
        ) (ctxt, [], ids, acc) values
        >>|? fun (ctxt, values, ids, acc) ->
        let v = VClosure { call_env = { call_env with values }; lambda } in
        (ctxt, v, ids, acc)

    | VUnit | VBool _ | VString _ | VBytes _ | VInt _ | VNat _
    | VSet _ | VDun _ | VKey _ | VKeyHash _ | VSignature _
    | VTimestamp _ | VAddress _ | VContractInstance _
    | VEntryPoint _ | VView _ | VOperation _ ->
        return (ctxt, v, ids, acc)

    | VPackedStructure _c -> (* Can not contain bigmaps *)
        return (ctxt, v, ids, acc)
  in
  aux ctxt fresh ids acc v >>|? fun (ctxt, v, ids, acc) ->
  (ctxt, Value.rec_closure v, ids, acc)

let collect_big_maps ctxt v =
  let open Value in
  let open Collections in
  let rec aux ctxt v acc =
    match Value.unrec_closure v with
    | VBigMap { id = Some id; _ } ->
        Gas.consume ctxt.actxt Love_gas.Cost_of.cycle >>? fun actxt ->
        ok (ZSet.add id acc, { ctxt with actxt })

    | VTuple vl | VList vl | VConstr (_, vl) | VPrimitive (_, _, vl) ->
        List.fold_left (fun acc v ->
          acc >>? fun (acc, ctxt) -> aux ctxt v acc
        ) (ok (acc, ctxt)) vl

    | VRecord fvl ->
        List.fold_left (fun acc (_, v) ->
          acc >>? fun (acc, ctxt) -> aux ctxt v acc
        ) (ok (acc, ctxt)) fvl

    | VMap vm -> (* Key should not contain bigmaps *)
        List.fold_left (fun acc (k, v) ->
          acc >>? fun (acc, ctxt) ->
          aux ctxt k acc >>? fun (acc, ctxt) ->
          aux ctxt v acc
        ) (ok (acc, ctxt)) (ValueMap.bindings vm)

    | VClosure { call_env = { values; _ }; _ } ->
        List.fold_left (fun acc (_id, vc) ->
          acc >>? fun (acc, ctxt) ->
          match vc with
          | Local v -> aux ctxt v acc
          | Global (Inlined (_, _)) (* no big_map at top-level *)
          | Global (Pointer _) -> ok (acc, ctxt)
        ) (ok (acc, ctxt)) values

    | VUnit | VBool _ | VString _ | VBytes _ | VInt _ | VNat _
    | VSet _ | VBigMap _ | VDun _ | VKey _ | VKeyHash _
    | VSignature _ | VTimestamp _ | VAddress _ | VContractInstance _
    | VEntryPoint _ | VView _ | VOperation _ ->
        ok (acc, ctxt)

    | VPackedStructure _c -> (* Can not contain bigmaps *)
        ok (acc, ctxt)
  in
  Lwt.return (aux ctxt v ZSet.empty)

let extract_big_map_diff ctxt ~temporary ~to_duplicate ~to_update v =
  let open Collections in
  let to_duplicate = ZSet.diff to_duplicate to_update in
  let fresh = if temporary
    then (fun ctxt ->
        Big_map.fresh_temporary ctxt.actxt
        |> fun (actxt, id) -> return ({ ctxt with actxt }, id))
    else (fun ctxt ->
        Big_map.fresh ctxt.actxt
        >>|? fun (actxt, id) -> { ctxt with actxt }, id) in
  extract_big_map_updates ctxt fresh to_duplicate [] v
  >>=? fun (ctxt, v, alive, diffs) ->
  let diffs = if temporary then diffs else
      let dead = ZSet.diff to_update alive in
      ZSet.fold (fun id acc -> Op.Clear id :: acc) dead [] :: diffs in
  match diffs with
  | [] -> return (v, None, ctxt)
  | diffs -> return (v, Some (List.flatten diffs (* do not reverse *)), ctxt)


let contract_at ctxt env a ct =
  Love_context.get_script ctxt a >>=? fun (ctxt, script) ->
  match script with
    | None -> return (ctxt, Value.none ())
    | Some (code, _storage) ->
      begin match ct with
        | Love_type.StructType sr -> sig_of_sigref ctxt env sr
        | ContractInstance cn ->
            Love_env.find_struct_opt ctxt env cn >>|? fun (ctxt, res) ->
            match res with
            | Some (_path, s) -> ctxt, sig_of_structure ~only_typedefs:true s
            | None -> raise (InvariantBroken "Structure not found")
      end >>=? fun (ctxt, sg) ->
      let deps = match code.kind with
        | Love_type.Module -> []
        | Love_type.Contract l -> l in
      Love_typechecker.extract_deps None ctxt
        (Love_tenv.empty code.kind ()) deps
      >>|? fun (ctxt, deps_env) ->
      let cs = sig_of_structure ~only_typedefs:false code in
      let tenv = Love_tenv.contract_sig_to_env None cs deps_env in
      let find_sig n =
        match Love_tenv.find_signature n tenv with
        | None -> raise (InvariantBroken ("Signature not found (at)"))
        | Some r -> Love_tenv.env_to_contract_sig r.result
      in
      match Love_type.sub_contract find_sig sg cs with
      | Ok -> (ctxt, Value.some (VContractInstance (sg, a)))
      | TypeError _
      | TypeDefError _
      | SigContentError _
      | Other _ -> (ctxt, Value.none ())


let eval_prim apply_value ctxt env (prim, xl) args =
  let open Collections in
  let module C = Compare in
  let module G = Love_gas.Cost_of in
      let ( gas, f ) = generic_primitives.(prim.prim_id) in
      consume_gas ctxt gas >>=? fun ctxt ->
      f apply_value ctxt env prim xl (List.rev args)

let eval apply_value ctxt env (prim, xl) args =
  let res = List.compare_length_with args (arity prim) in
  if Compare.Int.(res < 0) then
    return (ctxt, VPrimitive (prim, xl, args))
  else if Compare.Int.(res > 0) then
    raise (InvariantBroken "Love_primitive called with too many arguments")
  else
    eval_prim apply_value ctxt env (prim, xl) args
