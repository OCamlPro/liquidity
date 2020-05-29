(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Dune_lang_repr.For_all (* Script_repr = Dune_lang_repr *)

(* Delegated storage changed type of value from Contract_hash to
   Contract_repr. Move all 'delegated' data into a storage with
   the original type, then copy over into the new storage. *)
let migrate_delegated ctxt contract =
  let path = "contracts" :: (* module Contract *)
             "index" :: (* module Indexed_context *)
             Contract_repr.Index.to_path contract [
               "delegated" ; (* module Delegated *)
             ] in
  let path_tmp = "contracts" :: (* module Contract *)
                 "index" :: (* module Indexed_context *)
                 Contract_repr.Index.to_path contract [
                   "delegated_004" ; (* module Delegated *)
                 ] in
  Raw_context.dir_mem ctxt path >>= fun exists ->
  if exists then
    Raw_context.copy ctxt path path_tmp >>=? fun ctxt ->
    Raw_context.remove_rec ctxt path >>= fun ctxt ->
    Storage.Contract.Delegated_004.fold (ctxt, contract) ~init:(Ok ctxt) ~f:(fun delegated ctxt ->
        Lwt.return ctxt >>=? fun ctxt ->
        let originated = Contract_repr.originated_contract_004 delegated in
        Storage.Contract.Delegated.add (ctxt, contract) originated >>= fun ctxt ->
        return ctxt
      ) >>=? fun ctxt ->
    Raw_context.remove_rec ctxt path_tmp >>= fun ctxt ->
    return ctxt
  else
    return ctxt

let transform_script:
  (manager_pkh: Signature.Public_key_hash.t ->
   script_code: Script_michelson.expr ->
   script_storage: Script_michelson.expr ->
   Script_repr.lazy_expr * Script_repr.lazy_expr) ->
  manager_pkh: Signature.Public_key_hash.t ->
  Raw_context.t ->
  Contract_repr.t ->
  Script_michelson.expr ->
  Raw_context.t tzresult Lwt.t =
  fun transformation ~manager_pkh ctxt contract code ->
    Storage.Contract.Storage.get ctxt contract >>=? fun (_ctxt, storage) ->
    Lwt.return (Script_repr.force_decode storage) >>=? fun (storage, _gas_cost) ->
    let storage = Dune_lang_repr.get_michelson storage in
    let (migrated_code, migrated_storage) =
      transformation manager_pkh code storage
    in
    (* Set the migrated script code for free *)
    Storage.Contract.Code.set_free ctxt contract migrated_code >>=? fun (ctxt, code_size_diff) ->
    (* Set the migrated script storage for free *)
    Storage.Contract.Storage.set_free ctxt contract migrated_storage >>=? fun (ctxt, storage_size_diff) ->
    Storage.Contract.Used_storage_space.get ctxt contract >>=? fun used_space ->
    let total_size = Z.(add (of_int code_size_diff) (add (of_int storage_size_diff) used_space)) in
    (* Free storage space for migrated contracts *)
    Storage.Contract.Used_storage_space.set ctxt contract total_size >>=? fun ctxt ->
    Storage.Contract.Paid_storage_space.get ctxt contract >>=? fun paid_space ->
    if Compare.Z.(paid_space < total_size) then
      Storage.Contract.Paid_storage_space.set ctxt contract total_size >>=? fun ctxt ->
      return ctxt
    else
      return ctxt

let manager_script_storage: Signature.Public_key_hash.t -> Script_repr.lazy_expr =
  fun manager_pkh ->
  let open Micheline in
  Script_repr.lazy_expr @@ strip_locations @@
  (* store in optimized binary representation - as unparsed with [Optimized]. *)
  let bytes = Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding manager_pkh in
  Bytes (0, bytes)

(* If the given contract is not allocated, we'll allocate it with 1 cent.
   so that the migrated contracts' managers don't have to pay origination burn *)
let allocate_contract ctxt contract balance_updates =
  Contract_storage.allocated ctxt contract >>=? function
  | true ->
      return ( ctxt, balance_updates )
  | false ->
      let amount = Tez_repr.one_cent in
      Contract_storage.credit ctxt contract amount >>=? fun ctxt ->
      let balance_updates =
        ( Delegate_storage.Contract contract, Delegate_storage.Credited amount )
        :: balance_updates
      in
      return ( ctxt, balance_updates )

(* Process an individual contract *)
let process_contract_add_manager contract (ctxt, balance_updates) =
  let open Legacy_script_support_repr in
  match Contract_repr.is_originated contract with
  | None ->
      begin
        Storage.Contract.Manager.get_option ctxt contract >>=? function
        | None -> return_false
        | Some (Manager_repr.Hash _) -> return_false
        | Some (Manager_repr.Public_key _) -> return_true
      end >>=? fun revealed ->
      if revealed then
        return (ctxt, balance_updates)
      else
        begin
          Storage.Contract.Balance.get_option ctxt contract
          >>=? function
          | Some balance -> return balance
          | None -> return Tez_repr.zero
        end >>=? fun balance ->
        if Tez_repr.( balance >= Tez_repr.ten ) then
          Storage.Contract.Legacy2019.set ctxt contract true >>= fun ctxt ->
          return (ctxt, balance_updates)
        else
          return (ctxt, balance_updates)
  | Some _ -> begin
      Storage.Contract.Counter.remove ctxt contract >>= fun ctxt ->
      Storage.Contract.Spendable_004.mem ctxt contract >>= fun is_spendable ->
      Storage.Contract.Delegatable_004.mem ctxt contract >>= fun is_delegatable ->
      Storage.Contract.Spendable_004.del ctxt contract >>= fun ctxt ->
      Storage.Contract.Delegatable_004.del ctxt contract >>= fun ctxt ->
      (* Try to get script code (ignore ctxt update to discard the initialization) *)
      Storage.Contract.Code.get_option ctxt contract >>=? fun (_ctxt, code) ->
      (* Get the manager of the originated contract *)
      Contract_storage.get_manager_004 ctxt contract >>=? fun manager_pkh ->
      let manager = Contract_repr.implicit_contract manager_pkh in
      Storage.Contract.Manager.remove ctxt contract >>= fun ctxt ->
      match code with
      | Some code ->
          (*
          | spendable | delegatable | template         |
          |-----------+-------------+------------------|
          | true      | true        | add_do           |
          | true      | false       | add_do           |
          | false     | true        | add_set_delegate |
          | false     | false       | nothing          |
          *)
          Lwt.return (Dune_lang_repr.force_decode code) >>=? fun (code, _gas_cost) ->
          let code = Dune_lang_repr.get_michelson code in
          if is_spendable then
            transform_script add_do ~manager_pkh ctxt contract code >>=? fun ctxt ->
            allocate_contract ctxt manager balance_updates
          else if is_delegatable then
            transform_script add_set_delegate ~manager_pkh ctxt contract code >>=? fun ctxt ->
            allocate_contract ctxt manager balance_updates
          else if has_default_entrypoint code then
            transform_script
              (fun ~manager_pkh:_ ~script_code  ~script_storage ->
                 (add_root_entrypoint script_code,
                  Dune_lang_repr.canon script_storage))
              ~manager_pkh ctxt contract code
            >>=? fun ctxt ->
            return (ctxt, balance_updates)
          else
            return (ctxt, balance_updates)
      | None ->
          Delegate_storage.get ctxt contract >>=? fun contract_delegate ->
          Delegate_storage.get ctxt manager >>=? fun manager_delegate ->
          Contract_storage.get_balance ctxt contract >>=? fun balance ->
          (* automatically delegate the manager *)
          begin
            match contract_delegate, manager_delegate with
            | None, _ -> return (ctxt, balance_updates)
            | Some contract_delegate, None ->
                if Tez_repr.(balance = Tez_repr.zero) then
                  return (ctxt, balance_updates)
                else
                  allocate_contract ctxt manager balance_updates
                  >>=? fun ( ctxt, balance_updates ) ->
                  Delegate_storage.set ctxt manager (Some contract_delegate)
                  >>=? fun ctxt ->
                  return (ctxt, balance_updates)
            | Some _contract_delegate, Some _manager_delegate ->
                return (ctxt, balance_updates)
          end >>=? fun (ctxt, balance_updates) ->
          (* Initialize the script code for free *)
          Storage.Contract.Code.init_free ctxt contract
            manager_script_code >>=? fun (ctxt, code_size) ->
          let storage = manager_script_storage manager_pkh in
          (* Initialize the script storage for free *)
          Storage.Contract.Storage.init_free ctxt contract storage >>=? fun (ctxt, storage_size) ->
          let total_size = Z.(add (of_int code_size) (of_int storage_size)) in
          (* Free storage space for migrated contracts *)
          Storage.Contract.Paid_storage_space.init_set ctxt contract total_size >>= fun ctxt ->
          Storage.Contract.Used_storage_space.init_set ctxt contract total_size >>= fun ctxt ->
          allocate_contract ctxt manager balance_updates
    end

(* The [[update_contract_script]] function returns a copy of its
   argument (the Micheline AST of a contract script) with "ADDRESS"
   replaced by "ADDRESS; CHAIN_ID; PAIR".

   [[Micheline.strip_locations]] should be called on the resulting
   Micheline AST to get meaningful locations. *)

let rec update_contract_script : ('l, 'p) Micheline.node -> ('l, 'p) Micheline.node
  = function
    | Micheline.Seq (_,
                     Micheline.Prim (_, Michelson_v1_primitives.I_ADDRESS, [], []) ::
                     l) ->
        Micheline.Seq (0,
                       Micheline.Prim (0, Michelson_v1_primitives.I_ADDRESS, [], []) ::
                       Micheline.Prim (0, Michelson_v1_primitives.I_CHAIN_ID, [], []) ::
                       Micheline.Prim (0, Michelson_v1_primitives.I_PAIR, [], []) :: l)
    | Micheline.Seq (_, a :: l) ->
        let a' = update_contract_script a in
        let b = Micheline.Seq (0, l) in
        let b' = update_contract_script b in
        begin match b' with
          | Micheline.Seq (_, l') ->
              Micheline.Seq (0, a' :: l')
          | _ -> assert false
        end
    | Micheline.Prim (_, p, l, annot) ->
        Micheline.Prim (0, p, List.map update_contract_script l, annot)
    | script -> script

let migrate_multisig_script (ctxt : Raw_context.t) (contract : Contract_repr.t)
    (code : Script_repr.expr) : Raw_context.t tzresult Lwt.t =
  let migrated_code =
    Script_repr.lazy_expr @@ Micheline.strip_locations @@
    update_contract_script @@ Micheline.root (Dune_lang_repr.get_michelson code)
  in
  Storage.Contract.Code.set_free ctxt contract migrated_code >>=? fun (ctxt, _code_size_diff) ->
  (* Set the spendable and delegatable flags to false so that no entrypoint gets added by
     the [[process_contract_add_manager]] function. *)
  Storage.Contract.Spendable_004.set ctxt contract false >>= fun ctxt ->
  Storage.Contract.Delegatable_004.set ctxt contract false >>= fun ctxt ->
  return ctxt

(* The hash of the multisig contract; only contracts with this exact
   hash are going to be updated by the [[update_contract_script]]
   function. *)
let multisig_hash : Script_expr_hash.t =
  Script_expr_hash.of_bytes_exn @@
  MBytes.of_hex @@
  `Hex "475e37a6386d0b85890eb446db1faad67f85fc814724ad07473cac8c0a124b31"

let process_contract_multisig (contract : Contract_repr.t)
    ( (ctxt,balance_updates) as acc ) =
  Contract_storage.get_script ctxt contract >>=? fun (ctxt, script_opt) ->
  match script_opt with
  | None ->
      (* Do nothing on scriptless contracts *)
      return acc
  | Some { Script_repr.code = code ; Script_repr.storage = _storage } ->
      (* The contract has some script, only try to modify it if it has
         the hash of the multisig contract *)
      Lwt.return (Script_repr.force_decode code) >>=? fun (code, _gas_cost) ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Script_repr.expr_encoding code
      in
      let hash = Script_expr_hash.hash_bytes [ bytes ] in
      if (hash = multisig_hash) then
        migrate_multisig_script ctxt contract code >>=? fun ctxt ->
        return (ctxt, balance_updates)
      else
        return acc

(* Process an individual contract *)
let process_contract contract ctxt =
  process_contract_multisig contract ctxt >>=? fun ctxt ->
  process_contract_add_manager contract ctxt >>=? fun ctxt ->
  return ctxt

let invoice_contract ctxt kt1_addr amount =
  let amount = Tez_repr.of_mutez_exn (Int64.(mul 1_000_000L (of_int amount))) in
  match Contract_repr.of_b58check kt1_addr with
  | Ok recipient -> begin
      Contract_storage.credit ctxt recipient amount >>= function
      | Ok ctxt -> return ctxt
      | Error _ -> return ctxt end
  | Error _ -> return ctxt

(* Extract Big_maps from their parent contract directory,
   recompute their used space, and assign them an ID. *)
let migrate_contract_big_map ctxt contract =
  Storage.Contract.Code.get_option ctxt contract >>=? function
  | ctxt, None -> return ctxt
  | ctxt, Some code ->
      Storage.Contract.Storage.get ctxt contract >>=? fun (ctxt, storage) ->
      let extract_big_map_types expr =
        let open Michelson_v1_primitives in
        let open Micheline in
        match Micheline.root (Dune_lang_repr.get_michelson expr) with
        | Seq (_, [ Prim (_, K_storage, [ expr ], _) ; _ ; _ ])
        | Seq (_, [ _ ; Prim (_, K_storage, [ expr ], _) ; _ ])
        | Seq (_, [ _ ; _ ; Prim (_, K_storage, [ expr ], _) ]) ->
            begin match expr with
              | Prim (_, T_pair, [ Prim (_, T_big_map, [ kt ; vt ], _ ) ; _ ], _) -> Some (kt, vt)
              | _ -> None
            end
        | _ -> None in
      let rewrite_big_map expr id =
        let open Michelson_v1_primitives in
        let open Micheline in
        match Micheline.root (Dune_lang_repr.get_michelson expr) with
        | Prim (_, D_Pair, [ Seq (_, _ (* ignore_unused_origination_literal *)) ; pannot ], sannot) ->
            Micheline.strip_locations (Prim (0, D_Pair, [ Int (0, id) ; pannot ], sannot))
        | _ -> assert false in
      Lwt.return (Script_repr.force_decode code) >>=? fun (code, _) ->
      match extract_big_map_types code with
      | None -> return ctxt
      | Some (kt, vt) ->
          Lwt.return (Script_repr.force_decode storage) >>=? fun (storage, _) ->
          Storage.Big_map.Next.incr ctxt >>=? fun (ctxt, id) ->
          let contract_path suffix =
            "contracts" :: (* module Contract *)
            "index" :: (* module Indexed_context *)
            Contract_repr.Index.to_path contract suffix in
          let old_path = contract_path [ "big_map" ] in
          let storage = rewrite_big_map storage id in
          Storage.Contract.Storage.set ctxt contract (Script_repr.lazy_expr storage) >>=? fun (ctxt, _) ->
          let kt = Micheline.strip_locations (Script_michelson.strip_annotations kt) in
          let vt = Micheline.strip_locations (Script_michelson.strip_annotations vt) in
          Storage.Big_map.Key_type.init ctxt id kt >>=? fun ctxt ->
          Storage.Big_map.Value_type.init ctxt id vt >>=? fun ctxt ->
          Raw_context.dir_mem ctxt old_path >>= fun exists ->
          if exists then
            let read_size ctxt key =
              Raw_context.get ctxt key >>=? fun len ->
              match Data_encoding.(Binary.of_bytes int31) len with
              | None -> assert false
              | Some len -> return len in
            let iter_sizes f (ctxt, acc) =
              let rec dig i path (ctxt, acc) =
                if Compare.Int.(i <= 0) then
                  Raw_context.fold ctxt path ~init:(ok (ctxt, acc)) ~f:begin fun k acc ->
                    Lwt.return acc >>=? fun (ctxt, acc) ->
                    match k with
                    | `Dir _ -> return (ctxt, acc)
                    | `Key file ->
                        match List.rev file with
                        | last :: _ when Compare.String.(last = "data") ->
                            return (ctxt, acc)
                        | last :: _ when Compare.String.(last = "len") ->
                            read_size ctxt file >>=? fun len ->
                            return (ctxt, f len acc)
                        | _ -> assert false
                  end
                else
                  Raw_context.fold ctxt path ~init:(ok (ctxt, acc)) ~f:begin fun k acc ->
                    Lwt.return acc >>=? fun (ctxt, acc) ->
                    match k with
                    | `Dir k -> dig (i-1) k (ctxt, acc)
                    | `Key _ -> return (ctxt, acc)
                  end in
              dig Script_expr_hash.path_length old_path (ctxt, acc) in
            iter_sizes
              (fun s acc -> (acc |> Z.add (Z.of_int s) |> Z.add (Z.of_int 65)))
              (ctxt, (Z.of_int 0)) >>=? fun (ctxt, total_bytes) ->
            Storage.Big_map.Total_bytes.init ctxt id total_bytes >>=? fun ctxt ->
            let new_path = "big_maps" :: (* module Big_map *)
                           "index" :: (* module Indexed_context *)
                           Storage.Big_map.Index.to_path id [
                             "contents" ; (* module Delegated *)
                           ] in
            Raw_context.copy ctxt old_path new_path >>=? fun ctxt ->
            Raw_context.remove_rec ctxt old_path >>= fun ctxt ->
            read_size ctxt (contract_path [ "len" ; "code" ]) >>=? fun code_size ->
            read_size ctxt (contract_path [ "len" ; "storage" ]) >>=? fun storage_size ->
            let total_bytes =
              total_bytes |>
              Z.add (Z.of_int 33) |>
              Z.add (Z.of_int code_size) |>
              Z.add (Z.of_int storage_size) in
            Storage.Contract.Used_storage_space.get ctxt contract >>=? fun previous_size ->
            Storage.Contract.Paid_storage_space.get ctxt contract >>=? fun paid_bytes ->
            let change = Z.sub paid_bytes previous_size in
            Storage.Contract.Used_storage_space.set ctxt contract total_bytes >>=? fun ctxt ->
            Storage.Contract.Paid_storage_space.set ctxt contract (Z.add total_bytes change)
          else
            Storage.Big_map.Total_bytes.init ctxt id Z.zero >>=? fun ctxt ->
            return ctxt

let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Dune_debug.printf ~n:0 "prepare_first_block";
  Raw_context.prepare_first_block
    ~level ~timestamp ~fitness ctxt >>=? fun (previous_protocol, ctxt) ->
  (* We just increased the protocol revision. Let's check that this
     implementation supports it, or fail. *)
  Dune_storage.check_protocol_revision ctxt >>=? fun () ->
  Storage.Big_map.Next.init ctxt >>=? fun ctxt ->
  match previous_protocol with
  | Genesis param ->
      Commitment_storage.init ctxt param.commitments >>=? fun ctxt ->
      Roll_storage.init ctxt >>=? fun ctxt ->
      Seed_storage.init ctxt >>=? fun ctxt ->
      Contract_storage.init ctxt >>=? fun ctxt ->
      Bootstrap_storage.init ctxt
        ~typecheck
        ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt >>=? fun ctxt ->
      Vote_storage.init ctxt >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0 >>=? fun ctxt ->
      Vote_storage.freeze_listings ctxt >>=? fun ctxt ->
      Storage.Pending_protocol_init.init ctxt 0 >>=? fun ctxt ->
      return ctxt
  | Athens_004 ->
      Dune_debug.printf ~n:0 "Protocol upgrade: phase 1.1";
      Roll_storage.Delegate.compute_nrolls ctxt >>=? fun ctxt ->
      Storage.Vote.Current_quorum_004.get ctxt >>=? fun quorum ->
      Storage.Vote.Participation_ema.init ctxt quorum >>=? fun ctxt ->
      Storage.Vote.Current_quorum_004.delete ctxt >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0 >>=? fun ctxt ->
      Storage.Last_block_priority.delete ctxt >>=? fun ctxt ->
      Dune_debug.printf ~n:0 "Protocol upgrade: phase 1.2";
      Storage.Contract.fold ctxt ~init:(Ok ctxt)
        ~f:(fun contract ctxt ->
            Lwt.return ctxt >>=? fun ctxt ->
            (* type of 'delegated' changed from a set of KT1 to a set of
               contracts, including dn1. *)
            migrate_delegated ctxt contract >>=? fun ctxt ->

            migrate_contract_big_map ctxt contract
          ) >>=? fun ctxt ->
      Dune_debug.printf ~n:0 "Protocol upgrade: phase 1.3";
      Dune_storage.get_foundation_pubkey ctxt >>= fun superadmin_pk ->
      Contract_storage.change_superadmin ctxt
        (Contract_repr.implicit_contract
           (Signature.Public_key.hash superadmin_pk)) true >>= fun ctxt ->
      Dune_storage.remove_foundation_pubkey ctxt >>= fun ctxt ->
      (* Set that we are waiting for phase 2 *)
      Storage.Pending_protocol_init.init ctxt 1 >>=? fun ctxt ->
      Dune_debug.printf ~n:0 "Protocol upgrade: phase 1 done";
      return ctxt

(* Since prepare_first_block is executed at the end of the last block
  of the previous protocol, we cannot correctly set the balance updates.
  So, we postpone the actions to the first block of the current protocol,
   using a value Pending_protocol_init in the context.
*)
let finish_prepare_first_block ctxt =
  let balance_updates = [] in
  Storage.Pending_protocol_init.get ctxt >>=? fun has_init ->
  if Compare.Int.equal has_init 1 then
    Storage.Pending_protocol_init.set ctxt 0 >>=? fun ctxt ->

    Dune_debug.printf ~n:0 "Protocol upgrade: phase 2.1";
    Storage.Contract.fold ctxt ~init:(Ok (ctxt,balance_updates))
      ~f:(fun contract acc ->
          Lwt.return acc >>=? fun acc ->
          process_contract contract acc
        ) >>=? fun (ctxt, balance_updates) ->
    Dune_debug.printf ~n:0 "Protocol upgrade: phase 2 done";

    return (ctxt, balance_updates)
  else
    return (ctxt, balance_updates)

let prepare ctxt ~level ~predecessor_timestamp ~timestamp ~fitness =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
  >>=? fun ctxt ->
  (* Check that the protocol revision in which we want to build a
     block is supported. Usefull for importing snapshots. *)
  Dune_storage.check_protocol_revision ctxt >>=? fun () ->
  return ctxt
