open LiquidClientUtils
open LiquidClientRequest
open LiquidClientSigs
open Lwt.Infix
open Dune_Network_Lib (* for crypto *)

module Make
    (L : LANG)
    (SourceFrom : TT)
    (TargetFrom : TT)
    (C : CONVERTER with type source_from_datatype := SourceFrom.datatype
              and type source_from_const := SourceFrom.const
              and type source_from_contract := SourceFrom.contract
              and type source_dest_datatype := L.Source.datatype
              and type source_dest_const := L.Source.const
              and type source_dest_contract := L.Source.contract
              and type target_from_datatype := TargetFrom.datatype
              and type target_from_const := TargetFrom.const
              and type target_from_contract := TargetFrom.contract
              and type target_dest_datatype := L.Target.datatype
              and type target_dest_const := L.Target.const
              and type target_dest_contract := L.Target.contract) = struct

  module C = C
  module L = L
  module RPC = LiquidClientRPCs.Make(L)
  module E = RPC.E
  module T = RPC.E.T

  open L
  open E
  open T
  open RPC


  type liq_big_map_diff = (bm_id, Source.const) Big_map_diff.t

  let get_private_key ?private_key () =
    match private_key, !LiquidOptions.private_key with
    | None, None -> failwith "Missing private key"
    | Some sk, _ | _, Some sk ->
      match Ed25519.Secret_key.of_b58check sk with
      | Ok sk -> sk
      | Error _ ->
        failwith "Bad private key: must be an Ed25519, \
                  base58-check encoded private key of the form edsk..."

  let get_public_key ?public_key () =
    match public_key, !LiquidOptions.public_key with
    | Some pk, _ | _, Some pk -> pk
    | None, None ->
      try get_private_key () |> get_public_key_from_secret_key
      with _ -> failwith "Missing public key"

  let get_source ?source () =
    match source, !LiquidOptions.source with
      | Some source, _ | _, Some source -> source
      | None, None ->
        try
          let pk = match Ed25519.Public_key.of_b58check (get_public_key ()) with
            | Ok pk -> pk
            | Error _ -> raise Exit in
          get_public_key_hash_from_public_key pk
        with _ ->
        try get_private_key () |> get_public_key_from_secret_key
        with _ -> failwith "Missing source"

  let get_next_counter source =
    match !LiquidOptions.counter with
    | None ->
      get_counter source >>= fun counter ->
      Lwt.return (counter+1)
    | Some counter -> Lwt.return counter

  let big_map_info storage storage_ty id =
    Source.list_big_maps storage storage_ty
    |> List.find_opt (fun ((Bm_id i | Bm_name (i, _)), _, _) -> i = id)

  let id_of_info id info = match info with
    | None -> Bm_id id
    | Some (id, _, _) -> id

  let convert_big_map_diff_item storage storage_ty =
    let open Big_map_diff in
    function
    | Big_map_add { id; key_hash; key; value } ->
      let info = big_map_info storage storage_ty id in
      let id = id_of_info id info in
      let key, value = match info with
        | None ->
          decompile_const key,
          decompile_const value
        | Some (_, tk, tv) ->
          decompile_const key ~ty:tk,
          decompile_const value ~ty:tv in
      Big_map_add { id; key_hash; key; value }
    | Big_map_remove { id; key_hash; key } ->
      let info = big_map_info storage storage_ty id in
      let id = id_of_info id info in
      let key = match info with
        | None -> decompile_const key
        | Some (_, tk, _) -> decompile_const key ~ty:tk in
      Big_map_remove { id; key_hash; key }
    | Big_map_delete { id } ->
      let info = big_map_info storage storage_ty id in
      let id = id_of_info id info in
      Big_map_delete { id }
    | Big_map_alloc { id } ->
      let info = big_map_info storage storage_ty id in
      let id = id_of_info id info in
      Big_map_alloc { id }
    | Big_map_copy { source_id; destination_id } ->
      let source_info = big_map_info storage storage_ty source_id in
      let source_id = id_of_info source_id source_info in
      let destination_info = big_map_info storage storage_ty destination_id in
      let destination_id = id_of_info destination_id destination_info in
      Big_map_copy { source_id; destination_id }

  let convert_big_map_diff storage storage_ty l =
    List.map (convert_big_map_diff_item storage storage_ty) l

  (* let decode_convert_big_map_diff storage storage_ty json_opt =
   *   match json_opt with
   *   | None -> []
   *   | Some json ->
   *     decode_big_map json
   *     |> convert_big_map_diff storage storage_ty *)

  let convert_manager_operation =
    function
    | Operation.Reveal s -> SourceOperation.Reveal s
    | Delegation s -> SourceOperation.Delegation s
    | Transaction {amount; destination; entrypoint; parameters = None} ->
      SourceOperation.Transaction {amount; destination; entrypoint; parameters = None}
    | Transaction {amount; destination; entrypoint; parameters = Some p} ->
      SourceOperation.Transaction {amount; destination; entrypoint;
                   parameters = Some (decompile_const p)}
    | Origination {delegate; script= None; balance} ->
      SourceOperation.Origination {delegate; script= None; balance}
    | Origination {delegate; script= Some (c, s); balance} ->
      SourceOperation.Origination {delegate; balance;
                   script= Some (decompile_contract c, decompile_const s); }

  let convert_operation = function
    | Operation.Manager m ->
      SourceOperation.Manager (convert_manager_operation m)
    | Activate_account { pkh; secret } ->
      SourceOperation.Activate_account { pkh; secret }

  let convert_internal_operation op =
    { SourceOperation.op = convert_operation op.Operation.op;
      source = op.Operation.source;
      nonce = op.Operation.nonce }

  let get_head ?head () =
    match head with
    | Some head -> Lwt.return head
    | None -> get_head ()

  let run_pre ?(debug=false) ?(amount = !LiquidOptions.amount)
      source_contract target_contract loc_table ?source entry_name input storage =
    let rpc = if debug then RPC.trace else RPC.run in
    let storage_ty = Source.storage source_contract in
    let input_ty =
      match List.assoc_opt entry_name (Source.entries source_contract) with
      | Some ty -> ty
      | None -> failwith ("Contract has no entry point " ^ entry_name) in
    let input = compile_const ~ty:input_ty input in
    let storage = compile_const ~ty:storage_ty storage in
    get_head () >>= fun head ->
    let operation = Run_code.Input.{
      script = target_contract;
      entrypoint = entry_name;
      input;
      storage;
      amount;
      chain_id = head.chain_id;
      source;
    } in
    rpc ~loc_table operation
    >>= fun Run_code.Result.{ storage ; operations; big_map_diff; trace } ->
    let storage = decompile_const ~ty:storage_ty storage in
    let big_map_diff = convert_big_map_diff storage storage_ty big_map_diff in
    let operations = List.map convert_internal_operation operations in
    let trace = match trace with
      | None -> None
      | Some trace -> Some (E.convert_trace ~loc_table trace) in
    Lwt.return (operations, storage, big_map_diff, trace)

  let run ~debug ?source ?amount contract entry_name parameter storage =
    let source = get_source ?source () in
    let target_contract, _, loc_table = compile_contract contract in
    run_pre ~debug contract
      target_contract loc_table ~source entry_name parameter storage

  let run_debug ?source ?amount liquid entry_name input storage =
    run ~debug:true ?source liquid entry_name input storage
    >>= function
    | (nbops, sto, big_diff, Some trace) ->
      Lwt.return (nbops, sto, big_diff, trace)
    | _ -> assert false

  let run ?source ?amount liquid entry_name input storage =
    run ~debug:false ?source liquid entry_name input storage
    >>= fun (nbops, sto, big_diff, _) ->
    Lwt.return (nbops, sto, big_diff)

  let get_storage contract address =
    let _ = compile_contract contract in
    RPC.get_storage address >|= fun storage ->
    try decompile_const storage ~ty:(Source.storage contract)
    with _ ->
      Format.eprintf "Could not convert constant to contract storage type.@.";
      decompile_const storage

  let get_big_map_value big_map_info key =
    let ((Bm_id id | Bm_name (id, _)), key_ty, val_ty) = big_map_info in
    let key_t = compile_const ~ty:key_ty key in
    let ty = compile_datatype key_ty in
    RPC.pack ~data:key_t ~ty >>= fun packed_key ->
    let hash_key_b58 =
      ExprHash.hash_bytes [Bigstring.of_bytes packed_key]
      |> ExprHash.to_b58check in
    RPC.get_big_map_hash_value id hash_key_b58 >|= function
    | None -> None
    | Some expr ->
      try Some (decompile_const expr ~ty:val_ty)
      with _ ->
        Format.eprintf "Could not convert constant to value type.@.";
        Some (decompile_const expr)


  let is_revealed source =
    RPC.get_manager_key source >|= function
    | None -> false
    | Some _ -> true


  let big_map_elements id big_map_diff =
    let open Big_map_diff in
    List.fold_left (fun acc -> function
        | Big_map_add { id = (Bm_id i | Bm_name (i, _)); key; value } when id = i ->
          (key, value) :: acc
        | _ -> acc
      ) [] big_map_diff |> List.rev

  let build_big_map_subst const const_ty big_map_diff =
    let open Big_map_diff in
    Source.list_big_maps const const_ty |>
    List.map (fun ((Bm_id i | Bm_name (i, _)), _, _) ->
        i, big_map_elements i big_map_diff
      )

  let replace_init_big_maps big_map_diff storage storage_ty =
    Source.apply_big_map_subst
      (build_big_map_subst storage storage_ty big_map_diff)
      storage


  let init_storage ?source contract comp_init init_params =
    let source =
      try Some (get_source ?source ())
      with _ -> None in
    match comp_init, init_params with
    | No_init, [c]
    | Init_constant c, [] -> Lwt.return c

    | No_init, [] ->
      raise (ResponseError "init_storage: Missing init")
    | No_init, _ ->
      raise (ResponseError "init_storage: No initializer, cannot take arguments")
    | Init_constant _, _ ->
      raise (ResponseError "init_storage: Constant initializer, cannot take arguments")

    | Init_code (c, args_tys), _ ->
      let l_req, l_giv = List.length args_tys, List.length init_params in
      if l_req <> l_giv then
        raise
          (ResponseError
             (Printf.sprintf
                "init_storage: init storage needs %d arguments, but was given %d"
                l_req l_giv
             ));
      let eval_input_storage =
        try
          Source.default_empty_const (Source.storage contract)
        with Not_found -> failwith "could not construct dummy storage for eval"
      in
      let eval_input_parameter = match init_params with
        | [] -> Source.unit
        | [x] -> x
        | _ -> Source.tuple init_params
      in

      let ct, _, loc_table = compile_contract c in
      run_pre c ct loc_table ?source "default"
        eval_input_parameter eval_input_storage
      >>= fun (_, eval_init_storage, big_map_diff, _) ->
      (* Add elements of big map *)
      let eval_init_storage =
        replace_init_big_maps big_map_diff eval_init_storage (Source.storage contract) in
      Lwt.return eval_init_storage


  let estimate_gas_storage ~loc_table ?head operation =
    get_head ?head () >>= fun head ->
    let open Run_operation in
    RPC.run_operation ~loc_table ~chain_id:head.Header.chain_id operation
    >>= fun { Result.contents }  ->
    let (_, metadata) as res = match contents with
      | [ (Operation.Manager { op = Reveal _ }, _)  ; y ] -> y
      | [ y ] -> y
      | _ -> invalid_arg "estimate_gas_storage" in
    let extract_consumed = function
      | Result.Failed errors -> Lwt.return_error ("failed", errors)
      | Backtracked (errors, _) -> Lwt.return_error ("backtracked", errors)
      | Skipped -> Lwt.return_error ("skipped", [])
      | Other json ->
        Lwt.return_error
          ("unexpected node response metadata for run_operation", [json])
      | Applied res ->
        let allocated = List.length res.originated_contracts +
                        if res.allocated_destination_contract then 1 else 0 in
        let consumed_storage = allocated * 257 + res.paid_storage_size_diff in
        Lwt.return_ok (res.consumed_gas, consumed_storage)
    in
    extract_consumed metadata.Result.operation_result
    >>= fun res  ->
    Lwt_list.fold_left_s (fun acc (_, internal_result) ->
        extract_consumed internal_result
        >>= function
        | Error (int_st, internal_errs) -> (match acc with
            | Error (st, errs) -> Lwt.return_error (String.concat "/" [st; int_st],
                                                    errs @ internal_errs)
            | Ok _ -> Lwt.return_error (int_st, internal_errs)
          )
        | Ok (internal_gas, internal_storage) -> match acc with
          | Error errs -> Lwt.return_error errs
          | Ok (gas, storage) ->
            Lwt.return_ok (gas + internal_gas, storage + internal_storage)
      )
      res
      metadata.Result.internal_operation_results
    >>= function
    | Error (st, errs) ->
      raise_response_error ~loc_table st
        (`A (List.map Json_repr.from_any errs))
    | Ok (consumed_gas, consumed_storage) ->
    Format.eprintf "Estimated gas limit:\t %d\nEstimated storage limit: %d@."
      consumed_gas consumed_storage;
    Lwt.return (consumed_gas, consumed_storage)

  let rec forge_op ?head ?source ?public_key
      ?fee ?gas_limit ?storage_limit ?real_op_size
      ~loc_table op =
    let source = get_source ?source () in
    get_head ?head () >>= fun head ->
    RPC.get_constants ()
    >>= fun Constants.{ hard_gas_limit_per_operation; hard_storage_limit_per_operation } ->
    get_next_counter source >>= fun counter ->
    is_revealed source >>= fun source_revealed ->
    let storage_limit = match storage_limit with
      | Some l -> l
      | None -> hard_storage_limit_per_operation in
    let gas_limit = match gas_limit with
      | Some l -> l
      | None -> hard_gas_limit_per_operation in
    let computed_fee = match real_op_size with
      | None -> Z.zero
      | Some size ->
        let gas_limit =
          if source_revealed then gas_limit else gas_limit + 10000 in
        compute_fees ~gas_limit ~size
    in
    let computed_fee = match fee with
      | None -> LiquidNumber.tez_of_mic_mutez computed_fee
      | Some fee when Z.compare (LiquidNumber.mic_mutez_of_tez fee) computed_fee < 0 ->
        Format.kasprintf failwith
          "Fee too low, operation would never be included: given %s, but required \
           at least %s in fee."
          LiquidNumber.(liq_of_tez fee)
          LiquidNumber.(liq_of_tez @@ tez_of_mic_mutez computed_fee)
      | Some fee -> fee in
    let operation counter = Operation.(Manager {
        source;
        fee = computed_fee;
        counter; gas_limit; storage_limit;
        op;
    }) in
    let operations =
      if source_revealed then
        [operation counter]
      else
        let edpk = get_public_key ?public_key () in
        let reveal = Operation.(Manager {
            source;
            fee = {tezzies = "0"; mutez = None};
            counter;
            gas_limit = 1_0000;
            storage_limit = 0;
            op = Reveal edpk;
          }) in
        [reveal; operation (counter + 1)]
    in
    let data = Operation.{
        branch = head.Header.hash;
        contents = operations;
        signature = if real_op_size = None then Some dummy_sign else None
      } in
    match real_op_size with
    | None ->
      Lwt.return (data, None, loc_table)
    | Some size ->
      RPC.forge_operation ~loc_table data >>= fun op_bytes ->
      let actual_size = Bytes.length op_bytes + 64 in
      if actual_size <= size then begin
        Format.eprintf "Computed fee:\t\t %s%s@."
          (LiquidNumber.liq_of_tez computed_fee)
          (LiquidOptions.curreny ());
        Lwt.return (data, Some op_bytes, loc_table)
      end else
        (* Fix point to estimate size of operation which depends on fees
           which depends on size of operation *rolleyes* *)
        forge_op ~head ~source ?public_key ~real_op_size:actual_size
          ?fee ~gas_limit ~storage_limit
          ~loc_table op

  let forge_deploy_op ?head ?source ?public_key ?(balance = !LiquidOptions.amount)
      ?fee ?gas_limit ?storage_limit ?real_op_size
      contract init_params =
    let target_contract, comp_init, loc_table = compile_contract contract in
    init_storage ?source contract comp_init init_params >>= fun init_storage ->
    let init_storage =
      compile_const ~ty:(Source.storage contract) init_storage in
    let op = Operation.(Origination {
        delegate = None;
        balance ;
        script = Some (target_contract, init_storage);
      }) in
    forge_op ?head ?source ?public_key
      ?fee ?gas_limit ?storage_limit ?real_op_size
      ~loc_table op

  let forge_deploy ?head ?source ?public_key ?balance
      contract init_params =
    forge_deploy_op ?head ?source ?public_key ?balance
      contract init_params >>= fun (operation, _, loc_table) ->
    estimate_gas_storage ~loc_table ?head operation >>= fun (est_gas_limit, est_storage_limit) ->
    let gas_limit = match !LiquidOptions.gas_limit with
      | None -> est_gas_limit
      | Some l when l < est_gas_limit ->
        failwith (Printf.sprintf "Gas limit below estimated (%d)" est_gas_limit)
      | Some l -> l in
    let storage_limit = match !LiquidOptions.storage_limit with
      | None -> est_storage_limit
      | Some l when l < est_storage_limit ->
        failwith (Printf.sprintf "Storage limit below estimated (%d)"
                    est_storage_limit)
      | Some l -> l in
    forge_deploy_op ?head ?source ?public_key ?balance ~real_op_size:0
      ?fee:!LiquidOptions.fee ~gas_limit ~storage_limit
      contract init_params
    >|= function
    | _, None, _ -> assert false
    | (operation, Some op, loc_table) ->
      (operation, op, loc_table)

  let inject_operation ?(force=false) ?loc_table ?sk ?head operations op_b =
    get_head ?head () >>= fun head ->
    let signed_op, op_hash, operation = match sk with
      | None ->
        let op_hash =
          Operation_hash.to_b58check @@
          Operation_hash.hash_bytes [ MBytes.of_bytes op_b ] in
        op_b, op_hash,
        Operation.{
          branch = head.Header.hash;
          contents = operations;
          signature = None;
        }
      | Some sk ->
        let op_b = MBytes.of_bytes op_b in
        let signature_b = sign sk op_b in
        let signature = Ed25519.Signature.to_b58check signature_b in
        let signed_op_b = MBytes.concat "" [op_b; signature_b] in
        let op_hash =
          Operation_hash.to_b58check @@
          Operation_hash.hash_bytes [ signed_op_b ] in
        MBytes.to_bytes signed_op_b, op_hash,
        Operation.{
          branch = head.Header.hash;
          contents = operations;
          signature = Some signature;
        }
    in
    RPC.preapply_operations ?loc_table ~protocol:head.Header.protocol [operation]
    >>= function
    | [] | _ :: _ :: _ -> assert false
    | [{ Run_operation.Result.contents }] ->
      Lwt_list.map_p (fun (content, metadata) ->
          match content with
          | Operation.Activate_account _ ->
            Lwt.return []
          | _ -> match metadata.Run_operation.Result.operation_result with
            | Run_operation.Result.Failed _ | Backtracked  _ | Skipped
              when force ->
              (* Allow injection even if there is an error *)
              Format.eprintf "Warning: injecting failing operation@.";
              Lwt.return []
            | Run_operation.Result.Failed errors
            | Backtracked (errors, _) ->
              raise_response_error ?loc_table "failed"
                (`A (List.map Json_repr.from_any errors))
            | Skipped -> raise_response_error ?loc_table "skipped" (`A [])
            | Applied { originated_contracts } ->
              Lwt.return originated_contracts
            | Other json ->
              raise_response_error ?loc_table
                "unexpected node response for preapply_operations"
                (`A [Json_repr.from_any json])
        ) contents
      >>= fun originated_contracts ->
      let originated_contracts = List.flatten originated_contracts in
      RPC.injection ?loc_table signed_op >>= fun injected_op_hash ->
      assert (injected_op_hash = op_hash);
      Lwt.return (injected_op_hash, originated_contracts)


  let deploy ?balance contract init_params =
    let sk = get_private_key () in
    let source = get_source () in
    let public_key = get_public_key_from_secret_key sk in
    get_head () >>= fun head ->
    forge_deploy ~head ~source ~public_key ?balance
      contract init_params
    >>= fun (op, op_bytes, loc_table) ->
    inject_operation ~loc_table ~sk ~head op.Operation.contents op_bytes >>= function
    | op_h, [c] -> Lwt.return (op_h, c)
    | op_h, [_; c] -> Lwt.return (op_h, c) (* with revelation *)
    | _ -> raise (ResponseError "deploy (inject)")

  let rec forge_call_op ?head ?source ?public_key ?(amount = !LiquidOptions.amount)
      ?fee ?gas_limit ?storage_limit ?real_op_size
      ~loc_table address ?contract entry_name input =
    let input_ty = match contract with
      | None -> None
      | Some c -> match List.assoc_opt entry_name (Source.entries c) with
        | None -> failwith ("Contract has no entry point " ^ entry_name)
        | ty -> ty in
    let input_t = compile_const ?ty:input_ty input in
    let op = Operation.(Transaction {
        amount;
        destination = address;
        entrypoint = entry_name;
        parameters = Some input_t;
      }) in
    forge_op ?head ?source ?public_key
      ?fee ?gas_limit ?storage_limit ?real_op_size
      ~loc_table op

  let forge_call ?head ?source ?public_key ?amount
      ?contract ~address ~entry input =
    let loc_table = match contract with
      | None -> []
      | Some contract ->
        let _, _, l = compile_contract contract in l
    in
    forge_call_op ?head ?source ?public_key ?amount ~loc_table ?contract
      address entry input >>= fun (operation, _, loc_table) ->
    estimate_gas_storage ~loc_table ?head operation >>= fun (est_gas_limit, est_storage_limit) ->
    let gas_limit = match !LiquidOptions.gas_limit with
      | None -> est_gas_limit
      | Some l when l < est_gas_limit ->
        failwith (Printf.sprintf "Gas limit below estimated (%d)" est_gas_limit)
      | Some l -> l in
    let storage_limit = match !LiquidOptions.storage_limit with
      | None -> est_storage_limit
      | Some l when l < est_storage_limit ->
        failwith (Printf.sprintf "Storage limit below estimated (%d)"
                    est_storage_limit)
      | Some l -> l in
    forge_call_op ?head ?source ?public_key ?amount ~loc_table ?contract ~real_op_size:0
      ?fee:!LiquidOptions.fee ~gas_limit ~storage_limit
      address entry input
    >|= function
    | _, None, _ -> assert false
    | (operation, Some op, loc_table) ->
      (operation, op, loc_table)

  let call ?contract ?amount ~address ~entry parameter =
    let sk = get_private_key () in
    let source = get_source () in
    let public_key = get_public_key_from_secret_key sk in
    get_head () >>= fun head ->
    forge_call ~head ~source ~public_key ?amount
      ?contract ~address ~entry parameter
    >>= fun (op, op_bytes, loc_table) ->
    inject_operation ~loc_table ~sk ~head op.Operation.contents op_bytes
    >|= fun (op_h, _) -> op_h


  let reveal () =
    let sk = get_private_key () in
    let source = get_source () in
    let public_key = get_public_key_from_secret_key sk in
    get_head () >>= fun head ->
    get_next_counter source >>= fun counter ->
    let reveal = Operation.(Manager {
        source;
        fee = {tezzies = "0"; mutez = None};
        counter;
        gas_limit = 1_0000;
        storage_limit = 0;
        op = Reveal public_key;
      })
    in
    let data = Operation.{
        branch = head.Header.hash;
        contents = [reveal];
        signature = None
      } in
    RPC.forge_operation data >>= fun op ->
    inject_operation ~sk ~head data.contents op >>= fun _ ->
    Lwt.return_unit

  let activate ~secret =
    let sk = get_private_key () in
    let source = get_source () in
    get_head () >>= fun head ->
    let activate = Operation.(Activate_account { pkh = source; secret }) in
    let data = Operation.{
        branch = head.Header.hash;
        contents = [activate];
        signature = None
      } in
    RPC.forge_operation data >>= fun op ->
    inject_operation ~sk ~head data.contents op >|= fun (op_h, _) ->
    op_h

  let inject ~operation ~signature =
    let signature =
      match Ed25519.Signature.of_b58check signature with
      | Error _ -> failwith "Cannot decode signature (must be valid edsig...)"
      | Ok s -> MBytes.to_bytes s in
    RPC.injection (Bytes.cat operation signature)

  let init_storage ?source contract init_params =
    let _, comp_init, _ = compile_contract contract in
    init_storage ?source contract comp_init init_params
    >|= compile_const ~ty:(Source.storage contract)

  let pack ~const ~ty =
    let const = compile_const ~ty const in
    let ty = compile_datatype ty in
    RPC.pack ~data:const ~ty

  let print_big_map_diff_item =
    let open Big_map_diff in
    function
    | Big_map_add { id; key_hash; key; value } ->
      let key = C.SourceConv.print_const key in
      let value = C.SourceConv.print_const value in
      Big_map_add { id; key_hash; key; value }
    | Big_map_remove { id; key_hash; key } ->
      let key = C.SourceConv.print_const key in
      Big_map_remove { id; key_hash; key }
    | Big_map_delete { id } ->
      Big_map_delete { id }
    | Big_map_alloc { id } ->
      Big_map_alloc { id }
    | Big_map_copy { source_id; destination_id } ->
      Big_map_copy { source_id; destination_id }

  let print_big_map_diff l =
    List.map print_big_map_diff_item l

  let print_stack stack_expr =
    List.map (fun (e, annot) ->
        let name = name_of_var_annot annot in
        C.SourceConv.print_const e, name
      ) stack_expr

  let print_trace t =
    List.map (fun T.Trace.{ loc; gas; stack } ->
        let stack = print_stack stack in
        T.Trace.{ loc; gas; stack }
      ) t

  module type S = sig
    type 'a t
    val init_storage :
      ?source:string ->
      SourceFrom.contract ->
      SourceFrom.const list -> TargetFrom.const t
    val run :
      ?amount : LiquidNumber.tez ->
      SourceFrom.contract ->
      string ->
      SourceFrom.const ->
      SourceFrom.const ->
      (SourceOperation.internal list * SourceFrom.const *
       (LiquidClientSigs.bm_id, SourceFrom.const) T.Big_map_diff.item
         list)
        t
    val run_debug :
      ?amount : LiquidNumber.tez ->
      SourceFrom.contract ->
      string ->
      SourceFrom.const ->
      SourceFrom.const ->
      (SourceOperation.internal list * SourceFrom.const *
       (LiquidClientSigs.bm_id, SourceFrom.const) Big_map_diff.item
         list *
       (L.Source.location, SourceFrom.const) Trace.trace_item list)
        t
    val deploy :
      ?balance : LiquidNumber.tez ->
      SourceFrom.contract ->
      SourceFrom.const list -> (string * string) t
    val get_storage :
      SourceFrom.contract -> string -> SourceFrom.const t
    val get_big_map_value :
      LiquidClientSigs.bm_id * Source.datatype * Source.datatype ->
      SourceFrom.const -> SourceFrom.const option t
    val call :
      ?contract:SourceFrom.contract ->
      ?amount : LiquidNumber.tez ->
      address:string -> entry:string -> SourceFrom.const -> string t
    val activate : secret:string -> string t
    val inject : operation:bytes -> signature:string -> string t
    val pack :
      const:SourceFrom.const -> ty:SourceFrom.datatype -> bytes t
    val forge_deploy :
      ?head:Header.t ->
      ?source:string ->
      ?public_key:string ->
      ?balance : LiquidNumber.tez ->
      SourceFrom.contract -> SourceFrom.const list -> bytes t
    val forge_call :
      ?head:Header.t ->
      ?source:string ->
      ?public_key:string ->
      ?contract:SourceFrom.contract ->
      ?amount : LiquidNumber.tez ->
      address:string -> entry:string -> SourceFrom.const -> bytes t
  end

  (* Withoud optional argument head *)
  module Async : S with type 'a t := 'a Lwt.t = struct

    let init_storage ?source contract args =
      let contract = C.SourceConv.parse_contract contract in
      let args = List.map C.SourceConv.parse_const args in
      init_storage ?source contract args
      >|= fun storage ->
      C.TargetConv.print_const storage

    let run ?amount contract entry_name input storage =
      let contract = C.SourceConv.parse_contract contract in
      let input = C.SourceConv.parse_const input in
      let storage = C.SourceConv.parse_const storage in
      run ?amount contract entry_name input storage
      >|= fun (ops, storage, bm) ->
      (ops, C.SourceConv.print_const storage, print_big_map_diff bm)

    let run_debug ?amount contract entry_name input storage =
      let contract = C.SourceConv.parse_contract contract in
      let input = C.SourceConv.parse_const input in
      let storage = C.SourceConv.parse_const storage in
      run_debug ?amount contract entry_name input storage
      >|= fun (ops, storage, bm, trace) ->
      (ops, C.SourceConv.print_const storage, print_big_map_diff bm, print_trace trace)

    let deploy ?balance contract args =
      let contract = C.SourceConv.parse_contract contract in
      let args = List.map C.SourceConv.parse_const args in
      deploy ?balance contract args

    let get_storage contract address =
      let contract = C.SourceConv.parse_contract contract in
      get_storage contract address
      >|= fun storage ->
      C.SourceConv.print_const storage

    let get_big_map_value id key =
      let key = C.SourceConv.parse_const key in
      get_big_map_value id key
      >|= function
      | None -> None
      | Some v -> Some (C.SourceConv.print_const v)

    let call ?contract ?amount ~address ~entry parameter =
      let contract = match contract with
        | None -> None
        | Some c -> Some (C.SourceConv.parse_contract c) in
      let parameter = C.SourceConv.parse_const parameter in
      call ?contract ?amount ~address ~entry parameter

    let activate = activate
    let inject = inject
    let pack ~const ~ty =
      let ty = C.SourceConv.parse_datatype ty in
      let const = C.SourceConv.parse_const const in
      pack ~const ~ty

    let forge_deploy ?head ?source ?public_key ?balance contract args =
      let contract = C.SourceConv.parse_contract contract in
      let args = List.map C.SourceConv.parse_const args in
      forge_deploy ?head ?source ?public_key ?balance contract args
      >>= fun (_, op, _) -> Lwt.return op

    let forge_call ?head ?source ?public_key ?contract ?amount ~address ~entry parameter =
      let contract = match contract with
        | None -> None
        | Some c -> Some (C.SourceConv.parse_contract c) in
      let parameter = C.SourceConv.parse_const parameter in
      forge_call ?head ?source ?public_key ?contract ?amount ~address ~entry parameter
      >>= fun (_, op, _) -> Lwt.return op
  end

  module Sync : S with type 'a t := 'a = struct
    let init_storage ?source a b = Lwt_main.run @@ Async.init_storage ?source a b
    let forge_deploy ?head ?source ?public_key ?balance a b =
      Lwt_main.run @@ Async.forge_deploy ?head ?source ?public_key ?balance a b
    let forge_call ?head ?source ?public_key ?contract ?amount ~address ~entry d =
      Lwt_main.run @@ Async.forge_call ?head ?source ?public_key ?contract ?amount ~address ~entry d
    let run ?amount a b c d = Lwt_main.run @@ Async.run ?amount a b c d
    let run_debug ?amount a b c d = Lwt_main.run @@ Async.run_debug ?amount a b c d
    let deploy ?balance a b = Lwt_main.run @@ Async.deploy ?balance a b
    let get_storage a b = Lwt_main.run @@ Async.get_storage a b
    let get_big_map_value b c = Lwt_main.run @@ Async.get_big_map_value b c
    let call ?contract ?amount ~address ~entry d = Lwt_main.run @@ Async.call ?contract ?amount ~address ~entry d
    let activate ~secret = Lwt_main.run @@ Async.activate ~secret
    let inject ~operation ~signature =
      Lwt_main.run @@ Async.inject ~operation ~signature
    let pack ~const ~ty = Lwt_main.run @@ Async.pack ~const ~ty
  end

end
