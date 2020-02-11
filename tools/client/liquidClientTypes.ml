open LiquidClientUtils
open Dune_Network_Lib

module Make (L : LiquidClientSigs.LANG) = struct
  open L

  let int_string = Json_encoding.(conv string_of_int int_of_string string)

  let bytes_hex =
    let open Json_encoding in
    conv
      (fun b -> Hex.show (Hex.of_bytes b))
      (fun h -> Hex.to_bytes (`Hex h))
      string

  type error = Json_repr.any

  let tez_encoding =
    let open Json_encoding in
    conv
      (fun x -> LiquidNumber.mic_mutez_of_tez x |> Z.to_string)
      (fun x -> Z.of_string x |> LiquidNumber.tez_of_mic_mutez)
      string

  module Header = struct
    type t = {
      hash : string;
      chain_id : string;
      predecessor : string;
      protocol : string;
    }

    let encoding =
      let open Json_encoding in
      conv_ignore_extra
        (fun {hash; chain_id; predecessor; protocol} ->
           (hash, chain_id, predecessor, protocol))
        (fun (hash, chain_id, predecessor, protocol) ->
           {hash; chain_id; predecessor; protocol})
        (obj4
           (req "hash" string)
           (req "chain_id" string)
           (req "predecessor" string)
           (req "protocol" string))
  end

  module Constants = struct

    type t = {
      hard_gas_limit_per_operation : int;
      hard_storage_limit_per_operation : int;
    }

    let encoding =
      let open Json_encoding in
      conv_ignore_extra
        (fun {hard_gas_limit_per_operation;
              hard_storage_limit_per_operation} ->
          (hard_gas_limit_per_operation,
           hard_storage_limit_per_operation))
        (fun (hard_gas_limit_per_operation,
              hard_storage_limit_per_operation) ->
          {hard_gas_limit_per_operation;
           hard_storage_limit_per_operation})
        (obj2
           (req "hard_gas_limit_per_operation" int_string)
           (req "hard_storage_limit_per_operation" int_string))
  end

  module Balance_update = struct
    type category =
      | Rewards
      | Fees
      | Deposits
    type t =
      | Contract of {
          contract : string;
          change : LiquidNumber.tez;
        }
      | Freezer of {
          category : category;
          delegate : string;
          cycle : int;
          change : LiquidNumber.tez
        }

    let encoding =
      let open Json_encoding in
      union [
        case_ignore_extra
          (obj3
            (req "kind" (constant "contract"))
            (req "contract" string)
            (req "change" tez_encoding))
          (function
            | Contract { contract; change } -> Some ((), contract, change)
            | _ -> None)
          (fun ((), contract, change) ->
             Contract { contract; change });

        case_ignore_extra
          (obj5
             (req "kind" (constant "freezer"))
             (req "category" (string_enum [
                  "rewards", Rewards;
                  "fees", Fees;
                  "deposits", Deposits;
                ]))
             (req "delegate" string)
             (req "cycle" int)
             (req "change" tez_encoding))
          (function
            | Freezer { category; delegate; cycle; change } ->
              Some ((), category, delegate, cycle, change)
            | _ -> None)
          (fun ((), category, delegate, cycle, change) ->
             Freezer { category; delegate; cycle; change });
      ]

  end

  module Trace = struct

    type ('loc, 'const) trace_item = {
      loc : 'loc option;
      gas : int;
      stack : ('const * string option) list;
    }

    type ('loc, 'const) t = ('loc, 'const) trace_item list

    let encoding loc_enc const_enc =
      let open Json_encoding in
      list @@ conv_ignore_extra
        (fun { loc; gas; stack } -> (loc, gas, stack))
        (fun (loc, gas, stack) -> { loc; gas; stack })
        (obj3
           (opt "location" loc_enc)
           (req "gas" int_string)
           (req "stack" (list (obj2
                                 (req "item" const_enc)
                                 (opt "annot" string)))))

  end

  module Big_map_diff = struct

    type ('id, 'const) item =
      | Big_map_add of { id : 'id;
                         key_hash : string;
                         key : 'const;
                         value : 'const }
      | Big_map_remove of { id : 'id;
                            key_hash : string;
                            key : 'const }
      | Big_map_delete of { id : 'id }
      | Big_map_alloc of { id : 'id }
      | Big_map_copy of { source_id : 'id;
                          destination_id : 'id }

    type ('id, 'const) t = ('id, 'const) item list

    let item_encoding id const =
      let open Json_encoding in
      union [
        case_ignore_extra
          (obj5
            (req "action" (constant "update"))
            (req "big_map" id)
            (req "key_hash" string)
            (req "key" const)
            (req "value" const))
          (function
            | Big_map_add { id; key_hash; key; value } ->
              Some ((), id, key_hash, key, value)
            | _ -> None)
          (fun ((), id, key_hash, key, value) ->
             Big_map_add { id; key_hash; key; value });

        case_ignore_extra
          (obj4
             (req "action" (constant "update"))
             (req "big_map" id)
             (req "key_hash" string)
             (req "key" const))
          (function
            | Big_map_remove { id; key_hash; key } ->
              Some ((), id, key_hash, key)
            | _ -> None)
          (fun ((), id, key_hash, key) ->
             Big_map_remove { id; key_hash; key });

        case_ignore_extra
          (obj2
             (req "action" (constant "remove"))
             (req "big_map" id))
          (function
            | Big_map_delete { id } -> Some ((), id)
            | _ -> None)
          (fun ((), id) -> Big_map_delete { id });

        case_ignore_extra
          (obj2
             (req "action" (constant "alloc"))
             (req "big_map" id))
          (function
            | Big_map_alloc { id } -> Some ((), id)
            | _ -> None)
          (fun ((), id) -> Big_map_alloc { id });

        case_ignore_extra
          (obj3
             (req "action" (constant "alloc"))
             (req "source_big_map" id)
             (req "destination_big_map" id))
          (function
            | Big_map_copy { source_id; destination_id } ->
              Some ((), source_id, destination_id)
            | _ -> None)
          (fun ((), source_id, destination_id) ->
             Big_map_copy { source_id; destination_id });
      ]

    let encoding id const = Json_encoding.list @@ item_encoding id const

  end

  module OperationMake (T : sig
      type const
      type contract
      val const_encoding : const Json_encoding.encoding
      val contract_encoding : contract Json_encoding.encoding
    end) = struct
    open T

    type manager_operation_content =
      | Reveal of string
      | Transaction of {
          amount : LiquidNumber.tez;
          destination : string;
          entrypoint : string;
          parameters : const option;
        }
      | Origination of {
          delegate: string option ;
          script: (contract * const) option ;
          balance: LiquidNumber.tez ;
        }
      | Delegation of string option


    type 'manager operation_content =
      | Manager of 'manager
      | Activate_account of {
          pkh : string;
          secret : string;
        }

    type manager_operation = {
      op : manager_operation_content;
      source : string;
      fee : LiquidNumber.tez;
      counter : int;
      gas_limit : int;
      storage_limit : int;
    }

    type operation = manager_operation operation_content

    type internal = {
      source : string;
      nonce : int;
      op : manager_operation_content operation_content;
    }

    type t = {
      branch : string;
      contents : operation list;
      signature : string option;
    }

    let manager_operation_content_encoding =
      let open Json_encoding in
      union [
        case_ignore_extra
          (obj2
            (req "kind" (constant "reveal"))
            (req "public_key" string))
          (function
            | Reveal pk ->
              Some ((), pk)
            | _ -> None)
          (fun ((), pk) -> Reveal pk);

        case_ignore_extra
          (obj4
            (req "kind" (constant "transaction"))
            (req "amount" tez_encoding)
            (req "destination" string)
            (dft "parameters"
               (obj2
                  (dft "entrypoint" string "default")
                  (opt "value" const_encoding)
               )
               ("default", None)))
          (function
            | Transaction { amount; destination; entrypoint; parameters } ->
              Some ((), amount, destination, (entrypoint, parameters))
            | _ -> None)
          (fun ((), amount, destination, (entrypoint, parameters)) ->
             Transaction { amount; destination; entrypoint; parameters });

        case_ignore_extra
          (obj4
            (req "kind" (constant "origination"))
            (req "balance" tez_encoding)
            (opt "delegate" string)
            (opt "script"
               (obj2
                  (req "code" contract_encoding)
                  (req "storage" const_encoding)
               )))
          (function
            | Origination { balance; delegate; script } ->
              Some ((), balance, delegate, script)
            | _ -> None)
          (fun ((), balance, delegate, script) ->
             Origination { balance; delegate; script });

        case_ignore_extra
          (obj2
            (req "kind" (constant "delegation"))
            (opt "delegate" string))
          (function
            | Delegation pk ->
              Some ((), pk)
            | _ -> None)
          (fun ((), pk) -> Delegation pk);
      ]

    let operation_content_encoding manager_encoding =
      let open Json_encoding in
      union [
        case
          manager_encoding
          (function Manager m -> Some m
                  | _ -> None)
          (fun m -> Manager m);

        case_ignore_extra
          (obj3
            (req "kind" (constant "activate_account"))
            (req "pkh" string)
            (req "secret" string))
          (function
            | Activate_account { pkh ; secret } ->
              Some ((), pkh, secret)
            | _ -> None)
          (fun ((), pkh, secret) -> Activate_account { pkh ; secret });
      ]

    let manager_operation_encoding =
      let open Json_encoding in
      conv_ignore_extra
        (fun { op; source; fee; counter; gas_limit; storage_limit } ->
           (op, (source, fee, counter, gas_limit, storage_limit)))
        (fun (op, (source, fee, counter, gas_limit, storage_limit)) ->
           { op; source; fee; counter; gas_limit; storage_limit })
        (merge_objs
          manager_operation_content_encoding
          (obj5
             (req "source" string)
             (req "fee" tez_encoding)
             (req "counter" int_string)
             (req "gas_limit" int_string)
             (req "storage_limit" int_string)))

    let operation_encoding =
      operation_content_encoding manager_operation_encoding

    let internal_encoding =
      let open Json_encoding in
      conv_ignore_extra
        (fun { source; nonce; op } -> ((source, nonce), op))
        (fun ((source, nonce), op) -> { source; nonce; op })
        (merge_objs
           (obj2
              (req "source" string)
              (req "nonce" int))
           (operation_content_encoding manager_operation_content_encoding))

    let encoding =
      let open Json_encoding in
      conv_ignore_extra
        (fun { branch; contents; signature } ->
           (branch, contents, signature))
        (fun (branch, contents, signature) ->
           { branch; contents; signature })
        (obj3
           (req "branch" string)
           (req "contents" (list operation_encoding))
           (opt "signature" string))

  end

  module Operation = OperationMake(Target)
  module SourceOperation = OperationMake(Source)

  module Run_code = struct

    module Input = struct

      type t = {
        script : Target.contract;
        entrypoint : string;
        input: Target.const;
        storage: Target.const;
        amount: LiquidNumber.tez;
        chain_id: string;
        source: string option;
      }

      let encoding =
        let open Json_encoding in
        conv_ignore_extra
          (fun { script; entrypoint; input; storage; amount; chain_id; source } ->
             (script, entrypoint, input, storage, amount, chain_id, source))
          (fun (script, entrypoint, input, storage, amount, chain_id, source) ->
             { script; entrypoint; input; storage; amount; chain_id; source })
          (obj7
             (req "script" Target.contract_encoding)
             (req "entrypoint" string)
             (req "input" Target.const_encoding)
             (req "storage" Target.const_encoding)
             (req "amount" tez_encoding)
             (req "chain_id" string)
             (opt "source" string))
    end


    module Result = struct

      type t = {
        storage: Target.const;
        operations: Operation.internal list;
        big_map_diff: (int, Target.const) Big_map_diff.t;
        trace: (Target.location, Target.const) Trace.t option
      }

      let encoding =
        let open Json_encoding in
        conv_ignore_extra
          (fun { storage; operations; big_map_diff; trace } ->
             (storage, operations, big_map_diff, trace))
          (fun (storage, operations, big_map_diff, trace) ->
             { storage; operations; big_map_diff; trace })
          (obj4
             (req "storage" Target.const_encoding)
             (dft "operations" (list Operation.internal_encoding) [])
             (dft "big_map_diff"
                (Big_map_diff.encoding int_string Target.const_encoding)
                [])
             (opt "trace" (Trace.encoding Target.loc_encoding Target.const_encoding)))
    end
  end

  module Run_operation = struct

    module Input = struct
      type t = {
        operation : Operation.t;
        chain_id : string;
      }

      let encoding =
        let open Json_encoding in
        conv_ignore_extra
          (fun { operation; chain_id } ->
             (operation, chain_id))
          (fun (operation, chain_id) ->
             { operation; chain_id })
          (obj2
             (req "operation" Operation.encoding)
             (req "chain_id" string))
    end

    module Result = struct
      (* type origination_result_content = {
       *   big_map_diff: (int, Target.const) Big_map_diff.t;
       *   balance_updates: Balance_update.t list;
       *   originated_contracts: string list;
       *   consumed_gas: int;
       *   storage_size: int;
       *   paid_storage_size_diff: int;
       * }
       * type transaction_result_content = {
       *   storage: Target.const;
       *   big_map_diff: (int, Target.const) Big_map_diff.t;
       *   balance_updates: Balance_update.t list;
       *   originated_contracts: string list;
       *   consumed_gas: int;
       *   storage_size: int;
       *   paid_storage_size_diff: int;
       *   allocated_destination_contract: bool;
       * }
       * type reveal_result_content = {
       *   consumed_gas: int;
       * }
       * type delegation_result_content = reveal_result_content
       * type 'a result =
       *   | Applied of 'a
       *   | Failed of error list
       *   | Skipped
       *   | Backtracked of error list * 'a *)
      type result_content =
        | Transaction of {
            storage: Target.const;
            big_map_diff: (int, Target.const) Big_map_diff.t;
            balance_updates: Balance_update.t list;
            originated_contracts: string list;
            consumed_gas: int;
            storage_size: int;
            paid_storage_size_diff: int;
            allocated_destination_contract: bool;
          }
        | Origination of {
            big_map_diff: (int, Target.const) Big_map_diff.t;
            balance_updates: Balance_update.t list;
            originated_contracts: string list;
            consumed_gas: int;
            storage_size: int;
            paid_storage_size_diff: int;
          }
        | Reveal_Delegation of {
            consumed_gas: int;
          }
        (* | Activate_account *)
      type result =
        | Applied of result_content
        | Failed of error list
        | Skipped
        | Backtracked of error list * result_content
        | Other of Json_repr.any
      type metadata = {
        balance_updates : Balance_update.t list;
        operation_result : result;
        internal_operation_results : (Operation.internal * result) list;
      }
      type t = {
        contents : (Operation.operation * metadata) list;
        signature : string option;
      }

      let result_content_encoding =
        let open Json_encoding in
        union [
          case_ignore_extra
            (obj8
               (req "storage" Target.const_encoding)
               (dft "big_map_diff"
                  (Big_map_diff.encoding int_string Target.const_encoding)
                  [])
               (dft "balance_updates" (list Balance_update.encoding) [])
               (dft "originated_contracts" (list string) [])
               (dft "consumed_gas" int_string 0)
               (dft "storage_size" int_string 0)
               (dft "paid_storage_size_diff" int_string 0)
               (dft "allocated_destination_contract" bool false))
            (function
              | Transaction {
                  storage;
                  big_map_diff;
                  balance_updates;
                  originated_contracts;
                  consumed_gas;
                  storage_size;
                  paid_storage_size_diff;
                  allocated_destination_contract;
                } -> Some (
                  storage,
                  big_map_diff,
                  balance_updates,
                  originated_contracts,
                  consumed_gas,
                  storage_size,
                  paid_storage_size_diff,
                  allocated_destination_contract
                )
              | _ -> None)
            (fun (
               storage,
               big_map_diff,
               balance_updates,
               originated_contracts,
               consumed_gas,
               storage_size,
               paid_storage_size_diff,
               allocated_destination_contract
             ) ->
               Transaction {
                 storage;
                 big_map_diff;
                 balance_updates;
                 originated_contracts;
                 consumed_gas;
                 storage_size;
                 paid_storage_size_diff;
                 allocated_destination_contract;
               });
          case_ignore_extra
            (obj6
               (dft "big_map_diff"
                  (Big_map_diff.encoding int_string Target.const_encoding)
                  [])
               (dft "balance_updates" (list Balance_update.encoding) [])
               (dft "originated_contracts" (list string) [])
               (dft "consumed_gas" int_string 0)
               (dft "storage_size" int_string 0)
               (dft "paid_storage_size_diff" int_string 0))
            (function
              | Origination {
                  big_map_diff;
                  balance_updates;
                  originated_contracts;
                  consumed_gas;
                  storage_size;
                  paid_storage_size_diff;
                } -> Some (
                  big_map_diff,
                  balance_updates,
                  originated_contracts,
                  consumed_gas,
                  storage_size,
                  paid_storage_size_diff
                )
              | _ -> None)
            (fun (
               big_map_diff,
               balance_updates,
               originated_contracts,
               consumed_gas,
               storage_size,
               paid_storage_size_diff
             ) ->
               Origination {
                 big_map_diff;
                 balance_updates;
                 originated_contracts;
                 consumed_gas;
                 storage_size;
                 paid_storage_size_diff;
               });

          case_ignore_extra
            (obj1
               (dft "consumed_gas" int_string 0))
            (function
              | Reveal_Delegation { consumed_gas } ->
                Some consumed_gas
              | _ -> None)
            (fun consumed_gas -> Reveal_Delegation { consumed_gas });
        ]

      let result_encoding =
        let open Json_encoding in
        union [
          case_ignore_extra
            (merge_objs
               (obj1 (req "status" (constant "applied")))
               result_content_encoding)
            (function
              | Applied r -> Some ((), r)
              | _ -> None)
            (fun ((), r) -> Applied r);

          case_ignore_extra
            (obj2
               (req "status" (constant "failed"))
               (req "errors" (list any_value)))
            (function
              | Failed errs -> Some ((), errs)
              | _ -> None)
            (fun ((), errs) -> Failed errs);

          case_ignore_extra
            (obj1
               (req "satus" (constant "skipped")))
            (function
              | Skipped -> Some ()
              | _ -> None)
            (fun () -> Skipped);

          case_ignore_extra
            (merge_objs
               (obj2
                  (req "status" (constant "backtracked"))
                  (dft "errors" (list any_value) []))
               result_content_encoding)
            (function
              | Backtracked (errs, r) -> Some (((), errs), r)
              | _ -> None)
            (fun (((), errs), r) -> Backtracked (errs, r));

          case
            any_value
            (function
              | Other json -> Some json
              | _ -> None)
            (fun json -> Other json);
        ]

      let metadata_encoding =
        let open Json_encoding in
        conv_ignore_extra
          (fun { balance_updates;
                 operation_result;
                 internal_operation_results } ->
            (balance_updates, operation_result, internal_operation_results))
          (fun
            (balance_updates, operation_result, internal_operation_results) ->
            { balance_updates;
              operation_result;
              internal_operation_results })
        (obj3
          (req "balance_updates" (list Balance_update.encoding))
          (dft "operation_result" result_encoding (Other (Json_repr.to_any (`O []))))
          (dft "internal_operation_results"
             (list
                (merge_objs
                   Operation.internal_encoding
                   (obj1 (req "result" result_encoding))
                )) []))

      let encoding =
        let open Json_encoding in
        conv_ignore_extra
          (fun { contents; signature } -> (contents, signature))
          (fun (contents, signature) -> { contents; signature })
          (obj2
             (req "contents"
                (list
                   (merge_objs
                      Operation.operation_encoding
                      (obj1 (req "metadata" metadata_encoding)))
                   ))
             (opt "signature" string))

    end

  end

end
