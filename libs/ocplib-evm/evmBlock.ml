(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open EvmTypes

let block_header_of_rlp rlp =
  match rlp with
  | L [
      S parentHash;
      S ommersHash;
      S benefiary;
      S stateRoot;
      S transactionsRoot;
      S receiptsRoot;
      S bh_logsBloom;
      S difficulty;
      S number;
      S gasLimit;
      S gasUsed;
      S timestamp;
      S bh_extraData;
      S bh_mixHash;
      S bh_nonce;
    ] ->
     let bh_parentHash = BH parentHash in
     let bh_ommersHash = OH ommersHash in
     let bh_benefiary = ADDR benefiary in
     let bh_stateRoot = SR stateRoot in
     let bh_transactionsRoot = TR transactionsRoot in
     let bh_receiptsRoot = RR receiptsRoot in
     let bh_difficulty = EvmRLP.decodeInt64 difficulty in
     let bh_number = EvmRLP.decodeInt64 number in
     let bh_gasLimit = EvmRLP.decodeInt64 gasLimit in
     let bh_gasUsed = EvmRLP.decodeInt64 gasUsed in
     let bh_timestamp = EvmRLP.decodeInt64 timestamp in
     {
       bh_parentHash;
       bh_ommersHash;
       bh_benefiary;
       bh_stateRoot;
       bh_transactionsRoot;
       bh_receiptsRoot;
       bh_logsBloom;
       bh_difficulty;
       bh_number;
       bh_gasLimit;
       bh_gasUsed;
       bh_timestamp;
       bh_extraData;
       bh_mixHash;
       bh_nonce;
     }


  | _ ->
     Printf.kprintf failwith "block_of_rlp:\n%s"
                    (EvmPrinter.string_of_rlp rlp)

let genesis_hash =
  BH (Hex_encode.hex_decode "0000000000000000000000000000000000000000000000000000000000000000")

let transaction_of_rlp rlp =
  match rlp with
  | L [
      S tr_nonce;
      S tr_gasPrice;
      S tr_gasLimit;
      S tr_to;
      S tr_value;
      S tr_data;
      S tr_v;
      S tr_r;
      S tr_s;
    ] ->
     let tr_gasPrice = EvmRLP.decodeInt64 tr_gasPrice in
     let tr_gasLimit = EvmRLP.decodeInt64 tr_gasLimit in
     let tr_to = Some (ADDR tr_to) in
     let tr_value = EvmRLP.decodeZ tr_value in
     {
       tr_nonce;
       tr_gasPrice;
       tr_gasLimit;
       tr_to;
       tr_value;
       tr_data;
       tr_v;
       tr_r;
       tr_s;
     }
  | _ ->
     Printf.kprintf failwith "transaction_of_rlp:\n%s"
                    (EvmPrinter.string_of_rlp rlp)

let transactions_of_body_rlp rlp =
  match rlp with
  | L [ L transactions; L _ ] -> transactions
  | _ ->
     Printf.kprintf failwith "transaction_of_body_rlp:\n%s"
                    (EvmPrinter.string_of_rlp rlp)

let codeHash_none = Nocrypto.Hash.digest `SHA3_KEC ""

                                         (*
c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
                                          *)

let account_state_of_rlp rlp =
  match rlp with
  | L [
      S acc_nonce;
      S acc_balance;
      S acc_storageRoot;
      S acc_codeHash;
    ] ->
     let acc_nonce = EvmRLP.decodeZ acc_nonce in
     let acc_balance = EvmRLP.decodeZ acc_balance in
     let acc_codeHash =
       if acc_codeHash = codeHash_none then None
       else Some acc_codeHash
     in
     {
       acc_nonce;
       acc_balance;
       acc_storageRoot;
       acc_codeHash;
     }

  | _ ->
     Printf.kprintf failwith "account_state_of_rlp:\n%s"
                    (EvmPrinter.string_of_rlp rlp)
