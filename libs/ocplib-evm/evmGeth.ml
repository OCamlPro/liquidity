(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open EvmTypes

let geth_dirname = Filename.concat (Sys.getenv "HOME") ".ethereum/geth"
let chaindata_dirname = Filename.concat geth_dirname "chaindata"
let nodekey_filename = Filename.concat geth_dirname "nodekey"

(*
  let nodekey =
    let ic = open_in nodekey_filename in
    let nodekey = input_line ic in
    close_in ic;
    Hex_encode.hex_decode nodekey
  in
 *)

let headHeaderKey = "LastHeader"
let headBlockKey  = "LastBlock"
let headFastKey   = "LastFast"

let blockHashPrefix = "H"
let headerPrefix = "h"
let bodyPrefix = "b"
let tdSuffix = "t"
let numSuffix = "n"

                      (*
	numSuffix           = []byte("n")   // headerPrefix + num (uint64 big endian) + numSuffix -> hash
                       *)

let blockReceiptsPrefix = "r"

                            (*
	lookupPrefix        = []byte("l")   // lookupPrefix + hash -> transaction/receipt lookup metadata
	preimagePrefix      = "secure-key-" // preimagePrefix + hash -> preimage
                       *)

let close_and_exit db = LevelDB.close db; exit 2

let getHeadBlock db =
  match LevelDB.get db headBlockKey with
  | None -> failwith "getHeadBlock"
  | Some s -> BH s

let encodeBlockNumber n =
  let s = Bytes.create 8 in
  EndianString.BigEndian.set_int64 s 0 n;
  s

let getBlockNumber db (BH hash) =
  let hash = blockHashPrefix ^ hash in
  match LevelDB.get db hash with
  | None -> failwith "getBlockNumber"
  | Some s ->
     EndianString.BigEndian.get_int64 s 0

let getBlockHeaderRLP db (BH hash) number =
  let hash = headerPrefix ^ encodeBlockNumber number ^ hash in
  match LevelDB.get db hash with
  | None -> failwith "getBlockHeaderRLP"
  | Some s -> s

let getBlockBodyRLP db (BH hash) number =
  let hash = bodyPrefix ^ encodeBlockNumber number ^ hash in
  match LevelDB.get db hash with
  | None -> failwith "getBlockBodyRLP"
  | Some s -> s

let getBlockTotalDifficulty db (BH hash) number =
  let hash = headerPrefix ^ encodeBlockNumber number ^ hash
             ^ tdSuffix in
  match LevelDB.get db hash with
  | None -> failwith "getBlockTotalDifficulty"
  | Some s -> s

let getBlockHeader db hash number =
  let rlp = getBlockHeaderRLP db hash number in
  EvmRLP.decode rlp

let getBlockBody db hash number =
  let rlp = getBlockBodyRLP db hash number in
  EvmRLP.decode rlp

let getBlockReceipts db (BH hash) number =
  let hash = blockReceiptsPrefix ^ encodeBlockNumber number ^ hash in
  match LevelDB.get db hash with
  | None -> failwith "getBlockReceipts"
  | Some rlp -> EvmRLP.decode rlp

let check db s msg =
  Printf.eprintf "Db query %s %S: %!" msg s;
  begin
    match LevelDB.get db s with
    | None -> Printf.eprintf "absent"
    | Some s -> Printf.eprintf "present(size %d)" (String.length s)
  end;
  Printf.eprintf "\n%!"

let ecdsa_context = Secp256k1.Context.create [
                        Secp256k1.SIGN;
                        Secp256k1.VERIFY;
                      ]

let load () =
  let db = LevelDB.open_db chaindata_dirname in
  Printf.eprintf "Leveldb: %S opened\n%!" chaindata_dirname;
  (*
  let npairs = ref 0 in
  LevelDB.iter (fun key value ->
      incr npairs;
      true
    ) db;
  Printf.eprintf "LevelDB: iterated on %d pairs\n%!" !npairs;
   *)
  let headBlock = getHeadBlock db in
  Printf.eprintf "headBlock = %s\n%!"
                 (EvmPrinter.string_of_block_hash headBlock);
  let headBlockNumber = getBlockNumber db headBlock in
  Printf.eprintf "headBlockNumber = %Ld\n%!" headBlockNumber;
  let headBlockHeaderRLP = getBlockHeaderRLP db headBlock headBlockNumber in
  let headBlockBodyRLP = getBlockBodyRLP db headBlock headBlockNumber in
  let headBlockTD = getBlockTotalDifficulty db headBlock headBlockNumber in

  let bh_rlp = getBlockHeader db headBlock headBlockNumber in
  (*  Printf.eprintf "headBlockHeader:\n%s%!" (EvmRLP.to_string bh_rlp); *)
  let bh = EvmBlock.block_header_of_rlp bh_rlp in
  Printf.eprintf "headBlockHeader: %s\n%!"
                 (EvmPrinter.string_of_block_header bh);
  let body_rlp = getBlockBody db headBlock headBlockNumber in
  (*  Printf.eprintf "headBlockBody:\n%s%!" (EvmRLP.to_string body_rlp); *)
  let SR state_root = bh.bh_stateRoot in
  let trs = EvmBlock.transactions_of_body_rlp body_rlp in
  begin
  match trs with
  | [] -> assert false
  | tr_rlp :: _ ->
      let tr = EvmBlock.transaction_of_rlp tr_rlp in
      Printf.eprintf "Transaction:\n%s\n%!"
                     (EvmPrinter.string_of_transaction tr);
      match tr.tr_to with
      | None -> Printf.eprintf "Tr: No address\n%!"
      | Some (ADDR addr) ->
         Printf.eprintf "addr[%d]\n%!" (String.length addr);
         let addr = Nocrypto.Hash.digest `SHA3_KEC addr in
         check db addr "Tr: address";
         begin
           match EvmLDBTrie.lookup db state_root addr with
           | None -> Printf.eprintf "address not in Trie\n%!"
           | Some s ->
              let rlp = EvmRLP.decode s in
              Printf.eprintf "For address %s:\n%s\n%!"
                             (Hex_encode.hex_encode addr)
                             (EvmPrinter.string_of_rlp rlp)
         end;


         let tr_partial_rlp = match tr_rlp with
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
              L [
                  S tr_nonce;
                  S tr_gasPrice;
                  S tr_gasLimit;
                  S tr_to;
                  S tr_value;
                  S tr_data;
                ]
           | _ -> assert false
         in
         let tr_partial_s = EvmRLP.encode tr_partial_rlp in
         let tr_hash = Nocrypto.Hash.digest `SHA3_KEC tr_partial_s in

         let tr_s = EvmRLP.encode tr_rlp in
         let tx_hash = Nocrypto.Hash.digest `SHA3_KEC tr_s in

         Printf.eprintf "TxHash : %s\n%!" (Hex_encode.hex_encode tx_hash);

         let recsig = tr.tr_r ^ tr.tr_s in
         let id = match tr.tr_v with
           | "\027" -> 0
           | "\028" -> 1
           | "\037" -> assert false
           | _ -> assert false
         in
         match Secp256k1.ECDSA_recoverable.signature_parse_compact
                 ecdsa_context recsig  id with
         | None -> assert false
         | Some recsig ->
            match Secp256k1.ECDSA_recoverable.recover
                    ecdsa_context recsig tr_hash with
            | None -> assert false
            | Some pubkey ->
               let pubkey_s = Secp256k1.Pubkey.serialize
                                ecdsa_context pubkey [
                                  Secp256k1.EC_UNCOMPRESSED
                                ] in
               let pubkey_hash = Nocrypto.Hash.digest `SHA3_KEC pubkey_s
               in
               let addr = String.sub pubkey_s
                                     (String.length pubkey_hash-20) 20 in

               Printf.eprintf "from %s\n%!"
                              (Hex_encode.hex_encode addr);
               let addr = Nocrypto.Hash.digest `SHA3_KEC addr in
               begin
                 match EvmLDBTrie.lookup db state_root addr with
                 | None -> Printf.eprintf "address not in Trie\n%!"
                 | Some s ->
                    let rlp = EvmRLP.decode s in
                    Printf.eprintf "For address %s:\n%s\n%!"
                                   (Hex_encode.hex_encode addr)
                                   (EvmPrinter.string_of_rlp rlp)
               end;
  end;

  (*
  let rcpts_rlp = getBlockReceipts db headBlock headBlockNumber in
  Printf.eprintf "Receipts:\n%s\n%!"
                 (EvmPrinter.string_of_rlp rcpts_rlp);
   *)

  (*
  begin
    let SR root = bh.bh_stateRoot in
    EvmLDBTrie.iter db root (fun key value ->
                      let rlp = EvmRLP.decode value in
                      let acc = EvmBlock.account_state_of_rlp rlp in
                      Printf.eprintf "key %s -> val[%d]\n%s\n%!"
                                     (Hex_encode.hex_encode key)
                                     (String.length value)
                                     (EvmPrinter.string_of_account_state acc);

                      match EvmLDBTrie.lookup db root key with
                      | None -> assert false
                      | Some v ->
                         assert (v = value)
                    );
  end;

  begin
    let TR s = bh.bh_transactionsRoot in
    check db s "trans root"
  end;
   *)

  (*
  let rec iter bh =
    Printf.eprintf "block = %s\n%!" (string_of_block_hash bh);
    let number = getBlockNumber db bh in
    let header = getHeader db bh number in
    let h = EvmBlock.block_header_of_rlp header in
    let parent = h.parentHash in
    iter parent
  in
  iter headBlock;
   *)
  LevelDB.close db;
  Printf.eprintf "LevelDB: db closed\n%!"



                 (*
State Trie

There is one global state trie, and it updates over time. In it, a path is always: sha3(ethereumAddress) and a value is always: rlp(ethereumAccount). More specifically an ethereum account is a 4 item array of [nonce,balance,storageRoot,codeHash]. At this point it's worth noting that this storageRoot is the root of another patricia trie:

Storage Trie

Storage trie is where all contract data lives. There is a separate storage trie for each account. A path in this trie is somewhat complex but they depend on this: (here)[https://github.com/ethereum/wiki/wiki/JSON-RPC#eth_getstorageat].

Transactions Trie

There is a separate transactions trie for every block. A path here is: rlp(transactionIndex). transactionIndex is its index within the block it's mined. The ordering is mostly decided by a miner so this data is unknown until mined. After a block is mined, the transaction trie never updates.

Receipts Trie

Every block has its own Receipts trie. A path here is: rlp(transactionIndex). transactionIndex is its index within the block it's mined. Never updates.

                  *)
