(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open EvmTypes

let ins2str = Hashtbl.create 111
let str2ins = Hashtbl.create 111

let () =
  List.iter (fun (ins, str) ->
      Hashtbl.add ins2str ins str;
      Hashtbl.add str2ins str ins
    ) [
              STOP, "STOP";
              ADD, "ADD";
              MUL, "MUL";
              SUB, "SUB";
              DIV, "DIV";
              SDIV, "SDIV";
              MOD, "MOD";
              SMOD, "SMOD";
              ADDMOD, "ADDMOD";
              MULMOD, "MULMOD";
              EXP, "EXP";
              SIGNEXTEND, "SIGNEXTEND";
              LT, "LT";
              GT, "GT";
              SLT, "SLT";
              SGT, "SGT";

              EQ, "EQ";
              ISZERO, "ISZERO";
              AND, "AND";
              OR, "OR";
              XOR, "XOR";
              NOT, "NOT";
              BYTE, "BYTE";
              SHA3, "SHA3";

              ADDRESS, "ADDRESS";
              BALANCE, "BALANCE";
              ORIGIN, "ORIGIN";
              CALLER, "CALLER";
              CALLVALUE, "CALLVALUE";
              CALLDATALOAD, "CALLDATALOAD";
              CALLDATASIZE, "CALLDATASIZE";
              CALLDATACOPY, "CALLDATACOPY";
              CODESIZE, "CODESIZE";
              CODECOPY, "CODECOPY";
              GASPRICE, "GASPRICE";
              EXTCODESIZE, "EXTCODESIZE";
              EXTCODECOPY, "EXTCODECOPY";

              BLOCKHASH, "BLOCKHASH";
              COINBASE, "COINBASE";
              TIMESTAMP, "TIMESTAMP";
              NUMBER, "NUMBER";
              DIFFICULTY, "DIFFICULTY";
              GASLIMIT, "GASLIMIT";

              POP, "POP";
              MLOAD, "MLOAD";
              MSTORE, "MSTORE";
              MSTORE8, "MSTORE8";
              SLOAD, "SLOAD";
              SSTORE, "SSTORE";
              JUMP, "JUMP";
              JUMPI, "JUMPI";
              PC, "PC";
              MSIZE, "MSIZE";
              GAS, "GAS";
              JUMPDEST, "JUMPDEST";

              PUSH 1, "PUSH1";
              PUSH 2, "PUSH2";
              PUSH 3, "PUSH3";
              PUSH 4, "PUSH4";
              PUSH 5, "PUSH5";
              PUSH 6, "PUSH6";
              PUSH 7, "PUSH7";
              PUSH 8, "PUSH8";
              PUSH 9, "PUSH9";
              PUSH 10, "PUSH10";
              PUSH 11, "PUSH11";
              PUSH 12, "PUSH12";
              PUSH 13, "PUSH13";
              PUSH 14, "PUSH14";
              PUSH 15, "PUSH15";
              PUSH 16, "PUSH16";
              PUSH 17, "PUSH17";
              PUSH 18, "PUSH18";
              PUSH 19, "PUSH19";
              PUSH 20, "PUSH20";
              PUSH 21, "PUSH21";
              PUSH 22, "PUSH22";
              PUSH 23, "PUSH23";
              PUSH 24, "PUSH24";
              PUSH 25, "PUSH25";
              PUSH 26, "PUSH26";
              PUSH 27, "PUSH27";
              PUSH 28, "PUSH28";
              PUSH 29, "PUSH29";
              PUSH 30, "PUSH30";
              PUSH 31, "PUSH31";
              PUSH 32, "PUSH32";
              DUP 1, "DUP1";
              DUP 2, "DUP2";
              DUP 3, "DUP3";
              DUP 4, "DUP4";
              DUP 5, "DUP5";
              DUP 6, "DUP6";
              DUP 7, "DUP7";
              DUP 8, "DUP8";
              DUP 9, "DUP9";
              DUP 10, "DUP10";
              DUP 11, "DUP11";
              DUP 12, "DUP12";
              DUP 13, "DUP13";
              DUP 14, "DUP14";
              DUP 15, "DUP15";
              DUP 16, "DUP16";
              SWAP 1, "SWAP1";
              SWAP 2, "SWAP2";
              SWAP 3, "SWAP3";
              SWAP 4, "SWAP4";
              SWAP 5, "SWAP5";
              SWAP 6, "SWAP6";
              SWAP 7, "SWAP7";
              SWAP 8, "SWAP8";
              SWAP 9, "SWAP9";
              SWAP 10, "SWAP10";
              SWAP 11, "SWAP11";
              SWAP 12, "SWAP12";
              SWAP 13, "SWAP13";
              SWAP 14, "SWAP14";
              SWAP 15, "SWAP15";
              SWAP 16, "SWAP16";

              LOG 0, "LOG0";
              LOG 1, "LOG1";
              LOG 2, "LOG2";
              LOG 3, "LOG3";
              LOG 4, "LOG4";

              CREATE, "CREATE";
              CALL, "CALL";
              CALLCODE, "CALLCODE";
              RETURN, "RETURN";
              DELEGATECALL, "DELEGATECALL";
              SELFDESTRUCT, "SELFDESTRUCT";
            ]

let raise_invalid_instruction_argument ins =
  let (str, arg) =
    match ins with
    | PUSH n -> "PUSH", n
    | DUP n -> "DUP", n
    | SWAP n -> "SWAP", n
    | LOG n -> "LOG", n
    | _ -> assert false
  in
  raise (InvalidInstructionArgument (str, arg))

let string_of_instruction ins =
  try
    Hashtbl.find ins2str ins
  with Not_found ->
    raise_invalid_instruction_argument ins

let instruction_of_string str =
  try
    Hashtbl.find str2ins str
  with Not_found ->
    raise (UnknownInstruction str)

let string_of f x =
  let b = Buffer.create 16_000 in
  f b "" x;
  Buffer.contents b

let string_of_code =
  string_of (fun b indent code ->
      List.iter (fun (ins, arg) ->
          Buffer.add_string b (string_of_instruction ins);
          begin match arg with
          | None -> ()
          | Some arg ->
             Buffer.add_string b " 0x";
             Buffer.add_string b (Hex_encode.hex_encode arg);
          end;
          Buffer.add_char b '\n'
        ) code)

let string_of_block_hash (BH t) = Hex_encode.hex_encode t
let string_of_ommers_hash (OH t) = Hex_encode.hex_encode t
let string_of_address (ADDR t) = Hex_encode.hex_encode t
let string_of_state_root (SR t) = Hex_encode.hex_encode t
let string_of_receipts_root (RR t) = Hex_encode.hex_encode t
let string_of_transactions_root (TR t) = Hex_encode.hex_encode t

let bprint_block_header b indent bh =
  Printf.bprintf b "%s{\n" indent;
  Printf.bprintf b "%s parentHash  = %s\n" indent
                 (string_of_block_hash bh.bh_parentHash);
  Printf.bprintf b "%s ommersHash = %s\n" indent
                 (string_of_ommers_hash bh.bh_ommersHash);
  Printf.bprintf b "%s benefiary = %s\n" indent
                 (string_of_address bh.bh_benefiary);
  Printf.bprintf b "%s stateRoot = %s\n" indent
                 (string_of_state_root bh.bh_stateRoot);
  Printf.bprintf b "%s transactionsRoot = %s\n" indent
                 (string_of_transactions_root bh.bh_transactionsRoot);
  Printf.bprintf b "%s receiptsRoot = %s\n" indent
                 (string_of_receipts_root bh.bh_receiptsRoot);
  Printf.bprintf b "%s logsBloom[%d] = %s\n" indent
                 (String.length bh.bh_logsBloom)
                 (Hex_encode.hex_encode bh.bh_logsBloom);
  Printf.bprintf b "%s difficulty = %Ld\n" indent
                 bh.bh_difficulty;
  Printf.bprintf b "%s number = %Ld\n" indent
                 bh.bh_number;
  Printf.bprintf b "%s gasLimit = %Ld\n" indent
                 bh.bh_gasLimit;
  Printf.bprintf b "%s gasUsed = %Ld\n" indent
                 bh.bh_gasUsed;
  Printf.bprintf b "%s timestamp = %Ld\n" indent
                 bh.bh_timestamp;
  Printf.bprintf b "%s extraData[%d] = %s\n" indent
                 (String.length bh.bh_extraData)
                 (Hex_encode.hex_encode bh.bh_extraData);
  Printf.bprintf b "%s mixHash[%d] = %s\n" indent
                 (String.length bh.bh_mixHash)
                 (Hex_encode.hex_encode bh.bh_mixHash);
  Printf.bprintf b "%s nonce[%d] = %s\n" indent
                 (String.length bh.bh_nonce)
                 (Hex_encode.hex_encode bh.bh_nonce);
  Printf.bprintf b "%s}\n"

let string_of_block_header = string_of bprint_block_header

let bprint_transaction b indent tr =
  Printf.bprintf b "%s{\n" indent;
  Printf.bprintf b "%s nonce[%d] = %s\n" indent
                 (String.length tr.tr_nonce)
                 (Hex_encode.hex_encode tr.tr_nonce);
  Printf.bprintf b "%s gasPrice = %Ld\n" indent
                 tr.tr_gasPrice;
  Printf.bprintf b "%s gasLimit = %Ld\n" indent
                 tr.tr_gasLimit;
  Printf.bprintf b "%s to = %s\n" indent
                 (match tr.tr_to with
                  | None -> "()"
                  | Some addr -> string_of_address addr);
  Printf.bprintf b "%s value = %s\n" indent
                 (Z.to_string tr.tr_value);
  Printf.bprintf b "%s data[%d] = %s\n" indent
                 (String.length tr.tr_data)
                 (Hex_encode.hex_encode tr.tr_data);
  Printf.bprintf b "%s v[%d] = %s\n" indent
                 (String.length tr.tr_v)
                 (Hex_encode.hex_encode tr.tr_v);
  Printf.bprintf b "%s v[%d] = %s\n" indent
                 (String.length tr.tr_r)
                 (Hex_encode.hex_encode tr.tr_r);
  Printf.bprintf b "%s v[%d] = %s\n" indent
                 (String.length tr.tr_s)
                 (Hex_encode.hex_encode tr.tr_s);
  Printf.bprintf b "%s}\n"

let bprint_account_state b indent acc =
  Printf.bprintf b "%s{\n" indent;
  Printf.bprintf b "%s nonce = %s\n" indent
                 (Z.to_string acc.acc_nonce);
  Printf.bprintf b "%s balance = %s\n" indent
                 (Z.to_string acc.acc_balance);
  Printf.bprintf b "%s storageRoot = %s\n" indent
                 (Hex_encode.hex_encode acc.acc_storageRoot);
  Printf.bprintf b "%s codeHash = %s\n" indent
                 (match acc.acc_codeHash with
                  | None -> "none"
                  | Some s ->
                     Hex_encode.hex_encode s
                 );
  Printf.bprintf b "%s}\n"


let string_of_transaction = string_of bprint_transaction
let string_of_account_state = string_of bprint_account_state


let string_of_rlp item =
  let b = Buffer.create 1000 in
  let rec iter b indent item =
    match item with
    | S str ->
       Printf.bprintf b "%sS[%d] %S\n" indent (String.length str) str
    | L list ->
       Printf.bprintf b "%sL [\n" indent;
       List.iter (iter b (indent ^ "  ")) list;
       Printf.bprintf b "%s]\n" indent
  in
  iter b "" item;
  Buffer.contents b
