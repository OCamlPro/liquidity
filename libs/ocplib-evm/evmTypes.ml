(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* If there is a new instruction added, you should update the list in
EvmPrinter without expecting a warning... *)
type instruction =
 | STOP
 | ADD
 | MUL
 | SUB
 | DIV
 | SDIV
 | MOD
 | SMOD
 | ADDMOD
 | MULMOD
 | EXP
 | SIGNEXTEND
 | LT
 | GT
 | SLT
 | SGT

 | EQ
 | ISZERO
 | AND
 | OR
 | XOR
 | NOT
 | BYTE
 | SHA3

 | ADDRESS
 | BALANCE
 | ORIGIN
 | CALLER
 | CALLVALUE
 | CALLDATALOAD
 | CALLDATASIZE
 | CALLDATACOPY
 | CODESIZE
 | CODECOPY
 | GASPRICE
 | EXTCODESIZE
 | EXTCODECOPY

 | BLOCKHASH
 | COINBASE
 | TIMESTAMP
 | NUMBER
 | DIFFICULTY
 | GASLIMIT

 | POP
 | MLOAD
 | MSTORE
 | MSTORE8
 | SLOAD
 | SSTORE
 | JUMP
 | JUMPI
 | PC
 | MSIZE
 | GAS
 | JUMPDEST

 | PUSH of int (* 1 to 32 *)
 | DUP of int (* 1 to 16 *)
 | SWAP of int (* 1 to 16 *)
 | LOG of int (* 0 to 4 *)

 | CREATE
 | CALL
 | CALLCODE
 | RETURN
 | DELEGATECALL
 | SELFDESTRUCT


exception UnknownOpcode of int
exception UnknownInstruction of string

(* bad argument for instruction PUSH, DUP, SWAP or LOG *)
exception InvalidInstructionArgument of string * int

type code = (instruction * string option) list


type rlp =
  | S of string
  | L of rlp list


type block_hash = BH of string
type state_root = SR of string
type ommers_hash = OH of string
type address = ADDR of string
type transactions_root = TR of string
type receipts_root = RR of string

type block_header = {
    bh_parentHash : block_hash;
    bh_ommersHash : ommers_hash;
    bh_benefiary : address;
    bh_stateRoot : state_root;
    bh_transactionsRoot : transactions_root;
    bh_receiptsRoot : receipts_root;
    bh_logsBloom : string;
    bh_difficulty : int64;
    bh_number : int64;
    bh_gasLimit : int64;
    bh_gasUsed : int64;
    bh_timestamp : int64;
    bh_extraData : string;
    bh_mixHash : string;
    bh_nonce : string;
  }

type transaction = {
    tr_nonce : string;
    tr_gasPrice : int64;
    tr_gasLimit : int64;
    tr_to : address option;
    tr_value : Z.t;
    tr_data : string;
    tr_v : string;
    tr_r : string;
    tr_s : string;
  }

(* Content of each entry of state_root *)
type account_state = {
    acc_nonce : Z.t;
    acc_balance : Z.t;
    acc_storageRoot : string; (* db_key *)
    acc_codeHash : string option; (* KEC( code ) *)
  }
