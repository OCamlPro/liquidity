(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open EvmTypes

let code_of_instruction ins =
  match ins with
  | STOP -> 0x00
  | ADD -> 0x01
  | MUL -> 0x02
  | SUB -> 0x03
  | DIV -> 0x04
  | SDIV -> 0x05
  | MOD -> 0x06
  | SMOD -> 0x07
  | ADDMOD -> 0x08
  | MULMOD -> 0x09
  | EXP -> 0x0A
  | SIGNEXTEND -> 0x0B

  | LT -> 0x10
  | GT -> 0x11
  | SLT -> 0x12
  | SGT -> 0x13
  | EQ -> 0x14
  | ISZERO -> 0x15
  | AND -> 0x16
  | OR -> 0x17
  | XOR -> 0x18
  | NOT -> 0x19
  | BYTE -> 0x1A

  | SHA3 -> 0x20

  | ADDRESS -> 0x30
  | BALANCE -> 0x31
  | ORIGIN -> 0x32
  | CALLER -> 0x33
  | CALLVALUE -> 0x34
  | CALLDATALOAD -> 0x35
  | CALLDATASIZE -> 0x36
  | CALLDATACOPY -> 0x37
  | CODESIZE -> 0x38
  | CODECOPY -> 0x39
  | GASPRICE -> 0x3A
  | EXTCODESIZE -> 0x3B
  | EXTCODECOPY -> 0x3C

  | BLOCKHASH -> 0x40
  | COINBASE -> 0x41
  | TIMESTAMP -> 0x42
  | NUMBER -> 0x43
  | DIFFICULTY -> 0x44
  | GASLIMIT -> 0x45

  | POP -> 0x50
  | MLOAD -> 0x51
  | MSTORE -> 0x52
  | MSTORE8 -> 0x53
  | SLOAD -> 0x54
  | SSTORE -> 0x55
  | JUMP -> 0x56
  | JUMPI -> 0x57
  | PC -> 0x58
  | MSIZE -> 0x59
  | GAS -> 0x5A
  | JUMPDEST -> 0x5B

  | PUSH 1 -> 0x60
  | PUSH 2 -> 0x61
  | PUSH 3 -> 0x62
  | PUSH 4 -> 0x63
  | PUSH 5 -> 0x64
  | PUSH 6 -> 0x65
  | PUSH 7 -> 0x66
  | PUSH 8 -> 0x67
  | PUSH 9 -> 0x68
  | PUSH 10 -> 0x69
  | PUSH 11 -> 0x6A
  | PUSH 12 -> 0x6B
  | PUSH 13 -> 0x6C
  | PUSH 14 -> 0x6D
  | PUSH 15 -> 0x6E
  | PUSH 16 -> 0x6F
  | PUSH 17 -> 0x70
  | PUSH 18 -> 0x71
  | PUSH 19 -> 0x72
  | PUSH 20 -> 0x73
  | PUSH 21 -> 0x74
  | PUSH 22 -> 0x75
  | PUSH 23 -> 0x76
  | PUSH 24 -> 0x77
  | PUSH 25 -> 0x78
  | PUSH 26 -> 0x79
  | PUSH 27 -> 0x7A
  | PUSH 28 -> 0x7B
  | PUSH 29 -> 0x7C
  | PUSH 30 -> 0x7D
  | PUSH 31 -> 0x7E
  | PUSH 32 -> 0x7F
  | DUP 1 -> 0x80
  | DUP 2 -> 0x81
  | DUP 3 -> 0x82
  | DUP 4 -> 0x83
  | DUP 5 -> 0x84
  | DUP 6 -> 0x85
  | DUP 7 -> 0x86
  | DUP 8 -> 0x87
  | DUP 9 -> 0x88
  | DUP 10 -> 0x89
  | DUP 11 -> 0x8A
  | DUP 12 -> 0x8B
  | DUP 13 -> 0x8C
  | DUP 14 -> 0x8D
  | DUP 15 -> 0x8E
  | DUP 16 -> 0x8F
  | SWAP 1 -> 0x90
  | SWAP 2 -> 0x91
  | SWAP 3 -> 0x92
  | SWAP 4 -> 0x93
  | SWAP 5 -> 0x94
  | SWAP 6 -> 0x95
  | SWAP 7 -> 0x96
  | SWAP 8 -> 0x97
  | SWAP 9 -> 0x98
  | SWAP 10 -> 0x99
  | SWAP 11 -> 0x9A
  | SWAP 12 -> 0x9B
  | SWAP 13 -> 0x9C
  | SWAP 14 -> 0x9D
  | SWAP 15 -> 0x9E
  | SWAP 16 -> 0x9F

  | LOG 0 -> 0xA0
  | LOG 1 -> 0xA1
  | LOG 2 -> 0xA2
  | LOG 3 -> 0xA3
  | LOG 4 -> 0xA4

  | CREATE -> 0xF0
  | CALL -> 0xF1
  | CALLCODE -> 0xF2
  | RETURN -> 0xF3
  | DELEGATECALL -> 0xF4
  | SELFDESTRUCT -> 0xFF

  | PUSH _
    | SWAP _
    | DUP _
    | LOG _ -> EvmPrinter.raise_invalid_instruction_argument ins

let instruction_of_code = function
  | 0x00 -> STOP
  | 0x01 -> ADD
  | 0x02 -> MUL
  | 0x03 -> SUB
  | 0x04 -> DIV
  | 0x05 -> SDIV
  | 0x06 -> MOD
  | 0x07 -> SMOD
  | 0x08 -> ADDMOD
  | 0x09 -> MULMOD
  | 0x0A -> EXP
  | 0x0B -> SIGNEXTEND

  | 0x10 -> LT
  | 0x11 -> GT
  | 0x12 -> SLT
  | 0x13 -> SGT

  | 0x14 -> EQ
  | 0x15 -> ISZERO
  | 0x16 -> AND
  | 0x17 -> OR
  | 0x18 -> XOR
  | 0x19 -> NOT
  | 0x1A -> BYTE

  | 0x20 -> SHA3

  | 0x30 -> ADDRESS
  | 0x31 -> BALANCE
  | 0x32 -> ORIGIN
  | 0x33 -> CALLER
  | 0x34 -> CALLVALUE
  | 0x35 -> CALLDATALOAD
  | 0x36 -> CALLDATASIZE
  | 0x37 -> CALLDATACOPY
  | 0x38 -> CODESIZE
  | 0x39 -> CODECOPY
  | 0x3A -> GASPRICE
  | 0x3B -> EXTCODESIZE
  | 0x3C -> EXTCODECOPY

  | 0x40 -> BLOCKHASH
  | 0x41 -> COINBASE
  | 0x42 -> TIMESTAMP
  | 0x43 -> NUMBER
  | 0x44 -> DIFFICULTY
  | 0x45 -> GASLIMIT

  | 0x50 -> POP
  | 0x51 -> MLOAD
  | 0x52 -> MSTORE
  | 0x53 -> MSTORE8
  | 0x54 -> SLOAD
  | 0x55 -> SSTORE
  | 0x56 -> JUMP
  | 0x57 -> JUMPI
  | 0x58 -> PC
  | 0x59 -> MSIZE
  | 0x5A -> GAS
  | 0x5B -> JUMPDEST

  | 0x60 -> PUSH 1
  | 0x61 -> PUSH 2
  | 0x62 -> PUSH 3
  | 0x63 -> PUSH 4
  | 0x64 -> PUSH 5
  | 0x65 -> PUSH 6
  | 0x66 -> PUSH 7
  | 0x67 -> PUSH 8
  | 0x68 -> PUSH 9
  | 0x69 -> PUSH 10
  | 0x6A -> PUSH 11
  | 0x6B -> PUSH 12
  | 0x6C -> PUSH 13
  | 0x6D -> PUSH 14
  | 0x6E -> PUSH 15
  | 0x6F -> PUSH 16
  | 0x70 -> PUSH 17
  | 0x71 -> PUSH 18
  | 0x72 -> PUSH 19
  | 0x73 -> PUSH 20
  | 0x74 -> PUSH 21
  | 0x75 -> PUSH 22
  | 0x76 -> PUSH 23
  | 0x77 -> PUSH 24
  | 0x78 -> PUSH 25
  | 0x79 -> PUSH 26
  | 0x7A -> PUSH 27
  | 0x7B -> PUSH 28
  | 0x7C -> PUSH 29
  | 0x7D -> PUSH 30
  | 0x7E -> PUSH 31
  | 0x7F -> PUSH 32
  | 0x80 -> DUP 1
  | 0x81 -> DUP 2
  | 0x82 -> DUP 3
  | 0x83 -> DUP 4
  | 0x84 -> DUP 5
  | 0x85 -> DUP 6
  | 0x86 -> DUP 7
  | 0x87 -> DUP 8
  | 0x88 -> DUP 9
  | 0x89 -> DUP 10
  | 0x8A -> DUP 11
  | 0x8B -> DUP 12
  | 0x8C -> DUP 13
  | 0x8D -> DUP 14
  | 0x8E -> DUP 15
  | 0x8F -> DUP 16
  | 0x90 -> SWAP 1
  | 0x91 -> SWAP 2
  | 0x92 -> SWAP 3
  | 0x93 -> SWAP 4
  | 0x94 -> SWAP 5
  | 0x95 -> SWAP 6
  | 0x96 -> SWAP 7
  | 0x97 -> SWAP 8
  | 0x98 -> SWAP 9
  | 0x99 -> SWAP 10
  | 0x9A -> SWAP 11
  | 0x9B -> SWAP 12
  | 0x9C -> SWAP 13
  | 0x9D -> SWAP 14
  | 0x9E -> SWAP 15
  | 0x9F -> SWAP 16

  | 0xA0 -> LOG 0
  | 0xA1 -> LOG 1
  | 0xA2 -> LOG 2
  | 0xA3 -> LOG 3
  | 0xA4 -> LOG 4

  | 0xF0 -> CREATE
  | 0xF1 -> CALL
  | 0xF2 -> CALLCODE
  | 0xF3 -> RETURN
  | 0xF4 -> DELEGATECALL
  | 0xFF -> SELFDESTRUCT

  | n -> raise (UnknownOpcode n)

let disassemble s =
  let rec iter pc s code =
    if pc = String.length s then List.rev code
    else
      let n = Char.code s.[pc] in
      let ins = instruction_of_code n in
      Printf.eprintf "%s\n%!" (EvmPrinter.string_of_instruction ins);
      match ins with
      | PUSH n ->
         let arg = String.sub s (pc+1) n in
         iter (pc+1+n) s ( (ins, Some arg) :: code )
      | _ ->
         iter (pc+1) s ( (ins,None) :: code)
  in
  iter 0 s []

let assemble code =
  let b = Buffer.create 16_000 in
  List.iter (fun (ins, arg) ->
      Buffer.add_char b (char_of_int (code_of_instruction ins));
      match arg with
      | None -> ()
      | Some arg -> Buffer.add_string b arg
    ) code;
  Buffer.contents b
