(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let print_evm_of_hexstring hexstr =
  let str = Hex_encode.hex_decode hexstr in
  Printf.printf "%s%!"
                (EvmPrinter.string_of_code
                   (EvmCode.disassemble str))

let print_evm_of_codefile filename =
  let str = FileString.read_file filename in
  Printf.printf "%s%!"
                (EvmPrinter.string_of_code
                   (EvmCode.disassemble str))
;;
let print_evm_of_hexfile filename =
  let ic = open_in filename in
  let hexstr = input_line ic in
  close_in ic;
  let str = Hex_encode.hex_decode hexstr in
  Printf.printf "%s%!"
                (EvmPrinter.string_of_code
                   (EvmCode.disassemble str))
;;
let work_done = ref false

let arg_usage = "EVM code printer"

let arg_list = [
    "--disass-hex-file", Arg.String (fun s ->
                    print_evm_of_hexfile s;
                    work_done := true),
    "FILE Print EVM code from file containing hexa code";
    "--disass-code-file", Arg.String (fun s ->
                    print_evm_of_codefile s;
                    work_done := true),
    "FILE Print EVM code from file";
    "--disass-hex-string", Arg.String (fun s ->
                      print_evm_of_hexstring s;
                      work_done := true),
    "HEXSTRING Print EVM code from string";

    "--geth", Arg.Unit (fun () ->
                 EvmGeth.load ();
                 work_done := true), " ";
  ]

let arg_anon s =
  Printf.eprintf "Error: unexpected argument %S\n%!" s;
  Arg.usage arg_list arg_usage;
  exit 2

let () =
  Arg.parse arg_list arg_anon arg_usage;
  if not !work_done then begin
      Arg.usage arg_list arg_usage;
      exit 0
    end
