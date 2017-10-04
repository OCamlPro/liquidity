(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open LiquidTypes


let pp_ksprintf ?before k fmt = (* From Location in OCaml *)
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  begin match before with
    | None -> ()
    | Some f -> f ppf
  end;
  Format.kfprintf
    (fun _ ->
      Format.pp_print_flush ppf ();
      let msg = Buffer.contents buf in
      k msg)
    ppf fmt

let noloc = { loc_file = "<unspecified>"; loc_pos = None }

let raise_error ?(loc=noloc) =
  pp_ksprintf (fun msg -> raise (LiquidError { err_loc = loc;
                                               err_msg = msg }))

let print_loc ppf loc =
  match loc.loc_pos with
  | Some ( (begin_line, begin_char) , (end_line, end_char) ) ->
     Format.fprintf ppf "%s:%d.%d-%d.%d"
                    loc.loc_file
                    begin_line begin_char
                    end_line end_char
  | None ->
     Format.fprintf ppf "%s" loc.loc_file

let report_error { err_loc; err_msg } =
  Format.eprintf "%a: Error: @[%s@]\n%!" print_loc err_loc err_msg

let default_warning_printer loc w =
  Format.eprintf "%a: Warning: @[%a@]\n%!" print_loc loc
  (fun fmt -> function
     | Unused name ->
       Format.fprintf fmt "unused variable %S" name)
  w

let warning_printer = ref default_warning_printer

let warn loc w = !warning_printer loc w

let loc_in_file loc_file = { loc_file; loc_pos = None }
