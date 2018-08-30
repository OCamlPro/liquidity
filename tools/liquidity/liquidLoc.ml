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

let noloc = noloc

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

let report_error ?(kind="Error") fmt { err_loc; err_msg } =
  Format.fprintf fmt "%a: %s: @[%s@]\n%!" print_loc err_loc kind err_msg

let default_warning_printer loc w =
  Format.eprintf "%a: Warning: @[%a@]\n%!" print_loc loc
  (fun fmt -> function
     | Unused name ->
       Format.fprintf fmt "unused variable %S" name
     | UnusedMatched constr ->
       Format.fprintf fmt
         "unused branch, constructor %S is already matched" constr
     | IgnoredTypeAnnot ty ->
       Format.fprintf fmt "ignored type annotation: %s" ty
  ) w

let warning_printer = ref default_warning_printer

let warn loc w = !warning_printer loc w

let loc_in_file loc_file = { loc_file; loc_pos = None }

let compare_pos (l1, c1) (l2, c2) =
  let c = compare l1 l2 in
  if c <> 0 then c else compare c1 c2

let max_pos p1 p2 = if p1 <= p2 then p2 else p1
let min_pos p1 p2 = if p1 <= p2 then p1 else p2

let merge l1 l2 =
  let loc_file = l1.loc_file in
  let loc_pos = match l1.loc_pos, l2.loc_pos with
    | None, None -> None
    | None, Some p | Some p, None -> Some p
    | Some (b1, e1), Some (b2, e2) -> Some(min_pos b1 b2, max_pos e1 e2)
  in
  { loc_file; loc_pos }
