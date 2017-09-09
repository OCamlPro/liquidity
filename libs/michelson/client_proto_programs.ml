(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context
open Script

let report_parse_error _prefix exn _lexbuf =
  let open Lexing in
  let open Script_located_ir in
  let print_loc ppf ((sl, sc), (el, ec)) =
    if sl = el then
      if sc = ec then
      Format.fprintf ppf
        "at line %d character %d"
        sl sc
      else
      Format.fprintf ppf
        "at line %d characters %d to %d"
        sl sc ec
    else
      Format.fprintf ppf
        "from line %d character %d to line %d character %d"
        sl sc el ec in
  match exn with
  | Missing_program_field n ->
      failwith "missing script %s" n
  | Illegal_character (loc, c) ->
      failwith "%a, illegal character %C" print_loc loc c
  | Illegal_escape (loc, c) ->
      failwith "%a, illegal escape sequence %S" print_loc loc c
  | Failure s ->
      failwith "%s" s
  | exn ->
      failwith "%s" @@ Printexc.to_string exn

let print_location_mark ppf = function
  | None -> ()
  | Some l -> Format.fprintf ppf " /* %d */" l

let no_locations _ = None

let rec print_expr_unwrapped locations ppf = function
  | Script.Prim (loc, name, []) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "%s" name
        | Some _ as l -> Format.fprintf ppf "(%s%a)" name print_location_mark l
      end
  | Script.Prim (loc, name, args) ->
      Format.fprintf ppf "@[<hov 2>%s%a@ %a@]"
        name print_location_mark (locations loc)
        (Format.pp_print_list
           ~pp_sep: Format.pp_print_space
           (print_expr locations))
        args
  | Script.Seq (loc, []) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "{}"
        | Some _ as l -> Format.fprintf ppf "{%a }" print_location_mark l
      end
  | Script.Seq (loc, exprs) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "@[<hv 2>{ "
        | Some _ as l -> Format.fprintf ppf "@[<hv 2>{%a@ " print_location_mark l
      end ;
      Format.fprintf ppf "%a@] }"
        (Format.pp_print_list
           ~pp_sep: (fun ppf () -> Format.fprintf ppf " ;@ ")
           (print_expr_unwrapped locations))
        exprs
  | Script.Int (loc, n) ->
      Format.fprintf ppf "%s%a" n print_location_mark (locations loc)
  | Script.String (loc, s) ->
      Format.fprintf ppf "%S%a" s print_location_mark (locations loc)

and print_expr locations ppf = function
  | Script.Prim (_, _, _ :: _) as expr ->
      Format.fprintf ppf "(%a)" (print_expr_unwrapped locations) expr
  | expr -> print_expr_unwrapped locations ppf expr

let print_typed_code locations ppf (expr, type_map) =
  let print_stack ppf = function
    | [] -> Format.fprintf ppf "[]"
    | more ->
        Format.fprintf ppf "@[<hov 2>[ %a ]@]"
          (Format.pp_print_list
             ~pp_sep: (fun ppf () -> Format.fprintf ppf " :@ ")
             (print_expr_unwrapped no_locations))
          more in
  let rec print_typed_code_unwrapped ppf expr =
    match expr with
    | Script.Prim (loc, name, []) ->
        Format.fprintf ppf "%s%a"
          name print_location_mark (locations loc)
    | Script.Prim (loc, name, args) ->
        Format.fprintf ppf "@[<hov 2>%s%a@ %a@]"
          name print_location_mark (locations loc)
          (Format.pp_print_list
             ~pp_sep: Format.pp_print_space
             print_typed_code)
          args
    | Script.Seq (loc, []) ->
        begin match List.assoc loc type_map with
          | exception Not_found -> Format.fprintf ppf "{}"
          | (first, _) ->
              match locations loc with
              | None ->
                  Format.fprintf ppf "{} /* %a */"
                    print_stack first
              | Some _ as l ->
                  Format.fprintf ppf "{%a %a }"
                    print_location_mark l print_stack first
        end
    | Script.Seq (loc, exprs) ->
        begin match locations loc with
          | None ->
              Format.fprintf ppf "@[<v 2>{ "
          | Some _ as l ->
              Format.fprintf ppf "@[<v 2>{%a@,"
                print_location_mark l
        end ;
        let rec loop = function
          | [] -> assert false
          | [ Script.Int (loc, _) | String (loc, _) | Prim (loc, _, _) as expr ] ->
              begin match List.assoc loc type_map with
                | exception Not_found ->
                    Format.fprintf ppf "%a }@]"
                      print_typed_code_unwrapped expr
                | (before, after) ->
                    Format.fprintf ppf "/* %a */@,%a@,/* %a */ }@]"
                      print_stack before
                      print_typed_code_unwrapped expr
                      print_stack after
              end ;
          | Script.Int (loc, _) | String (loc, _) | Prim (loc, _, _) as expr :: rest ->
              begin match List.assoc loc type_map with
                | exception Not_found ->
                    Format.fprintf ppf "%a ;@,"
                      print_typed_code_unwrapped expr ;
                    loop rest
                | (before, _) ->
                    Format.fprintf ppf "/* %a */@,%a ;@,"
                      print_stack before
                      print_typed_code_unwrapped expr ;
                    loop rest
              end ;
          | [ Seq (_, _) as expr ] ->
              Format.fprintf ppf "%a }@]"
                print_typed_code_unwrapped expr
          | Seq (_, _) as expr :: rest ->
              Format.fprintf ppf "%a@,"
                print_typed_code_unwrapped expr ;
              loop rest in
        loop exprs ;
    | Script.Int (loc, n) ->
        Format.fprintf ppf "%s%a" n print_location_mark (locations loc)
    | Script.String (loc, s) ->
        Format.fprintf ppf "%S%a" s print_location_mark (locations loc)
  and print_typed_code ppf = function
    | Script.Prim (_, _, _ :: _) as expr ->
        Format.fprintf ppf "(%a)" print_typed_code_unwrapped expr
    | expr -> print_typed_code_unwrapped ppf expr in
  print_typed_code_unwrapped ppf expr

let print_program locations ppf ((c : Script.code), type_map) =
  Format.fprintf ppf
    "@[<v 0>@[<hov 2>storage@ %a ;@]@,\
     @[<hov 2>parameter@ %a ;@]@,\
     @[<hov 2>return@ %a ;@]@,\
     @[<hov 2>code@ %a@]@]"
    (print_expr no_locations) c.storage_type
    (print_expr no_locations) c.arg_type
    (print_expr no_locations) c.ret_type
    (print_typed_code locations) (c.code, type_map)

let parse_program s =
  let lexbuf = Lexing.from_string s in
  try
    return
      (Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf |>
       List.map Script_located_ir.strip_locations |> fun fields ->
       let rec get_field n = function
         | Script.Prim (_, pn, [ ctns ]) :: _ when n = pn -> ctns
         | _ :: rest -> get_field n rest
         | [] -> raise (Script_located_ir.Missing_program_field n) in
       Script.{ code = get_field "code" fields ;
                arg_type = get_field "parameter" fields ;
                ret_type = get_field "return" fields ;
                storage_type = get_field "storage" fields }
      )
  with
  | exn -> report_parse_error "program: " exn lexbuf

let parse_data s =
  let lexbuf = Lexing.from_string s in
  try
    match Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf with
    | [node] -> return (Script_located_ir.strip_locations node)
    | _ -> failwith "single data expression expected"
  with
  | exn -> report_parse_error "data: " exn lexbuf

let parse_data_type s =
  let lexbuf = Lexing.from_string s in
  try
    match Concrete_parser.tree Concrete_lexer.(token (init_state ())) lexbuf with
    | [node] -> return (Script_located_ir.strip_locations node)
    | _ -> failwith "single type expression expected"
  with
  | exn -> report_parse_error "data_type: " exn lexbuf

let unexpand_macros type_map (program : Script.code) =
  let open Script in
  let rec caddr type_map acc = function
    | [] -> Some (List.rev acc)
    | Prim (loc, "CAR" , []) :: rest when List.mem_assoc loc type_map ->
        caddr type_map ((loc, "A") :: acc) rest
    | Prim (loc, "CDR" , []) :: rest when List.mem_assoc loc type_map ->
        caddr type_map ((loc, "D") :: acc) rest
    | _ -> None in
  let rec unexpand type_map node =
    match node with
    | Seq (loc, l) ->
        begin match caddr type_map [] l with
          | None ->
              let type_map, l =
                List.fold_left
                  (fun (type_map, acc) e ->
                     let type_map, e = unexpand type_map e in
                     type_map, e :: acc)
                  (type_map, [])
                  l in
              type_map, Seq (loc, List.rev l)
          | Some l ->
              let locs, steps = List.split l in
              let name = "C" ^ String.concat "" steps ^ "R" in
              let first, last = List.hd locs, List.hd (List.rev locs) in
              let (before, _) = List.assoc first type_map in
              let (_, after) = List.assoc last type_map in
              let type_map =
                List.filter
                  (fun (loc, _) -> not (List.mem loc locs))
                  type_map in
              let type_map = (loc, (before, after)):: type_map in
              type_map, Prim (loc, name, [])
        end
    | oth -> type_map, oth in
  let type_map, code = unexpand type_map program.code in
  type_map, { program with code }
