open LiquidClientUtils
open LiquidClientRequest
open LiquidClientSigs
open LiquidClientTypes
open LiquidTypes
open Lwt.Infix

module Liquidity = LiquidityLang

type error = location * string
type trace = (location, syntax_const Lazy_superposed.t) Trace.t

exception RuntimeError of error * trace option
exception LocalizedError of error
exception RuntimeFailure of error * syntax_const Lazy_superposed.t option * trace option

module Make (L : LANG) = struct
  open L
  module T = LiquidClientTypes.Make(L)

  let name_of_var_annot = function
    | None -> None
    | Some annot ->
      try Scanf.sscanf annot "@%s" (function
          | "" -> None
          | s -> Some s
        )
      with Scanf.Scan_failure _ | End_of_file -> None

  let convert_stack stack_expr =
    List.(rev @@ rev_map (fun (e, annot) ->
        let name = name_of_var_annot annot in
        let c = decompile_const e |> Liquidity.const#ast in
        c, name
      ) stack_expr)

  let convert_trace ~loc_table t =
    List.(rev @@ rev_map (fun Trace.{ loc; gas; stack } ->
        let loc =  match loc with
          | None -> None
          | Some loc -> match List.assoc_opt loc loc_table with
            | Some (loc, _) -> Some loc
            | None -> None
        in
        let stack = convert_stack stack in
        Trace.{ loc; gas; stack }
      ) t)

  let trace_of_json ~loc_table ?(error=false) trace_r =
    let trace_expr =
      Json_encoding.destruct
        (Trace.encoding Target.loc_encoding Target.const_encoding) trace_r in
    let trace_expr = match List.rev trace_expr with
      | (Trace.{loc = Some l ; gas; _} :: _) as rtrace_expr when error ->
        let extra = Trace.{loc = Some (Target.next_loc l); gas; stack = []} in
        List.rev (extra :: rtrace_expr)
      | _ -> trace_expr in
    convert_trace ~loc_table trace_expr


  let fail_of_err loc ~loc_table err =
    let json = Ezjsonm.find err ["with"] in
    let err_loc, _ (* failwith_ty *) = List.assoc loc loc_table in
    let failed_with_expr = Json_encoding.destruct Target.const_encoding json in
    let failed_with = decompile_const failed_with_expr |> Liquidity.const#ast in
    err_loc, Some failed_with

  let error_trace_of_err loc ~loc_table err =
    let err_loc, _ = List.assoc loc loc_table in
    try
      let json = Ezjsonm.find err ["trace"] in
      let trace = trace_of_json ~loc_table ~error:true json in
      err_loc, Some trace
    with Not_found -> err_loc, None

  let raise_error_from_l ?loc_table err_msg l =
    let default_error () =
      let last_descr = match List.rev l with
        | (_, _, _, _, Some descr, _) :: _ -> "\n  " ^ descr
        | _ -> ""
      in
      let err_l =
        List.map (fun (kind, id, _, title, descr, _) ->
            match title with
            | Some t -> t
            | None -> Printf.sprintf "%s: %s" kind id
          ) l
        |> String.concat "\n- "
      in
      Printf.sprintf "in %s\n- %s%s" err_msg err_l last_descr
    in
    match loc_table with
    | None -> raise (ResponseError (default_error ()))
    | Some loc_table ->
      let err_msg = Printf.sprintf "in %s" err_msg in
      try
        List.iter (fun (kind, id, loc, title, descr, err) ->
            let is_rejected =
              match String.rindex_opt id '.' with
              | None -> false
              | Some i ->
                match String.sub id i (String.length id - i) with
                | ".script_rejected" | ".scriptRejectedRuntimeError" -> true
                | _ -> false
            in
            match loc, kind, is_rejected with
            | Some loc, "temporary", true ->
              let err_loc, fail_v = fail_of_err loc ~loc_table err in
              let _, trace = error_trace_of_err loc ~loc_table err in
              raise (RuntimeFailure ((err_loc, err_msg), fail_v, trace))
            | Some loc, "temporary", _ ->
              let title = match title with Some t -> t | None -> id in
              let err_msg = String.concat "\n- " [err_msg; title] in
              let err_loc, trace = error_trace_of_err loc ~loc_table err in
              raise (RuntimeError ((err_loc, err_msg), trace))
            | Some loc, _, _ ->
              let err_loc, _ = List.assoc loc loc_table in
              let err_msg = default_error () in
              raise (LocalizedError (err_loc, err_msg))
            | _ -> ()
          ) l;
        raise (ResponseError (default_error ()))
      with Not_found -> raise (ResponseError (default_error ()))

  let extract_errors_from_json r schema =
    try
      let schema_l = Ezjsonm.find schema ["oneOf"] in
      try
        Ezjsonm.find r ["error"], schema_l
      with Not_found ->
      match Ezjsonm.get_list (fun x -> x) r with
      | err :: _ ->
        begin try
            let r = Ezjsonm.find err ["ecoproto"] in
            let id = Ezjsonm.find err ["id"] |> Ezjsonm.get_string in
            let schema_l =
              schema_l
              |> Ezjsonm.get_list (fun s ->
                  try
                    let s_id =
                      Ezjsonm.find s ["properties"; "id"; "enum"]
                      |> Ezjsonm.get_list Ezjsonm.get_string
                      |> function [s] -> s | _ -> assert false
                    in
                    if s_id <> id then
                      None
                    else
                      Some (Ezjsonm.find s
                              ["properties"; "ecoproto"; "items"; "oneOf"])
                  with Not_found -> None
                )
              |> List.find (function None -> false | Some _ -> true)
              |> function None -> assert false | Some s -> s
            in
            r, schema_l
          with Not_found  -> r, schema_l
        end
      | [] -> r, schema_l
      | exception Ezjsonm.Parse_error _ -> r, schema_l
    with Not_found -> r, schema

  let rec descr_of_id id schema =
    try
      schema
      |> Ezjsonm.get_list (fun s ->
          try
            let schema = Ezjsonm.find s ["oneOf"] in
            descr_of_id id schema
          with Not_found ->
          try
            let s_id =
              Ezjsonm.find s ["properties"; "id"; "enum"]
              |> Ezjsonm.get_list Ezjsonm.get_string
              |> function [s] -> s | _ -> assert false
            in
            if s_id <> id then
              None, None
            else (
              let t =
                try Some (Ezjsonm.find s ["title"] |> Ezjsonm.get_string)
                with Not_found -> None
              in
              let d =
                try Some (Ezjsonm.find s ["description"] |> Ezjsonm.get_string)
                with Not_found -> None
              in
              (t, d)
            )
          with Not_found ->
            None, None
        )
      |> List.find (function Some _, _ | _, Some _ -> true | _ -> false)
    with Not_found | Ezjsonm.Parse_error _ ->
      None, None

  (*
  let error_encoding =
    let open Json_encoding in
    union [
      case_ignore_extra
        (obj2
           (req "kind" (constant "generic"))
           (req "error" string))
        (fun _ -> None)
        (fun ((), err) -> raise (ResponseError err));

      case_ignore_extra
        (obj6
           (req "kind" string)
           (req "id" string)
           (opt "location" Target.loc_encoding)
           (opt "loc" Target.loc_encoding)
           (opt "with" any_value)
           (opt "trace" any_value)
        )
        (fun (kind, id, loc, fail_with_json, trace_json) ->
           Some (kind, id, loc, None, fail_with_json, trace_json))
        (fun (kind, id, location, loc, fail_with_json, trace_json) ->
           let loc = match location with None -> loc | Some _ as l -> l in
           (kind, id, loc, fail_with_json, trace_json));
    ]
    *)
  
  let raise_response_error ?loc_table msg r =
    Lazy.force error_schema >>= fun error_schema ->
    let err, schema = extract_errors_from_json r error_schema in
    let l =
      try
        Ezjsonm.get_list (fun err ->
            let kind = Ezjsonm.find err ["kind"] |> Ezjsonm.get_string in
            if kind = "generic" then begin
              let err = Ezjsonm.find err ["error"] |> Ezjsonm.get_string in
              raise (ResponseError err)
            end;
            let id = Ezjsonm.find err ["id"] |> Ezjsonm.get_string in
            let title, descr = descr_of_id id schema in
            let loc =
              try Some (Ezjsonm.find err ["location"]
                        |> Json_encoding.destruct Target.loc_encoding)
              with Not_found ->
              try Some (Ezjsonm.find err ["loc"]
                        |> Json_encoding.destruct Target.loc_encoding)
              with Not_found -> None
            in
            kind, id, loc, title, descr, err
          ) err
      with Ezjsonm.Parse_error _ | Not_found -> []
    in
    raise_error_from_l ?loc_table msg l

end
