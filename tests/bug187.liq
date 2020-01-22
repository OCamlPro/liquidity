type storage = {
  mile_id: string list;
  owner_addr: address list;
}

let[@noinline] list_mem k l =
  List.fold (fun (e,acc) ->
      match acc with
      | Some _ -> acc
      | None ->
          if e = k
          then Some e
          else acc
    ) l None

let%entry default (i, a) s =
  let s = match list_mem i s.mile_id with
    | Some _ -> { mile_id = []; owner_addr = [] }
    | None -> s in
  let s = match list_mem a s.owner_addr with
    | Some _ -> { mile_id = []; owner_addr = [] }
    | None -> s in
  let s = match list_mem 1 [2] with
    | Some _ -> { mile_id = []; owner_addr = [] }
    | None -> s in
  ([], s)
