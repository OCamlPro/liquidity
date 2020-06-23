type storage = {
  big : (int, string) big_map;
  nothing : unit
}

let%entry default (param : int) storage =
  let _v = match Map.find param storage.big with
    | None -> failwith ("param is not in the map", param)
    | Some v -> v
  in
  [], storage
