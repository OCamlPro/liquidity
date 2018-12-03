type storage = (int, string) map
let%entry main (id : int) map =
  let _v = match Map.find id map with
    | None -> failwith ("id is not in the map", id)
    | Some v -> v
  in
  ([] : operation list), map
