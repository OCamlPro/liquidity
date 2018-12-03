type storage = (string, int) map
let%entry main _ map =
  let negated_map = Map.map (fun (_key, v) -> - v) map in
  ([] : operation list), negated_map
