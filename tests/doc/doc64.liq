type storage = (string, int) map
let%entry default () map =
  let negated_map = Map.map (fun (_key, v) -> - v) map in
  [], negated_map
