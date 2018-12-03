type storage = (address, string) map
let%entry main _ owners_map =
  if Map.size owners_map = 0p then failwith "no owners";
  ([] : operation list), owners_map
