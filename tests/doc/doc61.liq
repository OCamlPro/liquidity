type storage = (address, string) map
let%entry main () owners_map =
  if Map.size owners_map = 0p then failwith "no owners";
  [], owners_map
