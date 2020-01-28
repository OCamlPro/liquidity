type storage = nat
let%entry main () start_level =
  if Current.block_level () < start_level then
    failwith "not started";
  [], start_level
