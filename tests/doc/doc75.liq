type storage = nat
let%entry default () start_level =
  if Current.block_level () < start_level then
    failwith "not started";
  [], start_level
