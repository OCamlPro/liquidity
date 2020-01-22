type storage = unit
let%entry default () _ =
  let s = Bytes.concat [ 0x616161; 0x616161 ] in
  if Bytes.length s <> 6p then failwith "bad concat !";
  [], ()
