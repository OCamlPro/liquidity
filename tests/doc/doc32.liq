type storage = unit
let%entry main _ _ =
  let s = Bytes.concat [ 0x616161; 0x616161 ] in
  if Bytes.length s <> 6p then failwith "bad concat !";
  ([] : operation list), ()
