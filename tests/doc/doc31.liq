type storage = unit
let%entry main _ _ =
  let s = Bytes.pack (1, 2, 3, 4) in
  let n = Bytes.length s in
  if n > 16p then failwith "serialization too long";
  ([] : operation list), ()
