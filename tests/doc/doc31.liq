type storage = unit
let%entry default () _ =
  let s = Bytes.pack (1, 2, 3, 4) in
  let n = Bytes.length s in
  if n > 16p then failwith "serialization too long";
  [], ()
