type storage = unit
let%entry default () _ =
  let b = Bytes.pack [1; 2; 3; 4; 5] in
  let hash = Crypto.sha256 b in
  if hash = 0x then failwith "?";
  [], ()
