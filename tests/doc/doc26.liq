type storage = bytes
let%entry default () _ =
  let b = Bytes.pack [1; 2; 3] in
  let h = Crypto.sha512 b in
  if Bytes.length h <> 64p then failwith "incorrect size";
  if h <> 0x97f36bcf0a1d65c0d49852a56d93f3b1b15712a94e251ad88a619b2db7bfa34b85e3a7fc8dff5254bf0eacad4d979430cb1f12a7b094ecf295020597f9de7254 then
    failwith "incorrect hash";
  [], h
