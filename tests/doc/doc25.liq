type storage = bytes
let%entry default () _ =
  let b = Bytes.pack "This is a message" in
  let h = Crypto.sha512 b in
  if Bytes.length h <> 32p then failwith "incorrect size";
  if h <> 0x8624d6634774f992f349961d6991f57b6b437e2a48aebafcca03f14e29252f5e then
    failwith "incorrect hash";
  [], h
