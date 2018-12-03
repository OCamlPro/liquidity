type storage = bytes
let%entry main _ _ =
  let b = 0xdeadbeef in
  let h = Crypto.blake2b b in
  if Bytes.length h <> 32p then failwith "incorrect size";
  if h <> 0xf3e925002fed7cc0ded46842569eb5c90c910c091d8d04a1bdf96e0db719fd91 then
    failwith "incorrect hash";
  ([] : operation list), h
