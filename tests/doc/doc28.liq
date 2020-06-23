type storage = key

let%entry default ((message : string), (signature : signature)) key =
  let bytes = Bytes.pack message in
  if not (Crypto.check key signature bytes) then
    failwith "Wrong signature";
  [], key
