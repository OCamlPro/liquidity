type storage = bytes
let%entry main _ _ =
  let b = 0x616161 in
  let s = b @ b in
  let b' = match Bytes.sub 3p 3p s with
    | Some b -> b
    | None -> failwith () in
  ([] : operation list), b'
