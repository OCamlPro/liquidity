type storage = timestamp

let%entry main () _ =
  let now = Current.time() in
  [], now
