type storage = timestamp

let%entry default () _ =
  let now = Current.time() in
  [], now
