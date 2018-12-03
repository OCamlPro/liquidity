type storage = nat
let%entry main _ _ =
  let s = "Hello world" in
  let len = String.length s in
  ([] : operation list), len
