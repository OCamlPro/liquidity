type storage = nat
let%entry main () _ =
  let s = "Hello world" in
  let len = String.length s in
  [], len
