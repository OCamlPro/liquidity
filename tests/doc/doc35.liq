type storage = nat
let%entry default () _ =
  let s = "Hello world" in
  let len = String.length s in
  [], len
