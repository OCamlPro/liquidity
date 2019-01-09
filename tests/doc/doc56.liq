type storage = (int, string) map
let%entry main () map =
  let map = Map.add 1 "Hello" map in
  let map = Map.add 2 "World" map in
  [], map
