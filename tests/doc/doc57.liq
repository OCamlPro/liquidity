type storage = (int, string) map
let%entry main (id : int) map =
  let map = Map.remove id map in
  [], map
