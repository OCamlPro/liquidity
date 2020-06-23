type storage = (int, string) map
let%entry default (id : int) map =
  let map = Map.remove id map in
  [], map
