type storage = int list
let%entry default () list =
  let list = List.map (fun x -> x + 1) list in
  [], list
