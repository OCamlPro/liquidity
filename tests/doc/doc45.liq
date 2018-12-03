type storage = int list
let%entry main _ list =
  let list = List.map (fun x -> x + 1) list in
  ([] : operation list), list
