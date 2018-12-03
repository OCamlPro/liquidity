type storage = int set
let%entry main _ my_set =
  let my_set = Set.remove 10 my_set in
  ([] : operation list), my_set
