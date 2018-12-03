type storage = {
  big : (int, string) big_map;
  nothing : unit
}

let%entry main _ storage =
  let big = Map.add 10 "ten" storage.big in
  let big = Map.remove 0 big in
  let big = Map.update 0 (Some "zero") big in
  let big = Map.update 1 (None : string option) big in
  let storage = storage.big <- big in
  ([] : operation list), storage
