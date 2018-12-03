type storage = (string, int) map
let%entry main _ map =
  let sum_vals = Map.fold (fun ((_, v), acc) -> acc + v) map 0 in
  if sum_vals <= 0 then
    failwith "Need at least one positive";
  ([] : operation list), map
