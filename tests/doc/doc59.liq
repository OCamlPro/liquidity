type storage = (int, string) map
let%entry main ((id : int), (v : string)) map =
  let new_map = Map.update id (None : string option) map in (* removed *)
  let new_map = Map.update id (Some v) new_map in (* added *)
  ([] : operation list), new_map
