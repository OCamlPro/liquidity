type storage = (int, string) map
let%entry default ((id : int), (v : string)) map =
  let new_map = Map.update id None map in (* removed *)
  let new_map = Map.update id (Some v) new_map in (* added *)
  [], new_map
