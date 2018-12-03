type storage = unit
let%entry main (my_set : int set) _ =
  let cardinal = Set.size my_set in
  if cardinal < 10p then failwith "too few elements";
  ([] : operation list), ()
