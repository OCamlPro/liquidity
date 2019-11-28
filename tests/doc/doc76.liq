type storage = unit
let%entry main () () =
  if Current.collect_call () then
    failwith "Cannot be called in a collect call";
  [], ()
