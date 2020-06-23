type storage = unit
let%entry default () () =
  if Current.collect_call () then
    failwith "Cannot be called in a collect call";
  [], ()
