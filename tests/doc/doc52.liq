type storage = unit
let%entry default (my_set : int set) _ =
  Set.iter (fun ele -> if ele < 0 then failwith "negative integer") my_set;
  [], ()
