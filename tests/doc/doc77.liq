type storage = unit
let%entry main addr () =
  if Current.balance () < Contract.get_balance addr then
    failwith "balance too big";
  [], ()
