type storage = unit
let%entry main (list : nat list) _ =
  List.iter (fun x ->
      if x < 10p then failwith "error, element two small"
    ) list;
  ([] : operation list), ()
