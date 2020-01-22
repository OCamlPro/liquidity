type storage = unit
let%entry default (list : nat list) _ =
  List.iter (fun x ->
      if x < 10p then failwith "error, element two small"
    ) list;
  [], ()
