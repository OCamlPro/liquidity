type storage = (string, int) map
let%entry main () map =
  Map.iter (fun (_, v) ->
      if v < 0 then
        failwith "No option should be negative"
    ) map;
  [], map
