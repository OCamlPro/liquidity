type storage = int set
let%entry default () my_set =
  let my_set = Set.add 3 my_set in
  if not ( Set.mem 3 my_set ) then
    failwith "Missing integer 3 in int set";
  [], my_set
