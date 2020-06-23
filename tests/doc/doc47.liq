type storage = int set
let%entry default () my_set =
  let my_set = Set.update 3 true my_set in (* add 3 *)
  let my_set = Set.update 10 false my_set in (* remove 10 *)
  [], my_set
