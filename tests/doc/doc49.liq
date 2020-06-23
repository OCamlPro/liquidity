type storage = int set
let%entry default () my_set =
  let my_set = Set.remove 10 my_set in
  [], my_set
