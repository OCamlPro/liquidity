type storage = string list
let%entry default () old_list =
  let new_list = "Hello" :: old_list in
  [], new_list
