type storage = string list
let%entry main _ old_list =
  let new_list = "Hello" :: old_list in
  ([] : operation list), new_list
