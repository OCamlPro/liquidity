type storage = unit

let%entry main _ _ = (* accept funds *)
  [], ()

let%entry change_delegate (new_del : key_hash) _ =
  let op = Contract.set_delegate (Some new_del) in
  [op], ()

let%entry remove_delegate _ _ =
  let op = Contract.set_delegate None in
  [op], ()
