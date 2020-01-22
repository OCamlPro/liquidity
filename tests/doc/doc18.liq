type storage = unit

let%entry default () () = (* accept funds *)
  [], ()

let%entry change_delegate (new_del : key_hash) () =
  let op = Contract.set_delegate (Some new_del) in
  [op], ()

let%entry remove_delegate () () =
  let op = Contract.set_delegate None in
  [op], ()
