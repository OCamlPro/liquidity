type storage = address

let%entry default (delegate : key_hash) _ =
  let initial_storage = (10DUN, "Hello") in
  let (op, addr) =
    Contract.create
      ~storage:initial_storage ~delegate:(Some delegate) ~amount:10DUN
      (contract struct
        type storage = dun * string
        let%entry default () s  = [], s
      end)
  in
  [op], addr
