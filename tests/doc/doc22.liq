type storage = address

let%entry main ((delegate : key_hash), (manager : key_hash)) _ =
  let delegatable = true in
  let spendable = false in
  let initial_storage = (10DUN,"Hello") in
  let (op, addr) =
    Contract.create ~storage:initial_storage ~manager ~spendable
      ~delegatable ~delegate:(Some delegate) ~amount:10DUN
      (contract struct
        type storage = dun * string
        let%entry main () s  = [], s
      end)
  in
  [op], addr
