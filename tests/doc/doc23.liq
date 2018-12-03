type storage = address

contract S = struct
  type storage = tez * string
  let%entry main _ s  = ([] : operation list), s
end

let%entry main ((delegate : key_hash), (manager : key_hash)) _ =
  let delegatable = true in
  let spendable = false in
  let initial_storage = (10tz,"Hello") in
  let (op, addr) =
    Contract.create ~storage:initial_storage ~manager ~spendable
      ~delegatable ~delegate:(Some delegate) ~amount:10tz (contract S) in
  [op], addr
