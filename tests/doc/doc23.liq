type storage = address

contract S = struct
  type storage = dun * string
  let%entry default () s  = [], s
end

let%entry default (delegate : key_hash) _ =
  let initial_storage = (10DUN,"Hello") in
  let (op, addr) =
    Contract.create
      ~storage:initial_storage ~delegate:(Some delegate) ~amount:10DUN
      (contract S) in
  [op], addr
