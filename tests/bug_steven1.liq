type storage = address 
contract C = struct 
  type storage = A | B
  let%entry main (_delegate : key_hash) _ = [], A
end
let%entry main ((delegate : key_hash), (manager : key_hash)) (s : storage) =
  let delegatable = true in
  let spendable = false in
  let initial_storage = C.A in
  let (op, addr) =
    Contract.create ~storage:initial_storage ~manager ~spendable
      ~delegatable ~delegate:(Some delegate) ~amount:10tz
      (contract C)
  in
  [], addr
