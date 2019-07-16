type storage = address

contract S = struct
  type x = {w : int; r : bool }
  type storage = { some_tez : tez; some_string : string ; x : x}
  let%entry main () s  =
    if s.some_tez < 10tz then failwith "less-than-ten" ;
    [], s
end

let%entry main ((delegate : key_hash), (manager : key_hash)) _ =
  let delegatable = true in
  let spendable = false in
  let initial_storage = { S.some_tez = 10tz; S.some_string = "Hello" ; S.x = { S.w = 0; S.r = true }} in
  let (op, addr) =
    Contract.create ~storage:initial_storage ~manager ~spendable
      ~delegatable ~delegate:(Some delegate) ~amount:10tz (contract S) in
  [op], addr
