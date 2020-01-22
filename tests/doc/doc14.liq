contract type My = sig
  val%entry my_entry : int
end

type storage = unit

let%entry main ((amount : dun ), (p : int), (c : address)) _ =
  let op1 = c.my_entry p ~amount in
  (* this is syntactic sugar for: *)
  let op2 = Contract.call ~dest:c ~entry:my_entry ~parameter:p ~amount in
  [op1; op2], ()
