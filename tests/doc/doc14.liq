contract type My = sig
  type storage
  val%entry my_entry : int -> _
end

type storage = unit

let%entry main ((amount : tez ), (p : int), (c : My.instance)) _ =
  let op1 = c.my_entry p ~amount in
  (* this is syntactic sugar for: *)
  let op2 = Contract.call ~dest:c ~entry:my_entry ~parameter:p ~amount in
  [op1; op2], ()
