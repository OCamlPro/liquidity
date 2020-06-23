module Lib = struct
  let[@noinline] sender_check addr =
  if Current.sender () <> addr then
    failwith ()
  else
    ()
end

contract C = struct
    type storage = unit
    let%entry default () s =
      Lib.sender_check (Current.source ());       (* Cannot access Lib here *)
      [], ()
end

type storage = unit

let%entry default manager s =
  Lib.sender_check (Current.source ());            (* this line has no failure *)
  let op, _ = Contract.create ~storage:() ~delegate:(Some manager) ~amount:0tz
  (contract struct
    type storage = unit
    let%entry default () s =
      Lib.sender_check (Current.source ());       (* Cannot access Lib here *)
      [], ()
  end) in
  [op], ()
