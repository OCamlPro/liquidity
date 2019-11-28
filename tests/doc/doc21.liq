type storage = unit

let%entry main () _ =
  let me = Contract.self () in
  let op = me.other 10 ~amount:0DUN in
  [op], ()

let%entry other (x : int) _ =
  if x < 0 then failwith ();
  [], ()
