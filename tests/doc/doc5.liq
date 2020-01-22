type storage = address

let%entry default () owner =
  let addr = Current.source () in
  if addr <> owner then
    Current.failwith ("Not allowed");
  [], owner
