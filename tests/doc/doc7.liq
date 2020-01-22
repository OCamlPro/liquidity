type storage = unit

let%entry default (param : string) _ =
  if String.length param > 256p then
    Current.failwith ("Parameter too long", param);
  [], ()
