[%%version 0.13]

let%init storage (myname : string) =
  Map.add myname 0 (Map ["ocaml", 0; "pro", 0])

let%entry main
  (parameter : string)
  (storage : (string, int) map)
  : unit * (string, int) map =

  let amount = Current.amount() in

  if amount < 5.00tz then
    Current.fail ()
  else
    match Map.find parameter storage with
    | None -> Current.fail ()
    | Some x ->
       let storage = Map.add parameter (x+1) storage in
       ( (), storage )

