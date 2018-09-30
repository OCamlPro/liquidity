[%%version 0.3]

let%init storage (myname : string) =
  Map.add myname 0 (Map ["ocaml", 0; "pro", 0])

let%entry main
  (parameter : string)
  (storage : (string, int) map) =

  let amount = Current.amount() in

  if amount < 5.00tz then
    Current.failwith "Not enough money, at least 5tz to vote"
  else
    match Map.find parameter storage with
    | None -> Current.failwith "Bad vote"
    | Some x ->
       let storage = Map.add parameter (x+1) storage in
       ( ([] : operation list), storage )
