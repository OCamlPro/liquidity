
type storage = (string, int) map

let%init initial_votes (myname : string) =
  Map.add myname 0 (Map ["ocaml", 0; "pro", 0])

let%entry default (choice : string) votes =

  let amount = Current.amount() in

  if amount < 5.00tz then
    Current.failwith "Not enough money, at least 5tz to vote"
  else
    match Map.find choice votes with
    | None -> Current.failwith "Bad vote"
    | Some x ->
       let votes = Map.add choice (x+1) votes in
       ( [], votes )
