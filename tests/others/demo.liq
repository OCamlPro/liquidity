
[%%version 0.1]

let%entry main
  (parameter : string)
  (storage : (string, int) map)
  [%return : unit] =

  let amount = Current.amount() in

  if amount < ("5.00" : tez) then
    Current.fail ()
  else
    match Map.find parameter storage with
    | None -> Current.fail ()
    | Some x ->
       let storage = Map.update parameter (Some (x+1)) storage in
       ( (), storage )

