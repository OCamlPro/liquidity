
let contract
      (amount : tez)
      (parameter : int)
      (storage : nat)
      (return : int) =

  match int storage / parameter with
  | None -> ( 0, 0p)
  | Some res ->
      ( res.(0), res.(1) )

































