type storage = unit
let%entry default addr () =
  match [%handle: val%entry default : unit] addr with
  | None -> failwith ()
  | Some c ->
    if Current.balance () < Contract.get_balance c then
      failwith "balance too big";
    [], ()
