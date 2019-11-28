type storage = key_hash
let%entry main () _ =
  match UnitContract.at (Current.sender ()) with
  | None -> failwith "can only be called by implicit contract"
  | Some c ->
    match Contract.is_implicit c with
    | None -> failwith "can only be called by implicit contract"
    | Some kh -> [], kh
