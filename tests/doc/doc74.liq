type storage = key_hash
let%entry default () _ =
  match [%handle: val%entry default : unit] (Current.sender ()) with
  | None -> failwith "can only be called by implicit contract"
  | Some c ->
    match Contract.is_implicit c with
    | None -> failwith "can only be called by implicit contract"
    | Some kh -> [], kh
