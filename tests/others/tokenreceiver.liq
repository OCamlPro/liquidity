type storage = address

let%init storage = KT1SbCLVnkPAZHZysSnYTYrtVoJZ8anP54iB (* token manager *)

contract type TOKEN = sig
  val%entry transfer : address * nat * bytes option
end

let%entry receiveTokens ((_from : address), amount, (data : bytes option)) token_manager =
  match data with
  | Some _ -> failwith "I don't accept data"
  | None ->
    match [%handle TOKEN.transfer] (Current.sender ()) with
    | None -> failwith "Receiving tokens from a non token contract"
    | Some token_contract ->
      (* forward tokens to manager *)
      [ token_contract.transfer (token_manager, amount, None) ~amount:0DUN ],
      token_manager
