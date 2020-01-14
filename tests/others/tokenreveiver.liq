type storage = address

let%init storage = KT1SbCLVnkPAZHZysSnYTYrtVoJZ8anP54iB (* token manager *)

contract type TOKEN = sig

  type storage

  contract type BalanceForwarder = sig
    type storage
    val%entry main : nat -> _
  end

  val%entry transfer : address * nat * bytes option -> _

  val%entry balanceOf : address * BalanceForwarder.instance -> _

  val%entry burn : nat -> _

  val%entry mint : (address * nat) list -> _
end

let%entry receiveTokens ((_from : address), amount, (data : bytes option)) token_manager =
  match data with
  | Some _ -> failwith "I don't accept data"
  | None ->
    match TOKEN.at (Current.sender ()) with
    | None -> failwith "Receiving tokens from a non token contract"
    | Some token_contract ->
      (* forward tokens to manager *)
      [ token_contract.transfer (token_manager, amount, None) ~amount:0DUN ],
      token_manager
