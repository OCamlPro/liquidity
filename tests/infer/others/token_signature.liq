[%%version 0.402]

contract type NatContract = sig
  type storage
  val%entry main : nat -> _
end

contract type NatNatContract = sig
  type storage
  val%entry main : nat * nat -> _
end


contract type Token = sig

  type account = {
    balance : nat;
    allowances : (address, nat) map;
  }

  type storage = {
    accounts : (address, account) big_map;
    version : nat; (* version of token standard *)
    totalSupply : nat;
    decimals : nat;
    name : string;
    symbol : string;
    owner : address;
  }

  val%entry transfer : (address * nat) -> _
  val%entry approve : (address * nat) -> _
  val%entry transferFrom : (address * address * nat) -> _
  val%entry balanceOf : (address * NatContract.instance) -> _
  val%entry allowance : (address * address * NatNatContract.instance) -> _
  val%entry createAccount : (address * nat) -> _
  val%entry createAccounts : (address * nat) list -> _

end
