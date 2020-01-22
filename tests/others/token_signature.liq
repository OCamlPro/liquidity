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

  val%entry transfer : (address * nat)
  val%entry approve : (address * nat)
  val%entry transferFrom : (address * address * nat)
  val%entry balanceOf : (address * address)
  val%entry allowance : (address * address * address)

end
