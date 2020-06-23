[%%version 2.0]

contract type TOKEN = sig

  type accounts = (address, nat) big_map

  type metadata = {
    version : nat * nat;
    decimals : nat;
    name : string;
    symbol : string;
  }

  type internal_storage

  type storage = {
    accounts : accounts;
    totalSupply : nat;
    metadata : metadata;
    internal : internal_storage;
  }

  val%entry transfer : address * nat * bytes option
  val%entry balanceOf : address * [%handle: address * nat]
  val%entry totalSupply : [%handle: nat]
  val%entry burn : nat
end
