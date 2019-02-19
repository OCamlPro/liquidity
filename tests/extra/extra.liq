external get_balance : [%stack: UnitContract.instance] -> [%stack: tez] = "GET_BALANCE"
external block_level : unit -> [%stack: nat] = "BLOCK_LEVEL"
external is_implicit : [%stack: UnitContract.instance] -> [%stack: key_hash option] = "IS_IMPLICIT"
external get_storage : [%type: 'a] -> [%stack: UnitContract.instance] -> [%stack: 'a option] = "GET_STORAGE"
external reveal : [%stack: address] -> [%stack: key] -> [%stack: operation] = "REVEAL"
external increase_balance : [%stack: tez] -> unit = "INCREASE_BALANCE" [@@effect]
external test_fail : [%stack: 'a] -> unit = "FAILWITH" [@@effect]
