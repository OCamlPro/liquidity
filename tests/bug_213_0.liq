module FA12 = struct
  contract type ContractType  =
  sig
    type storage
    [%%entry
      :val main : parameter:address -> storage -> (operation list * storage)]
  end
  contract type ContractType3  =
  sig
    type storage
    [%%entry
      :val main : parameter:nat -> storage -> (operation list * storage)]
  end
  contract type ContractType4  =
  sig
    type storage
    [%%entry
      :val main : parameter:nat -> storage -> (operation list * storage)]
  end
  contract type ContractType5  =
  sig
    type storage
    [%%entry
      :val main : parameter:nat -> storage -> (operation list * storage)]
  end
  contract type Fa12 = sig
    val%entry main:
      ((address * (address * nat)),
       ((address * nat),
        (((address * address) * ContractType5.instance),
         ((address * ContractType4.instance),
          ((unit * ContractType3.instance),
           (bool,
            (address,
             ((unit * ContractType.instance),
              ((address * nat), (address * nat)) variant) variant)
              variant)
             variant)
            variant)
           variant)
          variant)
         variant)
        variant
      -> _
  end
end

type storage = unit
let%entry test_token (addr: address) storage =
  begin match (Contract.at addr : FA12.Fa12.instance option) with
    | None -> failwith ("Cannot recover contract from:", addr)
    | Some token_contract ->
        let op =
          Contract.call ~dest:token_contract (Left (addr, (addr, 2p))) ~amount:0tz
        in
        [op], storage
  end
