module FA12 = struct
  contract type ContractType  =
  sig
    [%%entry
      :val main : address]
  end
  contract type ContractType3  =
  sig
    [%%entry
      :val main : nat]
  end
  contract type ContractType4  =
  sig
    [%%entry
      :val main : nat]
  end
  contract type ContractType5  =
  sig
    [%%entry
      :val main : nat]
  end
  contract type Fa12 = sig
    val%entry main:
      ((address * (address * nat)),
       ((address * nat),
        (((address * address) * address),
         ((address * address),
          ((unit * address),
           (bool,
            (address,
             ((unit * address),
              ((address * nat), (address * nat)) variant) variant)
              variant)
             variant)
            variant)
           variant)
          variant)
         variant)
        variant
     
  end
end

type storage = unit
let%entry test_token (addr: address) storage =
  begin match [%handle FA12.Fa12.main] addr  with
    | None -> failwith ("Cannot recover bool contract from:", addr)
    | Some token_contract ->
        let op =
          token_contract.main (Left (addr, (addr, 2p))) ~amount:0tz
        in
        [op], storage
  end
