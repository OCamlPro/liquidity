type storage = unit
contract type BoolContract = sig
  type storage
  val%entry main : bool -> _
end

let%entry main (addr : address) _ =
  begin match (Contract.at addr : BoolContract.instance option) with
    | None -> failwith ("Cannot recover bool contract from:", addr)
    | Some _my_contract -> ()
  end;
  ([] : operation list), ()
