type storage = unit
contract type BoolContract = sig
  val%entry default : bool
end

let%entry default (addr : address) _ =
  begin match [%handle BoolContract.default] addr with
    | None -> failwith ("Cannot recover bool contract from:", addr)
    | Some _my_handle -> ()
  end;
  [], ()
