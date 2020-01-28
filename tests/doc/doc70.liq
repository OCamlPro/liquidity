contract type UnitContract = sig
  type storage
  val%entry main : unit -> storage -> operation list * storage
end

type storage = unit

let%entry main (c : UnitContract.instance) _ =
  [c.main ~amount:0DUN ()], ()
