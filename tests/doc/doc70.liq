contract type UnitContract = sig
  type storage
  val%entry default : unit
end

type storage = unit

let%entry default (c : address) _ =
  match [%handle UnitContract.default] c with
  | None -> failwith ()
  | Some c -> [c.default ~amount:0DUN ()], ()
