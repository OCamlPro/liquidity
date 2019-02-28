module C1 = struct

  module C2 = struct

    type t = { x : tez; y : bool }

    contract C3 = struct

        type storage = unit
        let%entry main (_x:int) _ = ([] : operation list), ()

    end
    contract type C = sig

        type storage = unit
        val%entry main : t -> _

    end

  end
  
end

contract type C = sig

  type t = { x : timestamp; y : address }
  type storage = unit
  val%entry main : t -> _
end

type storage = C1.C2.C.instance * C.instance

let%entry main (_k : key_hash) storage =
  let x = { C1.C2.x = 1tz ; C1.C2.y = true } in
  let op = storage.(0).main x ~amount:0tz in
  [op], storage
