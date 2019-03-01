module C1 = struct

  module C2 = struct

    type t = { x : tez; y : bool }

    contract C3 = struct

        type storage = unit
        let%entry main (_x:int) _ = ([] : operation list), ()

    end
    contract C = struct

        type storage = unit
        let%entry main (_x:t) _ = ([] : operation list), ()
        (* val%entry main : t -> _ *)

        let mc = 1tz
    end

    let my = 2tz

  end
  
end

contract type C = sig

  type t = { x : timestamp; y : address }
  type storage = unit
  val%entry main : t -> _
end

module Y = C1.C2
contract X = Y.C

type storage = X.instance * C.instance

let%entry main (_k : key_hash) storage =
  let x = { Y.x = 1tz + Y.C.mc + Y.my ; Y.y = true } in
  let op = storage.(0).main x ~amount:0tz in
  [op], storage
