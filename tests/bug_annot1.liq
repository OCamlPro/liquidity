module C1 = struct

  module C2 = struct

    type t = { x : tez; y : bool }

    contract C3 = struct

        type storage = unit
        let%entry default (_x:int) _ = ([] : operation list), ()

    end
    contract C = struct

        type storage = unit
        let%entry default (_x:t) _ = ([] : operation list), ()
        (* val%entry default : t *)

        let mc = 1tz
    end

    let my = 2tz

  end
  
end

contract type C = sig

  type t = { x : timestamp; y : address }
  type storage = unit
  val%entry default : t
end

module Y = C1.C2
module R = Y
contract X = R.C

type storage = address * address

let%entry default (_k : key_hash) storage =
  let x = { Y.x = 1tz + Y.C.mc + Y.my ; R.y = true } in
  let op = storage.(0).default x ~amount:0tz in
  [op], storage
