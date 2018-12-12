contract C1 = struct

  contract C2 = struct

    contract C3 = struct

        type storage = unit
        let%entry main (x:int) _ = ([] : operation list), ()

    end
    contract type C = sig

        type storage = unit
        val%entry main : int -> _

    end

  end
  
end

contract type C = sig
  type storage = unit
  val%entry main : nat -> _
end

type storage = C1.C2.C.instance * C.instance

let%entry main (_k : key_hash) storage =
  let x = 1 + 1 in
  let op = storage.(0).main x ~amount:0tz in
  [op], storage
