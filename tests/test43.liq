contract C = struct

  type storage = nat

  contract type NatContract = sig
    val%entry default : nat
  end

  let%entry c_entry1 forward storage =
    [ forward.default storage ~amount:0tz ], storage

  let%entry c_entry2 forward storage =
    [ forward.default storage ~amount:1tz ], storage

  let%entry default (_: int) storage =
    [], storage

end

type storage = unit

let%entry my_entry1 () () =
  [], ()

let%entry my_entry2 c storage =
  [ c.main 0 ~amount:0tz ], storage
