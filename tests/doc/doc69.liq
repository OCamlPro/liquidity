contract C = struct

  type storage = int

  let succ (x : int) = x + 1 [@@inline]

  let%init storage = 0

  let%entry main (u : unit) storage =
    ([] : operation list), succ storage

end

type storage = C.storage

let%entry main (x : int) s =
  ([] : operation list), x + s
