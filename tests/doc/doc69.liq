contract C = struct

  type storage = int

  let succ x = x + 1 [@@inline]

  let%init storage = 0

  let%entry default (u : unit) storage =
    [], succ storage

end

type storage = C.storage

let%entry default (x : int) s =
  [], x + s
