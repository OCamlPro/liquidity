module M = struct
  type t = int
  let f (x : int) = x + 1
end

contract C = struct
  type storage = M.t

  let%init storage = 0

  let succ x = M.f x [@@inline]
  let[@private] prev x = x + 1 [@@inline]

  let%entry default () storage =
    [], prev (succ storage)
end

module M2 = struct
  type t = bool

  module MI = struct
    type r = t
    let m_and (x, y) : bool = x && y
  end
end

module MI_alias = M2.MI
contract C_alias = C

type storage = M.t

let%entry default (c : address) s =
  [c.main () ~amount:0DUN], C.succ (M.f (2 * s))
