
type storage = bool

let%entry main
    (parameter : bool)
    storage =

  [],
  (parameter <> storage)
  &&
  (parameter = storage)
