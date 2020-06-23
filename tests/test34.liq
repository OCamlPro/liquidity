
type storage = bool

let%entry default
    (parameter : bool)
    storage =

  [],
  (parameter <> storage)
  &&
  (parameter = storage)
