[%%version 0.3]

type storage = bool

let%entry main
    (parameter : bool)
    (storage : bool) =

  ([] : operation list),
  (parameter <> storage)
  &&
  (parameter = storage)
