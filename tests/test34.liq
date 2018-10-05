[%%version 0.4]

type storage = bool

let%entry main
    (parameter : bool)
    (storage : bool) =

  ([] : operation list),
  (parameter <> storage)
  &&
  (parameter = storage)
