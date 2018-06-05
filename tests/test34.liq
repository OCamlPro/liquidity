[%%version 0.2]

let%entry main
    (parameter : bool)
    (storage : bool) =

  ([] : operation list),
  (parameter <> storage)
  &&
  (parameter = storage)