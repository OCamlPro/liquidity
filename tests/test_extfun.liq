let version = 1.0

let f ( arg : unit * int ) = arg.(0)

let contract
      (parameter : int)
      (storage : unit)
      (return : unit) =
  let storage = Lambda.pipe (storage, parameter) f in
  ( (), storage )
