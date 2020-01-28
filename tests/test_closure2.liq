

type storage = int

let%entry main
      (parameter : bool)
      storage =
  let x = if parameter  then 10 else 0 in
  let[@noinline] f (arg, _) (y:int) =
    if arg then x + y else y - x
  in
  let p = f (parameter, storage) in
  let storage = f (parameter, storage) 199 in
  let storage = p (199 + storage) in
  ( [], storage )
