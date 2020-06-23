

type t = tez
type storage = (t * tez)

let%entry default
      (_parameter : timestamp)
      storage =

      let p = get storage 0 in
      let n = get storage 1 in
      let _cost = 1.00tz + p + n in
      ( [], storage )
