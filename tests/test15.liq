
[%%version 0.5]

type point = { x : string; y : bool; z : int option }
type vector = { orig : point; dest : point }

type storage = {
  v : vector;
  whatever : unit;
}

let%entry main
      (parameter : int)
      (storage : storage) =

(*      let storage = storage.v.dest.z <- parameter in *)
      let storage = storage.v.orig <- { x="0"; y=true; z = Some parameter } in
      ( [], storage )
