[%%version 0.2]

type bm = (string, int) big_map
type bm1 = bm
type r = int option
type storage = {
  bm : bm1;
  r : r;
}

let%init storage : storage = { bm = BigMap []; r = None }

let%entry main
      (parameter : string)
      (storage : storage) =
  let storage =
    storage.bm <- match Map.find parameter storage.bm with
    | None ->
      Map.add parameter 0 storage.bm
    | Some x ->
      if x < 0 then
        Map.remove parameter storage.bm
      else if x < 10 then
        Map.add parameter (x + 1) storage.bm
      else if x > 100 then
        Map.add parameter (-1) storage.bm
      else
        Map.add parameter (2 * x) storage.bm
  in
  let r = Map.find parameter storage.bm in
  let storage = storage.r <- r in
  (([] : operation list), storage)
