
type storage = int

type t = (* TODO strange *)
  { x : 'a ; y : 'b }

let mk_t (x, y) =
  { x; y }

let%entry main
      parameter
      storage =

  let w = mk_t (parameter, 99) in
  if not w.x then failwith ();
  let v = mk_t (false, Some 0) in
  if v.x then failwith ();

  let a = Left 3 in
  let b = Right a in

  let storage = match b with
  | Left x -> x
  | Right r ->
     match r with
     | Left x -> x
     | Right _ -> storage
  in

  let c = Left storage in
  (match c with
   | Left _ -> ()
   | Right _ -> failwith ());

  ( [], storage )
