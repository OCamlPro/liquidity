type storage = int

type ('a, 'b) t =
  { x : 'a ; y : 'b }

type ('aa, 'bb, 'cc) u =
  | A of 'aa
  | B of 'bb
  | C of 'cc

type ('a, 'b) r = ('a, 'b) t

let mk_t (x, y) =
  { x; y }

(* let mk_somet (x, y) =
 *   { x = Some x ; y = Some y } *)

let%entry main
      parameter
      storage =

  let w = (mk_t (parameter, 99) : (bool, int) r) in
  if not w.x then failwith ();
  let v = mk_t (false, Some 0) in
  if v.x then failwith ();

  (* let ww = mk_somet (9p, 10tz) in
   * begin match ww.x with
   * | Some _ -> ()
   * | None  -> failwith () end; *)

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

  let x = A 0 in
  begin match x with
    | A a -> if a <> 0 then failwith ()
    | _ -> failwith ()
  end;

  let z = 0p in
  let x = C z in
  begin match x with
    | B a -> if a <> 0tz then failwith ()
    (* | A a -> if a then failwith () *)
    | _ -> failwith ()
  end;

  ( [], storage )
