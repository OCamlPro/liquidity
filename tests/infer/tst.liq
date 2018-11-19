
[%%version 0.4]

type storage = int

(* type r = { x : int; y : int }
 * type s = { x2 : int; z : int } *)

(*type 'a e = C1 | C2 of 'a*)
(*type ei = int e*)
(*type e = C1 | C2 of 'a*)

(* type e1 = C1 | C2 | C3 | C4
 * type e2 = C5 | C6 *)

let%entry main (parameter : UnitContract.instance) storage =
(*
  let f x y = x + y in

  let g = f in

  let z = f 1 2 in ();

  let w = f 3 4 in ();
*)
(*
  let f x =
    let t = x.(3) in
    let u = x.(4) in
    42
  in

  let z = f (1,2,3,4,5,6) in ();
*)

  let f x = let t = x.(3) in 42 in

  let g x = let t = x.(4) in 42  in

  let h x =
    let z = f x in ();
    let w = g x in ()
  in
  
  h (1,2,3,4,5,6);
  
  
  (* let x = C2 in
   * let b = match x with
   *   | _ -> false in *)
(*
let f x =
    match x with
    (* | C1 -> true *)
    | _ -> false
  in
  let z = f C1 in
*)
(* let f x = Contract.call ~dest:x ~amount:1tz () in
 * let t = f parameter in *)

  
(*let f x = Current.gas x in
let t = f () in
*)

(*
let f t1 t2 = t1 / t2 in

let t1 = 10tz in
let t2 = 5tz in

let r = f t1 t2 in
*)

(*
  let f r = r.x in
  let z = f { x = 4; y = 4 } in
*)
(*let f l =
    3 :: l
  in
  let l2 = f [1;2] in
  let f3 x l =
    x :: l
  in
  let t = f3 1 [2] in*)

(*
  let f x = Some x in
  let t = f 4 in
  let x = () in
*)

(*  let x = { x = 3; y = 4 } in
  let z = x.(2) in*)
(*  let f x =
    x.(0) + x.(1)
  in 
  let z = f (2,3) in
  let f2 x =
    x.(0) <- 1
  in
  let z2 = f2 (2,3) in*)
(*  let x = parameter + 10 in
  let f = fun ( arg : int * int ) ->
    arg.(1) + x
  in*)
(*  let g1 a1 a2 =
    a1 - a2
  in
  let g2 = g1 1p 2 in*)
(*  let g3 arg =
    arg + 1tz
  in*)
(*  let g4 arg =
    let x = arg.(0) + arg.(1) in
    x + 1tz
  in*)
(*  let g5 x = x + 1 in*)
(*  let f ( arg : 'b * 'b ) =
    arg.(1) + x
  in*)
(*  let storage = (parameter, storage) |> f in*)
  [], storage
