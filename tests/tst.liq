

type storage = int

(* type r = { x : int; y : bool }
 * type s = { x2 : int; z : int } *)

(*type 'a e = C1 | C2 of 'a*)
(*type ei = int e*)
(*type e = C1 | C2 of 'a*)

(* type e1 = C1 | C2 | C3 | C4
 * type e2 = C5 | C6 *)

(* let f x y = (x, y) *)


(* contract type NatContract = sig
 *   type storage
 *   val%entry default : nat
 * end
 * 
 * contract type NatNatContract = sig
 *   type storage
 *   val%entry default : nat * nat
 * end *)



let%entry default (_parameter : int) storage =

  let g x = if x < 10p then failwith "error, element two small" in
  let f l = List.iter g l in
  let _ = f [1p;2p;3p;4p] in

  (* let f g1 g2 g3 g4 x = g1 (g2 (g3 (g4 x))) in
   * let k z = z in
   * let t = f k k k k 42 in *)


  (* let f (cont) = cont.main 10p ~amount:0tz in
   * let g (cont) = cont.main (10p, 42p) ~amount:0tz in *)
  

  (* let f r = r.(-2) in
   * let r = { x = 4; y = false } in
   * let res = f r in *)
  
  (* let m = (Map [1,1;2,2] : (int, int) map) in *)
  (* let m = Map [] in
   * let m2 = Map.add 1 1 m in *)

  (* let f (x, _) = x in
   * let _ = f parameter in *)

  (* let f x = x + x >= 0 in *)

  (* let _ = f 4 in *)
  (* let _ = f "toto" in *)


  (* let f a b = a :: b in *)
(*
  let z = 42 in

  let fp x y = (f x, f y) in

  let i = (fp 0 "toto").(0) 4 in
  let j = (fp 1 "titi").(0) "toto" in
  let k = (fp 2 "tutu").(1) 5 in
  let l = (fp 3 "tata").(1) "titi" in
  let _ = z in
*)

  
(*
  let f x y = (x, y) in

  let fp x y = (f x, f y) in
  
  let i = (fp 0 "toto").(0) 4 in
  let j = (fp 1 "titi").(0) "toto" in
  let k = (fp 2 "tutu").(1) 5 in
  let l = (fp 3 "tata").(1) "titi" in

=>

  let f_int_int x y = (x, y) in
  let f_int_string x y = (x, y) in
  let f_int_X x y = (x, y) in
  let f_string_int x y = (x, y) in
  let f_string_string x y = (x, y) in
  let f_string_X x y = (x, y) in

  let fp_int_string_int_X    x y = (f_int_int    x, f_string_X      y) in
  let fp_int_string_string_X x y = (f_int_string x, f_string_X      y) in
  let fp_int_string_X_int    x y = (f_int_X      x, f_string_int    y) in
  let fp_int_string_X_string x y = (f_int_X      x, f_string_string y) in

  let i = (fp_int_string_int_X    0 "toto").(0) 4 in
  let j = (fp_int_string_string_X 1 "titi").(0) "toto" in
  let k = (fp_int_string_X_int    2 "tutu").(1) 5 in
  let l = (fp_int_string_X_string 3 "tata").(1) "titi" in


  
  let f2 z = ((fun x -> (x, z)), (fun x -> (z, x))) in
  (*  'a -> ('b -> 'b * 'a) * ('c -> 'a * 'c) *)

  let x = (f2 42).(0) 43 in
  let y = (f2 "toto").(1) "titi" in

=>
  let f2_int_int_X (z:int) =
    ((fun (x:int) -> (x, z)), (fun (x:X) -> (z, x))) in
  let f2_string_X_string (z:string) =
    ((fun (x:X) -> (x, z)), (fun (x:string) -> (z, x))) in

  let x = (f2_int_int_X 42).(0) 43 in
  let y = (f2_string_X_string "toto").(1) "titi" in
*)


  
  
(*

let f x y z = e1 in e2






*)

  
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
(*
  let f x = let t = x.(3) in 42 in

  let g x = let t = x.(4) in 42  in

  let h x =
    let z = f x in ();
    let w = g x in ()
  in
  
  h (1,2,3,4,5,6);
 *) 
  
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
