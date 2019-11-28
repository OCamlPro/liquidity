type storage = unit

type t = { x : int; y : nat }

(* type of bool_to_int is inferred to: bool -> int *)
let bool_to_int c =
  if c then 1 else 0

(* type of pos is inferred to: int -> bool *)
let pos i = i > 0

let%entry main param  _ =

  (* type of l is inferred to: (dun, int) variant *)
  let l = Left 1DUN in
  begin match l with
    | Right x -> if x > 0 then failwith ()
    | Left _ -> ()
  end;

  (* type of param is inferred to: t *)
  if not (pos param.x) then failwith ();

  let v_packed = Bytes.pack (0DUN, 6, 88) in
  let v_unpacked = Bytes.unpack v_packed in
  begin match v_unpacked with
    | None -> failwith ()
    | Some (x, y, z) ->
      if x <> 0DUN || (z <> 0 && y = z)  then failwith ()
  end;
  (* type of v_unpacked is infered to: (dun * int * int) *)

  (* type of [] is inferred to: operation list *)
  ([], ())
