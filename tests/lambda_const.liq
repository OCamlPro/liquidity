type storage = (int -> tez) option

let%init storage =
  (* let f =  fun x -> if x > 0 then 1tz else 2tz in
   * Some (f) *)
  Some (fun x -> if x > 0 then 1tz else 2tz)

let%entry default new_f _ =
  [], Some new_f
