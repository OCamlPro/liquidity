type storage = unit
let%entry main _ _ =
  let list = List.rev [7; 5; 10] in
  (* list = [10; 5; 7] *)
  begin match list with
    | x :: _ -> if x <> 10 then failwith ()
    | _ -> ()
  end;
  ([] : operation list), ()
