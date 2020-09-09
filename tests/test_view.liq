contract type C0 = sig

  type storage
  val%entry default : unit
  val%view get_i : unit -> int
end

type storage = (int, bool) map

let%entry default (c : [%view (get_i : bool -> int)]) s =
  if Contract.view c get_i false > 0 then failwith ();
  [], s

let%entry default2 (c : address) s =
  match [%view C0.get_i] c with
  | None -> failwith ()
  | Some c ->
    if Contract.view c get_i () > 0 then failwith ();
    [], s

let%entry default3 (c : address) s =
  if Contract.view c get_i false > 0 then failwith ();
  [], s

let%entry default4 (c : address) s =
  match C0.at c with
  | None -> failwith ()
  | Some c ->
    if Contract.view c get_i () > 0 then failwith ();
    [], s

let%view is_ok i storage : bool =
  match Map.find i storage with
  | Some b -> b
  | None -> false
