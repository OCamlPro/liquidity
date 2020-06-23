type storage = {
  big : (int, string) big_map;
  nothing : unit
}

let%entry default (param : int) storage =
  if not (Map.mem param storage.big) then
    failwith ("param is not in the map", param);
  [], storage
