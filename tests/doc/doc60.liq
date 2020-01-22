type storage = (address, string) map
let%entry default () owners_map =
  let sender = Current.sender () in
  if not ( Map.mem sender owners_map ) then
    failwith ("not allowed", sender);
  [], owners_map
