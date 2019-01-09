type storage = unit

let%entry main () _ =
  let remaining_gas = Current.gas () in
  if remaining_gas < 1000p then
    Current.failwith ("Not enough gas", remaining_gas);
  [], ()
