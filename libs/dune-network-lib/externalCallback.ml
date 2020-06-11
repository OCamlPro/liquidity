
type callback = Callback : (unit -> 'a) -> callback

let table = Hashtbl.create 1

let register (s : string) f = Hashtbl.add table s (Callback f)
let callback (s : string) () =
  let Callback f = Hashtbl.find table s in
  Obj.magic (f ())
