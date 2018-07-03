(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2017       .                                          *)
(*    Fabrice Le Fessant, OCamlPro SAS <fabrice@lefessant.net>            *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* This module is completely unsafe: it can only by used to execute a
file that has been correctly typechecked by the `liquidity`
typechecker.  *)

type failure = Failure : 'a -> failure
exception Fail of failure

type integer =
  Int of Z.t
| Tez of Z.t
| Timestamp of Z.t
type timestamp = integer
type tez = integer
type nat = integer
type bytes = string

type key = Key of string
type key_hash = Key_hash of string
type signature = Signature of string
type 'a contract = Contract of string
type address = Address of string
type operation

module Signature : sig
  val of_string : string -> signature
end = struct
  let of_string s = Signature s
end

module Key : sig
  val of_string : string -> key
  end = struct
  let of_string s = Key s
end

module Key_hash : sig
  val of_string : string -> key_hash
  end = struct
  let of_string s = Key_hash s
end

module Address : sig
  val of_string : string -> address
  end = struct
  let of_string s = Address s
end

module Tez : sig

  val of_string : string -> tez

  end = struct

  let of_string s =
    let (tezzies, centiles) =
      try
        let pos = String.index s '.' in
        let tezzies = String.sub s 0 pos in
        let len = String.length s in
        let centiles = "0" ^ String.sub s (pos+1) (len-pos-1) in
        Z.of_string tezzies, Z.of_string centiles
      with Not_found ->
        Z.of_string s, Z.of_int 0
    in
    Tez (Z.add (Z.mul (Z.of_int 100) tezzies) centiles)

end

module Int : sig

  val of_string : string -> integer

end = struct

  let of_string n = Int (Z.of_string n)

end

module Timestamp : sig
  val of_string : string -> timestamp
end = struct
  let of_string time = assert false
end

module Current : sig

  val amount : unit -> tez
  val fail : unit -> 'a
  val failwith : 'a -> 'b
  val time : unit -> timestamp
  val balance : unit -> tez
  val gas : unit -> tez (* NOT TESTED *)
  val contract : unit -> 'a contract (* unsafe, NOT IMPLEMENTED !! *)
  val source : unit -> address (* NOT TESTED *)

end = struct

  let amount () = Tez (Z.of_int 100)
  let failwith (type a) (x:a) = raise (Fail (Failure x))
  let fail () = failwith ()
  let time () = Timestamp (Z.of_float (Unix.gettimeofday ()))
  let balance () = assert false (* TODO *)
  let gas () = assert false
  let contract () = assert false
  let source () = assert false
end


let z_of_int = function
    Tez n -> n
  | Int n -> n
  | Timestamp n -> n

module Array : sig
  val get : 'a -> integer -> 'b
  val set : 'a -> integer -> 'b -> 'a

end = struct (* Arrays are for tuples, not typable in OCaml *)

  let get t n =
    let n = z_of_int n in
    let n = Z.to_int n in
    Obj.magic (Obj.field (Obj.magic  t) n)

  let set t n x =
    let n = z_of_int n in
    let n = Z.to_int n in
    let t = Obj.repr t in
    let t = Obj.dup t in
    Obj.set_field t n (Obj.repr x);
    Obj.magic t

end

module Map : sig

  type ('key, 'value) map

  val empty : unit -> ('key,'value) map
  val make : ('key * 'value) list -> ('key, 'value) map
  val reduce : ( ('key * 'value) * 'acc -> 'acc) ->
               ('key,'value) map -> 'acc -> 'acc
  val fold : ( ('key * 'value) * 'acc -> 'acc) ->
               ('key,'value) map -> 'acc -> 'acc
  val iter : ( ('key * 'value) -> unit) -> ('key,'value) map -> unit

  val map : ( 'key * 'value -> 'res) ->
               ('key,'value) map ->
               ('key,'res) map

  val find : 'key -> ('key, 'value) map -> 'value option

  val update : 'key -> 'value option  -> ('key, 'value) map ->
               ('key, 'value) map

  val add : 'key -> 'value  -> ('key, 'value) map -> ('key, 'value) map
  val remove : 'key -> ('key, 'value) map -> ('key, 'value) map

  val mem : 'key -> ('key, 'value) map -> bool (* NOT TESTED *)
  val size : ('key, 'value) map -> nat

  val map_fold :
    ( ('key * 'value) * 'acc -> 'res * 'acc) ->
    ('key,'value) map -> 'acc ->
    ('key,'res) map * 'acc

end = struct

  module ObjMap = Map.Make(struct
                            type t = Obj.t
                            let compare = compare
                            end)

  type ('key, 'value) map
  let empty _ = Obj.magic ObjMap.empty
  let make list =
    let map =
      List.fold_left (fun map (key,value) ->
          let key = Obj.repr key in
          let value = Obj.repr value in
          ObjMap.add key value map
        ) (empty 0) list
    in
    Obj.magic map

  let reduce f map acc =
    let f = (Obj.magic f : (Obj.t * 'value) * Obj.t -> Obj.t) in
    let acc = Obj.repr acc in
    let map = (Obj.magic map : 'value ObjMap.t) in
    let (acc : Obj.t) = ObjMap.fold (fun key value acc ->
                            f ( (key,value), acc )
                          ) map acc
    in
    Obj.magic acc

  let fold f map acc = reduce f map acc

  let iter f map =
    let f = (Obj.magic f : (Obj.t * 'value) -> unit) in
    let map = (Obj.magic map : 'value ObjMap.t) in
    ObjMap.iter (fun key value ->
        f ( (key,value) )
      ) map

  let map f map =
    let f = (Obj.magic f : Obj.t * 'value -> 'value) in
    let map = (Obj.magic map : 'value ObjMap.t) in
    let map = ObjMap.map (fun key value -> f (key,value)) map in
    Obj.magic map

  let find key map =
    try
      let key = Obj.repr key in
      let map = (Obj.magic map : 'value ObjMap.t) in
      Some (ObjMap.find key map)
    with Not_found -> None

  let update (key: 'key) (value: 'value option) (map: ('key, 'value) map) =
    let key = Obj.repr key in
    let map = (Obj.magic map : 'value ObjMap.t) in
    let map = match value with
      | Some value ->
        ObjMap.add key value map
      | None ->
        ObjMap.remove key map
    in
    Obj.magic map

  let add (key: 'key) (value: 'value) (map: ('key, 'value) map) =
    let key = Obj.repr key in
    let map = (Obj.magic map : 'value ObjMap.t) in
    let map = ObjMap.add key value map in
    Obj.magic map

  let remove (key: 'key) (map: ('key, 'value) map) =
    let key = Obj.repr key in
    let map = (Obj.magic map : 'value ObjMap.t) in
    let map = ObjMap.remove key map in
    Obj.magic map

  let mem (key: 'key) (map: ('key, 'value) map) =
    let key = Obj.repr key in
    let map = (Obj.magic map : 'value ObjMap.t) in
    ObjMap.mem key map

  let size map = Int (Z.of_int (ObjMap.cardinal (Obj.magic map)))

  let map_fold f map acc =
    fold (fun ((k, v), (map, acc)) ->
        let v', acc = f ((k, v), acc) in
        add k v' map, acc
      ) map (empty, acc)

end

module BigMap = Map

include Array (* Remove ? *)


type ('key,'value) map = ('key,'value) Map.map
type ('key,'value) big_map = ('key,'value) map

module Set : sig

  type 'key set
  val empty : unit -> 'key set
  val make : 'key list -> 'key set
  val update : 'key -> bool -> 'key set -> 'key set
  val add : 'key -> 'key set -> 'key set
  val remove : 'key -> 'key set -> 'key set
  val mem : 'key -> 'key set -> bool
  val reduce : ( 'key * 'acc -> 'acc) -> 'key set -> 'acc -> 'acc
  val fold : ( 'key * 'acc -> 'acc) -> 'key set -> 'acc -> 'acc
  val iter : ( 'key -> unit) -> 'key set -> unit
  val map : ('key -> 'res) -> 'key set -> 'res set
  val size : 'key set -> nat
  val map_fold :
    ( 'key * 'acc -> 'res * 'acc) ->
    'key set -> 'acc ->
    'res set * 'acc

end = struct

  module ObjSet = Set.Make(struct
                            type t = Obj.t
                            let compare = compare
                            end)

  type 'key set

  let empty _ = Obj.magic ObjSet.empty
  let make list =
    let set =
      List.fold_left (fun set key ->
          let key = Obj.repr key in
          ObjSet.add key set
        ) (empty 0) list
    in
    Obj.magic set
  let update key bool set =
    let key = Obj.repr key in
    let set = (Obj.magic set : ObjSet.t) in
    let set =
      if bool then
        ObjSet.add key set
      else
        ObjSet.remove key set
    in
    Obj.magic set
  let add key set =
    let key = Obj.repr key in
    let set = (Obj.magic set : ObjSet.t) in
    let set = ObjSet.add key set in
    Obj.magic set
  let remove key set =
    let key = Obj.repr key in
    let set = (Obj.magic set : ObjSet.t) in
    let set = ObjSet.remove key set in
    Obj.magic set
  let mem key set =
    let key = Obj.repr key in
    let set = (Obj.magic set : ObjSet.t) in
    ObjSet.mem key set

  let reduce f set acc =
    let f = (Obj.magic f : Obj.t * Obj.t -> Obj.t) in
    let acc = Obj.repr acc in
    let set = (Obj.magic set : ObjSet.t) in
    let (acc : Obj.t) = ObjSet.fold (fun key acc ->
                            f (key, acc )
                          ) set acc
    in
    Obj.magic acc

  let fold f set acc = reduce f set acc

  let map f set =  (* TODO, NOT TESTED *)
    let f = (Obj.magic f : Obj.t -> Obj.t) in
    let set = (Obj.magic set : ObjSet.t) in
    let set = ObjSet.map (fun x -> f x) set in
    Obj.magic set

  let iter f set =  (* TODO, NOT TESTED *)
    let f = (Obj.magic f : Obj.t -> unit) in
    let set = (Obj.magic set : ObjSet.t) in
    ObjSet.iter (fun x -> f x) set

  let size set = Int (Z.of_int (ObjSet.cardinal (Obj.magic set)))

  let map_fold f set acc =
    fold (fun (v, (set, acc)) ->
        let v', acc = f (v, acc) in
        add v' set, acc
      ) set (empty, acc)

end

type 'key set = 'key Set.set

module Arith : sig

  val (+) : integer -> integer -> integer
  val (-) : integer -> integer -> integer
  val ( * ) : integer -> integer -> integer
  val ( / ) : integer -> integer -> (integer * integer) option
  val (~-) : integer -> integer
  val lnot : integer -> integer
  val (land) : integer -> integer -> integer
  val (lor) : integer -> integer -> integer
  val (lxor) : integer -> integer -> integer
  val (lsl) : integer -> integer -> integer
  val (lsr) : integer -> integer -> integer
  val xor : bool -> bool -> bool

  val int : integer -> integer
  val abs : integer -> integer

end = struct

  let (+) = Z.add
  let (+) x y =
    match x,y with
    | Timestamp x, Int y
    | Int x, Timestamp y
      -> Timestamp (x + y)
    | Tez x, Tez y -> Tez (x+y)
    | Int x, Int y -> Int (x+y)
    | Tez _, (Int _|Timestamp _)
      | (Int _ | Timestamp _), Tez _
    | Timestamp _, Timestamp _
      -> assert false


  let (-) = Z.sub
  let (-) x y =
    match x,y with
    | Timestamp x, Timestamp y -> Int (x - y)
    | Timestamp x, Int y
      -> Timestamp (x - y)
    | Tez x, Tez y -> Tez (x-y)
    | Int x, Int y -> Int (x-y)
    | Tez _, (Int _|Timestamp _)
      | (Int _ | Timestamp _), Tez _
    | Int _, Timestamp _
      -> assert false

  let (~-) = Z.neg
  let (~-) x =
    match x with
    | Int x -> Int (- x)
    | Tez _ | Timestamp _ -> assert false

  let xor x y = (x || y) && not (x && y)

  let lnot = Z.lognot
  let lnot x =
    match x with
    | Int x -> Int (lnot x)
    | Tez _ | Timestamp _
      -> assert false

  let (land) = Z.(land)
  let (land) x y =
    match x,y with
    | Int x, Int y -> Int (x land y)
    | (Tez _ | Timestamp _| Int _), (Tez _ | Timestamp _ | Int _)
      -> assert false

  let (lor) = Z.(lor)
  let (lor) x y =
    match x,y with
    | Int x, Int y -> Int (x lor y)
    | (Tez _ | Timestamp _| Int _), (Tez _ | Timestamp _ | Int _)
      -> assert false

  let (lxor) = Z.(lxor)
  let (lxor) x y =
    match x,y with
    | Int x, Int y -> Int (x lxor y)
    | (Tez _ | Timestamp _| Int _), (Tez _ | Timestamp _ | Int _)
      -> assert false

  let (lsl) x y =
    match x,y with
    | Int x, Int y -> Int (Z.shift_left x (Z.to_int y))
    | (Tez _ | Timestamp _| Int _), (Tez _ | Timestamp _ | Int _)
      -> assert false

  let (lsr) x y =
    match x,y with
    | Int x, Int y -> Int (Z.shift_right x (Z.to_int y))
    | (Tez _ | Timestamp _| Int _), (Tez _ | Timestamp _ | Int _)
      -> assert false

  let ediv x y =
    try
      let (q, r) = Z.ediv_rem x y in
      Some (q, r)
    with _ -> None

  let (/) x y =
    try
      let (q, r) =
        let x = z_of_int x in
        let y = z_of_int y in
        Z.ediv_rem x y in
      Some (match x,y with
              Tez _, Tez _ -> Int q, Tez r
            | Tez _, Int _ -> Tez q, Tez r
            | Int _, Int _ -> Int q, Int r
            | Int _, Tez _
              | Int _, Timestamp _
              | Tez _, Timestamp _
              | Timestamp _, Tez _
              | Timestamp _, Int _
              | Timestamp _, Timestamp _
                                 -> assert false
           )
    with _ -> None

  let ( * ) = Z.mul
  let ( * ) x y =
    match x,y with
    | Tez x, Int y
    | Int x, Tez y -> Tez (x * y)
    | Int x, Int y -> Int (x * y)
      | Tez _, Tez _
        | Int _, Timestamp _
      | Tez _, Timestamp _
      | Timestamp _, Tez _
      | Timestamp _, Int _
      | Timestamp _, Timestamp _
      -> assert false

  let int x = x

  let abs = function Int x -> Int (Z.abs x)
                   | Tez _
                   | Timestamp _ -> assert false

end

let (@) = (^)

include Arith

module Lambda : sig
  val pipe : 'a -> ('a -> 'b) -> 'b
end = struct
  let pipe x f = f x
end

module Loop : sig
  val loop : ('a -> bool * 'a) -> 'a -> 'a
end = struct
  let rec loop f x =
    let (bool, ret) = f x in
    if bool then loop f ret
    else ret
end

module Contract : sig

  val of_string : string -> 'a contract

  val call : 'arg contract -> tez -> 'arg -> operation

  val manager : 'a -> 'b
  val create : key_hash -> key_hash option ->
               bool -> bool -> tez ->
              'b -> ( 'a  -> 'b -> (operation list * 'b) ) ->
               operation * 'a contract
end = struct

  let of_string s = Contract s
  let call contract amount arg = assert false (* TODO *)
  let manager _contract = assert false (* TODO *)
  let create _manager _delegate
             _delegatable _spendable _amount
             _storage _f = assert false (* TODO *)
end

type ('a,'b) variant = Left of 'a | Right of 'b

module List : sig

  val reduce : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
  val fold : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
  val iter : ('a -> unit) -> 'a list -> unit
  val map : ('a -> 'b) -> 'a list -> 'b list
  val rev : 'a list -> 'a list
  val size : 'a list -> nat
  val map_fold :
    ( 'key * 'acc -> 'res * 'acc) ->
    'key list -> 'acc ->
    'res list * 'acc

end = struct

  let rec reduce f list b =
    match list with
    | [] -> b
    | a :: list ->
       reduce f list (f (a,b))

  let fold f list b = reduce f list b

  let iter = List.iter
  let map = List.map
  let rev = List.rev
  let size list = Int (Z.of_int (List.length list))

  let map_fold f list acc =
    let list, acc =
      fold (fun (v, (list, acc)) ->
        let v', acc = f (v, acc) in
        v' :: list, acc
        ) list ([], acc) in
    rev list, acc

end

module Account : sig
  val create : key_hash -> key_hash option ->
               bool -> tez -> operation * unit contract
  val default : key_hash -> unit contract
end = struct
  let create key key_opt _spendable _amount = assert false (* TODO NOT TESTED *)
  let default _key = assert false (* TODO *)
end

module Crypto : sig
  val hash : 'a -> string
  val hash_key : key -> key_hash
  val check : key -> signature -> string -> bool
end = struct
  let hash _ = assert false (*TODO *)
  let hash_key _ = assert false (*TODO *)
  let check _key _sig _hash = assert false (* TODO *)
end

type int = integer
