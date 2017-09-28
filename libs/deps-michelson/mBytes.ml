type t = bytes

let create = String.create
let length = String.length
let compare = compare

include EndianString.BigEndian

let to_string s = s
let of_string s = s
let blit = String.blit
let blit_from_string = String.blit
let blit_to_bytes = String.blit
let copy = String.copy
let sub = String.sub
let substring = String.sub
let concat = (^)
