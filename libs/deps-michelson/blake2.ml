(* Code from BLAKE/BLAKE *)
external blake2b : (* out *)string -> (* in *)string -> (* key *)string -> int = "blake2b_ml"


let blake2b ?(key = "") input =
  let hash = String.make 64 '\000' in
  let n = blake2b hash input key in
  assert (n = 0);
  hash


external blake2b_size_of_context : unit -> int = "blake2b_size_of_context_ml"

type blake2b_ctx
external blake2b_init : string -> int -> blake2b_ctx = "blake2b_init_ml"
external blake2b_init_key : string -> int -> string -> blake2b_ctx = "blake2b_init_key_ml"
external blake2b_update : blake2b_ctx -> string -> unit = "blake2b_update_ml"
external blake2b_final : blake2b_ctx -> string -> unit = "blake2b_final_ml"


let blake2b_ctx_size = blake2b_size_of_context ()

let blake2b_init ?(size=64) () =
  let ctx = String.make blake2b_ctx_size '\000' in
  blake2b_init ctx size, String.make size '\000'

let blake2b_update (ctx,_) input = blake2b_update ctx input

let blake2b_final (ctx, hash) =
  blake2b_final ctx hash;
  hash

let blake2b input =
  let ctx = blake2b_init () in
  blake2b_update ctx input;
  blake2b_final ctx


(* Same interface as Tezos' Blake2 *)
module Blake2b : sig
  type t
  type hash = Hash of Cstruct.t

  val init : int -> t

  val update : t -> Cstruct.t -> unit

  val final : t -> hash

  val direct : Cstruct.t -> int -> hash
end = struct

  type t = blake2b_ctx * string
  type hash = Hash of Cstruct.t

  let init size = blake2b_init ~size ()

  let update t input = blake2b_update t (Cstruct.to_string input)

  let final t = Hash (Cstruct.of_string (blake2b_final t))

  let direct input size =
  let t = init size in
  update t input;
  final t

end
