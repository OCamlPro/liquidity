
#include "../../dune-network/src/proto_005_PsBabyM1/lib_protocol/raw_context.ml"

module M = struct

  type key = string list
  type value = MBytes.t

  module Internal = Map.Make(struct type t = key
                                    let compare = Pervasives.compare end)

  type t = value Internal.t

  let mem m k = Lwt.return @@ Internal.mem k m
  let dir_mem m k = Lwt.return @@ false
  let get m k = Lwt.return @@ Internal.find_opt k m
  let set m k v = Lwt.return @@ Internal.add k v m
  let copy m ~from ~to_ =
    Lwt.return @@ match Internal.find_opt from m with
                  | None -> None
                  | Some v -> Some (Internal.add to_ v m)
  let del m k = Lwt.return @@ Internal.remove k m
  let remove_rec m k = Lwt.return @@ Internal.remove k m
  let fold m _k ~init ~f:_ = Lwt.return @@ init

  let set_protocol m _k = Lwt.return @@ m
  let fork_test_chain m ~protocol:_ ~expiration:_ = Lwt.return @@ m

end

type _ Context.kind += Simple : M.t Context.kind

let initialize constants op_hash index =
  let constants = Data_encoding.Binary.to_bytes_exn
                    Constants_repr.parametric_encoding constants in
  let first_level = Data_encoding.Binary.to_bytes_exn
                      first_level_encoding Raw_level_repr.root in
  let ctxt = M.Internal.empty in
  let ctxt = M.Internal.add version_key (MBytes.of_string version_value) ctxt in
  let ctxt = M.Internal.add first_level_key first_level ctxt in
  let ctxt = M.Internal.add protocol_param_key (MBytes.of_string "{}") ctxt in
  let ctxt = M.Internal.add constants_key constants ctxt in

  let ops =
    (module M : Tezos_protocol_environment.CONTEXT with type t = 'ctxt) in

  let context = Context.Context { ops; ctxt; kind = Simple } in

  prepare (Int32.of_int 0) (Time.epoch) (Time.epoch) [] context
  >>|? fun ctxt ->

  let nonce = Contract_repr.initial_origination_nonce op_hash in
  let nonce = Some (Contract_repr.set_origination_nonce_index nonce index) in
  { ctxt with origination_nonce = nonce }
