open Dune_Network_Lib (* for crypto *)

let dummy_sign = "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQ\
                  rUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q"

let minimal_fees = Z.of_int 100
let nanotez_per_gas_unit = Z.of_int 100
let nanotez_per_byte = Z.of_int 1000
let to_nanotez m = Z.mul (Z.of_int 1000) m
let of_nanotez n = Z.div (Z.add (Z.of_int 999) n) (Z.of_int 1000)

let compute_fees ~gas_limit ~size =
  let minimal_fees_in_nanotez = to_nanotez minimal_fees in
  let fees_for_gas_in_nanotez =
    Z.mul nanotez_per_gas_unit (Z.of_int gas_limit) in
  let fees_for_size_in_nanotez = Z.mul nanotez_per_byte (Z.of_int size) in
  let fees_in_nanotez =
    Z.add minimal_fees_in_nanotez @@
    Z.add fees_for_gas_in_nanotez fees_for_size_in_nanotez in
  of_nanotez fees_in_nanotez

let compute_gas_limit ~fee ~size =
  let minimal_fees_in_nanotez = to_nanotez minimal_fees in
  let fees_for_size_in_nanotez = Z.mul nanotez_per_byte (Z.of_int size) in
  let fee_in_nanotez = to_nanotez fee in
  let fees_for_gas_in_nanotez =
    Z.sub fee_in_nanotez @@
    Z.add minimal_fees_in_nanotez fees_for_size_in_nanotez in
  Z.div fees_for_gas_in_nanotez nanotez_per_gas_unit
  |> Z.to_int
  |> max 0



let mk_json_obj fields =
  fields
  |> List.map (fun (f,v) -> "\"" ^ f ^ "\":" ^ v)
  |> String.concat ","
  |> fun fs -> "{" ^ fs ^ "}"

let mk_json_arr l = "[" ^ String.concat "," l ^ "]"


let get_json_string s =
  try Scanf.sscanf s "%S" (fun x -> x)
  with _ -> raise Not_found

let get_json_int s =
  try Scanf.sscanf s "%d" (fun x -> x)
  with _ ->
  try Scanf.sscanf s "\"%d\"" (fun x -> x)
  with _ -> raise Not_found

module ExprHash = struct
  let prefix = "\013\044\064\027" (* expr(54) *)
  include Blake2B.Make(Base58)(struct
      let name = "script_expr"
      let title = "A script expression hash"
      let b58check_prefix = prefix
      let size = None
    end)
end

let get_public_key_hash_from_public_key pk =
  match !LiquidOptions.network with
  | Tezos_network ->
    pk
    |> Ed25519.Public_key_hash_tezos.of_public_key
    |> Ed25519.Public_key_hash_tezos.to_b58check
  | Dune_network ->
    pk
    |> Ed25519.Public_key_hash_dune.of_public_key
    |> Ed25519.Public_key_hash_dune.to_b58check

let get_public_key_hash_from_secret_key sk =
  let pk = Sodium.Sign.secret_key_to_public_key sk in
  get_public_key_hash_from_public_key pk

let get_public_key_from_secret_key sk =
  sk
  |> Sodium.Sign.secret_key_to_public_key
  (* Replace by this when tezos is fixed *)
  (* |> Ed25519.Secret_key.to_public_key *)
  |> Ed25519.Public_key.to_b58check

let hash msg =
  Blake2B.(to_bytes (hash_bytes [MBytes.of_string "\x03"; msg]))

let sign sk op_b =
  Ed25519.sign sk (hash op_b)


let conv_ignore_extra ffrom fto enc =
  let open Json_encoding in
  conv
    (fun x -> ffrom x, ())
    (fun (x, ()) -> fto x)
    (merge_objs enc unit (* = ignore *))

let case_ignore_extra enc proj inj =
  let open Json_encoding in
  case
    (merge_objs enc unit (* = ignore *))
    (fun x -> match proj x with
       | None -> None
       | Some x -> Some (x, ()))
    (fun (x, ()) -> inj x)
