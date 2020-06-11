(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2019 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                                                                          *)
(*  This program is free software: you can redistribute it and/or modify    *)
(*  it under the terms of the GNU General Public License as published by    *)
(*  the Free Software Foundation, either version 3 of the License, or       *)
(*  (at your option) any later version.                                     *)
(*                                                                          *)
(*  This program is distributed in the hope that it will be useful,         *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*  GNU General Public License for more details.                            *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.  *)
(****************************************************************************)


module Public_key_hash = struct
  include Blake2B.MakeMulti(Base58)(struct
      let name  = "P256.Public_key_hash"
      let title = "A P256 public key hash"
      let b58check_prefix = Base58.DunePrefix.p256_public_key_hash
      let alternate_prefixes = [ Base58.Prefix.p256_public_key_hash ]
      let size = Some 20
    end)
  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

module Public_key = struct

  type t = |
  let rien (x:t) = match x with | _ -> .

  let name  = "P256.Public_key"
  let title = "A P256 public key"

  let to_bytes = rien
  let of_bytes_opt _ = None

  let to_string = rien
  let of_string_opt _ = None

  let size = 33

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.p256_public_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  let hash = rien

  include Compare.Make(struct
      type nonrec t = t
      let compare a _ = rien a
    end)

  include Helpers.MakeRaw(struct
      type nonrec t = t
      let name = name
      let of_bytes_opt = of_bytes_opt
      let of_string_opt = of_string_opt
      let to_string = to_string
    end)

  include Helpers.MakeB58(struct
      type nonrec t = t
      let name = name
      let b58check_encoding = b58check_encoding
    end)

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        conv to_bytes of_bytes_exn (Fixed.bytes size)
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

module Secret_key = struct

  type t = |
  let rien (x:t) = match x with | _ -> .

  let name = "P256.Secret_key"
  let title = "A P256 secret key"

  let size = 32

  let of_bytes_opt _ = None
  let to_bytes = rien

  let to_string = rien
  let of_string_opt _ = None

  let to_public_key = rien

  type Base58.data +=
    | Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix: Base58.Prefix.p256_secret_key
      ~length: size
      ~to_raw: to_string
      ~of_raw: of_string_opt
      ~wrap: (fun x -> Data x)

  include Compare.Make(struct
      type nonrec t = t
      let compare a = rien a
    end)

  include Helpers.MakeRaw(struct
      type nonrec t = t
      let name = name
      let of_bytes_opt = of_bytes_opt
      let of_string_opt = of_string_opt
      let to_string = to_string
    end)

  include Helpers.MakeB58(struct
      type nonrec t = t
      let name = name
      let b58check_encoding = b58check_encoding
    end)

  include Helpers.MakeEncoder(struct
      type nonrec t = t
      let name = name
      let title = title
      let raw_encoding =
        let open Data_encoding in
        conv to_bytes of_bytes_exn (Fixed.bytes size)
      let of_b58check = of_b58check
      let of_b58check_opt = of_b58check_opt
      let of_b58check_exn = of_b58check_exn
      let to_b58check = to_b58check
      let to_short_b58check = to_short_b58check
    end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

end

type t = |
let rien (x:t) = match x with | _ -> .

type watermark = MBytes.t

let name = "P256"
let title = "A P256 signature"

let size = 64

let of_bytes_opt _ = None
let to_bytes = rien

let to_string = rien
let of_string_opt _ = None

type Base58.data +=
  | Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix: Base58.Prefix.p256_signature
    ~length: size
    ~to_raw: to_string
    ~of_raw: of_string_opt
    ~wrap: (fun x -> Data x)

include Compare.Make(struct
    type nonrec t = t
    let compare a _ = rien a
  end)

include Helpers.MakeRaw(struct
    type nonrec t = t
    let name = name
    let of_bytes_opt = of_bytes_opt
    let of_string_opt = of_string_opt
    let to_string = to_string
  end)

include Helpers.MakeB58(struct
    type nonrec t = t
    let name = name
    let b58check_encoding = b58check_encoding
  end)

include Helpers.MakeEncoder(struct
    type nonrec t = t
    let name = name
    let title = title
    let raw_encoding =
      let open Data_encoding in
      conv to_bytes of_bytes_exn (Fixed.bytes size)
    let of_b58check = of_b58check
    let of_b58check_opt = of_b58check_opt
    let of_b58check_exn = of_b58check_exn
    let to_b58check = to_b58check
    let to_short_b58check = to_short_b58check
  end)

let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

let zero = (Obj.magic () : t)

let sign ?watermark:_ sk _msg = Secret_key.rien sk

let check ?watermark:_ _public_key signature _msg = rien signature

let generate_key ?seed () =
  assert false

let deterministic_nonce sk _msg = Secret_key.rien sk

let deterministic_nonce_hash sk _msg = Secret_key.rien sk
