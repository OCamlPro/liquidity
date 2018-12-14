module Hash = struct
  module type S = sig
    val bytes : int
    val digest : Bigstring.t -> Bigstring.t
    module HMAC : sig
      val digest :
        key:Bigstring.t -> msg:Bigstring.t -> Bigstring.t
    end
  end

  module Make (H : Digestif.S) = struct

    let bytes = H.digest_size

    let digest big =
      let s = H.digest_bigstring big in
      Bigstring.of_string (H.to_raw_string s)

    module HMAC = struct

      let digest ~key ~msg =
        let s = H.hmac_bigstring ~key msg in
        Bigstring.of_string (H.to_raw_string s)

    end
  end

  module SHA256 : S = Make (Digestif.SHA256)
  module SHA512 : S = Make (Digestif.SHA512)

end
