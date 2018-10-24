module Hash = struct

  module SHA256 = struct

    let digest big =
      let s = Digestif.SHA256.digest_bigstring big in
      (* Since Digestif.SHA256.t is opaque, we need to convert it
      back to string before returning a Bigstring *)
      Bigstring.of_string (Digestif.SHA256.to_raw_string s)


  end

end
