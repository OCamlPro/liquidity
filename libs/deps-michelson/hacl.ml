module Hash = struct

  module SHA256 = struct

    let digest big =
      let s = Digestif.SHA256.digest_bigstring big in
      (* Since Digestif.SHA256.t is a `private string`, we need to cast it
      back to string before returning a Bigstring *)
      Bigstring.of_string (s :> string)


  end

end
