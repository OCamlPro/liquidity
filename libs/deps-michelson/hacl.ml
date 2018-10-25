module Hash = struct

  module SHA256 = struct

    let digest big =
      let s = Digestif.SHA256.digest_bigstring big in
      (* Since Digestif.SHA256.t is a `private string` in 0.6 and opaque in 0.7,
         we need to cast it back to string before returning a Bigstring.
         XXX: Ugly, we need to do this to support both digestif 0.6.1 and
         Digestif 0.7  *)
      let digestif_to_string : Digestif.SHA256.t -> string = Obj.magic in
      (* When everyone is on 0.7 use Digestif.SHA256.to_raw_string instead *)
      Bigstring.of_string (digestif_to_string s)


  end

end
