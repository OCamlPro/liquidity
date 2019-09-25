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
