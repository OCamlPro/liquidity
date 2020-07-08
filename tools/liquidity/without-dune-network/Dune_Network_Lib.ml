(****************************************************************************)
(*                               Liquidity                                  *)
(*                                                                          *)
(*                  Copyright (C) 2017-2020 OCamlPro SAS                    *)
(*                                                                          *)
(*                    Authors: Fabrice Le Fessant                           *)
(*                             Alain Mebsout                                *)
(*                             David Declerck                               *)
(*                             Steven De Oliveira                           *)
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

module Protocol = struct
  module Love_pervasives = struct
  end
  module Love_printer = struct
    module Ast = struct
      let print_structure _ = assert false
    end
  end
  module Love_json_encoding = struct
    module Ast = struct
      let top_contract_encoding = ()
    end
  end
  module Love_type_list = struct
    let init _ = ()
  end
  module Love_prim_list = struct
    let init _ = ()
  end
  module Love_tenv = struct
    let init_core_env _ = ()
  end
end
module Environment = struct
  module Data_encoding = struct
    module Json = struct
      let to_string _ = assert false
      let construct _ _ = assert false
      let convert _ = assert false
    end
  end
end
module Preprocess = struct
  let contract_ttfail_to_tvar _ =
    failwith "Mini version cannot compile to Love target."
end
module Love_ast = struct
  type t = {version : int * int; code : unit}
end
module Liq2love = struct
  let liqcontract_to_lovecontract ~ctr_name _ = assert false
  let print_contract_json _ = assert false
  let init () = ()
end
module Compil_utils = struct
  exception Liq2LoveError of string
end
