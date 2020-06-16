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
end
