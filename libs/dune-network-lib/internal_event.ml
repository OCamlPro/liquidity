module Legacy_logging = struct

  module type SEMLOG = sig end

  module Make_semantic (X : sig end) = struct
  end
end
