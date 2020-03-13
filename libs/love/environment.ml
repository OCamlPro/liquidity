
module Name = struct let name = "005-PsBabyM1" end

include Tezos_protocol_environment.MakeV1 (Name) ()
