type storage = unit

let%entry main _ _ =
  let op =
    Account.transfer ~dest:tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc ~amount:1tz in
  [op], ()
