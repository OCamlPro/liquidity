open LiquidityToMichelson

module AST =
  LiquidClient.Make
    (LiquidityToMichelson)
    (Liquidity)
    (Michelson)
    (NoConverter)

module String =
  LiquidClient.Make
    (LiquidityToMichelson)
    (StringLiquidity)
    (StringMichelson)
    (StringStringConverter)

module SJson =
  LiquidClient.Make
    (LiquidityToMichelson)
    (StringLiquidity)
    (JsonMichelson)
    (StringJsonConverter)

module Multi =
  LiquidClient.Make
    (LiquidityToMichelson)
    (MultiLiquidity)
    (MultiMichelson)
    (MultiConverter)
