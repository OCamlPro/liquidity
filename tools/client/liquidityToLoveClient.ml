open LiquidityToLove

module AST =
  LiquidClient.Make
    (LiquidityToLove)
    (Liquidity)
    (Love)
    (NoConverter)

module String =
  LiquidClient.Make
    (LiquidityToLove)
    (StringLiquidity)
    (StringLove)
    (StringStringConverter)

module SJson =
  LiquidClient.Make
    (LiquidityToLove)
    (StringLiquidity)
    (JsonLove)
    (StringJsonConverter)

module Multi =
  LiquidClient.Make
    (LiquidityToLove)
    (MultiLiquidity)
    (MultiLove)
    (MultiConverter)
