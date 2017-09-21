Liquidity: a Smart Contract Language for Tezos
==============================================

Liquidity is a language to program Smart Contracts for Tezos. It uses
the syntax of OCaml, and strictly complies to Michelson security
restrictions.

The Liquidity Project
---------------------

The Liquidity project contains:
* A compiler from Liquidity files (.liq extension) to Michelson
* A de-compiler from Michelson files (.tz extension) to Liquidity
* An evaluator of Michelson contracts

The Liquidity Language
----------------------

The Liquidity language provides the following features:

* Full coverage of the Michelson language: Anything that can be written in
  Michelson can be written in Liquidity

* Local variables instead of stack manipulations: values can be stored
  in local variables. The only restriction is that local variables do
  not survive to `Contract.call`, following the philosophy of Michelson
  to force explicite storage of values to limit reentrancy bugs.

* High-level types: types like sum-types and record-types can be defined
  and used in Liquidity programs.

Roadmap
-------

The current roadmap is:

* Development of an online editor for Liquidity

* Development of a proof assistant for Liquidity contracts

