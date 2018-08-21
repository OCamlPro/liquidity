
Introduction
============

The Tezos ledger uses a low-level statically-typed language for Smart
Contracts, called Michelson. Although Michelson can be used directly,
the lack of variables and the use of stack-based instructions make it
hard to write, and hard to read. Early in the life of Tezos, the need
rised for a higher-level language. Liquidity prototype was born in
June 2017 at OCamlPro, and officially released in February 2018 on the
Alphanet network of Tezos.

Liquidity follows Michelson type-system, but implied on a subset of
the OCaml syntax. It comes with compiler to Michelson, and a
decompiler that can translate Michelson contracts to Liquidity, for
auditing purpose.

The official websites for Liquidity are:

* `Official Page <http://www.liquidity-lang.org/>`__
* `Online Editor <http://www.liquidity-lang.org/edit>`__
* `Documentation <http://www.liquidity-lang.org/doc>`__
* `Github Project <http://github.com/OCamlPro/liquidity>`__

The Liquidity project contains:

* A compiler from Liquidity files (.liq extension) to Michelson
* A decompiler from Michelson files (.tz extension) to Liquidity

See `Examples <http://github.com/OCamlPro/liquidity/tree/master/tests>`__
in the `Github <http://github.com/OCamlPro/liquidity>`__ project.
