next | master
------------ | -------------
[![Travis-CI Build Status](https://travis-ci.org/OCamlPro/liquidity.svg?branch=next)](https://travis-ci.org/OCamlPro/liquidity) | [![Travis-CI Build Status](https://travis-ci.org/OCamlPro/liquidity.svg?branch=master)](https://travis-ci.org/OCamlPro/liquidity) 


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
  in local variables.

* High-level types: types like sum-types and record-types can be defined
  and used in Liquidity programs.

Branches
--------

The `master` branch contains the latest stable release. The `next`
branch contains the upcoming version: the language on the `next`
branch is for experimentation, and features may be modified before the
next release.


Installation and Usage
----------------------

See [installation instructions](docs/sphinx/src/installation/index.rst) and
[usage documentation](docs/sphinx/src/usage/index.rst).

Documentation
-------------

Documentation can be found in [the reference](docs/sphinx/src/reference/liquidity.rst)
and many examples are in the [tests](tests/) and
[tests/others](tests/others/) directories.

Status
------

All features of Michelson are supported in Liquidity. All tests from
https://github.com/tezos/tezos/tree/alphanet/test/contracts can be
decompiled to Liquidity and recompiled to Michelson.

Roadmap
-------

The current roadmap is:

* Development of an online editor for Liquidity

* Development of a proof assistant for Liquidity contracts

