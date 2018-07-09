Usage
=====

Dependencies
------------

You need the following OCaml dependencies to be installed (with
[opam](https://opam.ocaml.org/)) to compile `liquidity`:

* `ocp-build`
* `zarith`
* `ocplib-json-typed`
* `ocplib-endian`
* `calendar`
* `ocurl`
* `lwt`
* `bigstring`
* `digestif`
* `ezjsonm`
* `sodium`

Compilation and installation
----------------------------

In the top-directory, use:

* `make clone-tezos`
* `make -C tezos build-deps`
* `make`
* `make install`

If you want limited features, you can pass the first and second steps,
you will only get a program `liquidity-mini` which can compile
liquidity programs, but cannot decompile Michelson ones.

You can also use `make clean` to clean the directories,
and `make tests` to run the compiler on examples in the
`tests/` directory.

Usage
-----

Run the compiler with no options to get help.

The syntax of Liquidity is described in `docs/liquidity.md`.

Compiling a Contract
--------------------

```
./_obuild/liquidity/liquidity.asm tests/test13.liq
```

will generate `tests/test13.liq.tz`

Decompiling a Contract
----------------------

```
./_obuild/liquidity/liquidity.asm tests/test13.liq.tz
```

will generate `tests/test13.liq.tz.liq`


Running a Contract
------------------

If you have a Tezos node running on port 8732:
```
./_obuild/liquidity/liquidity.asm --amount 5tz --run tests/others/demo.liq '""' 'Map ["", 0]'
```

will output:
```
Map [("", 1)]
# Internal operations: 0
```
