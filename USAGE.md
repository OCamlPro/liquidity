Usage
=====

Dependencies
------------

You need the following dependencies to be installed to compile `liquidity`:
* `ocp-build`
* `zarith`
* `ocplib-json-typed`
* `ocplib-endian`
* `calendar`

Compilation and installation
----------------------------

In the top-directory, use `make` and `make install`

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

For example:
```
./_obuild/liquidity/liquidity.asm --load-arg tests/others/demo.arg  --load-storage tests/others/demo.tzp --exec tests/others/demo.tzs
```

will output:
```
Program "tests/others/demo.tzs" parsed
Result:
Unit
Storage:
(Map (Item "Heineken" 0) (Item "Guinness" 0) (Item "Corona" 1))
```
