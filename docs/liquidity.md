Liquidity, a simple language over Michelson
===========================================

Liquidity is a language to program Smart Contracts for Tezos. It uses
the syntax of OCaml, and strictly complies to Michelson security
restrictions.

The [Liquidity Project](http://github.com/OCamlPro/liquidity)
---------------------

The Liquidity project contains:
* A compiler from Liquidity files (.liq extension) to Michelson
* A de-compiler from Michelson files (.tz extension) to Liquidity
* An evaluator of Michelson contracts

See [examples](http://github.com/OCamlPro/liquidity/tree/master/tests)
in the [Github](http://github.com/OCamlPro/liquidity) project.

Contract Format
---------------

All the contracts have the following form:

```
let version = 1.0
<... local declarations ...>
let%entry main
      (parameter : TYPE)
      (storage : TYPE)
      (return : TYPE) =
      BODY
```

The `version` statement tells the compiler in which version of Liquidity
the contract is written. The compiler will reject any contract that has
a version that it does not understand (too old, more recent).

The `main` function is the default entry point for the contract.
`let%entry` is the construct used to declare entry points (there is
currently only one entry point, but there will be probably more in the
future).  Each of the three parameter of an entry point should have
its type specified. Using the names `parameter`, `storage` and
`return` for these three parameters is mandatory. `parameter` and
`storage` are the two arguments to the entry, while `return` has the
type of its return value.

A contract always returns a pair `(return, storage)`, where `return` is
the return value to the caller, and `storage` is the final state of the
contract after the call.

<... local declarations ...> is an optional set of optional type and
function declarations. Type declarations can be used to define records
and variants (sum-types), described later in this documentation.

Calling another contract
------------------------

Calling another contract is done by using the following form:

```
let (RESULT, storage) = Contract.call CONTRACT AMOUNT STORAGE ARG
in
BODY
```
where:
- `RESULT` is the identifier of the variable that will receive the value
  returned by the contract;
- `CONTRACT` is the value of the contract being called;
- `AMOUNT` is the value of the amount of Tez sent to the contract;
- `STORAGE` is the value of the storage saved before the call;
- `ARG` is the argument sent to the contract.
- `BODY` is some code to be executed after the contract, using the
  result of the call.

All variables are destroyed during the call, so any state that should
survive the call should be stored in the storage of the calling
contract. It is not a limitation, but a design choice: always
specifying the state at the call forces the programmer to think about
what would happen in the case of a recursive call.

Operators and functions
-----------------------

Here is a list of equivalences between MICHELSON instructions and
Liquidity functions:

* `FAIL` : `Current.fail ()`
* `SELF` : `Current.contract ()`
* `BALANCE` : `Current.balance ()`
* `NOW` : `Current.time ()`
* `AMOUNT` : `Current.amount ()`
* `STEPS_TO_QUOTA` : `Current.gas ()`
* `SOURCE arg_type res_type` : `( Source : (arg_type, res_type) contract)`
* `CONS` : `x :: y`
* `NIL ele_type` : `( [] : ele_type list )`
* `H` : `Crypto.hash x`
* `CHECK_SIGNATURE` : `Crypto.check sg`
* `CREATE_ACCOUNT` : `Account.create`
* `CREATE_CONTRACT` : `Contract.create`
* `MANAGER` : `Contract.manager ct`
* `EXEC` : `Lambda.pipe` or `|>`
* `DEFAULT_ACCOUNT` : `Account.default`

Comparisons:

* `COMPARE` : `compare x y`
* `COMPARE; EQ` : `x = y`
* `COMPARE; NEQ` : `x <> y`
* `COMPARE; LE` : `x <= y`
* `COMPARE; LT` : `x < y`
* `COMPARE; GE` : `x >= y`
* `COMPARE; GT` : `x > y`

On data structures:
* `MAP` : `Map.map` or `List.map`
* `GET` : `Map.find`
* `UPDATE`: `Map.update` or `Set.update`
* `MEM`: `Map.mem` or `Set.mem`
* `CONCAT` : `@`
* `REDUCE` : `Map.reduce` or `Set.reduce` or `List.reduce`
* `SIZE` : `List.size` or `Set.size` or `Map.size`

(it is possible to use the generic `Coll.` prefix for all collections,
but not in a polymorphic way, i.e. `Coll.` is immediately replaced by the
type-specific version for the type of its argument.)

Operations:
* `OR` : `x or y`
* `AND` : `x & y`
* `XOR` : `x xor y`
* `NOT` : `not x`
* `ABS` : `abs x`
* `INT` : `int x`
* `NEG` : `-x`
* `ADD` : `x + y`
* `SUB` : `x - y`
* `MUL` : `x * y`
* `EDIV` : `x / y`
* `LSR` : `x >> y`
* `LSL` : `x << y`

Constants
---------

As in Michelson, there are different types of integers:
* int : an unbounded integer, positive or negative, simply
    written `0`,`1`,`2`,`-1`,`-2`,...
* nat : an unbounded positive integer, written either with a `p` suffix
    (`0p`, `12p`, etc.) or as an integer with a type coercion ( `(0 : nat)` ).
* tez : an unbounded positive float of Tezzies, written either with
    a `t` suffix (`1.00t`, etc.) or as a string with type coercion
    (`("1.00" : tez)`).

Tuples
------

Tuples in Liquidity are compiled to pairs in Michelson:
```
x * y * z <=> pair x (pair y z)
```

Tuples can be accessed using the field access notation of Liquidity:
```
let t = (x,y,z) in
let should_be_true = t.(2) = z in
...
```

A new tuple can be created from another one using the field access update
notation of Liquidity:
```
let t = (1,2,3) in
let z = t.(2) <- 4 in
...
```

Records
-------

Record types can be declared and used inside a liquidity contract:
```
type storage = {
  x : string;
  y : int;
}
```
Such types can be created and used inside programs:
```
let r = { x = "foo"; y = 3 } in
r.x
```

Records are compiled as tuples.

Deep record creation is possible using the notation:
```
let r1 = { x = 1; y = { z = 3 } } in
let r2 = r1.y.z <- 4 in
...
```

Variants
--------

Variants should be defined before use, before the contract
declaration:

```
type t =
| X
| Y of int
| Z of string * nat
```

Variants can be created using:

```
let x = X 3 in
let y = Z s in
...
```

The `match` construct can be used to pattern-match on them, but only
on the first constructor:

```
match x with
| X -> ...
| Y i -> ...
| Z s -> ...
```

where `i` and `s` are variables that are bound by the construct to the
parameter of the variant.

A special case of variants is the `Left | Right` predefined variant,
called `variant`:
```
type (`left, `right) variant =
| Left of `left
| Right of `right
```

All occurrences of these variants should be constrained with type
annotations:

```
let x = (Left 3 : (int, string) variant) in
match x with
| Left left  -> ...
| Right right -> ...
```

Another special variant is the `Source` variant: it is used to refer to
the contract that called the current contract.

```
let s = ( Source : (unit, unit) contract ) in
...
```

As for `Left` and `Right`, `Source` occurrences should be constrained by
type annotations.

Functions
---------

As for Michelson, functions in Liquidity are not closures. They can only
access their only argument. The argument must be annotated with its
(monomorphic) type.

Function applications are done using the `Lambda.pipe` function or the
`|>` operator:

```
let succ = fun (x : int) -> x + 1 in
let one = 0 |> succ in
...
```

Loops
-----

Loops in liquidity share some syntax with functions, but the body of
the loop is not a function, so it can access the environment, as would
a closure do:

```
let end_loop = 5 in
let x = Loop.loop (fun x ->
  ...
  ( x < end_loop, x')
  ) x_init
in
...
```

As shown in this example, the body of the loop returns a pair, whose first
part is the condition to remain in the loop, and the second part is the
accumulator.

