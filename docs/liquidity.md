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
      [%return : TYPE] =
      BODY
```

The `version` statement tells the compiler in which version of Liquidity
the contract is written. The compiler will reject any contract that has
a version that it does not understand (too old, more recent).

The `main` function is the default entry point for the contract.
`let%entry` is the construct used to declare entry points (there is
currently only one entry point, but there will be probably more in the
future).  The declaration takes three parameters with names
`parameter`, `storage` and `return`. The first two parameters,
`parameter` and `storage` are arguments to the function, while the
latest one, `return`, is only used to specified the return type of the
function. The types of the three parameters must always be specified.

A contract always returns a pair `(return, storage)`, where `return`
is the return value to the caller, and `storage` is the final state of
the contract after the call. The type of the pair must match the type
of a pair with the two parameters, `return` and `storage`, that were
specified at the beginning of `main`.

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

* `FAIL` : `Current.fail ()`. Makes the contract abort.
* `SELF` : `Current.contract ()`. Returns the current contract being executed.
* `BALANCE` : `Current.balance ()`. Returns the current balance of the
       current contract.
* `NOW` : `Current.time ()`. Returns the timestamp of the block containing
       the transaction in the blockchain.
* `AMOUNT` : `Current.amount ()`. Returns the amount of tezzies that were
       transfered when the contract was called.
* `STEPS_TO_QUOTA` : `Current.gas ()`. Returns the current gas available
       to execute the end of the contract.
* `SOURCE arg_type res_type` : `( Source : (arg_type, res_type) contract)`.
       Returns the contract that called the current contract.
* `CONS` : `x :: y`
* `NIL ele_type` : `( [] : ele_type list )`
* `H` : `Crypto.hash x`. Returns the hash of its argument, whatever it is.
* `CHECK_SIGNATURE` : `Crypto.check key (signature,data)`. Returns `true` if
     the public key has been used to generate the signature of the data.
* `CREATE_ACCOUNT` : `Account.create`: creates a new account
* `CREATE_CONTRACT` : `Contract.create`: creates a new contract
* `MANAGER` : `Contract.manager ct`: returns the key of the manager of the
     contract in argument
* `EXEC` : `Lambda.pipe x f` or `x |> f` or `f x`, is the application of the
     lambda `f` on the argument `x`.
* `DEFAULT_ACCOUNT` : `Account.default key`. Returns the default contract
    (of type `(unit,unit) contract`) associated with a key.

Comparison operators
--------------------

These operators take two values of the same type, and return a boolean value:
* `COMPARE; EQ` : `x = y`
* `COMPARE; NEQ` : `x <> y`
* `COMPARE; LE` : `x <= y`
* `COMPARE; LT` : `x < y`
* `COMPARE; GE` : `x >= y`
* `COMPARE; GT` : `x > y`

The last one returns an integer:
* `COMPARE` : `compare x y`


Operations on data structures
-----------------------------
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

Liquidity also provides additional operations:
* `List.rev` : List reversal

Arithmetic and logic operators
------------------------------

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
    a `tz` suffix (`1.00tz`, etc.) or as a string with type coercion
    (`("1.00" : tez)`).

There are also three types of collections: lists, sets and
maps. Constants collections can be created directly:
* Lists: `["x";"y"]`;
* Sets: `Set [1;2;3;4]`;
* Maps: `Map [1, "x"; 2, "y"; 3, "z"]`;

In the case of an empty collection, whose type cannot be inferred, the
  type must be specified:
* Lists: `([] : int list)`
* Sets: `(Set : int set)`
* Maps: `(Map : (int, string) map)`


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

Functions and Closures
----------------------

Unlike Michelson, functions in Liquidity can also be closures. They can take
multiple arguments and are curryfied. Because closures are lambda-lifted, it is
however recommended to use a single tuple argument when possible.  Arguments
must be annotated with their (monomorphic) type. Primitive functions such as
`List.map` do not accept closures as their first arguments at the moment.

Function applications are often done using the `Lambda.pipe` function
or the `|>` operator:

```
  let succ = fun (x : int) -> x + 1 in
  let one = 0 |> succ in
...
```

but they can also be done directly:

```
...
  let succ (x : int) = x + 1 in
  let one = succ 0 in
...
```

A toplevel function can also be defined before the main entry point:
```
let version = 1.0
let succ (x : int) = x + 1
let%entry main ... =
   ...
   let one = succ 0 in
   ...
```

Closures can be created with the same syntax:
```
let p = 10 in
let sum_and_add_p (x : int) (y : int) = x + y + p in
let r = add_p 3 4 in
...
```

This is equivalent to:
```
let p = 10 in
let sum_and_add_p =
  fun (x : int) ->
    fun (y : int) ->
      x + y + p
in
let r = 4 |> (3 |> add_p) in
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

