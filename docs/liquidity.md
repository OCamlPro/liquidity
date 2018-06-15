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
* An interface to a Tezos node for manipulating Liquidity contracts

See [examples](http://github.com/OCamlPro/liquidity/tree/master/tests)
in the [Github](http://github.com/OCamlPro/liquidity) project.

Contract Format
---------------

All the contracts have the following form:

```ocaml
[%%version 0.2]

<... local declarations ...>

let%init storage
      (x : TYPE)
      (x : TYPE)
      ... =
      BODY

let%entry main
      (parameter : TYPE)
      (storage : TYPE) =
      BODY
```

The `version` statement tells the compiler in which version of
Liquidity the contract is written. The compiler will reject any
contract that has a version that it does not understand (too old, more
recent). We expect to reach version 1.0 at the launch of the Tezos
network.

The `main` function is the default entry point for the contract.
`let%entry` is the construct used to declare entry points (there is
currently only one entry point, but there will be probably more in the
future).  The declaration takes two parameters with names
`parameter`, `storage`, the arguments to the function. Their types must
always be specified. The return type of the function must also be
specified by a type annotation.

A contract always returns a pair `(operations, storage)`, where
`operations` is a list of internal operations to perform after
exectution of the contract, and `storage` is the final state of the
contract after the call. The type of the pair must match the type of a
pair where the first component is a list of opertations and the second
is the type of the argument `storage` of `main`.

`<... local declarations ...>` is an optional set of optional type and
function declarations. Type declarations can be used to define records
and variants (sum-types), described later in this documentation.

An optional initial storage or storage initializer can be given with
`let%init storage`. When deploying a Liquidity contract, if the
storage is not constant it is evaluated in the prevalidation context.


Types
-----

Types in Liquidity are monomorphic. The built-in base types are:

- `unit`: whose only constructor is `()`
- `bool`: Booleans
- `int`: unbounded integers
- `nat`: unbounded naturals
- `tez`: the type of amounts
- `string`: character strings
- `timestamp`: dates and timestamps
- `key`: cryptographic keys
- `key_hash`: hashes of cryptographic keys
- `signature`: cryptographic signatures
- `operation`: type of operations, can only be constructed
- `address`: abstract type of contract addresses

The other types are:
- tuples: noted `(t1 * t2 * t3)`
- option type: `'a option = None | Some of 'a`
- variant type: `('a, 'b) variant = Left of 'a | Right of 'b`
- lists: `'a list` is the type of lists of elements in `'a`
- sets: `'a set` is the type of sets of elements in `'a`
- maps: `('a, 'b) map` is the type of maps whose keys are of type
  `'a` and values of type `'b`
- big maps: `('a, 'b) big_map` is the type of lazily deserialized maps whose
  keys are of type `'a` and values of type `'b`
- contracts: `'a contract` for contracts whose parameter is of type `'a`
- functions: `'a -> 'b` is the type of functions from `'a` to `'b`

Record and variant types must be declared beforehand and are referred
to by their names.


Calling another contract
------------------------

Calling another contract is done by constructing an operation with the
built-in `Contract.call` function, and **returning** this value at the
end of the contract. Internal contract calls are performed after
execution of the contract is over, in the order in which the resulting
operations are returned.

```ocaml
let op = Contract.call CONTRACT AMOUNT ARG in
BODY
( op :: OTHER_OPERATIONS, STORAGE)
```
where:
- `CONTRACT` is the value of the contract being called;
- `AMOUNT` is the value of the amount of Tez sent to the contract;
- `ARG` is the argument sent to the contract.
- `BODY` is some code to be executed after the contract.

For the call to be actually performed by the blockchain, it _has_ to be
returned as part of the list of operations.
 
Operators and functions
-----------------------

Here is a list of equivalences between MICHELSON instructions and
Liquidity functions:

* `FAIL` : `Current.fail ()` or `Current.failwith "Message"`. Makes the contract abort.
* `SELF` : `Current.contract ()`. Returns the current contract being executed.
* `BALANCE` : `Current.balance ()`. Returns the current balance of the
       current contract.
* `NOW` : `Current.time ()`. Returns the timestamp of the block containing
       the transaction in the blockchain.
* `AMOUNT` : `Current.amount ()`. Returns the amount of tezzies that were
       transfered when the contract was called.
* `STEPS_TO_QUOTA` : `Current.gas ()`. Returns the current gas available
       to execute the end of the contract.
* `SOURCE` : `Contract.source`.
       Returns the address of the contract that called the current contract.
* `CONS` : `x :: y`
* `NIL ele_type` : `( [] : ele_type list )`
* `H` : `Crypto.hash x`. Returns the hash of its argument, whatever it is.
* `HASH_KEY` : `Crypto.hash_key k`. Compute the b58check of the key `k`.
* `CHECK_SIGNATURE` : `Crypto.check key signature data`. Returns `true` if
     the public key has been used to generate the signature of the data.
* `CREATE_ACCOUNT` : `Account.create`. Creates a new account.
* `CREATE_CONTRACT` : `Contract.create`. Creates a new contract.
* `SET_DELEGATE` : `Contract.set_delegate`. Sets the delegate (or unset,
  if argument is `None`) of the current contract.
* `MANAGER` : `Contract.manager ct`: returns the key hash of the manager
     of the contract in argument
* `CONTRACT param_type` : `(Contract.at addr : param_type contract
     option)`: returns the contract stored at this address, if it exists
* `EXEC` : `Lambda.pipe x f` or `x |> f` or `f x`, is the application of the
     lambda `f` on the argument `x`.
* `IMPLICIT_ACCOUNT` : `Account.default key_hash`. Returns the default contract
    (of type `unit contract`) associated with a key hash.

Comparison operators
--------------------

These operators take two values of the same type, and return a Boolean value:
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
* `GET` : `Map.find`
* `UPDATE`: `Map.update` or `Set.update`
* `MEM`: `Map.mem` or `Set.mem`
* `CONCAT` : `@`
* `SIZE` : `List.size` or `Set.size` or `Map.size`
* `ITER` : `List.iter` or `Set.iter` or `Map.iter` or `List.fold` or
  `Set.fold` or `Map.fold`
* `MAP` : `List.map` or `Set.map` or `Map.map` or `List.map_fold` or
  `Set.map_fold` or `Map.map_fold`

(it is possible to use the generic `Coll.` prefix for all collections,
but not in a polymorphic way, i.e. `Coll.` is immediately replaced by the
type-specific version for the type of its argument.)

Liquidity also provides additional operations:
* `List.rev : 'a list -> 'a list` : List reversal
* `Map.add : 'a -> 'b -> ('a, 'b) map -> ('a, 'b) map` : add (or
  replace) a binding to a map
* `Map.remove : 'a -> ('a, 'b) map -> ('a, 'b) map` : remove a binding,
  if it exists, in a map
* `Set.add : 'a -> 'a set -> 'a set` : add an element to a set
* `Set.remove : 'a -> 'a set -> 'a set` : remove an element, if it
  exists, in a set

Arithmetic and logic operators
------------------------------

* `OR` : `x || y` or `x lor y`
* `AND` : `x && y` or `x land y`
* `XOR` : `x xor y` or `x lxor y`
* `NOT` : `not x` or `lnot x`
* `ABS` : `abs x` with the difference that `abs` returns an integer
* `INT` : `int x`
* `NEG` : `-x`
* `ADD` : `x + y`
* `SUB` : `x - y`
* `MUL` : `x * y`
* `EDIV` : `x / y`
* `LSR` : `x >> y` or `x lsr y`
* `LSL` : `x << y` or `x lsl y`

For converting `int` to `nat`, Liquidity provides a special
pattern-matching construct `match%nat`, on two constructors `Plus` and
`Minus`. For instance, in the following where `x` has type `int`:

```ocaml
match%nat x with
| Plus p -> p + 1p
| Minus m -> m + 1p
```

`m` and `p` are of type `nat` and:

* `x = int m` when `x` is positive or null
* `x = - (int p)` when `x` is negative


Constants
---------

The unique constructor of type `unit` is `()`.

The two Booleans constants are:
* `true`
* `false`

As in Michelson, there are different types of integers:
* int : an unbounded integer, positive or negative, simply
    written `0`,`1`,`2`,`-1`,`-2`,...
* nat : an unbounded positive integer, written either with a `p` suffix
    (`0p`, `12p`, etc.) or as an integer with a type coercion ( `(0 : nat)` ).
* tez : an unbounded positive float of Tezzies, written either with
    a `tz` suffix (`1.00tz`, etc.) or as a string with type coercion
    (`("1.00" : tez)`).

Strings are delimited by the characters `"` and `"`.

Timestamps are written in ISO 8601 format, like in Michelson:
* `2015-12-01T10:01:00+01:00`

Keys and hashes are base58-check encoded, the same as in Michelson:
* `tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc` is a key hash
* `edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN` is a public key

Signatures are 64 bytes long hex encoded, prefixed with a backtick:
```
`96c724f3eab3da9eb0002caa5456aef9a7c716e6d6d20c07f3b3659369e7dcf5b66a5a8c33dac317fba6174217140b919493acd063c3800b825890a557c39e0a
```

There are also three types of collections: lists, sets and
maps. Constants collections can be created directly:
* Lists: `["x"; "y"]`;
* Sets: `Set [1; 2; 3; 4]`;
* Maps: `Map [1, "x"; 2, "y"; 3, "z"]`;
* Big maps: `BigMap [1, "x"; 2, "y"; 3, "z"]`;

In the case of an empty collection, whose type cannot be inferred, the
  type must be specified:
* Lists: `([] : int list)`
* Sets: `(Set : int set)`
* Maps: `(Map : (int, string) map)`
* Big maps: `(BigMap : (int, string) big_map)`


Tuples
------

Tuples in Liquidity are compiled to pairs in Michelson:
```
(x, y, z) <=> Pair x (Pair y z)
```

Tuples can be accessed using the field access notation of Liquidity:
```ocaml
let t = (x,y,z) in
let should_be_true = t.(2) = z in
...
```

A new tuple can be created from another one using the field access update
notation of Liquidity:
```ocaml
let t = (1,2,3) in
let z = t.(2) <- 4 in
...
```

Tuples can be deconstructed:
```ocaml
(* t : (int * (bool * nat) * int) *)
let _, (b, _), i = t in
...
(* b : bool
   i : int *)
```


Records
-------

Record types can be declared and used inside a liquidity contract:
```ocaml
type storage = {
  x : string;
  y : int;
}
```
Such types can be created and used inside programs:
```ocaml
let r = { x = "foo"; y = 3 } in
r.x
```

Records are compiled as tuples.

Deep record creation is possible using the notation:
```ocaml
let r1 = { x = 1; y = { z = 3 } } in
let r2 = r1.y.z <- 4 in
...
```

Variants
--------

Variants should be defined before use, before the contract
declaration:

```ocaml
type t =
| X
| Y of int
| Z of string * nat
```

Variants can be created using:

```ocaml
let x = X 3 in
let y = Z s in
...
```

The `match` construct can be used to pattern-match on them, but only
on the first constructor:

```ocaml
match x with
| X -> ...
| Y i -> ...
| Z s -> ...
```

where `i` and `s` are variables that are bound by the construct to the
parameter of the variant.

Parameters of variants can also be deconstructed when they are tuples,
so one can write:
```ocaml
match x with
| X -> ...
| Y i -> ...
| Z (s, n) -> ...
```


A special case of variants is the `Left | Right` predefined variant,
called `variant`:
```ocaml
type (`left, `right) variant =
| Left of `left
| Right of `right
```

All occurrences of these variants should be constrained with type
annotations:

```ocaml
let x = (Left 3 : (int, string) variant) in
match x with
| Left left  -> ...
| Right right -> ...
```

Another special variant is the `Source` variant: it is used to refer to
the contract that called the current contract.

```ocaml
let s = (Source : (unit, unit) contract) in
...
```

As for `Left` and `Right`, `Source` occurrences should be constrained by
type annotations.

Functions and Closures
----------------------

Unlike Michelson, functions in Liquidity can also be closures. They can take
multiple arguments and are curryfied. Because closures are lambda-lifted, it is
however recommended to use a single tuple argument when possible.  Arguments
must be annotated with their (monomorphic) type, while the return type
is inferred.

Function applications are often done using the `Lambda.pipe` function
or the `|>` operator:

```ocaml
  let succ = fun (x : int) -> x + 1 in
  let one = 0 |> succ in
...
```

but they can also be done directly:

```ocaml
...
  let succ (x : int) = x + 1 in
  let one = succ 0 in
...
```

A toplevel function can also be defined before the main entry point:
```ocaml
[%%version 0.2]

let succ (x : int) = x + 1

let%entry main ... =
   ...
   let one = succ 0 in
   ...
```

Closures can be created with the same syntax:
```ocaml
let p = 10 in
let sum_and_add_p (x : int) (y : int) = x + y + p in
let r = add_p 3 4 in
...
```

This is equivalent to:
```ocaml
let p = 10 in
let sum_and_add_p =
  fun (x : int) ->
    fun (y : int) ->
      x + y + p
in
let r = 4 |> (3 |> add_p) in
...
```


Functions with multiple arguments should take a tuple as argument because
curried versions will generate larger code and should be avoided
unless partial application is important. The previous function should
be written as:
```ocaml
let sum_and_add_p ((x : int), (y : int)) =
  let p = 10 in
  x + y + p
in
let r = add_p (3, 4) in
...
```


Loops
-----

Loops in liquidity share some syntax with functions, but the body of
the loop is not a function, so it can access the environment, as would
a closure do:

```ocaml
let end_loop = 5 in
let x = Loop.loop (fun x ->
    ...
    (x < end_loop, x')
  ) x_init
in
...
```

As shown in this example, the body of the loop returns a pair, whose first
part is the condition to remain in the loop, and the second part is the
accumulator.

