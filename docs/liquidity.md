Liquidity, a simple language over Michelson
===========================================

Contract Format
---------------

All the contracts have the following form:

```
let contract
      (amount : tez)
      (parameter : TYPE)
      (storage : TYPE)
      (return : TYPE) =
      BODY
```

where TYPE is a type and BODY is the code of the contract, using the
three arguments `amount`, `parameter` and `storage`. The `return`
variable cannot be used, it is special form to declare the return type
of the contract.  The `amount` argument is the amount of Tez sent to
the contract, `parameter` is the argument provided by the caller, and
`storage` is the initial state of the contract.

A contract always returns a pair `(return, storage)`, where `return` is
the return value to the caller, and `storage` is the final state of the
contract after the call.

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
contract. 

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
* `MANAGER` : `Contract.manager ct`
* `EXEC` : `Lambda.pipe` or `|>`

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

