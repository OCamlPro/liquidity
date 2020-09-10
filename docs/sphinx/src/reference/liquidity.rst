Liquidity Reference
===================

.. role:: love
   :class: only-love

.. role:: michelson
   :class: only-michelson


Contract Format
---------------

All the contracts have the following form::

 [%%version 2.1]
 
 <... local declarations ...>

 type storage = TYPE

 let%init storage
     (x : TYPE)
     (y : TYPE)
     ... =
     BODY
 
 let%entry entrypoint1
     (p1 : TYPE)
     (s1 : TYPE) =
     BODY

 let%entry entrypoint2
     (p2 : TYPE)
     (s2 : TYPE) =
     BODY

 let%entry default
     (parameter : TYPE)
     (storage : TYPE) =
     BODY

 let%view view1
     (parameter : TYPE)
     (storage : TYPE)
     : TYPE =
     BODY

  ...

The optional ``version`` statement tells the compiler in which version
of Liquidity the contract is written. The compiler will reject any
contract that has a version that it does not understand (too old, more
recent).

A contract is composed of type declarations, local values definitions,
an initializer, and a set of entry points and views. The type
``storage`` must be defined for all contracts.

Each entry point is a special function declared with the keyword
``let%entry``. An entry point must have two arguments, the first one
being the parameter (whose type can be inferred, but we recommend
writing a type annotation for documentation purposes) and the second
one is the storage. The return type of the function can be specified
but is not necessary. Each entry point and view must be given a unique
name within the same contract.

If there is an entry point named ``default``, it will be the default
entry point for the contract, *i.e.* the one that is called when the
entry point is not specified in ``Contract.call``. It is generally a
good idea to make this entry point take a parameter of type unit, so
that the code will be executed by any transfer made to it without
arguments. (This can code to prevent accidental token transfers for
instance.)

An entry point always returns a pair ``(operations, storage)``, where
``operations`` is a list of internal operations to perform after
execution of the contract, and ``storage`` is the final state of the
contract after the call. The type of the pair must match the type of a
pair where the first component is a list of opertations and the second
is the type of the storage argument.

.. topic:: Only with Love
   :class: love

   Views are effect-less functions that return values (usually by reading
   the storage) and that are accessible outside the contract. Each view
   takes a parameter and a storage, and returns a value. We recommend
   writing the parameter and return types for documentation.

``<... local declarations ...>`` is an optional set of type, function
and extended primitives declarations. Type declarations can be used to
define records and variants (sum-types), described later in this
documentation.

An optional initial storage or storage initializer can be given with
``let%init storage``. When deploying a Liquidity contract, if the
storage is not constant it is evaluated in the head context.

Note that types, values, entry points and values definitions can be
written in any order as long as they are defined before their use
(forward references are forbidden).

Basic Types and Values
----------------------

Types in Liquidity are monomorphic. They are all inherited from
Michelson, except for algebraic data types and records, that are
translated to Michelson types.

:love:`Only with love` Structured types are kept when compiling to Love.

Basic Types
~~~~~~~~~~~

The built-in base types are:

- ``unit``: whose only constructor is ``()``
- ``bool``: Booleans
- ``int``: Unbounded integers
- ``nat``: Unbounded naturals (positive integers)
- ``dun``: The type of amounts
- ``string``: character strings
- ``bytes``: bytes sequences
- ``timestamp``: dates and timestamps
- ``key``: cryptographic keys
- ``key_hash``: hashes of cryptographic keys
- ``signature``: cryptographic signatures
- ``operation``: type of operations, can only be constructed
- ``address``: abstract type of contract addresses
- ``chain_id``: abstract type for chain ids

Composite Types
~~~~~~~~~~~~~~~

Types can be composed using the following type operators:

- tuples: noted ``t1 * t2``, ``t1 * t2 * t3``, etc.
- functions: ``'a -> 'b`` is the type of functions from ``'a`` to
  ``'b``, equivalent to ``('a, 'b) lambda``.

and the following predefined combinators:
  
- lists: ``'a list`` is the type of lists of elements in ``'a``
- sets: ``'a set`` is the type of sets of elements in ``'a`` (``'a`` must be a comparable type)
- maps: ``('key, 'val) map`` is the type of maps whose keys are of type
  ``'key``, a comparable type, and values of type ``'val``;
- big maps: ``('key, 'val) big_map`` is the type of lazily
  deserialized maps whose keys are of type ``'key`` (a comparable
  type) and values of type ``'val``;
- contracts: ``S.instance`` is the type of contracts (instances) of signature
  ``S`` (see `Contract Types and Signatures`_);
  
and the predefined algebraic data types:

- option type: ``'a option = None | Some of 'a``
- variant type: ``('a, 'b) variant = Left of 'a | Right of 'b``

Record and variant types must be declared beforehand and are referred
to by their names.

User defined types can be parameterized by type variables. See
`Polymorphism`_ for the specifics and limitations.

Contract Handles and View Types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Contracts can either be manipulated (stored, passed as argument to a
function or a parameter to another contract) as *addresses* (this is a
representation for an untyped contract, whose signature is unknown) or
as *typed values*. Contracts can be freely converted to one type or
the other (the internal representation during execution remains
identical, but extra checks are performed when going from an address
to a typed contract). These typed values mention only a subset of the
contract signature. In fact they must mention only a single entry
point or view in the signature, this is why we call them *handles* (to
a specific entry point).

There are two kinds of types for these handles: handles to entry
points and handles to views.

Handles to entry points need only mention the type of the parameter as
such::

  %[handle 'parameter]

:love:`Only with love` Handles to views must mention the view name, the parameter type and
the return type::

  %[view (view_name : 'parameter -> 'return)]

These special types can be used anywhere a type is required.

Follow these links for the conversion primitives:

- :ref:`Converting handles addresses <handle_to_address>` : ``Contract.address``.
- :ref:`Converting addresses to entry point handles <handle_entry>`:
  ``[%handle: val%entry : ...]`` or ``[%handle C.entry]``.
- :love:`Only with love` :ref:`Converting addresses to view handles <handle_view>`:
  ``[%handle: val%view : ...]`` or ``[%view C.view]``.


Constant Values
~~~~~~~~~~~~~~~

The unique constructor of type ``unit`` is ``()``.

The two Booleans (``bool``) constants are:

* ``true``
* ``false``

As in Michelson, there are different types of integers:

* ``int`` : an unbounded integer, positive or negative, simply written
  ``0``, ``1``, ``2``, ``-1``, ``-2``, ...
* ``nat`` : an unbounded positive integer, written either with a ``p``
  suffix (``0p``, ``12p``, etc.) or as an integer with a type coercion
  ( ``(0 : nat)`` ).
* ``dun`` : an unbounded positive float of DUNs, written either
  with a ``DUN`` (or ``dun``) suffix (``1.00DUN``, etc.) or as a string with type
  coercion (``("1.00" : dun)``).

Strings (``string``) are delimited by the characters ``"`` and ``"``.

Bytes (``bytes``) are sequences of hexadecimal pairs preceeded by ``0x``, for
instance:

* ``0x``
* ``0xabcdef``

Timestamps (``timestamp``) are written in ISO 8601 format, like in Michelson:

* ``2015-12-01T10:01:00+01:00``

Keys, key hashes and signatures are base58-check encoded, the same as in Michelson:

* ``dn1HieGdCFcT8Lg9jDANfEGbJyt6arqEuSJb`` is a key hash (``key_hash``)
* ``edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN`` is a public
  key (``key``)
*
  ``edsigedsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMrCi5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7`` is a signature (``signature``)

There are also three types of collections: lists, sets and
maps. Constants collections can be created directly:

* Lists: ``["x"; "y"]`` for a ``string list``;
* Sets: ``Set [1; 2; 3; 4]`` for an ``int set``;
* Maps: ``Map [1, "x"; 2, "y"; 3, "z"]`` for a ``(int, string) map``;
* Big maps: ``BigMap [1, "x"; 2, "y"; 3, "z"]`` for a ``(int, string) big_map``

Options (``option``) can be defined with:

* An empty option: ``None``
* A valued option: ``Some 3``

Variants (``variant``) can be defined with:

* Left alternative: ``Left "hello"``
* Right alternative: ``Right 3``

for a ``(string, int) variant``.

The ``variant`` type is not supposed to be used by programmers, who
can defined their own algebraic data types. Instead, ``variant`` is
used when decompiling Michelson code.

It is also possible to coerce some constants between their inferred
type and another compatible type, using the notation
``( CONSTANT : NEWTYPE )``:

* A ``string`` can be coerced to ``dun`` (the string must contain an
  integer in mudun à la Michelson), ``timestamp``, ``key``,
  ``address``, ``_ contract``, ``key_hash`` and ``signature``.
* A ``bytes`` can be coerced to ``address``, ``_.instance``, ``key``,
  ``key_hash`` and ``signature``.
* An constant ``address`` can be coerced to a contract handle.
* A constant contract handle can be coerced to ``address``.
* A ``key_hash`` can be coerced to an ``address`` and a contract
  handle (to entry point ``default`` of parameter type ``unit``).

Starting with version ``0.5``, constant values such as ``[]``,
``Map``, ``Set``, ``None`` do not need to be annotated with their type
anymore. It will be inferred (when possible), see `Type inference`_).

Pure (not closures) lambdas are also constants in Liquidity.

* For instance ``fun (x : int) -> x + 1`` can be used anywhere that a
  constant of type ``int -> int`` is required.

Predefined Primitives
---------------------

There are two kinds of primitives in the language:

* **Prefix primitives** are used by putting the primitive before the
  arguments: ``prim x y z``. All alphanumerical primitives are prefix
  primitives, except ``lor``, ``lxor``, ``mod``, ``land``, ``lsl``,
  ``lsr`` and ``asr``.
* **Infix primitves** are used by putting the primitive between the
  arguments: ``x prim y``. Infix primitives are always operators
  (``+``, ``-``, etc.).

When the type of a primitive is specified, we extend the notation for
functions like this:

* ``TYPE_ARG -> TYPE_RESULT`` for a primitive with one argument
* ``TYPE_ARG1 -> TYPE_ARG2 -> TYPE_RESULT`` for a primitive with two arguments

Extended Primitives
~~~~~~~~~~~~~~~~~~~

:michelson:`Only with michelson`

Additional prefix Michelson primitives can be added to the language
through a local declaration as follows:

``external prim_name : TYPE1 -> ... -> TYPE_ARG1 -> ... -> TYPE_RESULT = "MINST" FLAGS``

Such declaration takes as input an arbitrary number of type arguments
(``TYPE1 -> ...``) of the form ``[%type: 'a]``, where ``'a`` is the
variable bound to the type.

Then follows an arbitrary (but non-null) number of typed arguments
(``TYPE_ARG1 -> ...``) of the form ``[%stack: TYPE]``, where ``TYPE``
corresponds to any Michelson type, possibly containing one or more of
the type variables introduced previously. Here, ``%stack`` means the
argument resides on the stack. It is mandatory for all arguments,
except when declaring a primitive that takes no argument, in which
case it takes a single argument of type ``unit``, without the
``%stack`` specifier (``[%stack: unit]`` would instead mean
that the primitive takes a unit value from the stack).

The result type (``TYPE_RESULT``) is specified using the same form as
arguments, i.e. ``[%stack: TYPE]``, where a bare ``unit`` indicates
a primitive that does not produce any value on the stack. It is
also possible to specify that the primitive returns several
values on the stack using a tuple notation :
``[%stack: TYPE1] * [%stack: TYPE2] * ...``. In this case, every
component of the tuple must have a ``%stack`` specifier and will
occupy a different stack cell. All the values will be assembled
into an actual tuple before being returned to Liquidity.

``MINST`` is the actual Michelson instruction to generate and will
be written as-is in the output file, followed by the given type
arguments, if any.

``FLAGS`` allows to give additional information about the primitive.
Currently, the only supported flag is ``[@@effect]``, which specifies
that the primitive may have side-effects. This prevents calls to
this primitive from being inlined or eliminated when the return
value is not used.

A call to an extended primitive is then performed as follows:

``prim_name TYPE1 ... ARG1 ...``

After the primitive name, a number of type arguments (``TYPE1 ...``)
of the form ``[%type: TYPE]`` may be given (if the primitive has
been declared to take type arguments), where ``TYPE`` is any
Michelson type. Then follow the actual arguments (``ARG1 ...``).


Comparison between values
~~~~~~~~~~~~~~~~~~~~~~~~~

All values are not comparable. Only two values of the following types
can be compared with each other:

* ``bool``
* ``int``
* ``nat``
* ``dun``
* ``string``
* ``bytes``
* ``timestamp``
* ``key_hash``
* ``address``

The following comparison operators are available:

* ``=`` : equal
* ``<>`` : not-equal
* ``<`` : strictly less
* ``<=`` : less or equal
* ``>`` : strictly greater
* ``>=`` : greater or equal

There is also a function ``compare : 'a -> 'a -> int`` to compare two
values and return an integer, as follows. ``compare x y``

* returns 0 if ``x`` and ``y`` are equal
* returns a strictly positive integer if ``x > y``
* returns a strictly negative integer if ``x < y``

The ``Current`` module
~~~~~~~~~~~~~~~~~~~~~~

* ``Current.balance: unit -> dun``: returns the balance of the current
  contract. The balance contains the amount of dun that was sent by
  the current operation. It is translated to ``BALANCE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc1.liq
  .. literalinclude:: ../../../../tests/doc/doc1.liq

* ``Current.time: unit -> timestamp``: returns the timestamp of the
  block in which the transaction is included. This value is chosen by
  the baker that is including the transaction, so it should not be
  used as a reliable source of alea.  It is translated to ``NOW`` in
  Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc2.liq
  .. literalinclude:: ../../../../tests/doc/doc2.liq

* ``Current.amount: unit -> dun``: returns the amount of dun
  transferred by the current operation (standard or internal
  transaction). It is translated to ``AMOUNT`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc3.liq
  .. literalinclude:: ../../../../tests/doc/doc3.liq

* ``Current.source: unit -> address``: returns the address that
  initiated the current top-level transaction in the blockchain. It is
  the same one for all the transactions resulting from the top-level
  transaction, standard and internal. It is the address that paid the
  fees and storage cost, and signed the operation on the
  blockchain. It is translated to ``SOURCE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc5.liq
  .. literalinclude:: ../../../../tests/doc/doc5.liq

* ``Current.sender: unit -> address``: returns the address that
  initiated the current transaction. It is the same as the source for
  the top-level transaction, but it is the originating contract for
  internal operations. It is translated to ``SENDER`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc6.liq
  .. literalinclude:: ../../../../tests/doc/doc6.liq

* ``failwith`` or ``Current.failwith: 'a -> 'b``: makes the current
  transaction and all its internal transactions fail. No modification
  is done to the context. The argument can be any value (often a
  string and some argument), the system will display it to explain why
  the transaction failed.

  .. tryliquidity:: ../../../../tests/doc/doc7.liq
  .. literalinclude:: ../../../../tests/doc/doc7.liq

* ``Current.block_level: unit -> nat``: returns the level of the block
  in which the transaction is included.

  .. tryliquidity:: ../../../../tests/doc/doc75.liq
  .. literalinclude:: ../../../../tests/doc/doc75.liq

* ``Current.collect_call: unit -> bool``: returns ``true`` if the
  current call is a collect call..

  .. tryliquidity:: ../../../../tests/doc/doc76.liq
  .. literalinclude:: ../../../../tests/doc/doc76.liq


Operations on tuples
~~~~~~~~~~~~~~~~~~~~

* ``get t n``, ``Array.get t n`` and ``t.(n)`` where ``n`` is a
  constant positive-or-nul int: returns the ``n``-th element of the
  tuple ``t``. Tuples are translated to Michelson by pairing on the
  right, i.e. ``(a,b,c,d)`` becomes ``(a, (b, (c, d)))``. In this
  example, ``a`` is the ``0``-th element.

  .. tryliquidity:: ../../../../tests/doc/doc8.liq
  .. literalinclude:: ../../../../tests/doc/doc8.liq

* ``set t n x``, ``Array.set t n x`` and ``t.(n) <- x`` where ``n`` is
  constant positive-or-nul int: returns the tuple where the ``n``-th element
  has been replaced by ``x``.

  .. tryliquidity:: ../../../../tests/doc/doc9.liq
  .. literalinclude:: ../../../../tests/doc/doc9.liq

Operations on numeric values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* ``+``: Addition. With the following types:
  
  * ``dun -> dun -> dun``
  * ``nat -> nat -> nat``
  * ``int|nat -> int|nat -> int``
  * ``timestamp -> int -> timestamp``
  * ``int -> timestamp -> timestamp``
    
    It is translated to ``ADD`` in Michelson.
    
* ``-``: Substraction. With the following types:
  
  * ``dun -> dun -> dun``
  * ``int|nat -> int|nat -> int``
  * ``timestamp -> int -> timestamp``
  * ``timestamp -> timestamp -> int``
  * ``int|nat -> int`` (unary negation)
  
    It is translated to ``SUB`` (or ``NEG`` for unary negation) in
    Michelson.

* ``*``: Multiplication. With the following types:

  * ``nat -> dun -> dun``
  * ``dun -> nat -> dun``
  * ``nat -> nat -> nat``
  * ``nat|int -> nat|int -> int``

    It is translated to ``MUL`` in Michelson.

    .. tryliquidity:: ../../../../tests/doc/doc10.liq
    .. literalinclude:: ../../../../tests/doc/doc10.liq


* ``/``: Euclidian division. With the following types:

  * ``nat -> nat -> ( nat * nat ) option``
  * ``int|nat -> int|nat -> ( int *  nat ) option``
  * ``dun -> nat -> ( dun * dun ) option``
  * ``dun -> dun -> ( nat * dun ) option``
  
    It is translated to ``EDIV`` in Michelson.

    .. tryliquidity:: ../../../../tests/doc/doc11.liq
    .. literalinclude:: ../../../../tests/doc/doc11.liq

* ``~-``: Negation. Type: ``int|nat -> int``
  
    It is translated to ``NEG`` in Michelson.
  
* ``lor``, ``or`` and ``||``: logical OR with the following types:

  * ``bool -> bool -> bool``
  * ``nat -> nat -> nat``
  
    It is translated to ``OR`` in Michelson.
    
* ``&``, ``land`` and ``&&``: logical AND with the following types:

  * ``bool -> bool -> bool``
  * ``nat|int -> nat -> nat``
  
    It is translated to ``AND`` in Michelson.

* ``lxor``, ``xor``: logical exclusive OR with the following types:

  * ``bool -> bool -> bool``
  * ``nat -> nat -> nat``
  
    It is translated to ``XOR`` in Michelson.
    
* ``not``: logical NOT

  * ``bool -> bool``
  * ``nat|int -> int`` (two-complement with sign negation)

    It is translated to ``NOT`` in Michelson.

* ``abs``: Absolute value. Type ``int -> int``

    It is translated to ``ABS; INT`` in Michelson.

* ``is_nat``: Maybe positive. Type ``int -> nat option``. It is
  translated to ``IS_NAT`` in Michelson.

    Instead of using ``is_nat``, it is recommended to use a specific form
    of pattern matching:

    .. tryliquidity:: ../../../../tests/doc/doc12.liq
    .. literalinclude:: ../../../../tests/doc/doc12.liq

* ``int``: To integer. Type ``nat -> int``

    It is translated to ``INT`` in Michelson.

* ``>>`` and ``lsr`` : Logical shift right. Type ``nat -> nat -> nat``

    It is translated to ``LSR`` in Michelson.

* ``<<`` and ``lsl`` : Logical shift left. Type ``nat -> nat -> nat``

    It is translated to ``LSL`` in Michelson.


Operations on contracts
~~~~~~~~~~~~~~~~~~~~~~~

* ``Contract.call: dest:(address | [%handle 'a] | 'S.instance) -> amount:dun ->
  ?entry:<entry_name> -> parameter:'a -> operation``. Forge an internal
  contract call. It is translated to ``TRANSFER_TOKENS`` in Michelson.
  Arguments can be labeled, in which case they can be given
  in any order. The entry point name is optional (``default`` by
  default). The destination is either a contract handle to an entry
  point, a contract instance, or an address (in the last two cases, an
  entry point must be specified).

  .. tryliquidity:: ../../../../tests/doc/doc13.liq
  .. literalinclude:: ../../../../tests/doc/doc13.liq

* ``<c.entry>: 'parameter -> amount:dun -> operation``. Forge an
  internal contract call. It is translated to ``TRANSFER_TOKENS`` in
  Michelson.  The amount argument can be labeled, in which case it can
  appear before the parameter. ``c`` is either a contract handle (of
  type ``[%handle 'parameter]``) or an address.

  .. tryliquidity:: ../../../../tests/doc/doc14.liq
  .. literalinclude:: ../../../../tests/doc/doc14.liq

* ``Account.transfer: dest:key_hash -> amount:dun ->
  operation``. Forge an internal transaction to the implicit (_i.e._
  default) account contract of ``dest``. Arguments can be labeled, in
  which case they can be given in any order. *The resulting operation
  cannot fail (if the transfer amount leaves more than 0.257DUN on both
  contracts).*

  .. tryliquidity:: ../../../../tests/doc/doc15.liq
  .. literalinclude:: ../../../../tests/doc/doc15.liq

* ``Contract.view: dest:(address | [%view (view_name : 'a -> 'b)]) ->
  ?view:<view_name> -> parameter:'a -> 'b``. Call a specific contract
  view.  Arguments can be labeled, in which case they can be given in
  any order. The view name is mandatory if an ``dest`` is an address
  (it is not required if ``dest`` is a view handle). The destination
  is either a view handle or an address (in which case, an view name
  point must be specified).
  :love:`Only with love`

  .. tryliquidity:: ../../../../tests/test_view.liq
  .. literalinclude:: ../../../../tests/test_view.liq
     :lines: 8-12

* ``Account.default: key_hash -> [%handle unit]``. Returns a contract
  handle to the ``default`` entry point of the implicit account
  associated to the given ``key_hash``. Transfers to it cannot
  fail. It is translated to ``IMPLICIT_ACCOUNT`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc17.liq
  .. literalinclude:: ../../../../tests/doc/doc17.liq

* ``Contract.set_delegate: key_hash option -> operation``. Forge a
  delegation operation for the current contract. A ``None`` argument
  means that the contract should have no delegate (it falls back to
  its manager). The delegation operation will only be executed in an
  internal operation if it is returned at the end of the entry point
  definition. It is translated to ``SET_DELEGATE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc18.liq
  .. literalinclude:: ../../../../tests/doc/doc18.liq

* .. _handle_to_address:

  ``Contract.address: [%handle 'a] -> address`` . Returns the address of
  a contract. The returned address can be converted to any entry point
  (or view) handle of the contract (contrary to ``Contract.untype``).

  .. tryliquidity:: ../../../../tests/doc/doc19.liq
  .. literalinclude:: ../../../../tests/doc/doc19.liq

* ``Contract.untype: [%handle 'a] -> address``. Returns the address
  corresponding to an untype version of the contract handle.

  .. tryliquidity:: ../../../../tests/doc/doc16.liq
  .. literalinclude:: ../../../../tests/doc/doc16.liq

* ``C.at: address -> C.instance option``. Returns the contract
  associated with the address and of type ``C``, if any.
  :love:`Only with love`

  .. tryliquidity:: ../../../../tests/doc/doc20.liq
  .. literalinclude:: ../../../../tests/doc/doc20.liq
     :lines: 13-18

* .. _handle_entry:

  ``[%handle: val%entry <entry_name> : 'a ] : address -> [%handle 'a]
  option``. Returns a contract handle to the entry point
  ``<entry_name>`` if the contract at the specified address has an
  entry point named ``<entry_name>`` of parameter type ``'a``. If no
  such entry point exists or the parameter type is different then this
  function returns ``None``. It is translated to ``CONTRACT`` in
  Michelson. For any contract or contract type ``C``, you can also use
  the syntactic sugar ``[%handle C.<entry_name>]`` instead.

  .. tryliquidity:: ../../../../tests/doc/doc20.liq
  .. literalinclude:: ../../../../tests/doc/doc20.liq
     :lines: 1-11

* .. _handle_view:

  ``[%handle: val%view <view_name> : 'a -> 'b] : address -> [%view
  (view_name : 'a -> 'b)] option``.  Returns a contract view handle to
  the view ``<view_name>`` if the contract at the specified address
  has an view named ``<view_name>`` of parameter type ``'a`` and
  return type ``'b``. If no such view exists or the parameter or
  return types are different then this function returns ``None``. For
  any contract or contract type ``C``, you can also use the syntactic
  sugar ``[%view C.<view_name>]`` instead.
  :love:`Only with love`

  .. tryliquidity:: ../../../../tests/test_view.liq
  .. literalinclude:: ../../../../tests/test_view.liq
     :lines: 14-19

* ``Contract.get_balance: [%handle 'a] -> dun``. Returns the balance
  of the contract.

  .. tryliquidity:: ../../../../tests/doc/doc77.liq
  .. literalinclude:: ../../../../tests/doc/doc77.liq

* ``Contract.is_implicit: [%handle unit] -> key_hash option``. Returns
  the key hash of a contract handle if it is an implicit one,
  otherwise, returns ``None``.

  .. tryliquidity:: ../../../../tests/doc/doc74.liq
  .. literalinclude:: ../../../../tests/doc/doc74.liq


* ``[%handle Self.<entry>] -> [%handle 'a]``. Returns a handle
  to the entry point ``<entry>`` of the currently executing
  contract. It is translated to ``SELF`` in Michelson. You can use the
  syntactic sugar ``Contract.self ()`` for ``[%handle Self.default]``.

  .. tryliquidity:: ../../../../tests/doc/doc21.liq
  .. literalinclude:: ../../../../tests/doc/doc21.liq

* ``Contract.create: delegate:key_hash option -> amount:dun -> storage:'storage
  -> code:(contract _) -> (operation, address)``. Forge an operation
  to originate a contract with code. The contract is only created when
  the operation is executed, so it must be returned by the
  transaction. Note that the code must be specified as a contract
  structure (inlined or not). It is translated to ``CREATE_CONTRACT``
  in Michelson.  ``Contract.create delegate_opt initial_amount
  initial_storage (contract C)`` forges an
  an origination operation for contract ``C`` with
  optional delegate ``delegate``, initial
  balance ``initial_amount`` and initial storage
  ``initial_storage``. Arguments can be named and put in any order.

  .. tryliquidity:: ../../../../tests/doc/doc22.liq
  .. literalinclude:: ../../../../tests/doc/doc22.liq

  The contract code parameter is a *first class* value, it can be
  written inlined as above, or equivalently the contract code can be
  referred to by its name (in scope) as below:

  .. tryliquidity:: ../../../../tests/doc/doc23.liq
  .. literalinclude:: ../../../../tests/doc/doc23.liq

    
Cryptographic operations
~~~~~~~~~~~~~~~~~~~~~~~~
              
* ``Crypto.blake2b: bytes -> bytes``. Computes the cryptographic hash of
  a bytes with the cryptographic Blake2b function. It is translated to
  ``BLAKE2B`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc24.liq
  .. literalinclude:: ../../../../tests/doc/doc24.liq

* ``Crypto.sha256: bytes -> bytes``. Computes the cryptographic hash
  of a bytes with the cryptographic Sha256 function. It is translated
  to ``SHA256`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc25.liq
  .. literalinclude:: ../../../../tests/doc/doc25.liq

* ``Crypto.sha512: bytes -> bytes``. Computes the cryptographic hash of
  a bytes with the cryptographic Sha512 function. It is translated to ``SHA512`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc26.liq
  .. literalinclude:: ../../../../tests/doc/doc26.liq
  
* ``Crypto.hash_key: key -> key_hash``. Hash a public key and encode
  the hash in B58check. It is translated to ``HASH_KEY`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc27.liq
  .. literalinclude:: ../../../../tests/doc/doc27.liq

* ``Crypto.check: key -> signature -> bytes -> bool``. Check that the
  signature corresponds to signing the (Blake2b hash of the) sequence
  of bytes with the public key. It is translated to
  ``CHECK_SIGNATURE`` in Michelson. Signatures generated by
  ``dune-client sign bytes ...`` can be checked this way.

  .. tryliquidity:: ../../../../tests/doc/doc28.liq
  .. literalinclude:: ../../../../tests/doc/doc28.liq
  
Operations on bytes
~~~~~~~~~~~~~~~~~~~
              
* ``Bytes.pack: 'a -> bytes``. Serialize any data to a binary
  representation in a sequence of bytes. It is translated to ``PACK``
  in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc29.liq
  .. literalinclude:: ../../../../tests/doc/doc29.liq

* ``Bytes.unpack: bytes -> 'a option``. Deserialize a sequence of
  bytes to a value from which it was serialized. The expression must
  be annotated with the (option) type that it should return. It is
  translated to ``UNPACK`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc30.liq
  .. literalinclude:: ../../../../tests/doc/doc30.liq

* ``Bytes.length`` or ``Bytes.size: bytes -> nat``. Return the size of
  the sequence of bytes. It is translated to ``SIZE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc31.liq
  .. literalinclude:: ../../../../tests/doc/doc31.liq

* ``Bytes.concat: bytes list -> bytes``. Append all the sequences of
  bytes of a list into a single sequence of bytes. It is translated to
  ``CONCAT`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc32.liq
  .. literalinclude:: ../../../../tests/doc/doc32.liq

* ``Bytes.slice`` or ``Bytes.sub" of type ``nat -> nat -> bytes ->
  bytes option``. Extract a sequence of bytes within another sequence
  of bytes. ``Bytes.slice start len b`` extracts the bytes subsequence
  of ``b`` starting at index ``start`` and of length ``len``. A return
  value ``None`` means that the position or length was invalid. It
  is translated to ``SLICE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc33.liq
  .. literalinclude:: ../../../../tests/doc/doc33.liq

* ``( @ ) : bytes -> bytes -> bytes``. Append two sequences of bytes into a
  single sequence of bytes. ``b1 @ b2`` is syntactic sugar for ``Bytes.concat
  [b1; b2]``.

  .. tryliquidity:: ../../../../tests/doc/doc34.liq
  .. literalinclude:: ../../../../tests/doc/doc34.liq

Operations on strings
~~~~~~~~~~~~~~~~~~~~~

A string is a fixed sequence of characters. They are restricted to the
printable subset of 7-bit ASCII, plus some escaped characters (``\n``,
``\t``, ``\b``, ``\r``, ``\\``, ``\"``).


* ``String.length`` or ``String.size`` of type ``string ->
  nat``. Return the size of the string in characters. It is translated
  to ``SIZE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc35.liq
  .. literalinclude:: ../../../../tests/doc/doc35.liq

* ``String.slice`` or ``String.sub`` with type ``nat -> nat -> string
  -> string option``. ``String.sub start len s`` returns a substring
  of a string ``s`` at the given starting at position ``len`` with the
  specified length ``len``, or ``None`` if invalid. It is
  translated to ``SLICE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc36.liq
  .. literalinclude:: ../../../../tests/doc/doc36.liq

* ``String.concat: string list -> string``. Append all strings of a
  list into a single string. It is translated to ``CONCAT`` in
  Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc37.liq
  .. literalinclude:: ../../../../tests/doc/doc37.liq

* ``( @ ) : string -> string -> string``. Append two strings into a single
  string. ``s1 @ s2`` is syntactic sugar for ``String.concat
  [s1; s2]``.

  .. tryliquidity:: ../../../../tests/doc/doc38.liq
  .. literalinclude:: ../../../../tests/doc/doc38.liq


Operations on lambdas
~~~~~~~~~~~~~~~~~~~~~

* ``Lambda.pipe`` or ``( |> )`` of type ``'a -> ('a -> 'b) -> 'b`` or ``'a
  -> ('a,'b) closure -> 'b``. Applies a function or closure to its
  argument.

* ``( @@ ) : ('a -> 'b) -> 'a -> 'b`` is also function application.

  .. tryliquidity:: ../../../../tests/doc/doc39.liq
  .. literalinclude:: ../../../../tests/doc/doc39.liq

Operations on lists              
~~~~~~~~~~~~~~~~~~~

Lists are immutable data structures containing values (of any type)
that can only be accessed in a sequential order. Since they are
immutable, all **modification** primitives return a new list, and the
list given in argument is unmodified.

* ``( :: ) : 'a -> 'a list -> 'a list`` Add a new element at the head
  of the list. The previous list becomes the tail of the new list.  It
  is translated to ``CONS`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc40.liq
  .. literalinclude:: ../../../../tests/doc/doc40.liq

* ``List.rev : 'a list -> 'a list`` Return the list in the reverse order.

  .. tryliquidity:: ../../../../tests/doc/doc41.liq
  .. literalinclude:: ../../../../tests/doc/doc41.liq

* ``List.length`` or ``List.size: 'a list -> nat``. Return the length
  of the list. It is translated to ``SIZE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc42.liq
  .. literalinclude:: ../../../../tests/doc/doc42.liq

* ``List.iter: ('a -> unit) -> 'a list -> unit``. Iter the function on
  all the elements of a list. Since no value can be returned, it can
  only be used for side effects, i.e. to fail the transaction.  It is
  translated to ``ITER`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc43.liq
  .. literalinclude:: ../../../../tests/doc/doc43.liq

* ``List.fold: ('elt * 'acc -> 'acc) -> 'elt list -> 'acc ->
  'acc``. Iter on all elements of a list, while modifying an
  accumulator. It is translated to ``ITER`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc44.liq
  .. literalinclude:: ../../../../tests/doc/doc44.liq

* ``List.map: ('a -> 'b) -> 'a list -> 'b list``. Return a
  list with the result of applying the function on each element of the
  list. It is translated to ``MAP`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc45.liq
  .. literalinclude:: ../../../../tests/doc/doc45.liq

* ``List.map_fold: ('a * 'acc -> 'b * 'acc) -> 'a list -> 'acc
  -> 'b list * 'acc``.  Return a list with the result of applying the
  function on each element of the list, plus an accumulator. It is
  translated to ``MAP`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc46.liq
  .. literalinclude:: ../../../../tests/doc/doc46.liq

Operations on sets
~~~~~~~~~~~~~~~~~~

Sets are immutable data structures containing unique values (a
comparable type). Since they are immutable, all **modification**
primitives return a new updated set, and the set given in argument is
unmodified.

* ``Set.update: 'a -> bool -> 'a set -> 'a set``. Update a set for a
  particular element. If the boolean is ``true``, the element is
  added. If the boolean is ``false``, the element is removed. It is
  translated to ``UPDATE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc47.liq
  .. literalinclude:: ../../../../tests/doc/doc47.liq

* ``Set.add: 'a -> 'a set -> 'a set`` . Add an element to a set, if
  not present. ``Set.add x s`` is syntactic sugar for ``Set.update
  x true s``.

  .. tryliquidity:: ../../../../tests/doc/doc48.liq
  .. literalinclude:: ../../../../tests/doc/doc48.liq

* ``Set.remove: 'a -> 'a set -> 'a set``. Remove an element to a
  set, if present. ``Set.remove x s`` is syntactic sugar for ``Set.update
  x false s``.

  .. tryliquidity:: ../../../../tests/doc/doc49.liq
  .. literalinclude:: ../../../../tests/doc/doc49.liq

* ``Set.mem: 'a -> 'a set -> bool``. Return ``true`` if the element is
  in the set, ``false`` otherwise. It is translated to ``MEM`` in
  Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc50.liq
  .. literalinclude:: ../../../../tests/doc/doc50.liq

* ``Set.cardinal`` or ``Set.size`` with type ``'a set -> nat``. Return
  the number of elements in the set. It is translated to ``SIZE`` in
  Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc51.liq
  .. literalinclude:: ../../../../tests/doc/doc51.liq

* ``Set.iter: ('ele -> unit) -> 'ele set -> unit``. Apply a function
  on all elements of the set. Since no value can be returned, it can
  only be used for side effects, i.e. to fail the transaction.  It is
  translated to ``ITER`` in Michelson.
  
  .. tryliquidity:: ../../../../tests/doc/doc52.liq
  .. literalinclude:: ../../../../tests/doc/doc52.liq

Operations on maps
~~~~~~~~~~~~~~~~~~

Maps are immutable data structures containing associations between
keys (a comparable type) and values (any type). Since they are
immutable, all **modification** primitives return a new updated map,
and the map given in argument is unmodified.
  
* ``Map.add: 'key -> 'val -> ('key, 'val) map -> ('key, 'val)
  map``. Return a map with a new association between a key and a
  value. If an association previously existed for the same key, it is
  not present in the new map. It is translated with ``UPDATE`` in
  Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc56.liq
  .. literalinclude:: ../../../../tests/doc/doc56.liq

* ``Map.remove: 'key -> ('key,'val) map -> ('key,'val) map``. Return a
  map where any associated with the key has been removed. It is
  translated with ``UPDATE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc57.liq
  .. literalinclude:: ../../../../tests/doc/doc57.liq

* ``Map.find: 'key -> ('key,'val) map -> 'val option``. Return the
  value associated with a key in the map. It is translated to ``GET``
  in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc58.liq
  .. literalinclude:: ../../../../tests/doc/doc58.liq

* ``Map.update: 'key -> 'val option -> ('key,'val) map -> ('key,'val)
  map``. Return a new map where the association between the key and
  the value has been removed (case ``None``) or added/updated (case
  ``Some v``). It is translated to ``UPDATE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc59.liq
  .. literalinclude:: ../../../../tests/doc/doc59.liq

* ``Map.mem: 'key -> ('key, 'val) map -> bool``. Return ``true`` if an
  association exists in the map for the key, ``false`` otherwise. It
  is translated to ``MEM`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc60.liq
  .. literalinclude:: ../../../../tests/doc/doc60.liq

* ``Map.cardinal`` or ``Map.size`` with type ``('key,'val) map ->
  nat``. Return the number of associations (i.e. uniq keys) in the
  map. It is translated to ``SIZE`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc61.liq
  .. literalinclude:: ../../../../tests/doc/doc61.liq

* ``Map.iter: ('key * 'val -> unit) -> ('key,'val) map ->
  unit``. Apply a function on all associations in the map. Since no
  value can be returned, it can only be used for side effects, *i.e.* to
  fail the transaction. It is translated to ``ITER`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc62.liq
  .. literalinclude:: ../../../../tests/doc/doc62.liq

* ``Map.fold: (('key * 'val) * 'acc -> 'acc) -> ('key,'val) map ->
  'acc -> 'acc``. Apply a function on all associations of the map,
  updating and returning an accumulator. It is translated to ``ITER``
  in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc63.liq
  .. literalinclude:: ../../../../tests/doc/doc63.liq

* ``Map.map: ('key * 'src -> 'dst) -> ('key,'src) map -> ('key,'dst)
  map``. Apply a function on all associations of a map, and return a
  new map where keys are now associated with the return values of the
  function. It is translated to ``MAP`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc64.liq
  .. literalinclude:: ../../../../tests/doc/doc64.liq

* ``Map.map_fold: (('key * 'src) * 'acc -> 'dst * 'acc) -> ('key,'src)
  map -> 'acc -> ('key,'dst) map * 'acc``.  Apply a function on all
  associations of a map, returning both a new map and an updated
  accumulator. It is translated to ``MAP`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc65.liq
  .. literalinclude:: ../../../../tests/doc/doc65.liq

  
Operations on Big maps
~~~~~~~~~~~~~~~~~~~~~~

Big maps are a specific kind of maps, optimized for storing. They can
be updated incrementally and scale to a high number of associations,
whereas standard maps will have an expensive serialization and
deserialization cost. Big maps cannot be iterated and cannot have big
maps as their keys or as their elements.

* ``Map.find: 'key -> ('key,'val) big_map -> 'val option``. Return the
  value associated with a key in the map. It is translated to ``GET``
  in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc66.liq
  .. literalinclude:: ../../../../tests/doc/doc66.liq

* ``Map.mem: 'key -> ('key, 'val) big_map -> bool``. Return ``true`` if an
  association exists in the map for the key, ``false`` otherwise. It
  is translated to ``MEM`` in Michelson.

  .. tryliquidity:: ../../../../tests/doc/doc67.liq
  .. literalinclude:: ../../../../tests/doc/doc67.liq

* ``Map.update: 'key -> 'val option -> ('key,'val) big_map -> ('key,'val)
  big_map``. Return a new map where the association between the key and
  the value has been removed (case ``None``) or added/updated (case
  ``Some v``). It is translated to ``UPDATE`` in Michelson.

* ``Map.add: 'key -> 'val -> ('key, 'val) big_map -> ('key, 'val)
  big_map``. Syntactic sugar for ``Map.update (Some ...)``.

* ``Map.remove: 'key -> ('key,'val) big_map -> ('key,'val) big_map``.
  Syntactic sugar for ``Map.update None``.

  .. tryliquidity:: ../../../../tests/doc/doc68.liq
  .. literalinclude:: ../../../../tests/doc/doc68.liq

Operations on generic collections
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These primitives should not be used directly in Liquidity. They are
only used by the decompiler. They are automatically replaced during
typing by the corresponding primitive for the collection of the
argument (in either ``List``, ``Set``, ``Map``, ``String`` or
``Bytes``). However, they can be used to write some polymorphic code on
collections.

* ``Coll.update`` 
* ``Coll.mem``    
* ``Coll.find``   
* ``Coll.size``   
* ``Coll.concat`` 
* ``Coll.slice``  
* ``Coll.iter``   
* ``Coll.fold``   
* ``Coll.map``    
* ``Coll.map_fold``


The Modules and Contracts System
--------------------------------

The system described in this section allows to define several
contracts and modules in the same file, to reference contracts by
their names, and to call contracts defined in other files.

The notion of *contract and module structures* in Liquidity is a way
to define namespaces and to encapsulate types, values and contracts in
packages. These packages are called structures and are introduced with
the ``struct`` keyword. Modules, introduced with the keyword
``module``, can contain types and values but cannot contain any entry
points. Contracts are introduced with the keyword ``contract``, they
can contain types, values and must have *at least one* entry point.

Types in scope (defined before their use) can be referred to anywhere,
provided they are adequately qualified (with a dot ``.`` notation).

Values are exported outside the module or the contract by default,
which means they can be used by other modules and contracts. One can
annotate the value with ``[@private]`` to prevent exporting the value.

For instance the following example defines a module ``M`` with a type
``t`` and an exported function ``f``.

.. tryliquidity:: ../../../../tests/doc/doc73.liq
.. literalinclude:: ../../../../tests/doc/doc73.liq
   :lines: 1-4

The contract ``C`` can be defined as such. It uses the type ``t`` of
``M``, written ``M.t`` and the function ``f`` of ``M`` written
``M.f``. The function ``succ`` is exported and can be called with
``C.succ`` outside the contract, whereas ``prev`` cannot (the compiler
will complain that is does not know the symbol ``C.prev`` if we try to
use it elsewhere).

.. tryliquidity:: ../../../../tests/doc/doc73.liq
.. literalinclude:: ../../../../tests/doc/doc73.liq
   :lines: 6-16

The toplevel contract can use elements from either structures. Here we
use types and functions from both ``M`` and ``C`` and call the entry
point ``default`` of a contract instance of type ``C``.

.. tryliquidity:: ../../../../tests/doc/doc73.liq
.. literalinclude:: ../../../../tests/doc/doc73.liq
   :lines: 30-33

Module and Contract Aliases
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Modules and contracts can be arbitrarily nested and aliases can be
defined by simply giving the qualified name (instead of the whole structure).

.. tryliquidity:: ../../../../tests/doc/doc73.liq
.. literalinclude:: ../../../../tests/doc/doc73.liq
   :lines: 18-28

First Class Contract Structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Contracts structures (note we are not talking about *contract
instances* here) can also be used as first class values:

.. tryliquidity:: ../../../../tests/doc/doc23.liq
.. literalinclude:: ../../../../tests/doc/doc23.liq

**Handles** to contracts can be called with three different syntaxes:

- ``Contract.call ~dest:c ~amount:1DUN ~parameter:"hello"``
- ``Contract.call ~dest:c ~amount:1DUN ~entry:default ~parameter:"hello"``
- ``c.default "hello" ~amount:1DUN``

These calls are all equivalent when c is an address or a handle to the
default entry point.

Toplevel Contracts
~~~~~~~~~~~~~~~~~~

A contract defined at toplevel in a file ``path/to/my_contract.liq``
implicitly defines a contract structure named ``My_contract`` which
can be called in other Liquidity files.


Contract Types and Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A contract is a first class object in Liquidity only for the
instruction ``Contract.create``, while contract *handles* can be
used like any other values. Contract signatures are introduced with
the keyword ``sig`` and defined with the keyword ``contract type``::

  contract type S = sig
    type t1 = ...
    type t2 = ...
    val%entry entry1 : TYPE
    val%entry entry2 : TYPE
    val%entry default : TYPE
    val%view view1 : TYPE -> TYPE
    val%view view2 : TYPE -> TYPE
    ...
  end

A contract signature can contain:

- type declarations,
- declarations for the entry point signatures with the special keyword
  ``val%entry`` in which only the type parameter must be specified,
- declarations for the view signatures with the special keyword
  ``val%view`` whose type must be an arrow type of the parameter to
  the result type.

.. topic:: Only with Love
   :class: love

   The type of a contract (instance) whose signature is `C` is written
   ``C.instance``. Note that ``C`` must be declared as a contract or a
   contract signature beforehand if we want to declare values of type
   ``C.instance``.

   For example::

     type t = {
       counter : int;
       dest : C.instance;
     }

   is a record type with a contract field ``dest`` of signature ``C``.


Predefined Contract Signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The contract signature ``UnitContract`` is built-in, in Liquidity, and
stands for contracts with a single entry point ``default`` whose
parameter is of type ``unit``:

.. tryliquidity:: ../../../../tests/doc/doc70.liq
.. literalinclude:: ../../../../tests/doc/doc70.liq


Type inference and Polymorphism
-------------------------------

A new addition of version ``0.5`` of the Liquidity compiler is a type
inference algorithm (a variant of Hindley-Milner type inference) which
works in the presence of parametric types and polymorphic values
(functions) and can infer parametric types.

Type inference
~~~~~~~~~~~~~~

A consequence of this addition is that most type annotations in
Liquidity are now unnecessary, but can be used to restrict types or to
enforce a constraint. This makes programs more readable by removing
superfluous noise.

In particular, types of entry point parameters, storage initializer
parameters, constant values (like ``[]``, ``None``, *etc.*) and
functions are not necessary anymore.

The following example shows type inference at work.

.. tryliquidity:: ../../../../tests/doc/doc71.liq
.. literalinclude:: ../../../../tests/doc/doc71.liq

Polymorphism
~~~~~~~~~~~~

In general, values in Liquidity cannot be polymorphic: type variables
must (and will) be instantiated (by inference and
monomorphization). This restriction is inherited from
Michelson. However there is still a way to write *polymorphic
functions*. This is especially useful to write reusable
code. Polymorphic functions are transformed into several monomorphized
versions. For instance a function ``f : 'a option -> int`` will be
transformed into two functions ``f_i : int option`` and
``f_Ln : nat list option`` if it is used with both an integer argument
and a list of naturals argument in the code.

To make this extension even more useful, Liquidity allows user
declared type to be *parameterized* by one or more type
variables. Every type variable that appears in the type definition
must also appear in the type name declaration (on the left hand side).

The following example defines a record type ``('a, 'b) t`` with two
fields whose type are parameters. The function ``mk_t`` builds values
of type ``t`` with it argument. ``mk_t`` has the polymorphic type
``mk_t : ('a * 'b) -> ('a, b') t``.

.. tryliquidity:: ../../../../tests/doc/doc72.liq
.. literalinclude:: ../../../../tests/doc/doc72.liq

The type of storage cannot be polymorphic, however it can contain
*weak type variables* (like ``'_a``), which means they must be the
same for every instance (*i.e.* there can only be one instance of type
``storage``). For example writing ``type '_a storage = '_a`` allows
type storage to be inferred.

ReasonML Syntax
---------------

Liquidity supports two syntaxes:

* OCaml syntax (OCaml with Dune-specific changes)
* `ReasonML syntax <https://reasonml.github.io/>`_ (with Dune-specific changes)

ReasonML Compiler Arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, the compiler uses expects the OCaml syntax, and outputs
files in OCaml syntax. This behavior changes with the file extension
and with the ``--re`` argument.  Files that end in ``.reliq`` will be
parsed as ReasonML Liquidity files. The decompiler will ouptu files in
ReasonML syntax when given the flag ``-re``. If your file is
``test.reliq``, you can compile it using::

  liquidity test.reliq

You can also convert a file from one syntax to another, using the
``--convert FILE`` argument. For example, a file in OCaml-syntax can
be converted to ReasonML syntax::

  $ liquidity --convert test19.liq

    type storage = {
      key,
      hash: bytes,
      c: address,
    };

    let%init storage: storage = {
      key: 0x0085b1e4560f47f089d7b97aabcf46937a4c137a9c3f96f73f20c83621694e36d5,
      hash: 0xabcdef,
      c: KT1LLcCCB9Fr1hrkGzfdiJ9u3ZajbdckBFrF,
    };

    contract PlusOne = {
      type storage = int;

      type t =
        | A
        | B;

      let%init init_storage = (x: bool, y: int) =>
        if (x == false) {
          0;
        } else {
          y;
        };

      let%entry default = (_: unit, s) => ([], s + 1);
    };

    let%entry default = (sign: signature, storage) => {
      let x = PlusOne.A;
      switch (x) {
      | PlusOne.B => failwith()
      | _ => ()
      };
      let c = Contract.self();
      let key_hash = Crypto.hash_key(storage.key);
      if (key_hash == tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx) {
        Current.failwith();
      };
      if (key_hash
          == Crypto.hash_key(
               edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV,
             )) {
        Current.failwith();
      };
      let delegate = Some(key_hash);
      let spendable = Crypto.check(storage.key, sign, storage.hash);
      let amount = Current.amount();
      let amount =
        switch (amount / 2p) {
        | None => Current.failwith() /* not possible */
        | Some(qr) => qr
        };

      let delegatable = false;
      let _cocococ = [%handle PlusOne.default](storage.c);
      let _op1 = Self.default(sign, ~amount=0DUN);
      let (c_op, c_addr) =
        Contract.create(
          ~delegate,
          ~amount=amount[0],
          ~storage=9,
          (contract PlusOne),
        );

      let storage = storage.c = c_addr;
      ([c_op], storage);
    };

The same file can be converted back and forth::

  $ liquidity --convert test19.liq > test19.reliq
  $ liquidity --convert test19.reliq > test19.liq

Beware however that the conversion from ReasonML syntax back to the
OCaml one erases the comments.

ReasonML Syntax Extensions
~~~~~~~~~~~~~~~~~~~~~~~~~~

Liquidity borrows most of ReasonML syntax, with a few changes, similar to
the changes in the OCaml syntax:

* The ``module`` keyword is replaced by the ``contract`` keyword, to define
  contracts and contract signatures
* Dune-specific literals are available, such as ``12.2DUN``,
  ``dn1c35okrd97ZfiH6X2j8DiD3dSkCqVkGkZN``, etc.
* Tezos-specific literals are available, such as ``12.2tz``,
  ``tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx``, etc.

A good way to learn this syntax is to use the syntax conversion
argument of the compiler (``--convert FILE``).
