


Liquidity Reference
===================

Contract Format
---------------

All the contracts have the following form::

 [%%version 0.3]
 
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


The ``version`` statement tells the compiler in which version of
Liquidity the contract is written. The compiler will reject any
contract that has a version that it does not understand (too old, more
recent). We expect to reach version 1.0 at the launch of the Tezos
network.

The ``main`` function is the default entry point for the contract.
``let%entry`` is the construct used to declare entry points (there is
currently only one entry point, but there will be probably more in the
future).  The declaration takes two parameters with names
``parameter``, ``storage``, the arguments to the function. Their types must
always be specified. The return type of the function must also be
specified by a type annotation.

A contract always returns a pair ``(operations, storage)``, where
``operations`` is a list of internal operations to perform after
exectution of the contract, and ``storage`` is the final state of the
contract after the call. The type of the pair must match the type of a
pair where the first component is a list of opertations and the second
is the type of the argument ``storage`` of ``main``.

``<... local declarations ...>`` is an optional set of optional type and
function declarations. Type declarations can be used to define records
and variants (sum-types), described later in this documentation.

An optional initial storage or storage initializer can be given with
``let%init storage``. When deploying a Liquidity contract, if the
storage is not constant it is evaluated in the prevalidation context.

Basic Types and Values
----------------------

Types in Liquidity are monomorphic. They are all inherited from
Michelson, except for algebraic data types and records, that are
translated to Michelson types.

Basic Types
~~~~~~~~~~~

The built-in base types are:

- ``unit``: whose only constructor is ``()``
- ``bool``: Booleans
- ``int``: Unbounded integers
- ``nat``: Unbounded naturals (positive integers)
- ``tez``: The type of amounts
- ``string``: character strings
- ``bytes``: bytes sequences
- ``timestamp``: dates and timestamps
- ``key``: cryptographic keys
- ``key_hash``: hashes of cryptographic keys
- ``signature``: cryptographic signatures
- ``operation``: type of operations, can only be constructed
- ``address``: abstract type of contract addresses

Composite Types
~~~~~~~~~~~~~~~
  
Types can be composed using the following type operators:

- tuples: noted ``(t1 * t2 * t3)``
- functions: ``'a -> 'b`` is the type of functions from ``'a`` to ``'b``

and the following predefined combinators:
  
- lists: ``'a list`` is the type of lists of elements in ``'a``
- sets: ``'a set`` is the type of sets of elements in ``'a``
- maps: ``('a, 'b) map`` is the type of maps whose keys are of type
  ``'a`` and values of type ``'b``;
- big maps: ``('a, 'b) big_map`` is the type of lazily deserialized maps whose
  keys are of type ``'a`` and values of type ``'b``;
- contracts: ``'a contract`` for contracts whose parameter is of type ``'a``;
- lambdas: ``('a,'b) lambda`` for a function from type ``'a`` to type ``'b``.
  
and the predefined algebraic data types:

- option type: ``'a option = None | Some of 'a``
- variant type: ``('a, 'b) variant = Left of 'a | Right of 'b``

Record and variant types must be declared beforehand and are referred
to by their names.


Constants Values
~~~~~~~~~~~~~~~~

The unique constructor of type ``unit`` is ``()``.

The two Booleans (``bool``) constants are:

* ``true``
* ``false``

As in Michelson, there are different types of integers:

* ``int`` : an unbounded integer, positive or negative, simply written ``0``,``1``,``2``,``-1``,``-2``,...
* ``nat`` : an unbounded positive integer, written either with a ``p`` suffix (``0p``, ``12p``, etc.) or as an integer with a type coercion ( ``(0 : nat)`` ).
* ``tez`` : an unbounded positive float of Tezzies, written either with a ``tz`` suffix (``1.00tz``, etc.) or as a string with type coercion (``("1.00" : tez)``).

Strings (``string``) are delimited by the characters ``"`` and ``"``.

Bytes (``bytes``) are sequences of hexadecimal pairs preceeded by ``0x``, for
instance:

* ``0x``
* ``0xabcdef``

Timestamps (``timestamp``) are written in ISO 8601 format, like in Michelson:

* ``2015-12-01T10:01:00+01:00``

Keys, key hashes and signatures are base58-check encoded, the same as in Michelson:

* ``tz1YLtLqD1fWHthSVHPD116oYvsd4PTAHUoc`` is a key hash (``key_hash``)
* ``edpkuit3FiCUhd6pmqf9ztUTdUs1isMTbF9RBGfwKk1ZrdTmeP9ypN`` is a public
  key (``key``)
*
  ``edsigedsigthTzJ8X7MPmNeEwybRAvdxS1pupqcM5Mk4uCuyZAe7uEk68YpuGDeViW8wSXMr
  Ci5CwoNgqs8V2w8ayB5dMJzrYCHhD8C7`` is a signature (``signature``)

There are also three types of collections: lists, sets and
maps. Constants collections can be created directly:

* Lists: ``["x"; "y"]`` for a ``string list``;
* Sets: ``Set [1; 2; 3; 4]`` for an ``int set``;
* Maps: ``Map [1, "x"; 2, "y"; 3, "z"]`` for a ``(int, string) map``;
* Big maps: ``BigMap [1, "x"; 2, "y"; 3, "z"]`` for a ``(int, string) big_map``

In the case of an empty collection, whose type cannot be inferred, the type must be specified:

* Lists: ``([] : int list)``
* Sets: ``(Set : int set)``
* Maps: ``(Map : (int, string) map)``
* Big maps: ``(BigMap : (int, string) big_map)``

Options (``option``) can be defined with:

* An empty option: ``(None : int option)``
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

* A ``string`` can be coerced to ``tez`` (the string must contain an
  integer in mutez à la Michelson), ``timestamp``, ``key``,
  ``address``, ``_ contract``, ``key_hash`` and ``signature``.
* A ``bytes`` can be coerced to ``address``, ``_ contract``, ``key``,
   ``key_hash`` and ``signature``.
* An ``address`` can be coerced to ``_ contract``.
* A ``_ contract`` can be coerced to ``address``.
* A ``key_hash`` can be coerced to ``unit contract`` and ``address``.


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

When the type of a primitive is specified, the notation is:

* ``TYPE_ARG -> TYPE_RESULT`` for a primitive with one argument
* ``TYPE_ARG1 -> TYPE_ARG2 -> TYPE_RESULT`` for a primitive with two arguments
  

Comparison between values
~~~~~~~~~~~~~~~~~~~~~~~~~

All the values are not comparable. Only two values of the following
types can be compared with each other:

* ``bool``
* ``int``
* ``nat``
* ``tez``
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
* ``>`` : strictly more
* ``>=`` : more or equal

There is also a function ``compare x y`` to compare two values and return
an integer, as follows:

* returns 0 if ``x`` and ``y`` are equal
* returns a strictly positive integer if ``x > y``
* returns a strictly negative integer if ``x < y``

The ``Current`` module
~~~~~~~~~~~~~~~~~~~~~~

* ``Current.balance: unit -> tez``: ``Current.balance ()`` returns the
  balance of the current contract. The balance contains the amount of
  tez that was sent by the current operation. It is translated to
  ``BALANCE`` in Michelson.
* ``Current.time: unit -> timestamp``: ``Current.time ()`` returns the
  timestamp of the block in which the transaction is included. This
  value is chosen by the baker that is including the transaction, so
  it should not be used as a reliable source of alea.  It is translated
  to ``NOW`` in Michelson.
* ``Current.amount: unit -> tez``: ``Current.amount ()`` returns the
  amount of tez transferred by the current operation (standard or
  internal transaction). It is translated to ``AMOUNT`` in Michelson.
* ``Current.gas: unit -> nat``: ``Current.gas ()`` returns the amount
  of gas available to execute the rest of the transaction. It is
  translated to ``STEPS_TO_QUOTA`` in Michelson.
* ``Current.source: unit -> address``: ``Current.source ()`` returns
  the address that initiated the current transaction in the
  blockchain. It is the same one for all the operations in the
  transaction, standard and internal. It is the address that paid the
  fees and storage cost, and signed the operation on the
  blockchain. It is translated to ``SOURCE`` in Michelson.
* ``Current.sender: unit -> address``: ``Current.sender ()`` returns
  the address that initiated the current operation. It is the same as
  the source for the toplevel operation, but it is the originating
  contract for internal operations. It is translated to ``SENDER`` in
  Michelson.
* ``failwith`` or ``Current.failwith: 'a -> 'b``: makes the current
  transaction and all its internal transactions fail. No modification
  is done to the context.

  
Operations on tuples
~~~~~~~~~~~~~~~~~~~~

* ``get t n``, ``Array.get t n`` and ``t.(n)`` where ``n`` is a
  constant positive-or-nul int: returns the ``n``-th element of the
  tuple ``t``. Tuples are translated to Michelson by pairing on the
  right, i.e. ``(a,b,c,d)`` becomes ``(a, (b, (c,d)))``. In this
  example, ``a`` is the ``0``-th element.
* ``set t n x``, ``Array.set t n x`` and ``t.(n) <- x`` where ``n`` is
  constant positive-or-nul int: returns the tuple where the ``n``-th element
  has been replaced by ``x``.

Operations on numeric values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* ``+``: Addition. With the following types:
  
  * ``tez -> tez -> tez``
  * ``nat -> nat -> nat``
  * ``int|nat -> int|nat -> int``
  * ``timestamp -> int|nat -> timestamp``
  * ``int|nat -> timestamp -> timestamp``
    
* ``-``: Substraction. With the following types:
  
  * ``tez -> tez -> tez``
  * ``int|nat -> int|nat -> int``
  * ``timestamp -> int|nat -> timestamp``
  * ``timestamp -> timestamp -> int``
  * ``int|nat -> int`` (negation)
  
* ``*``: Multiplication. With the following types:

  * ``nat -> tez -> tez``
  * ``tez -> nat -> tez``
  * ``nat -> nat -> nat``
  * ``nat|int -> nat|int -> int``
    
* ``/``: Euclidian division. With the following types:

  * ``nat -> nat -> ( nat * nat ) option``
  * ``int|nat -> int|nat -> ( int *  nat ) option``
  * ``tez -> nat -> ( tez * tez ) option``
  * ``tez -> tez -> ( nat * tez ) option``
    
* ``~-``: Negation. Type: ``int|nat -> int``
* ``lor``, ``or`` and ``||``: OR with the following types:

  * ``bool -> bool -> bool``
  * ``nat -> nat -> nat``
    
* ``&``, ``land`` and ``&&``: AND with the following types:

  * ``bool -> bool -> bool``
  * ``nat|int -> nat -> nat``
    
* ``lxor``, ``xor``: Exclusive OR with the following types:

  * ``bool -> bool -> bool``
  * ``nat -> nat -> nat``
    
* ``not``: NOT

  * ``bool -> bool``
  * ``nat|int -> int`` (two-complement with sign negation)

* ``abs``: Absolute value. Type ``int -> int``
* ``is_nat``: Maybe positive. Type ``int -> nat option``
* ``int``: To integer. Type ``nat -> int``
* ``>>`` and ``lsr`` : Logical shift right. Type ``nat -> nat -> nat``
* ``<<`` and ``lsl`` : Logical shift left. Type ``nat -> nat -> nat``

Operations on contracts
~~~~~~~~~~~~~~~~~~~~~~~

* ``Contract.set_delegate: key_hash option -> operation``. It is translated to ``SET_DELEGATE`` in Michelson.
* ``Contract.address: _ contract -> address`` . It is translated to ``ADDRESS`` in Michelson.
* ``Contract.self: unit -> 'a contract``. It is translated to ``SELF`` in Michelson.
* ``Account.create: key_hash -> key_hash option -> bool -> tez -> operation * address``.
  It is translated to ``CREATE_ACCOUNT`` in Michelson.
* ``Account.default: key_hash -> unit contract``.
  It is translated to ``DEFAULT_ACCOUNT`` in Michelson.
* ``Contract.create: ``: ``Contract.create delegate manager delegatable spendable amount storage (fun (p:ptype) (s:stype) -> ...)``
* ``Contract.at: address -> 'a contract option``. Must be annotated with the type of the contract.
* ``Contract.call: 'a contract -> tez -> 'a -> operation``
  
Cryptographic operations
~~~~~~~~~~~~~~~~~~~~~~~~
              
* ``Crypto.blake2b: bytes -> bytes`. It is translated to ``BLAKE2B`` in Michelson.
* ``Crypto.sha256: bytes -> bytes``. It is translated to ``SHA256`` in Michelson.
* ``Crypto.sha512: bytes -> bytes``. It is translated to ``SHA512`` in Michelson.
* ``Crypto.hash_key: key -> key_hash``. It is translated to ``HASH_KEY`` in Michelson.
* ``Crypto.check: key -> signature -> bytes -> bool``. It is translated to ``CHECK_SIGNATURE`` in Michelson.

Operations on bytes
~~~~~~~~~~~~~~~~~~~
              
* ``Bytes.pack: 'a -> bytes``. It is translated to ``PACK`` in Michelson.
* ``Bytes.unpack:  bytes -> 'a``.. It is translated to ``UNPACK`` in Michelson.
* ``Bytes.length`` or ``Bytes.size: bytes -> nat``. It is translated to ``SIZE`` in Michelson.
* ``Bytes.concat: bytes list -> bytes``. It is translated to ``CONCAT`` in Michelson.
* ``Bytes.slice`` or ``Bytes.sub: nat -> nat -> bytes ->  bytes``. It is translated to ``SLICE`` in Michelson.
* ``@: bytes -> bytes -> bytes``. It is translated to ``CONCAT`` in Michelson.

Operations on strings
~~~~~~~~~~~~~~~~~~~~~
              
* ``String.length`` or ``String.size``. It is translated to ``SIZE`` in Michelson.
* ``String.slice`` or ``String.sub``. It is translated to ``SLICE`` in Michelson.
* ``String.concat``. It is translated to ``CONCAT`` in Michelson.
* ``@``. It is translated to ``CONCAT`` in Michelson.


Operations on lambdas
~~~~~~~~~~~~~~~~~~~~~

*  ``Lambda.pipe`` or  ``|>`` of type ``'a -> ('a, 'b) lambda -> 'b`` or ``'a -> ('a,'b) closure -> 'b``

Operations on lists              
~~~~~~~~~~~~~~~~~~~

* ``List.rev``    Prim_list_rev;
* ``List.length``    Prim_list_size;
* ``List.size``    Prim_list_size;
* ``List.iter``    Prim_list_iter;
* ``List.fold``    Prim_list_fold;
* ``List.map``    Prim_list_map;
* ``List.map_fold``    Prim_list_map_fold;

Operations on sets
~~~~~~~~~~~~~~~~~~
              
* ``Set.update``    Prim_set_update;
* ``Set.add``    Prim_set_add;
* ``Set.remove``    Prim_set_remove;
* ``Set.mem``    Prim_set_mem;
* ``Set.cardinal``    Prim_set_size;
* ``Set.size``    Prim_set_size;
* ``Set.iter``    Prim_set_iter;
* ``Set.fold``    Prim_set_fold;
* ``Set.map``    Prim_set_map;
* ``Set.map_fold``    Prim_set_map_fold;

Operations on maps
~~~~~~~~~~~~~~~~~~

* ``Map.find``    Prim_map_find;
* ``Map.update``    Prim_map_update;
* ``Map.add``    Prim_map_add;
* ``Map.remove``    Prim_map_remove;
* ``Map.mem``    Prim_map_mem;
* ``Map.cardinal``    Prim_map_size;
* ``Map.size``    Prim_map_size;
* ``Map.iter``    Prim_map_iter;
* ``Map.fold``    Prim_map_fold;
* ``Map.map``    Prim_map_map;
* ``Map.map_fold``    Prim_map_map_fold;

Operations on generic collections
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
              
* ``Coll.update``    Prim_coll_update;
* ``Coll.mem``    Prim_coll_mem;
* ``Coll.find``    Prim_coll_find;
* ``Coll.size``   Prim_coll_size;
* ``Coll.concat``   Prim_concat;
* ``Coll.slice``   Prim_slice;
* ``Coll.iter``    Prim_coll_iter;
* ``Coll.fold``    Prim_coll_fold;
* ``Coll.map``    Prim_coll_map;
* ``Coll.map_fold``    Prim_coll_map_fold;


From Michelson to Liquidity
---------------------------

Here is a table of how Michelson instructions translate to Liquidity:

  
* ``ADDRESS``
* ``AMOUNT``: ``Current.amount()``
* ``ABS``: ``abs x``
* ``ADD``: ``x + y``
* ``AND``: ``x land y`` or ``x && y``
* ``BALANCE``: ``Current.balance()``
* ``BLAKE2B``
* ``CAR``: ``x.(0)``
* ``CDR``: ``x.(1)``
* ``CAST``
* ``CHECK_SIGNATURE``
* ``COMPARE``: ``compare x y``
* ``CONCAT``
* ``CONS``: ``x :: y``
* ``CONTRACT``
* ``CREATE_ACCOUNT``
* ``CREATE_CONTRACT``
* ``DIP``: stack manipulation
* ``DROP``: stack manipulation
* ``DUP``: stack manipulation
* ``EDIV``: ``x / y``
* ``EMPTY_MAP``
* ``EMPTY_SET``
* ``EQ``: ``x = y``
* ``EXEC``
* ``FAILWITH``
* ``GE``: ``x >= y``
* ``GET``
* ``GT``: ``x > y``
* ``HASH_KEY``
* ``IF``: ``if CONDITION then IF_TRUE else IF_FALSE``
* ``IF_CONS``
* ``IF_LEFT``
* ``IF_NONE``
* ``IMPLICIT_ACCOUNT``
* ``INT``: ``int x``
* ``ISNAT``:``is_nat x`` or ``match%int x with Plus x -> ... | Minus y -> ...``
* ``ITER``
* ``LAMBDA``
* ``LE``: ``x <= y``
* ``LEFT``
* ``LOOP``
* ``LOOP_LEFT``
* ``LSL``: ``x lsl y`` or ``x << y``
* ``LSR`` ``x lsr y`` or ``x >> y``
* ``LT``: ``x < y``
* ``MAP``
* ``MEM``
* ``MUL``: ``x * y``
* ``NEG``: ``~- x``
* ``NEQ``: ``x <> y``
* ``NIL``: ``( [] : int list)``
* ``NONE``: ``(None : int option)``
* ``NOT``: ``not x``
* ``NOW``
* ``OR``
* ``PACK``
* ``PAIR``
* ``PUSH``
* ``RENAME``
* ``RIGHT``
* ``SENDER``: ``Current.sender()``
* ``SIZE``
* ``SELF``
* ``SET_DELEGATE``
* ``SHA256``
* ``SHA512``
* ``SLICE``
* ``SOME``: ``Some x``
* ``SOURCE``: ``Current.source()``
* ``STEPS_TO_QUOTA``: ``Current.gas()``
* ``SUB``
* ``SWAP``
* ``TRANSFER_TOKENS``
* ``UNIT``
* ``UNPACK``
* ``UPDATE``
* ``XOR``

