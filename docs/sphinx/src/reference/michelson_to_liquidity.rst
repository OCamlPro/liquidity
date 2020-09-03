From Michelson to Liquidity
===========================

Here is a table of how Michelson instructions translate to Liquidity:


* ``ADDRESS``: ``Contract.untype c``
* ``AMOUNT``: ``Current.amount()``
* ``ABS``: ``match%nat x with Plus n -> | Minus n -> n``
* ``ADD``: ``x + y``
* ``AND``: ``x land y`` or ``x && y`` or ``x & y``
* ``BALANCE``: ``Current.balance()``
* ``BLAKE2B``: ``Crypto.blake2b bytes``
* ``CAR``: ``x.(0)``
* ``CDR``: ``x.(1)``
* ``CAST``: not available
* ``CHECK_SIGNATURE``: ``Crypto.check key sig bytes``
* ``COMPARE``: ``compare x y``
* ``CONCAT``: ``String.concat list`` or ``bytes.concat list``
* ``CONS``: ``x :: y``
* ``CONTRACT``
* ``CREATE_ACCOUNT``: ``Account.create``
* ``CREATE_CONTRACT``
* ``DIP``: automatic stack management
* ``DROP``: automatic stack management
* ``DUP``: automatic stack management
* ``EDIV``: ``x / y``
* ``EMPTY_BIG_MAP``: ``BigMap []``
* ``EMPTY_MAP``: ``Map []``
* ``EMPTY_SET``: ``Set []``
* ``EQ``: ``x = y``
* ``EXEC``: ``x |> f`` or ``f x`` or ``f @@ x``
* ``FAILWITH``: ``Current.failwith``
* ``GE``: ``x >= y``
* ``GET``: ``Map.find key map``
* ``GT``: ``x > y``
* ``HASH_KEY``: ``Crypto.hash_key k``
* ``IF``: ``if COND_EXPR then EXPR_IF_TRUE else EXPR_IF_FALSE``
* ``IF_CONS``: ``match list with [] -> EXPR | head :: tail -> EXPR``
* ``IF_LEFT``: ``match variant with Left x -> EXPR | Right x -> EXPR``
* ``IF_NONE``: ``match option with None -> EXPR | Some x -> EXPR``
* ``IMPLICIT_ACCOUNT``: ``Account.default``
* ``INT``: ``int x``
* ``ISNAT``:``is_nat x`` or ``match%int x with Plus x -> ... | Minus y -> ...``
* ``ITER``: ``List.iter``, ``Set.iter``, ``Map.iter``,
            ``List.fold``, ``Map.fold``
* ``LAMBDA``: ``fun x -> ...``
* ``LE``: ``x <= y``
* ``LEFT``: ``Left x``
* ``LOOP``: ``Loop.loop (fun x -> ...; (cond, x')) x0``
* ``LOOP_LEFT``: ``Loop.left (fun (x, acc) -> (Left x/Right res, acc)) x0 acc``
* ``LSL``: ``x lsl y`` or ``x << y``
* ``LSR``: ``x lsr y`` or ``x >> y``
* ``LT``: ``x < y``
* ``MAP``: ``List.map``, ``Map.map``,
           ``List.map_fold``, ``Map.map_fold``
* ``MEM``: ``Set.mem ele set``, ``Map.mem key map``
* ``MUL``: ``x * y``
* ``NEG``: ``~- x``
* ``NEQ``: ``x <> y``
* ``NIL``: ``( [] : int list)``
* ``NONE``: ``(None : int option)``
* ``NOT``: ``not x``
* ``NOW``: ``Current.time ()``
* ``OR``: ``x lor y``, or ``x || y``, or ``x or y``
* ``PACK``: ``Bytes.pack x``
* ``PAIR``: ``( x, y )``
* ``PUSH``, ``DIP``, ``DROP``, ``DIG``, ``DUG``, ``SWAP``: automatic stack management
* ``RENAME``: automatic annotations management
* ``RIGHT``: ``Right x``
* ``SENDER``: ``Current.sender()``
* ``SIZE``: ``List.size list``, ``String.size``, ``Bytes.size``, ``Set.size``
* ``SELF %e``: ``[%handle Self.e]``
* ``SET_DELEGATE``: ``Contract.set_delegate (Some keyhash)``
* ``SHA256``: ``Crypto.sha256 bytes``
* ``SHA512``: ``Crypto.sha512 bytes``
* ``SLICE``: ``String.sub pos len string`` or ``Bytes.sub``
* ``SOME``: ``Some x``
* ``SOURCE``: ``Current.source()``
* ``STEPS_TO_QUOTA``: ``Current.gas()`` (deprecated, works for
  decompilation only)
* ``SUB``: ``x - y``
* ``TRANSFER_TOKENS``: ``Contract.call contract amount param``
* ``UNIT``: ``()``
* ``UNPACK``: ``(unpack bytes : int list option)``
* ``UPDATE``: ``Set.update key true set`` or ``Map.update key (Some val) map``
* ``XOR``: ``x lxor y``
