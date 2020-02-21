Usage
=====

There are two tools provided by Liquidity.
1. ``liquidity``, the compiler. It can be used to:
   * compile Liquidity files (.liq) to Michelson
   * decompile Michelson files (.tz) to Liquidity
2. ``liquidity-client``, a client to interact with Liquidity contracts
   on Dune Network.

We recommend to read the :ref:`Manual Pages <manual-pages>` if you want to see all
options and examples for each command. The rest of this section shows
some typical command usage if you want to get started immediately.

Compiling a Liquidity file
--------------------------

Let's take a very simple Liquidity contract, stored in a file ``simple.liq``::
  
  type storage = int

  let%entry main (parameter : int) storage =
    ( [], storage + parameter )

This contract will add its argument to the storage, and do nothing else.

To compile the file, we can use::

  ─➤ liquidity simple.liq
  Main contract Simple
  File "simple.tz" generated

The ``liquidity`` compiler will try to compile any file with a
``.liq`` extension provided on the command line.
    
Let's have a look at the generated ``simple.tz`` file::

  parameter int;
  storage int;
  code { DUP ;
         DIP { CDR } ;
         CAR ;
         SWAP ;
         ADD ;
         NIL operation ;
         PAIR };

Note that we can use a more compact version, on a single line::

  ─➤ liquidity-mini --compact simple.liq
  Main contract Simple
  File "simple.tz" generated

   ─➤ cat simple.tz
  parameter int; storage int; code { DUP ; DIP { CDR } ; CAR ; DUP ; SWAP ; DROP ; SWAP ; ADD ; NIL operation ; PAIR };

  
In case of an error, for example if we set the storage to type ``nat``
instead of ``int``, the compiler will provide the location of the
error in a standard format (most editors can parse it so you can jump
with a single keystroke to the error location)::

  ─➤ liquidity simple.liq
  simple.liq:4.2-4.48: Error: Type error:  Unexpected type for return value.
  Expected type:
    (operation list * nat)
  Actual type:
    (operation list * int)
  (exit 1)


:ref:`-> More details on the compiler <liquidity>`.

Decompiling a Michelson file
----------------------------

Let's decompile the ``simple.tz`` file from the previous section::

  ─➤ liquidity simple.tz
  Program "simple.tz" parsed
  File "simple.tz.liq" generated

We can now check the result of the decompilation::
  
  ─➤ cat simple.tz.liq
  [%%version 0.4]
  type storage = int
  [%%entry
    let main (parameter : int) (storage : storage) =
      (([] : operation list), (storage + parameter)) ]

The ``liquidity`` tool will decompile in the same way any file with
the ``.tz`` extension provided on the command line.


Interacting with a Dune node
-----------------------------

Liquidity comes with a Dune Network client (also usable on the Tezos
network) which allows interactions with a node using Liquidity smart
contracts and Liquidity syntax.


Running a simulation of the contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have access to a node (for instance locally on port
8733), you can run contract call simulations of a contract by
specifying its current storage value, the entry point and the
transaction parameter::

  > liquidity-client run \
      --node http://127.0.0.1:8733 \
      --amount 2DUN \
      --files tests/others/demo.liq \
      default '"candidate 1"' 'Map ["candidate 1", 0; "candidate 2", 0]'

  Main contract Demo
  tests/others/demo.liq:13.4-13.61: Failed at runtime: in /chains/main/blocks/head/helpers/scripts/run_code
  Failed with "Not enough money, at least 5DUN to vote"

::

  > liquidity-client run \
      --node http://127.0.0.1:8733 \
      --amount 5DUN \
      --files tests/others/demo.liq \
      default '"candidate 1"' 'Map ["candidate 1", 0; "candidate 2", 0]'

  Main contract Demo
  Map [("candidate 2", 0); ("candidate 1", 1)]
  # Internal operations: 0


:ref:`-> More details on run <liquidity-client-run>`.

Deploying a contract
~~~~~~~~~~~~~~~~~~~~

To deploy a contract you need to forge a deployment operation, sign
this operation and inject it to a node. This can be performed
separately or all at once with the command ``deploy``.


Deploying a contract directly (unsafe)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Deploying a delegatable but non-spendable smart contract whose
Liquidity source code is contained in file ``contract.liq``, whose
initializer takes one string parameter as argument, and with initial
balance 2tz::

  liquidity-client deploy \
    --node http://127.0.0.1:8733 \
    --amount 2DUN \
    --private-key edsk2gL9deG8idefWJJWNNtKXeszWR4FrEdNFM5622t1PkzH66oH3r \
    --files contract.liq \
    '"first"'

Because we give the private key as an argument (notice that this
process is unsafe, and should only be used with private keys not
associated with real accounts on the mainnet) to sign the transaction,
we don't need to specify the source which will be inferred as being
the corresponding public key hash.

:ref:`-> More details on deploy <liquidity-client-deploy>`.

Deploying a contract with an offline signature
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The preferred way to proceed is to do this operation in three separate
phase, the second one being the offline signature.

First we need to produce (forge) an unsigned serialized deployment
operation::

  > liquidity-client forge-deploy \
      --files tests/others/multisig.liq \
      --node http://127.0.0.1:8733 \
      --source dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F \
      'Set [dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F]' 1p > op.bytes

Using the default client we can then sign this operation with an
account ``my_account`` on an offline machine. If this accounts
corresponds to a hardware wallet (like a ledger nano S) in the Dune
client, you will be required to confirm the signature. If this
accounts in an encrypted private key you will be asked to input your
password::

  > dune-client sign operation op.bytes for my_account --out op.signed

  Node: http://localhost:18735
  Parsed operation:
    Manager signed operations:
      From: dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F
      Fee to the baker: đ0.009661
      Expected counter: 1
      Gas limit: 70221
      Storage limit: 2874 bytes
      Balance updates:
        dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F ........... -đ0.009661
        fees(dn1G8vdxwTcD7Nqf5ewF8FAj6bRG8iqtTgba,0) ... +đ0.009661
      Origination:
        From: dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F
        Credit: đ0
        Script:
          ...
        Initial storage:
          (Pair { "dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F" } (Pair {} (Pair 1 1)))
        No delegate for this contract
        This origination was successfully applied
        Originated contracts:
          KT1JS5QEB64SvLGRPBYxpmDG8obHoeGJZG5D
        Storage size: 2617 bytes
        Paid storage size diff: 2617 bytes
        Consumed gas: 70221
        Balance updates:
          dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F ... -đ2.617
          dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F ... -đ0.257

  Would you like to sign this operation? [y/N]: y
  Yes
  Signature: edsigtxAkTRs3xqdKmTfSitdHc4o7Msjsg5bfSPCoMUS6mE3t89sAQWz6V8MJsfY3G4pTShBSEV5pXM66QZSCKpewxU5WCK345X
  Serialized signed operation written to op.signed

You can now inject the signed operation on the
newtork by simply issuing::

  > dune-client inject operation op.signed

  Parsed operation:
    ...
  Would you like to inject this operation? [y/N]: y
  Yes
  Operation successfully injected in the node.
  Operation hash is 'op47tCnmZkn6LToM4Yp8pTydesjJ3GWXhDrE2hFryvb9SAufRwm'
  Waiting for the operation to be included...
  This sequence of operations was run:
    ...
  New contract KT1Q69vbqSQmWDGZMoMde2BT7nBv5uj3sFG7 originated.


:ref:`-> More details on forge deploy <liquidity-client-forge-deploy>`.

Calling a contract
~~~~~~~~~~~~~~~~~~

To call an already deployed smart contract you need to forge a
transfer operation, sign this operation ans inject it to a Dune
node. This can be performed separately or all at once with the command
``call``.


Calling a contract directly (unsafe)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following command will call the ``demo.liq`` contract deployed at
address ``KT1Ukta5wAt5R87U2awCoYHJAVA38FeptagD`` on the testnet::

  > liquidity-client call \
      --node http://testnet-node.dunscan.io \
      --amount 5DUN \
      --private-key edsk2gL9deG8idefWJJWNNtKXeszWR4FrEdNFM5622t1PkzH66oH3r \
      --files tests/others/demo.liq \
      KT1Ukta5wAt5R87U2awCoYHJAVA38FeptagD main '"ocaml"'

  Main contract Demo
  Successful call to contract KT1Ukta5wAt5R87U2awCoYHJAVA38FeptagD in operation oosA6qjVjtFbE9tGsrzHtjB6zk27R3yRH61wKD55WJ1WWiVjhy2


:ref:`-> More details on call <liquidity-client-call>`.


Calling a contract with an offline signature
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The preferred way to proceed is to do this operation in three separate
phase, the second one being the offline signature.

First we need to produce (forge) an unsigned serialized deployment
operation::

  > liquidity-client forge-call \
      --node http://testnet-node.dunscan.io \
      --amount 5DUN \
      --source dn1GLMm5dMXRxCwqmkV22keRCcoWwrrani9F \
      --files tests/others/demo.liq \
      KT1Ukta5wAt5R87U2awCoYHJAVA38FeptagD main '"ocaml"' > op.bytes

::

  > dune-client sign operation op.bytes for my_account --out op.signed

  ...

Save this signature. You can now inject the signed operation on the
network with the same command as above.

:ref:`-> More details on forge call <liquidity-client-forge-call>`.

Generating initial storage
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  > liquidity-client init-storage \
      --node http://testnet-node.dunscan.io \
      --files tests/others/demo.liq \
      '"this"'

  Main contract Demo
  Constant initial storage generated in "tests/others/demo.liq.init.tz"

::

  > cat tests/others/demo.liq.init.tz

  { Elt  "ocaml" 0 ; Elt  "pro" 0 ; Elt  "this" 0}


:ref:`-> More details on init-storage <liquidity-client-init-storage>`.
