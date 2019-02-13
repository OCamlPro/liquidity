Usage
=====

There is only one tool, called ``liquidity``, provided by Liquidity.
It can be used to:

* compile Liquidity files (.liq) to Michelson
* decompile Michelson files (.tz) to Liquidity
* interact with a tezos-node to simulate or deploy a contract

Basic Usage
-----------

Let's run ``liquidity`` with no options::

  ─➤ liquidity
  liquidity [OPTIONS] FILES [COMMAND]

  The liquidity compiler can translate files from Liquidity to Michelson
  and from Michelson to Liquidity. Liquidity files must end with the .liq
  extension. Michelson files must end with the .tz extension.

  Available options:
    --verbose                Increment verbosity
    --version                Show version and exit
    -o <filename>            Output code in <filename>
    --main <ContractName>    Produce code for contract named <ContractName>
    --no-peephole            Disable peephole optimizations
    --type-only              Stop after type checking
    --parse-only             Stop after parsing
    --compact                Produce compact Michelson
    --json                   Output Michelson in JSON representation
    --amount <1.99tz>        Set amount for deploying or running a contract (default: 0tz)
    --fee <0.05tz>           Set fee for deploying a contract (default: 0.05tz)
    --source <tz1...>        Set the source for deploying or running a contract (default: none)
    --private-key <edsk...>  Set the private key for deploying a contract (default: none)
    --counter N              Set the counter for the operation instead of retrieving it
    --tezos-node <addr:port> Set the address and port of a Tezos node to run or deploy contracts (default: 127.0.0.1:8732)

  Available commands:
    --protocol               Specify protocol (mainnet, zeronet, alphanet) (detect if not specified)
    --run ENTRY              PARAMETER STORAGE Run Liquidity contract on Tezos node
    --delegatable            With --[forge-]deploy, deploy a delegatable contract
    --spendable              With --[forge-]deploy, deploy a spendable contract
    --init-storage           [INPUT1 INPUT2 ...] Generate initial storage
    --forge-deploy           [INPUT1 INPUT2 ...] Forge deployment operation for contract
    --deploy                 [INPUT1 INPUT2 ...] Deploy contract
    --get-storage <KT1...>   Get deployed contract storage
    --call <KT1...>          ENTRY PARAMETER Call deployed contract
    --data ENTRY             PARAMETER [STORAGE] Translate to Michelson
    --signature SIGNATURE    Set the signature for an operation
    --inject OPERATION.bytes Inject a sign operation

  Misc:
    -help                    Display this list of options
    --help                   Display this list of options


Note that this is equivalent to running ``liquidity`` with the ``-help`` or
``--help`` arguments.

We can ask the current version of the compiler::

  ─➤ liquidity --version
  0.4


Compiling a Liquidity file
--------------------------

Let's take a very simple Liquidity contract, stored in a file ``simple.liq``::
  
  [%%version 0.4]

  type storage = int

  let%entry main (parameter : int) storage =
    ( [], storage + parameter )

This contract will add its argument to the storage, and do nothing else.

To compile the file, we can use::

  ─➤ liquidity simple.liq
  Main contract Simple
  File "simple.tz" generated
  If tezos is compiled, you may want to typecheck with:
    tezos-client typecheck script simple.tz

The ``liquidity`` compiler will try to compile any file with a ``.liq`` extension provided on the command line.
    
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
  If tezos is compiled, you may want to typecheck with:
    tezos-client typecheck script simple.tz
  
   ─➤ cat simple.tz
  parameter int; storage int; code { DUP ; DIP { CDR } ; CAR ; DUP ; SWAP ; DROP ; SWAP ; ADD ; NIL operation ; PAIR };

  
In case of error, for example if we set the storage to type ``nat`` instead of ``int``, the compiler will provide the location of the error in a standard format::

  ─➤ liquidity simple.liq
  simple.liq:4.2-4.48: Error: Type error:  Unexpected type for return value.
  Expected type:
    (operation list * nat)
  Actual type:
    (operation list * int)
  (exit 1)


Relevant options:
  --verbose                Increment verbosity
  --no-peephole            Disable peephole optimizations
  --type-only              Stop after type checking
  --parse-only             Stop after parsing
  --single-line            Output Michelson on a single line
  --compact                Produce compact Michelson
  --json                   Output Michelson in JSON representation


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
      
Relevant options:
  --verbose                Increment verbosity


Interacting with a Tezos node
-----------------------------

Liquidity comes with a Tezos client which allows interactions with a
node using Liquidity smart contracts and Liquidity syntax.


Running a simulation of the contract
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have access to a Tezos node (for instance locally on port
8732), you can run contract call simulations of a contract by
specifying its current storage value, the entry point and the
transaction parameter::

  > liquidity \
      --tezos-node http://127.0.0.1:8732 \
      --amount 2tz \
      tests/others/demo.liq \
      --run main '"candidate 1"' 'Map ["candidate 1", 0; "candidate 2", 0]'

  Main contract Demo
  tests/others/demo.liq:13.4-13.61: Failed at runtime: in /chains/main/blocks/head/helpers/scripts/run_code
  Failed with "Not enough money, at least 5tz to vote"

::

  > liquidity \
      --tezos-node http://127.0.0.1:8732 \
      --amount 5tz \
      tests/others/demo.liq \
      --run main '"candidate 1"' 'Map ["candidate 1", 0; "candidate 2", 0]'

  Main contract Demo
  Map [("candidate 2", 0); ("candidate 1", 1)]
  # Internal operations: 0

Relevant options:
    --amount <1.99tz>        Set amount for deploying or running a contract (default: 0tz)
    --source <tz1...>        Set the source for deploying or running a contract (default: none)
    --tezos-node <addr:port>  Set the address and port of a Tezos node to run or deploy contracts (default: 127.0.0.1:8732)
    --run <ENTRY PARAMETER STORAGE>  Run Liquidity contract on Tezos node


Deploying a contract
~~~~~~~~~~~~~~~~~~~~

To deploy a contract you need to forge a deployment operation, sign
this operation and inject it to a Tezos node. This can be performed
separately or all at once with the command ``--deploy``.


Deploying a contract directly (unsafe)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Deploying a delegatable but non-spendable smart contract whose
Liquidity source code is contained in file ``contract.liq``, whose
initializer takes one string parameter as argument, and with initial
balance 2tz::

  liquidity \
    --tezos-node http://127.0.0.1:8732 \
    --amount 2tz \
    --fee 0tz \
    --delegatable \
    --private-key edsk2gL9deG8idefWJJWNNtKXeszWR4FrEdNFM5622t1PkzH66oH3r \
    --source tz1WWXeGFgtARRLPPzT2qcpeiQZ8oQb6rBZd \
    contract.liq \
    --deploy '"first"'

Because we give the private key as an argument (notice that this
process is unsafe, and should only be used with private keys not
associated with real accounts on the mainnet) to sign the transaction,
we don't need to specify the source which will be inferred as being
the corresponding public key hash.

Deploying a contract with an offline signature
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The preferred way to proceed is to do this operation in three separate
phase, the second one being the offline signature.

First we need to produce (forge) an unsigned serialized deployment
operation::

  > liquidity \
     --tezos-node http://127.0.0.1:8732 \
     --amount 2tz \
     --fee 0tz \
     --delegatable \
     --source tz1WWXeGFgtARRLPPzT2qcpeiQZ8oQb6rBZd \
     contract.liq \
     --forge-deploy '"first"' > my_op.bytes

Using the default client we can then sign this operation with an
account ``my_account`` on an offline machine. If this accounts
corresponds to a hardware wallet (like a ledger nano S) in the tezos
client, you will be required to confirm the signature. If this
accounts in an encrypted private key you will be asked to input your
password::

  > tezos-client sign bytes 0x03$(cat ./my_op.bytes) for my_account

  Signature: edsigtzxo2Q7wFiEjausSp7pKUXLK9PnPqf8rHEKdc18HtNVbZSg5WJyFJwk14w7mykCsq3nV5iB6Eo4gTX3y8Dv8tkn1EadRj7

Save this signature. You can now inject the signed operation on the
Tezos newtork by simply issuing::

  > liquidity \
     --tezos-node http://127.0.0.1:8732 \
     --signature edsigtzxo2Q7wFiEjausSp7pKUXLK9PnPqf8rHEKdc18HtNVbZSg5WJyFJwk14w7mykCsq3nV5iB6Eo4gTX3y8Dv8tkn1EadRj7 \
     --inject my_op.bytes

Relevant options:
    --amount <1.99tz>        Set amount for deploying or running a contract (default: 0tz)
    --fee <0.05tz>           Set fee for deploying a contract (default: 0.05tz)
    --source <tz1...>        Set the source for deploying or running a contract (default: none)
    --private-key <edsk...>  Set the private key for deploying a contract (default: none)
    --counter N              Set the counter for the operation instead of retrieving it
    --tezos-node <addr:port>  Set the address and port of a Tezos node to run or deploy contracts (default: 127.0.0.1:8732)
    --protocol                Specify protocol (mainnet, zeronet, alphanet) (detect if not specified)
    --delegatable             With --[forge-]deploy, deploy a delegatable contract
    --spendable               With --[forge-]deploy, deploy a spendable contract
    --forge-deploy <INPUTS>   Forge deployment operation for contract
    --deploy <INPUTS>         Deploy contract
    --signature <SIGNATURE>     Set the signature for an operation
    --inject <OPERATION.bytes>        Inject a sign operation

Calling a contract
~~~~~~~~~~~~~~~~~~

To call an already deployed smart contract you need to forge a
transfer operation, sign this operation ans inject it to a Tezos
node. This can be performed separately or all at once with the command
``--call``.


Calling a contract directly (unsafe)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The following command will call the ``demo.liq`` contract deployed at
address ``KT1Ukta5wAt5R87U2awCoYHJAVA38FeptagD`` on the zeronet::

  liquidity \
    --tezos-node http://zeronet-node.tzscan.io \
    --amount 5tz \
    --fee 0tz \
    --private-key edsk2gL9deG8idefWJJWNNtKXeszWR4FrEdNFM5622t1PkzH66oH3r \
    --source tz1WWXeGFgtARRLPPzT2qcpeiQZ8oQb6rBZd \
    tests/others/demo.liq \
    --call KT1Ukta5wAt5R87U2awCoYHJAVA38FeptagD main '"ocaml"'

  Main contract Demo
  Successful call to contract KT1Ukta5wAt5R87U2awCoYHJAVA38FeptagD in operation oosA6qjVjtFbE9tGsrzHtjB6zk27R3yRH61wKD55WJ1WWiVjhy2


Calling a contract with an offline signature
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The preferred way to proceed is to do this operation in three separate
phase, the second one being the offline signature.

First we need to produce (forge) an unsigned serialized deployment
operation::

  > liquidity \
      --tezos-node http://zeronet-node.tzscan.io \
      --amount 5tz \
      --fee 0tz \
      --source tz1WWXeGFgtARRLPPzT2qcpeiQZ8oQb6rBZd \
      tests/others/demo.liq \
      --forge-call KT1Ukta5wAt5R87U2awCoYHJAVA38FeptagD main '"ocaml"' > my_op.bytes

::

  > tezos-client sign bytes 0x03$(cat ./my_op.bytes) for my_account

  Signature:edsigu1xkB6tC2Sm39QaGtAzPbjdfWF7V9ctNVwGVH52zrmus921eVmdga2nZowGkF9HSagMNsw6ZaZ8xoKvvhyFgfgirR9Wuow

Save this signature. You can now inject the signed operation on the
Tezos newtork by simply issuing::

  > liquidity \
      --tezos-node http://zeronet-node.tzscan.io \
      --signature edsigu1xkB6tC2Sm39QaGtAzPbjdfWF7V9ctNVwGVH52zrmus921eVmdga2nZowGkF9HSagMNsw6ZaZ8xoKvvhyFgfgirR9Wuow \
      --inject my_op.bytes

  Operation injected: ooDm5JPw5fgaMyM6eAWJA1vW49jjPDC3KrxTU4UZkuPx952D59o

Relevant options:
    --amount <1.99tz>        Set amount for deploying or running a contract (default: 0tz)
    --fee <0.05tz>           Set fee for deploying a contract (default: 0.05tz)
    --source <tz1...>        Set the source for deploying or running a contract (default: none)
    --private-key <edsk...>  Set the private key for deploying a contract (default: none)
    --counter N              Set the counter for the operation instead of retrieving it
    --tezos-node <addr:port>  Set the address and port of a Tezos node to run or deploy contracts (default: 127.0.0.1:8732)
    --protocol                Specify protocol (mainnet, zeronet, alphanet) (detect if not specified)
    --call <KT1... ENTRY PARAMETER>  Call deployed contract
    --forge-call <KT1... ENTRY PARAMETER>  Forge call transaction operation
    --signature <SIGNATURE>     Set the signature for an operation
    --inject <OPERATION.bytes>        Inject a sign operation

Generating initial storage
~~~~~~~~~~~~~~~~~~~~~~~~~~

::

  > liquidity \
      --tezos-node http://zeronet-node.tzscan.io \
      tests/others/demo.liq
      --init-storage '"this"'

  Main contract Demo
  Evaluated initial storage: Map [("this", 0); ("pro", 0); ("ocaml", 0)]
  Constant initial storage generated in "tests/others/demo.liq.init.tz"

::

  > cat tests/others/demo.liq.init.tz

  { Elt  "ocaml" 0 ; Elt  "pro" 0 ; Elt  "this" 0}

Relevant options:
    --amount <1.99tz>        Set amount for deploying or running a contract (default: 0tz)
    --tezos-node <addr:port>  Set the address and port of a Tezos node to run or deploy contracts (default: 127.0.0.1:8732)
    --init-storage <INPUTS>   Generate initial storage
    --json                   Output Michelson in JSON representation
    -o <filename>            Output code in <filename>
