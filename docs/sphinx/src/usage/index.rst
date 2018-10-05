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
    ( ([] : operation list), storage + parameter )

This contract will add its argument to the storage, and do nothing else.

To compile the file, we can use::

  ─➤ liquidity simple.liq
  Main contract Simple
  File "simple.liq.tz" generated
  If tezos is compiled, you may want to typecheck with:
    tezos-client typecheck script simple.liq.tz

The ``liquidity`` compiler will try to compile any file with a ``.liq`` extension provided on the command line.
    
Let's have a look at the generated ``simple.liq.tz`` file::

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
  File "simple.liq.tz" generated
  If tezos is compiled, you may want to typecheck with:
    tezos-client typecheck script simple.liq.tz
  
   ─➤ cat simple.liq.tz
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

Let's decompile the ``simple.liq.tz`` file from the previous section::

  ─➤ liquidity simple.liq.tz
  Program "simple.liq.tz" parsed
  File "simple_liq.tz.liq" generated

We can now check the result of the decompilation::
  
  ─➤ cat simple_liq.tz.liq
  [%%version 0.4]
  type storage = int
  [%%entry
    let main (parameter : int) (storage : storage) =
      (([] : operation list), (storage + parameter)) ]

The ``liquidity`` tool will decompile in the same way any file with
the ``.tz`` extension provided on the command line.
      
Relevant options:
  -k                       Continue on error
  --verbose                Increment verbosity


Interacting with a Tezos node
-----------------------------

(TODO)
