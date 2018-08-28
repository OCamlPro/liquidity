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
  liquidity [OPTIONS] FILES

  The liquidity compiler can translate files from Liquidity to Michelson
  and from Michelson to Liquidity. Liquidity files must end with the .liq
  extension. Michelson files must end with the .tz extension.
  
  Available options:
  -k                       Continue on error
  --verbose                Increment verbosity
  --version                Show version and exit
  --no-peephole            Disable peephole optimizations
  --type-only              Stop after type checking
  --parse-only             Stop after parsing
  --single-line            Output Michelson on a single line
  --no-annot               Don't annotate Michelson with variable names
  --annot-prim             Annotate Michelson primitives directly
  --compact                Produce compact Michelson
  --json                   Output Michelson in JSON representation
  --amount <1.99tz>        Set amount for deploying or running a contract (default: 0tz)
  --fee <0.05tz>           Set fee for deploying a contract (default: 0.05tz)
  --source <tz1...>        Set the source for deploying or running a contract (default: none)
  --private-key <edsk...>  Set the private key for deploying a contract (default: none)
  --tezos-node <addr:port> Set the address and port of a Tezos node to run or deploy contracts (default: 127.0.0.1:8732)
  --run FILE.liq           PARAMETER STORAGE Run Liquidity contract on Tezos node
  --delegatable            With --[forge-]deploy, deploy a delegatable contract
  --spendable              With --[forge-]deploy, deploy a spendable contract
  --forge-deploy FILE.liq  INPUT1 INPUT2 ... Forge deployment operation for contract
  --deploy FILE.liq        INPUT1 INPUT2 ... Deploy contract
  --get-storage FILE.liq   <TZ1...> Get deployed contract storage
  --call FILE.liq          <TZ1...> PARAMETER Call deployed contract
  --data FILE.liq          PARAMETER STORAGE Translate to Michelson
  -help                    Display this list of options
  --help                   Display this list of options

Note that this is equivalent to running ``liquidity`` with the ``-help`` or
``--help`` arguments.

We can ask the current version of the compiler::

  ─➤ liquidity --version
  0.36


Compiling a Liquidity file
--------------------------

Let's take a very simple Liquidity contract, stored in a file ``simple.liq``::
  
  [%%version 0.36]
  
  let%entry main (parameter : int) (storage : int) =
    ( ([] : operation list), storage + parameter )

This contract will add its argument to the storage, and do nothing else.

To compile the file, we can use::

  ─➤ liquidity simple.liq
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

Note that we can use a more compact version, on a single line and without annotations::

  ─➤ liquidity-mini --compact simple.liq
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
  -k                       Continue on error
  --verbose                Increment verbosity
  --no-peephole            Disable peephole optimizations
  --type-only              Stop after type checking
  --parse-only             Stop after parsing
  --single-line            Output Michelson on a single line
  --no-annot               Don't annotate Michelson with variable names
  --annot-prim             Annotate Michelson primitives directly
  --compact                Produce compact Michelson
  --json                   Output Michelson in JSON representation


Decompiling a Michelson file
----------------------------

Let's decompile the ``simple.liq.tz`` file from the previous section::

  ─➤ liquidity simple.liq.tz
  Program "simple.liq.tz" parsed
  File "simple.liq.tz.liq" generated

We can now check the result of the decompilation::
  
  ─➤ cat simple.liq.tz.liq
  [%%version 0.36]
  [%%entry
    let main (parameter : int) (storage : int) =
      (([] : operation list), (storage + parameter)) ]

The ``liquidity`` tool will decompile in the same way any file with
the ``.tz`` extension provided on the command line.
      
Relevant options:
  -k                       Continue on error
  --verbose                Increment verbosity


Interacting with a Tezos node
-----------------------------

(TODO)

Relevant options:
 --amount <1.99tz>                           Set amount for deploying or running a contract (default: 0tz)
 --fee <0.05tz>                              Set fee for deploying a contract (default: 0.05tz)
 --source <tz1...>                           Set the source for deploying or running a contract (default: none)
 --private-key <edsk...>                     Set the private key for deploying a contract (default: none)
 --tezos-node <addr:port>                    Set the address and port of a Tezos node to run or deploy contracts (default: 127.0.0.1:8732)
 --delegatable                               With --[forge-]deploy, deploy a delegatable contract
 --spendable                                 With --[forge-]deploy, deploy a spendable contract
 --run <FILE PARAMETER STORAGE>              Run Liquidity contract on Tezos node
 --forge-deploy <FILE.liq INPUT1 INPUT2 ..>  Forge deployment operation for contract
 --deploy <FILE.liq INPUT1 INPUT2 ..>        Deploy contract
 --get-storage <FILE.liq TZ1>                Get deployed contract storage
 --call <FILE.liq TZ1 PARAMETER>             Call deployed contract
 --data <FILE.liq PARAMETER STORAGE>         Translate to Michelson



  
