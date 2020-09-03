Liquidity Grammar
-----------------

Toplevel:

* ``[%%version`` FLOAT ``]``
* Structure*

Contract:

* ``struct`` Structure* ``end``
* UIDENT

Module:

* ``struct`` ModStructure* ``end``
* UIDENT

ModStructure:

* ``type`` LIDENT ``=`` Type
* ``type`` LIDENT ``= {`` [ LIDENT ``:`` Type ``;``]+ ``}``
* ``type`` LIDENT ``=`` [ ``|`` UIDENT ``of`` Type ]+
* ``module`` UIDENT ``=`` Module
* ``contract`` UIDENT ``=`` Contract
* ``contract type`` UIDENT ``= sig`` Signature* ``end``
* ``let`` ``rec``? Annot* Pattern ``=`` Expression

Structure:

* ModStructure
* ``let%init storage =`` Expression
* ``let%entry`` LIDENT Pattern Pattern =`` Expression
* ``let%view`` LIDENT Pattern Pattern =`` Expression

Signature:

* ``type`` LIDENT ``=`` Type
* ``type`` LIDENT
* ``val%entry`` LIDENT ``:`` Type
* ``val%view`` LIDENT ``:`` Type ``->`` Type

Annot:

* ``[@inline]``
* ``[@private]``

Expression:

* LIDENT
* UIDENT ``.`` LIDENT
* [LIDENT ``.``]+ LIDENT
* [LIDENT ``.``]+ LIDENT ``<-`` Expression
* ``(`` Expression ``:`` Type ``)``
* ``if`` Expression ``then`` Expression
* ``if`` Expression ``then`` Expression ``else`` Expression
* ``Contract.create`` Expression Expression Expression Expression
  Expression Expression ``(contract`` Contract ``)``
* ``let`` ``rec``? Annot* Pattern ``=`` Expression ``in`` Expression
* Expression ``;`` Expression
* ``Loop.loop (fun`` Pattern ``->`` Expression ``)`` Expression
* ``Loop.left (fun`` Pattern ``->`` Expression ``)`` Expression
* Expression Expression
* ``match%nat`` Expression ``with | Plus`` LIDENT ``->`` Expression ``| Minus`` LIDENT ``->`` Expression
* ``match`` Expression ``with | Left`` LIDENT ``->`` Expression ``| Right`` LIDENT ``->`` Expression
* ``match`` Expression ``with | [] ->`` Expression ``|`` LIDENT ``::`` LIDENT ``->`` Expression
* ``match`` Expression ``with`` [ ``|`` MatchPattern ``->`` Expression ]*
* ``Left`` Expression
* ``Right`` Expression
* ``Some`` Expression
* Expression ``::`` Expression
* Constant

Pattern:

* LIDENT
* ``(`` LIDENT ``:`` Type ``)``
* ``_``
* ``(`` Pattern [``,`` Pattern]* ``)``

MatchPattern:

* UIDENT
* UIDENT Pattern


Type:

* ``unit``
* ``bool``
* ``int``
* ``nat``
* ``dun``
* ``string``
* ``bytes``
* ``timestamp``
* ``key``
* ``key_hash``
* ``signature``
* ``operation``
* ``address``
* ``chain_id``
* Type ``option``
* Type ``list``
* Type ``set``
* ``(`` Type ``,`` Type ``) variant``
* ``(`` Type ``,`` Type ``) map``
* ``(`` Type ``,`` Type ``) big_map``
* Type [ ``*`` Type]+
* Type ``->`` Type
* ``_``
* LIDENT

Constant:

* ``dn1`` B58Char+(33)
* ``dn2`` B58Char+(33)
* ``dn3`` B58Char+(33)
* ``edpk`` B58Char+(50)
* ``sppk`` B58Char+(50)
* ``p2pk`` B58Char+(50)
* ``edsig`` B58Char+(94)
* ``p2sig`` B58Char+(93)
* ``spsig1`` B58Char+(93)
* ``KT1`` B58Char+(33)
* ``0x`` [HexChar HexChar]*
* ``true``
* ``false``
* DIGIT [DIGIT | ``_``]*
* DIGIT [DIGIT | ``_``]* ``p``
* DIGIT [DIGIT | ``_``]* [``.`` [DIGIT | ``_``]*]? ``DUN``
* DAY [``T`` HOUR [ TIMEZONE ]?]?
* ``"`` CHAR* ``"``
* ``()``
* ``[`` Constant+`;` ``]``
* ``Map`` | ``Map`` ``[`` Constant+``;`` ``]``
* ``Set`` | ``Set`` ``[`` Constant+``;`` ``]``
* ``BigMap`` | ``BigMap`` ``[`` Constant+``;`` ``]``
* ``fun`` Pattern ``->`` Expression

B58Char:

* [ ``1``- ``9`` | ``A``-``H`` | ``J``-``N`` | ``P``-``Z`` | ``a``-``k`` | ``m``-``z`` ]


HexChar:

* [``0``-``9`` | ``A``-``F`` | ``a``-``f``]


LIDENT:

* [``a``-``z`` | ``_``] [``A``-``Z`` | ``a``-``z`` | ``_`` | ``'`` | ``0``-``9``]*


UIDENT:

* [``A``-``Z``] [``A``-``Z`` | ``a``-``z`` | ``_`` | ``'`` | ``0``-``9``]*


DIGIT:

* [``0``-``9``]


DAY:

* DIGIT+(4) ``-`` DIGIT+(2) ``-`` DIGIT+(2)


HOUR:

* DIGIT+(2) ``:`` DIGIT+(2) [``:`` DIGIT+(2)]?

TIMEZONE:

* ``+`` DIGIT+(2) ``:`` DIGIT+(2)
* ``Z``
