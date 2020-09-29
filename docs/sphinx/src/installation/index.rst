
Installation
============

Installation from Sources
-------------------------

To install Liquidity from sources, you will need a working
installation of OCaml with `OPAM
<http://opam.ocaml.org/doc/2.0/Install.html>`__ at least version 2.0.

As of Sept. 29, 2020, the following process should work:

1. Checkout the Github repository::
     
    git clone https://github.com/OCamlPro/liquidity
    cd liquidity
    
  This command should create a ``liquidity`` directory with the ``next`` branch.

2. Within the ``liquidity`` directory, the Dune Network sources in branch
   ``mainnet`` should be in a subdirectory ``dune-network``. This can be
   achieved either with a symbolic link, or by checkouting the sources::

     make clone-dune-network

3. Install Liquidity dependencies::

     make build-deps

4. Build and install::

     make
     make install

  The last command should install the command ``liquidity`` in the
  OPAM switch ``liquidity``.

5. Run a simple test::

     (cd tests && liquidity test0.liq)

6. Optionally, you can build some local documentation with sphinx
   and the Read-The-Docs theme (``pip3 install sphinx-rtd-theme``)::

     make doc

   The documentation should then be available in
   ``docs/sphinx/Liquidity.pdf``, ``docs/sphinx/Liquidity.epub`` and
   in the ``docs/sphinx/_site/`` sub-directory for HTML.
