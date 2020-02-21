SPHINXBUILD   = sphinx-build
SPHINXOPTS    = -aE -n
SOURCEDIR     = src
LIQUIDITY     = ../../liquidity
LIQUIDITYCLIENT = ../../liquidity-client

sources := $(shell find $(SOURCEDIR) -type f) Makefile ../../.git/logs/HEAD $(SOURCEDIR)/reference/michelson.rst

all: html pdf epub

html: _site
pdf: Liquidity.pdf
epub: Liquidity.epub

manpages:
	./mk_man.sh liquidity
	./mk_man.sh liquidity-client
	./mk_man.sh liquidity-client call
	./mk_man.sh liquidity-client call-arg
	./mk_man.sh liquidity-client deploy
	./mk_man.sh liquidity-client forge-call
	./mk_man.sh liquidity-client forge-deploy
	./mk_man.sh liquidity-client get-storage
	./mk_man.sh liquidity-client init-storage
	./mk_man.sh liquidity-client inject
	./mk_man.sh liquidity-client pack
	./mk_man.sh liquidity-client run

_site: $(sources) manpages
	$(SPHINXBUILD) -b html "$(SOURCEDIR)" _site $(SPHINXOPTS)

Liquidity.epub: $(sources) manpages
	$(SPHINXBUILD) -b epub "$(SOURCEDIR)" _epub $(SPHINXOPTS)
	mv _epub/Liquidity.epub Liquidity.epub

Liquidity.pdf: $(sources) manpages
	$(SPHINXBUILD) -b latex "$(SOURCEDIR)" _pdf $(SPHINXOPTS)
	cd _pdf && xelatex Liquidity && xelatex Liquidity
	mv _pdf/Liquidity.pdf Liquidity.pdf

clean:
	rm -rf _site _epub _pdf *~ Liquidity.pdf Liquidity.epub src/man/liquidity*
	find . -name '*~' -exec rm -f {} \;

web: _site
	cd _site && php -S localhost:10000

$(SOURCEDIR)/reference/michelson.rst:
	wget https://gitlab.com/dune-network/dune-network/raw/mainnet/docs/whitedoc/michelson.rst \
	-O $(SOURCEDIR)/reference/michelson.rst
