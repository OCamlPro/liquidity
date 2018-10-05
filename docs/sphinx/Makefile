SPHINXBUILD   = sphinx-build
SPHINXOPTS    = -aE -n
SOURCEDIR     = src

sources := $(shell find $(SOURCEDIR) -type f) Makefile ../../.git/logs/HEAD $(SOURCEDIR)/reference/michelson.rst

all: html pdf epub

html: _site
pdf: Liquidity.pdf
epub: Liquidity.epub

_site: $(sources)
	$(SPHINXBUILD) -b html "$(SOURCEDIR)" _site $(SPHINXOPTS)

Liquidity.epub: $(sources)
	$(SPHINXBUILD) -b epub "$(SOURCEDIR)" _epub $(SPHINXOPTS)
	mv _epub/Liquidity.epub Liquidity.epub

Liquidity.pdf: $(sources)
	$(SPHINXBUILD) -b latex "$(SOURCEDIR)" _pdf $(SPHINXOPTS)
	cd _pdf && xelatex Liquidity && xelatex Liquidity
	mv _pdf/Liquidity.pdf Liquidity.pdf

clean:
	rm -rf _site _epub _pdf *~ Liquidity.pdf Liquidity.epub
	find . -name '*~' -exec rm -f {} \;

web: _site
	cd _site && php -S localhost:10000

$(SOURCEDIR)/reference/michelson.rst:
	wget https://gitlab.com/tezos/tezos/raw/betanet/docs/whitedoc/michelson.rst \
	-O $(SOURCEDIR)/reference/michelson.rst
