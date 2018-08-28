SPHINXBUILD   = sphinx-build
SPHINXOPTS    = -aE -n
SOURCEDIR     = src

html: Makefile
	$(SPHINXBUILD) -b html "$(SOURCEDIR)" _site $(SPHINXOPTS)

epub: Makefile
	$(SPHINXBUILD) -b epub "$(SOURCEDIR)" _epub $(SPHINXOPTS)
	mv _epub/Liquidity.epub Liquidity.epub

pdf: Makefile
	$(SPHINXBUILD) -b latex "$(SOURCEDIR)" _pdf $(SPHINXOPTS)
	cd _pdf && xelatex Liquidity && xelatex Liquidity
	mv _pdf/Liquidity.pdf Liquidity.pdf

clean:
	rm -rf _site _epub _pdf *~ Liquidity.pdf Liquidity.epub
	find . -name '*~' -exec rm -f {} \;

web:
	cd _site && php -S localhost:10000
