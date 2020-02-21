#! /usr/bin/env sh

cmd=$@
file=$(echo $cmd | tr ' ' '-')
dsthtml=src/man/$file.html
dst=src/man/$file.rst

../../$cmd --help=groff | groff -Thtml > $dsthtml
echo "\`\`$cmd\`\`" > $dst
echo "============" >> $dst
echo >> $dst
echo ".. _$file:" >> $dst
echo ".. raw:: html" >> $dst
echo "  :file: $file.html"  >> $dst
