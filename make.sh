#!/bin/sh

Rscript -e "library(knitr);knit('jss2630.Rnw')"
pdflatex jss2630.tex
bibtex jss2630
pdflatex jss2630.tex
open jss2630.pdf
