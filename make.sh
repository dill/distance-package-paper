#!/bin/sh

Rscript -e "library(knitr);knit('paper.Rnw')"
pdflatex paper.tex
pdflatex paper.tex
open paper.pdf
