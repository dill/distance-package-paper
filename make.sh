#!/bin/sh

Rscript -e "library(knitr);knit('paper.Rnw')"
pdflatex paper.tex
bibtex paper
pdflatex paper.tex
open paper.pdf
