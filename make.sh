#!/bin/sh

R -e 'rmarkdown::render("paper.Rmd")'
open paper.pdf
