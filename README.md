A paper about `Distance`
========================

This is a repo for a paper about distance sampling in R, specifically using the package `Distance`.


## Building the paper

The paper is written in RMarkdown. The `.Rmd` file contains all the text and code to generate the PDF.

To build the paper, you can use RStudio create a new project and hit "Knit". This should generate the PDF. References are stored in `jstatsoft.bib`, which is a copy of my full bibliography (need to clean that up before submission).

## Contributing

To contribute, simply edit the `paper.Rmd` file, generate a new PDF and commit.

Must install `rticles`  
`devtools:::install_github("rstudio/rticles")`

