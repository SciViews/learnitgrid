# learnitgrid

Marking work on the basis of marking grids is time-consuming and requires particular attention to ensure fair marking from one student to another.

learnitgrid, via a new web application {shiny}, makes it possible to mark all the work in a series criterion by criterion. The relevant part of each piece of work, identified by a title or a piece label, is extracted from the R/R Markdown/ Quarto documents. These extracts are grouped and sorted automatically using a text similarity calculation algorithm in the {stringdist} package. This makes it easier to score similar responses in the same way. This method also reduces the time needed for correction. Clickable links allow quick access to the complete job and to any context (dataset, additional documents, etc.), which further improves the speed of correction and the user's comfort when filling in the criteria grids.

This package is associated with the [{learnitdown}](https://github.com/SciViews/learnitdown) package.

## Installation

You can also install the latest development version. Make sure you have the {remotes} R package installed:

``` r
install.packages("remotes")
```

Use `install_github()` to install the {learnitgrid} package from GitHub (source from **main** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/learnitgrid")
```

## Code of Conduct

Please note that the {learnitgrid} package is released with a [Contributor Code of Conduct](https://github.com/SciViews/learnitgrid/blob/main/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
