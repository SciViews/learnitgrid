% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{chunk_labels}
\alias{chunk_labels}
\title{Make proper Rmd/Qmd chunk labels from strings for parsermd::parse_rmd()}
\usage{
chunk_labels(x)
}
\arguments{
\item{x}{A character string of chunk labels to convert}
}
\value{
A character string of the same length as \code{x} with "educated" labels.
}
\description{
Make proper Rmd/Qmd chunk labels from strings for parsermd::parse_rmd()
}
\examples{
chunk_labels(c("Summer is hot", "", NA, " ", "Winter is cold  "))
}
