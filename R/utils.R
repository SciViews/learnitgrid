#' Make sure a directory exists, or create it
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dir_path_create <- function(...) {
  dir <- path(...)
  dir_create(dir)
  dir
}

#' Make sure a directory exists, or stop()
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
dir_path_check <- function(...) {
  dir <- path(...)
  if (!dir_exists(dir))
    stop("The directory '", dir, "' is not found!")
  dir
}

#' Make sure a file exists
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
file_path_check <- function(...) {
  file <- path(...)
  if (!file_exists(file))
    stop("The file '", file, "' is not found!")
  file
}

#' Read csv files without issuing messages
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
read <- function(file) {
  suppressMessages(data.io::read(file))
}

#' Strip www/ in front of the relative paths for the Shiny app
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
strip_www <- function(x) {
  sub("^www/", "", x)
}

#' Make proper chunk labels from strings for parsermd::parse_rmd()
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' chunk_labels(c("Summer is hot", "", NA, " ", "Winter is cold  "))
chunk_labels <- function(x) {
  # Note: knitr accepts spaces and accented character, but not parse_rmd()!
  is_text <- nchar(trimws(x)) > 0 & !is.na(x)
  x2 <- trimws(x[is_text])
  x2 <- make.names(x2)
  x2 <- iconv(x2, "utf8", "ASCII", sub = "byte")
  x2 <- gsub("[<>]", "", x2)
  x2 <- gsub("\\.", "_", x2)
  x[is_text] <- x2
  x
}

#' Make sure than chunk labels are written without spaces
#'
#' @param rmd
#'
#' @return
#' @export
#'
#' @examples
correct_rmd <- function(rmd) {
  # If rmd is the content of an Rmd file. We want to regularize all chunk labels
  # in order to get something form parsermd::parse_rmd()
  is_chunk_start <- grepl("```\\{r.*\\} *$", rmd)
  cs <- trimws(rmd[is_chunk_start])
  cs <- strsplit(sub("\\}$", ",}", cs), ",", fixed = TRUE) # Separate final }

  # Collect first items in a vector and strip leading ````{r
  labels <- trimws(substring(sapply(cs, function(x) x[1]), 6))
  # Make them proper chunk labels
  labels <- chunk_labels(labels)

  # Collect everything after first item from cs
  rest <- sapply(cs, function(x) paste(x[-1], collapse = ","))

  # Reconstitute the whole chunks with proper labels
  chunks <- sub(",\\}$", "}", paste0("```{r ", labels, ",", rest))

  # Inject those corrected chunks starts in the rmd data
  rmd[is_chunk_start] <- chunks

  rmd
}
