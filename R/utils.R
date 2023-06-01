#' Make sure a directory exists, or create it
#'
#' @param ... The successive folders that make the path.
#'
#' @return A **path** object is returned invisibly.
#' @export
#'
#' @examples
#' test_path <- dir_path_create(tempdir(), "dir_path_create_test", "subfolder")
#' test_path
#' dir.exists(test_path) # Should be TRUE
#' dir_path_check(test_path) # Return the path only if it exists
#' # Remove
#' unlink(test_path)
#' unlink(dirname(test_path))
dir_path_create <- function(...) {
  dir <- path(...)
  dir_create(dir)
  invisible(dir)
}

#' @describeIn dir_path_create Check that a directory exists.
#' @export
dir_path_check <- function(...) {
  dir <- path(...)
  if (!dir_exists(dir))
    stop("The directory '", dir, "' is not found!")
  invisible(dir)
}

#' @describeIn dir_path_create Check that a file exists.
#' @export
file_path_check <- function(...) {
  file <- path(...)
  if (!file_exists(file))
    stop("The file '", file, "' is not found!")
  invisible(file)
}

#' Make proper Rmd/Qmd chunk labels from strings for parsermd::parse_rmd()
#'
#' @param x A character string of chunk labels to convert
#'
#' @return A character string of the same length as `x` with "educated" labels.
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

#' Make sure that Rmd/Qmd chunk labels are written without spaces
#'
#' @param rmd Character string with the content of a Rmd/Qmd file.
#'
#' @return The same Rmd/Qmd content, but with "educated" chunk labels.
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
