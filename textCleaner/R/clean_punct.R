#' Clean punctuation and whitespace
#'
#' Adds space around punctuation, reduces multiple spaces to a single space,
#' removes spaces around apostrophes, and trims leading and trailing whitespace.
#'
#' @param text A character vector with text to format.
#' @return A character vector with formatted text.
#' @export
#' @examples
#' clean_punct("Hello, world! 'This is an example.'")
#' @importFrom stringr str_trim
#' @importFrom stringi stri_replace_all_regex
clean_punct <- function(text) {
  text <- gsub("([[:punct:]])", " \\1 ", text)
  text <- stringi::stri_replace_all_regex(text, "\\s+", " ")
  text <- gsub("'\\s+", "'", text)
  text <- gsub("\\s+'", "'", text)
  text <- stringr::str_trim(text)
  return(text)
}
