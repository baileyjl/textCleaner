#' Standardise Punctuation from Unicode to ASCII
#'
#' This function replaces specified Unicode punctuation characters with their ASCII equivalents in a given text.
#'
#' @param text A character vector containing the text to be cleaned.
#' @return A character vector with standardised punctuation.
#' @export
#' @examples
#' standardize_punctuation("‘This’ is “a” test—text")
#' @importFrom dplyr mutate
#' @importFrom purrr reduce
#' @import tibble
#' @import stringr
standardize_punctuation <- function(text) {
  character_mapping <- tibble::tibble(
    unwanted = c("\u2018", "\u2019", "\u201C", "\u201D", "\u2013",
                 "\u2014", "\u201E", "\u201A", "\u201B", "\u201C",
                 "\u201D", "\u201E", "\u2032", "\u2033"),
    wanted = c("'", "'", "\"", "\"", "-", "--", "\"", ",", "'", "\"",
               "\"", "\"", "'", "\"")
  )
  
  cleaned_text <- purrr::reduce(character_mapping$unwanted, function(txt, pattern) {
    gsub(pattern, character_mapping$wanted[character_mapping$unwanted == pattern], txt, fixed = TRUE)
  }, .init = text)
  
  return(cleaned_text)
}
