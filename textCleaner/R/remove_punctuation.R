#' Remove punctuation from text
#'
#' @param text The input text from which to remove punctuation
#' @return A character string without punctuation
#' @export
#' @examples
#' remove_punctuation("Hello, world!")
remove_punctuation <- function(text) {
  gsub("[[:punct:]]", "", text)
}
