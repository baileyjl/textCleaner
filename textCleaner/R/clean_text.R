#' Clean Text
#'
#' This function cleans text by removing URLs, replacing "&amp;" with "and",
#' replacing newline characters with space, adding space between text and emoji, removing extra spaces,
#' and trimming spaces at the beginning and end of the text.
#'
#' @param text A character string containing the text to be cleaned.
#' @param remove_urls Logical, indicating whether to remove URLs.
#' @param replace_amp Logical, indicating whether to replace "&amp;" with "and".
#' @param replace_newline Logical, indicating whether to replace newline characters with space.
#' @param space_emoji Logical, indicating whether to add space between text and emoji.
#' @param remove_extra_spaces Logical, indicating whether to remove extra spaces.
#' @param trim_spaces Logical, indicating whether to trim spaces at the beginning and end of the text.
#' @return A cleaned text as a character string.
#' @export
#' @importFrom stringr str_remove_all str_replace_all str_trim
#' @importFrom stringi stri_replace_all_regex
#' @examples
#' clean_text("This is a test with a URL http://test.com &amp; some emojis 😊", remove_urls = TRUE, replace_amp = TRUE)
clean_text <- function(text, remove_urls = TRUE, replace_amp = TRUE, replace_newline = TRUE, space_emoji = TRUE, remove_extra_spaces = TRUE, trim_spaces = TRUE) {
  if (remove_urls) {
    text <- stringr::str_remove_all(text, " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")
  }
  if (replace_amp) {
    text <- stringr::str_replace_all(text, "&amp;", "and")
  }
  if (replace_newline) {
    text <- stringr::str_replace_all(text, "\n", " ")
  }
  if (space_emoji) {
    text <- stringr::str_replace_all(text, "([[:alnum:][:punct:]])(\\p{So})", "\1 \2")
  }
  if (remove_extra_spaces) {
    text <- stringi::stri_replace_all_regex(text, "\\s+", " ")
  }
  if (trim_spaces) {
    text <- stringr::str_trim(text, "both")
  }
  return(text)
}
