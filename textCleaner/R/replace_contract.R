#' Replace contractions and clean text
#'
#' Replaces contractions with their expanded forms using either a predefined list from the 'lexicon' package or a custom list. Additionally, offers options to replace trailing 's with 'is', remove trailing white space, and replace remaining apostrophes with a space. Case sensitivity can be toggled.
#'
#' @param text A character vector containing the text to be cleaned.
#' @param custom_df An optional custom data frame containing contraction and expanded form mappings.
#' @param use_lexicon Logical, indicating whether to use the predefined contractions list from the 'lexicon' package.
#' @param ignore_case Logical, indicating whether to ignore case when replacing contractions.
#' @param replace_trailing_s Logical, indicating whether to replace trailing 's with 'is'.
#' @param trim_whitespace Logical, indicating whether to trim white space at the beginning and end of the text.
#' @param replace_apostrophe Logical, indicating whether to replace remaining apostrophes with a space.
#' @return A character vector with cleaned text.
#' @export
#' @examples
#' replace_contractions("He's not going to the party because he doesn't want to.", ignore_case = TRUE)
#' @importFrom stringr str_replace_all str_trim
#' @importFrom stringi stri_replace_all_regex
replace_contractions <- function(text, custom_df = NULL, use_lexicon = TRUE, ignore_case = TRUE, replace_trailing_s = TRUE, trim_whitespace = TRUE, replace_apostrophe = TRUE) {
  if (use_lexicon) {
    if (!requireNamespace('lexicon', quietly = TRUE)) {
      stop('lexicon package is required for use_lexicon option.')
    }
    df <- lexicon::key_contractions
  } else if (!is.null(custom_df)) {
    df <- custom_df
  } else {
    stop('No contractions data frame provided.')
  }
  for (i in seq_len(nrow(df))) {
    pattern <- paste0('(?<!\\S)', df$contraction[i], '(?!\\S)')
    text <- gsub(pattern, df$expanded[i], text, ignore.case = ignore_case, perl = TRUE)
  }
  if (replace_trailing_s) {
    text <- stringr::str_replace_all(text, if (ignore_case) '(?i)\\'s' else '\\'s', ' is')
  }
  if (trim_whitespace) {
    text <- stringr::str_trim(text)
  }
  if (replace_apostrophe) {
    text <- gsub('\'', ' ', text)
  }
  return(text)
}
