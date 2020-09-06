extractUsage <- function(glossary, word) {
  glossary_split <- stringr::str_split(glossary, ";")[[1]]

  # get usage from dictionary (if it exists)
  usage_ind <- grepl("\"", glossary_split)
  usage <- glossary_split[which(usage_ind)[1]]
  usage <- gsub("\"", "", usage)
  usage <- stringr::str_trim(usage)

  # see if usage from dictionary actually contains the word
  # which sometimes is not the case in wordnet
  word_in_usage <- grepl(word, tolower(usage), fixed = TRUE)

  if (any(usage_ind) && word_in_usage) {
    # use usage from dictionary
    usage <- glossary_split[which(usage_ind)[1]]
    usage <- gsub("\"", "", usage)
  } else {
    # no useful usage entry, return NA
    usage <- NA
  }

  usage <- stringr::str_trim(usage)

  usage
}
