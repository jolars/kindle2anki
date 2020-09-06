extractSynonyms <- function(terms, word) {
  synonyms <- unlist(lapply(terms, wordnet::getSynonyms))

  # don't keep main word in synonyms
  synonyms <- synonyms[!(synonyms %in% word)]
  synonyms <- paste(synonyms, collapse = ", ")
  synonyms <- stringr::str_trim(synonyms)

  synonyms
}
