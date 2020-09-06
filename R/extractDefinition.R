extractDefinition <- function(glossary) {
  glossary_split <- stringr::str_split(glossary, ";")[[1]]

  # get definition of word
  definition_ind <- !grepl("\"", glossary_split)
  definition <- glossary_split[which(definition_ind)[1]]
  definition <- stringr::str_trim(definition)

  definition
}
