#' Convert Vocabulary from Kindle to Anki
#'
#' This function converts and augments
#' a `*.db` Kindle database file (usually `vocab.db`)
#' to a `.tsv` file ready for import into Anki. It extracts the words
#' used and for then, for each word class (adjectives, nouns, verbs, adverbs)
#' scrapes the wordnet API for a set of definitions, synonyms, and usage.
#' Only the first usage entry is used. The result is a data table with
#' each word and its class, definition, synonyms, and usage. You will usually
#' want to call [listBooks()] on the `file` first and get the Amazon ID
#' to use in the `asin` argument to this function.
#'
#' @param file A path to a `.db` file.
#' @param asin Amazon ID (get it by calling [listBooks()] on `file` first.
#' @param outfile Output file; should end with `.tsv`.
#' @param prefer_dictionary_usage Whether to prefer the usage entry from the
#'   book (Kindle database) or from the wordnet database. If this is `TRUE` but
#'   there is no wordnet entry, the usage entry from the book will be returned.
#' @param progress_bar Should a progress bar we shown?
#'
#' @return
#' @export
#' @seealso [listBooks()]
#'
#' @examples
kindle2anki <- function(file,
                        asin,
                        outfile = paste0(asin, ".tsv"),
                        prefer_dictionary_usage = TRUE,
                        progress_bar = TRUE) {

  if (!file.exists(file))
    stop("file does not exist")

  con <- DBI::dbConnect(RSQLite::SQLite(), file)

  books   <- dplyr::as_tibble(dplyr::tbl(con, "BOOK_INFO"))
  lookups <- dplyr::as_tibble(dplyr::tbl(con, "LOOKUPS"))
  words   <- dplyr::as_tibble(dplyr::tbl(con, "WORDS"))

  DBI::dbDisconnect(con)

  asins <- books %>% dplyr::pull(asin)

  ind <- match(asin, asins)

  if (is.na(ind))
    stop("asin (Amazon ID) does not exist in database")

  id <- books %>%
    dplyr::filter(asin == !!asin) %>%
    dplyr::pull(id)

  word_key_usage <- dplyr::filter(lookups, book_key == !!id) %>%
    dplyr::select(word_key, usage) %>%
    dplyr::rename(id = word_key)

  id <- dplyr::pull(word_key_usage, id)

  stems <- dplyr::filter(words, id %in% !!id) %>%
    dplyr::select(id, stem)

  tmp <- dplyr::left_join(word_key_usage, stems, "id") %>%
    dplyr::select(stem, usage)

  stems <- dplyr::pull(tmp, stem)
  usages <- dplyr::pull(tmp, usage)
  classes <- c("ADJECTIVE", "NOUN", "ADVERB", "VERB")

  wordnet::initDict()

  out <- data.frame()

  if (progress_bar) {
    pb <- progress::progress_bar$new(total = length(stems))
  }

  for (i in seq_along(stems)) {
    if (progress_bar) {
      pb$tick()
    }

    word <- stems[i]

    filt <- wordnet::getTermFilter("ExactMatchFilter", word, TRUE)

    for (j in seq_along(classes)) {
      class <- classes[j]

      terms <- wordnet::getIndexTerms(class, 10, filt)

      if (is.null(terms))
        next

      for (k in seq_along(terms)) {
        synsets <- wordnet::getSynsets(terms[[k]])
        synonyms <- wordnet::getSynonyms(terms[[k]])

        # don't keep main word in synonyms
        synonyms <- synonyms[!(synonyms %in% word)]
        synonyms <- paste(synonyms, collapse = ", ")

        for (l in seq_along(synsets)) {
          glossary <- synsets[[l]]$getGloss()
          glossary_split <- stringr::str_split(glossary, ";")[[1]]

          usage_ind <- grepl("\"", glossary_split)
          definition_ind <- !usage_ind

          definition <- glossary_split[which(definition_ind)[1]]

          if (any(usage_ind) && prefer_dictionary_usage) {
            usage <- glossary_split[which(usage_ind)[1]]
            usage <- gsub("\"", "", usage)
            usage <- stringr::str_trim(usage)
          } else {
            usage <- usages[[i]]
          }

          out <- rbind(out,
                       data.frame(word = word,
                                  class = tolower(class),
                                  definition = definition,
                                  synonyms = synonyms,
                                  usage = usage))
        }
      }
    }
  }

  if (is.null(outfile)) {
    dplyr::as_tibble(out)
  } else {
    if (!dir.exists(dirname(outfile))) {
      dir.create(dirname(outfile), recursive = TRUE)
    }

    write.table(out,
                outfile,
                sep = "\t",
                col.names = FALSE,
                row.names = FALSE)
    invisible(out)
  }
}




