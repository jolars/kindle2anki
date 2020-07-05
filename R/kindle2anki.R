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
#' @param word_history Character vector of file names for previous
#'   `.tsv` exports used to keep a history
#'   of previously exported words in order to avoid duplicates.
#'
#' @return If `outfile` is not `NULL`, a data table is written to `outfile` and
#'   the data table is returned as a `tibble` invisibly. If `outfile` is
#'   `NULL`, the data table is returned visibly and no file it output.
#' @export
#' @seealso [listBooks()]
kindle2anki <- function(file,
                        asin,
                        outfile = paste0(asin, ".tsv"),
                        word_history = NULL,
                        prefer_dictionary_usage = TRUE,
                        progress_bar = TRUE) {

  if (!file.exists(file))
    stop("`file =", file, "` cannot be found")

  if (!is.null(word_history)) {
    if (is.list(word_history))
      word_history <- unlist(word_history)

    stopifnot(is.character(word_history))

    if (!file.exists(word_history))
      stop("`word_history = ", word_history, "` cannot be found")

    word_history <- lapply(word_history, read.table, sep = "\t")
    word_history <- do.call(rbind, word_history)
    colnames(word_history) <-
      c("word", "class", "definition", "synonyms", "usage")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), file)

  books   <- dplyr::as_tibble(dplyr::tbl(con, "BOOK_INFO"))
  lookups <- dplyr::as_tibble(dplyr::tbl(con, "LOOKUPS"))
  words   <- dplyr::as_tibble(dplyr::tbl(con, "WORDS"))

  DBI::dbDisconnect(con)

  asins <- dplyr::pull(books, asin)

  ind <- match(asin, asins)

  if (is.na(ind))
    stop("asin (Amazon ID) does not exist in database")

  id <- dplyr::filter(books, asin == !!asin) %>%
    dplyr::pull(id)

  word_key_usage <- dplyr::filter(lookups, book_key == !!id) %>%
    dplyr::select(word_key, usage) %>%
    dplyr::rename(id = word_key)

  id <- dplyr::pull(word_key_usage, id)

  stems <- dplyr::filter(words, id %in% !!id) %>%
    dplyr::select(id, stem)

  tmp <- dplyr::left_join(word_key_usage, stems, "id") %>%
    dplyr::select(stem, usage)

  classes <- c("ADJECTIVE", "NOUN", "ADVERB", "VERB")

  if (!is.null(word_history)) {
    # avoid duplicating words present in word_history
    stems_history <- word_history$word

    used_ind <- dplyr::pull(tmp, stem) %in% stems_history

    tmp <- tmp[-used_ind, ]
  }

  stems <- dplyr::pull(tmp, stem)
  usages <- dplyr::pull(tmp, usage)

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
        synonyms <- stringr::str_trim(synonyms)

        for (l in seq_along(synsets)) {
          glossary <- synsets[[l]]$getGloss()
          glossary_split <- stringr::str_split(glossary, ";")[[1]]

          # get usage from dictionary (if it exists)
          usage_ind <- grepl("\"", glossary_split)
          usage <- glossary_split[which(usage_ind)[1]]
          usage <- gsub("\"", "", usage)
          usage <- stringr::str_trim(usage)

          # see if usage from dictionary actually contains the word
          # which sometimes is not the case in wordnet
          word_in_usage <- grepl(word, tolower(usage), fixed = TRUE)

          # get definition of word
          definition_ind <- !usage_ind
          definition <- glossary_split[which(definition_ind)[1]]
          definition <- stringr::str_trim(definition)

          if (any(usage_ind) && prefer_dictionary_usage && word_in_usage) {
            # use usage from dictionary
            usage <- glossary_split[which(usage_ind)[1]]
            usage <- gsub("\"", "", usage)
          } else {
            # use usage from book
            usage <- usages[[i]]
          }

          usage <- stringr::str_trim(usage)

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

  out <- dplyr::as_tibble(out)

  if (is.null(outfile)) {
    out
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




