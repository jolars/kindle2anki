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

    for (i in seq_along(word_history)) {
      if (!file.exists(word_history[i]))
        stop("`word_history = ", word_history[i], "` cannot be found")
    }

    word_history <- lapply(word_history, read.table, sep = "\t")
    word_history <- do.call(rbind, word_history)
    colnames(word_history) <-
      c("ID", "word", "class", "definition", "synonyms", "usage", "language")
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

  language <- gsub("\\:.*", "", id, perl = TRUE)

  language_table <- data.frame(
    language = c("swedish", "english"),
    code = c("se", "en")
  )

  language <- vapply(language,
                     function(x) language_table$language[match(x, language_table$code)],
                     FUN.VALUE = character(1))

  stems <- dplyr::filter(words, id %in% !!id) %>%
    dplyr::select(id, stem)

  tmp <- dplyr::left_join(word_key_usage, stems, "id") %>%
    dplyr::select(stem, usage)

  classes <- c("ADJECTIVE", "NOUN", "ADVERB", "VERB")

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

      synsets <- unlist(lapply(terms, wordnet::getSynsets))

      synonyms <- extractSynonyms(terms, word)

      definition <- character(length(synsets))
      usage <- character(length(synsets))

      for (l in seq_along(synsets)) {
        glossary <- synsets[[l]]$getGloss()

        definition[l] <- extractDefinition(glossary)
        usage[l] <- extractUsage(glossary, word)
      }

      usage <- usage[!is.na(usage)]

      if (prefer_dictionary_usage && length(usage) > 0) {
        usage <- usage[1] # just pick the first usage entry
      } else {
        usage <- stringr::str_trim(usages[i])
      }

      if (length(definition) > 1) {
        definition <- paste0("<ul><li>",
                             paste0(definition, collapse = "</li><li>"),
                             "</li></ul>")
      }

      out <- rbind(out,
                   data.frame(ID = paste(word, tolower(class), language[i], sep = "_"),
                              word = word,
                              class = tolower(class),
                              definition = definition,
                              synonyms = synonyms,
                              usage = usage,
                              language = language[i]))
    }
  }

  out <- dplyr::as_tibble(out)

  # remove duplicates
  out <- out[!duplicated(out$ID), ]

  if (!is.null(word_history)) {
    # avoid duplicating words present in word_history
    used_ind <- dplyr::pull(out, ID) %in% word_history$ID

    out <- out[-used_ind, ]
  }

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






