#' Get Data Table of Book Titles and Amazon IDs
#'
#' @param file input file
#'
#' @return A table of book titles and amazon ids.
#' @export
listBooks <- function(file) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file)
  dplyr::tbl(con, "BOOK_INFO") %>%
    dplyr::select(title, asin) %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::arrange(desc(id))
}
