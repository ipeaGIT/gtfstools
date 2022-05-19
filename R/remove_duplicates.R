#' Remove duplicated entries
#'
#' Removes duplicated entries from GTFS objects tables.
#'
#' @template gtfs
#'
#' @return A GTFS object containing only unique entries.
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' # this gtfs includes some duplicated entries
#' gtfs$agency
#'
#' gtfs <- remove_duplicates(gtfs)
#' gtfs$agency
#'
#' @export
remove_duplicates <- function(gtfs) {

  checkmate::assert_class(gtfs, "dt_gtfs")

  for (table in names(gtfs)) gtfs[[table]] <- unique(gtfs[[table]])

  return(gtfs)

}
