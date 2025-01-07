#' Remove duplicated entries
#'
#' Removes duplicated entries from GTFS objects tables.
#'
#' @template gtfs
#'
#' @return A GTFS object containing only unique entries.
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
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

  gtfs <- assert_and_assign_gtfs_object(gtfs)

  for (table in names(gtfs)) gtfs[[table]] <- unique(gtfs[[table]])

  return(gtfs)

}
