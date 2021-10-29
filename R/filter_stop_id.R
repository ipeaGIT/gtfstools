#' Filter GTFS object by `stop_id`
#'
#' Filters a GTFS object by `stop_id`s, keeping (or dropping) relevant entries
#' in each file. In order to keep the integrity of trips as described in the
#' `stop_times` table, the `stop_id`s are actually used to filter `trip_id`s,
#' which are then used to filter the rest of the GTFS object.
#'
#' @param gtfs A GTFS object.
#' @param stop_id A character vector. The `stop_id`s used to filter the data.
#' @param keep A logical. Whether the entries related to the `trip_id`s that
#'   passes through the specified `stop_id`s should be kept or dropped (defaults
#'   to `TRUE`, which keeps the entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#' process.
#'
#' @family filtering functions
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#' stop_ids <- c("18848", "940004157")
#'
#' object.size(gtfs)
#'
#' # keeps entries related to passed shape_ids
#' smaller_gtfs <- filter_stop_id(gtfs, stop_ids)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to passed shape_ids
#' smaller_gtfs <- filter_stop_id(gtfs, stop_ids, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_stop_id <- function(gtfs, stop_id, keep = TRUE) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(stop_id)
  checkmate::assert_logical(keep, len = 1)

  env <- environment()

  if (gtfsio::check_fields_exist(gtfs, "stop_times", "stop_id")) {

    gtfsio::assert_fields_types(gtfs, "stop_times", "stop_id", "character")
    relevant_trips <- gtfs$stop_times[
      stop_id %chin% get("stop_id", envir = env)
    ]$trip_id

    gtfs <- filter_trip_id(gtfs, relevant_trips, keep)

  }

  return(gtfs)

}
