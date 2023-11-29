#' Filter GTFS object by `stop_id`
#'
#' Filters a GTFS object by `stop_id`s, keeping (or dropping) relevant entries
#' in each file.
#'
#' @template gtfs
#' @param stop_id A character vector. The `stop_id`s used to filter the data.
#' @param keep A logical. Whether the entries related to the `trip_id`s that
#'   passes through the specified `stop_id`s should be kept or dropped (defaults
#'   to `TRUE`, which keeps the entries).
#' @param full_trips A logical. Whether to keep all stops that compose trips
#'   that pass through the stops specified in `stop_id`. Defaults to `TRUE`, in
#'   order to preserve the behavior of the function in versions 1.2.0 and below.
#'   Please note that when `TRUE`, the resultant filtered feed may contain more
#'   stops than the ones specified in `stop_id` to preserve the integrity of the
#'   trips. IMPORTANT: using `full_trips = TRUE` is flagged as deprecated as of
#'   version 1.3.0 and this parameter will default to `FALSE` from version 2.0.0
#'   onward.
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
#' # keeps entries related to trips that pass through specified stop_ids
#' smaller_gtfs <- filter_by_stop_id(gtfs, stop_ids)
#' object.size(smaller_gtfs)
#'
#' # drops entries related to trips that pass through specified stop_ids
#' smaller_gtfs <- filter_by_stop_id(gtfs, stop_ids, keep = FALSE)
#' object.size(smaller_gtfs)
#'
#' @export
filter_by_stop_id <- function(gtfs, stop_id, keep = TRUE, full_trips = TRUE) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(stop_id, any.missing = FALSE)
  checkmate::assert_logical(keep, len = 1, any.missing = FALSE)
  checkmate::assert_logical(full_trips, len = 1, any.missing = FALSE)

  if (full_trips) {
    full_trips_deprecation_warning()

    env <- environment()

    if (gtfsio::check_field_exists(gtfs, "stop_times", "stop_id")) {
      gtfsio::assert_field_class(gtfs, "stop_times", "stop_id", "character")
      relevant_trips <- unique(
        gtfs$stop_times[stop_id %chin% get("stop_id", envir = env)]$trip_id
      )

      gtfs <- filter_by_trip_id(gtfs, relevant_trips, keep)
    }
  }

  return(gtfs)

}

full_trips_deprecation_warning <- function() {
  rlang::warn(
    class = "deprecated_full_trips_filter",
    message = c(
      paste0(
        "The 'filter_by_stop_id()' behavior of filtering by trips that ",
        "contain the specified stops has been DEPRECATED."
      ),
      "*" = paste0(
        "For backwards compatibility reasons, this behavior is still the ",
        "default as of version 1.3.0, and is controlled by the parameter ",
        "'full_trips'."
      ),
      "v" = paste0(
        "Please use 'full_trips = FALSE' to actually filter by 'stop_ids'. ",
        "This behavior will be the default from version 2.0.0 onward."
      ),
      "v" = paste0(
        "To achieve the old behavior, manually subset the 'stop_times' table ",
        "by 'stop_id' and specify the 'trip_ids' included in the output in ",
        "'filter_by_trip_id()'."
      )
    )
  )
}
