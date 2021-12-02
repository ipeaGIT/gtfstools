#' Filter GTFS object by weekday
#'
#' Filters a GTFS object by weekday, keeping (or dropping) the relevant entries
#' in each file.
#'
#' @param gtfs A GTFS object.
#' @param weekday A character vector. The weekdays used to filter the data.
#' Possible values are `c("monday", "tuesday", "wednesday", "thursday",
#' "friday", "saturday", "sunday")`.
#' @param combine A string. Specifies which logic operation (OR or AND) should
#' be used to filter the calendar table when multiple weekdays are specified.
#' Defaults to `"or"`. Please check the details and examples sections for more
#' information on this argument usage.
#' @param keep A logical. Whether the entries related to the specified weekdays
#' should be kept or dropped (defaults to `TRUE`, which keeps the entries).
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#' process.
#'
#' @details # `combine` usage
#' When filtering the calendar table using weekdays, one could reason about the
#' process in different ways. For example, you may want to keep only services
#' who run on mondays AND tuesdays. Or you may want to keep services that run
#' EITHER on mondays OR on tuesdays. The first case is the equivalent of
#' filtering using the expression `monday == 1 & tuesday == 1`, while the second
#' uses `monday == 1 | tuesday == 1`. You can use the `combine` argument to
#' control this behaviour.
#'
#' Please note that `combine` also works together with `keep`. Using the same
#' examples listed above, you could either keep the entries related to services
#' that run on mondays and tuesdays or drop them, depending on the value you
#' pass to `keep`.
#'
#' @family filtering functions
#'
#' @examples
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' object.size(gtfs)
#'
#' # keeps entries related to services than run EITHER on monday OR on sunday
#' smaller_gtfs <- filter_by_weekday(gtfs, weekday = c("monday", "sunday"))
#' smaller_gtfs$calendar[, c("service_id", "monday", "sunday")]
#' object.size(smaller_gtfs)
#'
#' # keeps entries related to services than run on monday AND on sunday
#' smaller_gtfs <- filter_by_weekday(
#'   gtfs,
#'   weekday = c("monday", "sunday"),
#'   combine = "and"
#' )
#' smaller_gtfs$calendar[, c("service_id", "monday", "sunday")]
#' object.size(smaller_gtfs)
#'
#' # drops entries related to services than run EITHER on monday OR on sunday
#' # the resulting gtfs shouldn't include any trips running on these days
#' smaller_gtfs <- filter_by_weekday(
#'   gtfs,
#'   weekday = c("monday", "sunday"),
#'   keep = FALSE
#' )
#' smaller_gtfs$calendar[, c("service_id", "monday", "sunday")]
#' object.size(smaller_gtfs)
#'
#' # drops entries related to services than run on monday AND on sunday
#' # the resulting gtfs may include trips that run on these days, but no trips
#' # that run on both these days
#' smaller_gtfs <- filter_by_weekday(
#'   gtfs,
#'   weekday = c("monday", "sunday"),
#'   combine = "and",
#'   keep = FALSE
#' )
#' smaller_gtfs$calendar[, c("service_id", "monday", "sunday")]
#' object.size(smaller_gtfs)
#' @export
filter_by_weekday <- function(gtfs, weekday, combine = "or", keep = TRUE) {
  days <- c(
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday"
  )
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_names(weekday, subset.of = days)
  checkmate::assert_character(combine, len = 1)
  checkmate::assert_names(combine, subset.of = c("or", "and"))
  checkmate::assert_logical(keep, len = 1)

  if (gtfsio::check_file_exists(gtfs, "calendar")) {
    gtfsio::assert_field_class(
      gtfs,
      "calendar",
      weekday,
      rep("integer", length(weekday))
    )

    combine_operator <- ifelse(combine == "or", " | ", " & ")

    expr <- paste(weekday, "==", "1", collapse = combine_operator)
    expr <- parse(text = expr)

    filtered_calendar <- gtfs$calendar[eval(expr)]
    relevant_services <- unique(filtered_calendar$service_id)

    gtfs <- filter_by_service_id(gtfs, relevant_services, keep)
  }

  return(gtfs)
}
