#' Check for GTFS text file existence
#'
#' Checks for the existence of a given file in a GTFS object. Must be used in
#' conjunction with \code{\link[checkmate]{assert}}.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param file File to check existence of.
#'
#' @return If the check is successful, returns \code{TRUE} invisibly. Else,
#'   returns a string with error message.
check_gtfs_file_exists <- function(gtfs, file) {

  checkmate::assert_class(gtfs, "gtfs")
  checkmate::assert_character(file)

  non_existent_file <- file[! file %chin% names(gtfs)]

  if (identical(non_existent_file, character(0))) {

    return(invisible(TRUE))

  } else {

    error_message <- ifelse(
      length(non_existent_file) == 1,
      paste0("Must contain '", non_existent_file, "' file"),
      paste0(
        "Must contain the following files: ",
        paste0("'", non_existent_file, "'", collapse = ", ")
      )
    )

    return(error_message)

  }

}



#' Convert string to time
#'
#' Converts strings in the "HH:MM:SS" format to \code{hms}.
#'
#' @param string A string in "HH:MM:SS" format.
#'
#' @return An \code{hms} object.
string_to_hms <- function(string) {

  checkmate::assert_character(string)

  split_string <- strsplit(string, ":")

  seconds_from_midgnight <- vapply(
    split_string,
    function(i) sum(as.integer(i) * c(3600L, 60L, 1L)),
    integer(1)
  )

  # "" strings result in seconds_to_midnight = 0. find those and replace to NA

  index_na <- which(lengths(split_string) == 0)
  seconds_from_midgnight[index_na] <- NA_integer_

  return(hms::hms(seconds = seconds_from_midgnight))

}



#' Create GTFS metadata
#'
#' Creates GTFS metadata (a list including all possible fields for each text
#' file, their column types and which ones are optional/required).
#'
#' @return A GTFS metadata list.
get_gtfs_meta <- function() {

  # required files ----------------------------------------------------------

  # agency
  assign("agency", list())
  agency$field <- c(
    "agency_id",
    "agency_name",
    "agency_url",
    "agency_timezone",
    "agency_lang",
    "agency_phone",
    "agency_fare_url",
    "agency_email"
  )
  agency$field_spec <- c("opt", "req", "req", "req", "opt", "opt", "opt", "opt")
  names(agency$field_spec) <- agency$field
  agency$coltype <- rep("character", length(agency$field))
  names(agency$coltype) <- agency$field
  agency$file_spec <- "req"

  # stops
  assign("stops", list())
  stops$field <- c(
    "stop_id",
    "stop_code",
    "stop_name",
    "stop_desc",
    "stop_lat",
    "stop_lon",
    "zone_id",
    "stop_url",
    "location_type",
    "parent_station",
    "stop_timezone",
    "wheelchair_boarding",
    "level_id",
    "platform_code"
  )
  stops$field_spec <- c("req", "opt", "req", "opt", "req", "req", rep("opt", 8))
  names(stops$field_spec) <- stops$field
  stops$coltype <- rep("character", length(stops$field))
  names(stops$coltype) <- stops$field
  stops$coltype[c("stop_lat", "stop_lon")] <- "double"
  stops$coltype[c("location_type", "wheelchair_boarding")] <- "integer"
  stops$file_spec <- "req"

  # routes
  assign("routes", list())
  routes$field <- c(
    "route_id",
    "agency_id",
    "route_short_name",
    "route_long_name",
    "route_desc",
    "route_type",
    "route_url",
    "route_color",
    "route_text_color",
    "route_sort_order",
    "continuous_pickup",
    "continuous_drop_off"
  )
  routes$field_spec <- c("req", "opt", "req", "req", "opt", "req", rep("opt", 6))
  names(routes$field_spec) <- routes$field
  routes$coltype <- rep("character", length(routes$field))
  names(routes$coltype) <- routes$field
  integer_fields <- c(
    "route_type",
    "route_sort_order",
    "continuous_pickup",
    "continuous_drop_off"
  )
  routes$coltype[integer_fields] <- "integer"
  routes$file_spec <- "req"

  # trips
  assign("trips", list())
  trips$field <- c(
    "route_id",
    "service_id",
    "trip_id",
    "trip_headsign",
    "trip_short_name",
    "direction_id",
    "block_id",
    "shape_id",
    "wheelchair_accessible",
    "bikes_allowed"
  )
  trips$field_spec <- c("req", "req", "req", rep("opt", 7))
  names(trips$field_spec) <- trips$field
  trips$coltype <- rep("character", length(trips$field))
  names(trips$coltype) <- trips$field
  integer_fields <- c(
    "direction_id",
    "wheelchair_accessible",
    "bikes_allowed"
  )
  trips$coltype[integer_fields] <- "integer"
  trips$file_spec <- "req"

  # stop_times
  assign("stop_times", list())
  stop_times$field <- c(
    "trip_id",
    "arrival_time",
    "departure_time",
    "stop_id",
    "stop_sequence",
    "stop_headsign",
    "pickup_type",
    "drop_off_type",
    "continuous_pickup",
    "continuous_drop_off",
    "shape_dist_traveled",
    "timepoint"
  )
  stop_times$field_spec <- c("req", "req", "req", "req", "req", rep("opt", 7))
  names(stop_times$field_spec) <- stop_times$field
  stop_times$coltype <- rep("character", length(stop_times$field))
  names(stop_times$coltype) <- stop_times$field
  integer_fields <- c(
    "stop_sequence",
    "pickup_type",
    "drop_off_type",
    "continuous_pickup",
    "continuous_drop_off",
    "timepoint"
  )
  stop_times$coltype[integer_fields] <- "integer"
  stop_times$coltype["shape_dist_traveled"] <- "double"
  stop_times$coltype[c("arrival_time", "departure_time")] <- "time"
  stop_times$file_spec <- "req"

  # calendar
  assign("calendar", list())
  calendar$field <- c(
    "service_id",
    "monday",
    "tuesday",
    "wednesday",
    "thursday",
    "friday",
    "saturday",
    "sunday",
    "start_date",
    "end_date"
  )
  calendar$field_spec <- rep("req", length(calendar$field))
  names(calendar$field_spec) <- calendar$field
  calendar$coltype <- c("character", rep("integer", 7), rep("date", 2))
  names(calendar$coltype) <- calendar$field
  calendar$file_spec <- "req"

  # optional files ----------------------------------------------------------

  # calendar_dates
  assign("calendar_dates", list())
  calendar_dates$field <- c("service_id", "date", "exception_type")
  calendar_dates$field_spec <- c("req", "req", "req")
  names(calendar_dates$field_spec) <- calendar_dates$field
  calendar_dates$coltype <- c("character", "date", "integer")
  names(calendar_dates$coltype) <- calendar_dates$field
  calendar_dates$file_spec <- "opt"

  # fare_attributes
  assign("fare_attributes", list())
  fare_attributes$field <- c(
    "agency_id",
    "fare_id",
    "price",
    "currency_type",
    "payment_method",
    "transfers",
    "transfer_duration"
  )
  fare_attributes$field_spec <- c("opt", "req", "req", "req", "req", "req", "opt")
  names(fare_attributes$field_spec) <- fare_attributes$field
  fare_attributes$coltype <- rep("character", length(fare_attributes$field))
  names(fare_attributes$coltype) <- fare_attributes$field
  integer_fields <- c("payment_method", "transfers", "transfer_duration")
  fare_attributes$coltype[integer_fields] <- "integer"
  fare_attributes$coltype["price"] <- "double"
  fare_attributes$file_spec <- "opt"

  # fare_rules
  assign("fare_rules", list())
  fare_rules$field <- c(
    "fare_id",
    "route_id",
    "origin_id",
    "destination_id",
    "contains_id"
  )
  fare_rules$field_spec <- c("req", "opt", "opt", "opt", "opt")
  names(fare_rules$field_spec) <- fare_rules$field
  fare_rules$coltype <- rep("character", length(fare_rules$field))
  names(fare_rules$coltype) <- fare_rules$field
  fare_rules$file_spec <- "opt"

  # shapes
  assign("shapes", list())
  shapes$field <- c(
    "shape_id",
    "shape_pt_lat",
    "shape_pt_lon",
    "shape_pt_sequence",
    "shape_dist_traveled"
  )
  shapes$field_spec <- c("req", "req", "req", "req", "opt")
  names(shapes$field_spec) <- shapes$field
  shapes$coltype <- c("character", rep("double", 2), "integer", "double")
  names(shapes$coltype) <- shapes$field
  shapes$file_spec <- "opt"

  # frequencies
  assign("frequencies", list())
  frequencies$field <- c(
    "trip_id",
    "start_time",
    "end_time",
    "headway_secs",
    "exact_times"
  )
  frequencies$field_spec <- c("req", "req", "req", "req", "opt")
  names(frequencies$field_spec) <- frequencies$field
  frequencies$coltype <- c("character", rep("time", 2), rep("integer", 2))
  names(frequencies$coltype) <- frequencies$field
  frequencies$file_spec <- "opt"

  # transfers
  assign("transfers", list())
  transfers$field <- c(
    "from_stop_id",
    "to_stop_id",
    "transfer_type",
    "min_transfer_time"
  )
  transfers$field_spec <- c("req", "req", "req", "opt")
  names(transfers$field_spec) <- transfers$field
  transfers$coltype <- c("character", "character", "integer", "integer")
  names(transfers$coltype) <- transfers$field
  transfers$file_spec <- "opt"

  # pathways
  assign("pathways", list())
  pathways$field <- c(
    "pathway_id",
    "from_stop_id",
    "to_stop_id",
    "pathway_mode",
    "is_bidirectional",
    "length",
    "traversal_time",
    "stair_count",
    "max_slope",
    "min_width",
    "signposted_as",
    "reversed_signposted_as"
  )
  pathways$field_spec <- c(rep("req", 5), rep("opt", 7))
  names(pathways$field_spec) <- pathways$field
  pathways$coltype <- rep("character", length(pathways$field))
  names(pathways$coltype) <- pathways$field
  integer_fields <- c(
    "pathway_mode",
    "is_bidirectional",
    "traversal_time",
    "stair_count"
  )
  pathways$coltype[integer_fields] <- "integer"
  pathways$coltype[c("length", "max_slope", "min_width")] <- "double"
  pathways$file_spec <- "opt"

  # levels
  assign("levels", list())
  levels$field <- c("level_id", "level_index", "level_name")
  levels$field_spec <- c("req", "req", "opt")
  names(levels$field_spec) <- levels$field
  levels$coltype <- c("character", "double", "character")
  names(levels$coltype) <- levels$field
  levels$file_spec <- "opt"

  # feed_info
  assign("feed_info", list())
  feed_info$field <- c(
    "feed_publisher_name",
    "feed_publisher_url",
    "feed_lang",
    "feed_start_date",
    "feed_end_date",
    "feed_version",
    "feed_contact_email",
    "feed_contact_url"
  )
  feed_info$field_spec <- c("req", "req", "req", rep("opt", 5))
  names(feed_info$field_spec) <- feed_info$field
  feed_info$coltype <- rep("character", length(feed_info$field))
  names(feed_info$coltype) <- feed_info$field
  feed_info$coltype[c("feed_start_date", "feed_end_date")] <- "date"
  feed_info$file_spec <- "opt"

  # translations
  assign("translations", list())
  translations$field <- c(
    "table_name",
    "field_name",
    "language",
    "translation",
    "record_id",
    "record_sub_id",
    "field_value"
  )
  translations$field_spec <- c("req", "req", "req", "req", "opt", "opt", "opt")
  names(translations$field_spec) <- translations$field
  translations$coltype <- c("integer", rep("character", 6))
  names(translations$coltype) <- translations$field
  translations$file_spec <- "opt"

  # attributions
  assign("attributions", list())
  attributions$field <- c(
    "attribution_id",
    "agency_id",
    "route_id",
    "trip_id",
    "organization_name",
    "is_producer",
    "is_operator",
    "is_authority",
    "attribution_url",
    "attribution_email",
    "attribution_phone"
  )
  attributions$field_spec <- c(rep("opt", 4), "req", rep("opt", 6))
  names(attributions$field_spec) <- attributions$field
  attributions$coltype <- rep("character", length(attributions$field))
  names(attributions$coltype) <- attributions$field
  integer_fields <- c("is_producer", "is_operator", "is_authority")
  attributions$coltype[integer_fields] <- "integer"
  attributions$file_spec <- "opt"

  # create meta object ####

  meta <- list(
    agency,
    stops,
    routes,
    trips,
    stop_times,
    calendar,
    calendar_dates,
    fare_attributes,
    fare_rules,
    shapes,
    frequencies,
    transfers,
    pathways,
    levels,
    feed_info,
    translations,
    attributions
  )

  attributes(meta) <- list(
    names = paste0(
      c("agency",
        "stops",
        "routes",
        "trips",
        "stop_times",
        "calendar",
        "calendar_dates",
        "fare_attributes",
        "fare_rules",
        "shapes",
        "frequencies",
        "transfers",
        "pathways",
        "levels",
        "feed_info",
        "translations",
        "attributions"
      ),
      ".txt"
    ),
    class = "gtfs_metadata"
  )

  return(meta)

}
