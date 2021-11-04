#' gtfstools: General Transit Feed Specification (GTFS) Editing and Analysing
#' Tools
#'
#' Utility functions to read, manipulate, analyse and write transit feeds in the
#' General Transit Feed Specification (GTFS) data format.
#'
#' @section Basic usage:
#' Please check the introductory vignette for basic usage:
#' - Run `vignette("gtfstools")`;
#' - Alternatively, check it on [`{gtfstools}` website](
#' https://ipeagit.github.io/gtfstools/articles/gtfstools.html).
#'
#' @docType package
#' @name gtfstools
#' @aliases gtfstools-package
#' @useDynLib gtfstools, .registration = TRUE
#'
#' @importFrom data.table := .I .SD %chin%
"_PACKAGE"

utils::globalVariables(
  c(
    ".",
    "stop_sequence",
    "departure_time_secs",
    "arrival_time_secs",
    "last_stop_departure",
    "segment",
    "duration",
    "result",
    "geometry",
    "shape_id",
    "shape_pt_sequence",
    "stop_sequence",
    "origin_file",
    "file_spec",
    "file_provided_status",
    "field_provided_status",
    "field_spec",
    "validation_details",
    "i.stop_lat",
    "i.stop_lon",
    "arrival_time",
    "departure_time",
    "i.duration",
    "speed",
    "route_id",
    "trip_id",
    "stop_id",
    "route_type",
    "parent_station",
    "agency_id",
    "fare_id",
    "service_id",
    "start_date",
    "end_date",
    "level_id",
    "origin_id",
    "destination_id",
    "contains_id",
    "from_stop_id",
    "to_stop_id"
  )
)
