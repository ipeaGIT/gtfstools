#' gtfstools: General Transit Feed Specification (GTFS) Editing and Analysing
#' Tools
#'
#' Utility functions to read, manipulate, analyse and write transit feeds in the
#' General Transit Feed Specification (GTFS) data format.
#'
#' @section Basic usage:
#' Please check the introductory vignette for basic usage:
#' \itemize{
#'   \item{Run \code{vignette("gtfstools")};}
#'   \item{Alternatively, check it on
#'     \href{https://ipeagit.github.io/gtfstools/articles/gtfstools.html}{
#'     \code{{gtfstools}} website}.}
#' }
#'
#' @docType package
#' @name gtfstools
#' @aliases gtfstools-package
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
    "parent_station"
  )
)
