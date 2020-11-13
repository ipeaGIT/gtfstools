#' @importFrom dplyr %>%
#' @importFrom data.table := .I .SD
NULL

utils::globalVariables(
  c(
    "stop_sequence",
    "departure_time_hms",
    "arrival_time_hms",
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
    "validation_details"
  )
)
