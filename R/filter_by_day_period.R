#' Filter GTFS object by day period
#'
#' Filters a GTFS object by a day period, keeping (or dropping) the relevant
#' entries in each file.
#'
#' @param gtfs A GTFS object.
#' @param from A string. The starting point of the day period, in the
#' "HH:MM:SS" format.
#' @param to A string. The ending point of the day period, in the "HH:MM:SS"
#' format.
#' @param keep A logical. Whether the entries related to the specified day
#' period should be kept or dropped (defaults to `TRUE`, which keeps the
#' entries).
#' @param full_trips A logical.
#' @param update_frequencies A logical.
#'
#' @return The GTFS object passed to the `gtfs` parameter, after the filtering
#' process.
#'
#' @family filtering functions
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' object.size(gtfs)
#'
#' @export
filter_by_day_period <- function(gtfs,
                                 from,
                                 to,
                                 keep = TRUE,
                                 full_trips = TRUE,
                                 update_frequencies = FALSE) {
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_string(from, pattern = "^\\d{2}:\\d{2}:\\d{2}$")
  checkmate::assert_string(to, pattern = "^\\d{2}:\\d{2}:\\d{2}$")
  checkmate::assert_logical(keep, len = 1)
  checkmate::assert_logical(full_trips, len = 1)
  checkmate::assert_logical(update_frequencies, len = 1)

  from_secs <- string_to_seconds(from)
  to_secs <- string_to_seconds(to)
  if (from_secs > to_secs) stop("'from' must be lower than 'to'.")

  # if the gtfs contains a frequencies table, it's important to keep the
  # stop_times of the trips listed in there intact.
  # this is because their stop_times are only templates, and the departure and
  # arrival times listed there should not be considered as is when filtering.
  # in such case, we remove the frequencies' entries that fall outside the
  # specified period and conditionally update the entries that were kept

  frequency_trips <- character()
  if (gtfsio::check_file_exists(gtfs, "frequencies")) {
    gtfsio::assert_field_class(
      gtfs,
      "frequencies",
      c("trip_id", "start_time", "end_time"),
      rep("character", 3)
    )

    gtfs$frequencies <- filter_frequencies(
      gtfs,
      from_secs,
      to_secs,
      keep,
      update_frequencies
    )
  }

  return(gtfs)
}


#' @keywords internal
filter_frequencies <- function(gtfs,
                               from_secs,
                               to_secs,
                               keep,
                               update_frequencies) {
  # conditionally create time-in-seconds columns, depending if they exist before
  # or not. if they didn't exist beforehand, they have to be removed afterwards

  if (!gtfsio::check_field_exists(gtfs, "frequencies", "start_time_secs")) {
    gtfs$frequencies[, start_time_secs := string_to_seconds(start_time)]
    created_start_secs <- TRUE
  }

  if (!gtfsio::check_field_exists(gtfs, "frequencies", "end_time_secs")) {
    gtfs$frequencies[, end_time_secs := string_to_seconds(end_time)]
    created_end_secs <- TRUE
  }

  # how we filter the frequencies table depend on the 'keep' argument.
  # if we are keeping, we want to keep entries that either one of 'from' or
  # 'to' fall between start_time and end_time and the entries in which both
  # start_time and end_time are between 'from' and 'to'.
  # if we are dropping, we also want to keep the entries that either one of
  # 'from'/'to' fall between start_time and end_time (because there may be
  # trips that happen either between from and start_time or between to and
  # end_time), but want to drop the entries in which both start_time and
  # end_time are between 'from' and 'to'

  gtfs$frequencies[
    ,
    `:=`(
      from_within = from_secs > start_time_secs & from_secs < end_time_secs,
      to_within = to_secs > start_time_secs & to_secs < end_time_secs,
      within_from_to = start_time_secs >= from_secs & end_time_secs <= to_secs
    )
  ]

  if (keep) {
    filtered_frequencies <- gtfs$frequencies[
      from_within == TRUE | to_within == TRUE | within_from_to == TRUE
    ]
  } else {
    filtered_frequencies <- gtfs$frequencies[
      from_within == TRUE | to_within == TRUE | within_from_to == FALSE
    ]
  }

  # update the start and end time in the frequencies table if update_frequencies
  # is TRUE

  if (update_frequencies) {
    filtered_frequencies <- update_frequencies_times(
      filtered_frequencies,
      from_secs,
      to_secs,
      keep
    )
  }

  # conditionally remove time-in-seconds columns

  if (exists("created_start_secs")) {
    gtfs$frequencies[, start_time_secs := NULL]
    filtered_frequencies[, start_time_secs := NULL]
  }

  if (exists("created_end_secs")) {
    gtfs$frequencies[, end_time_secs := NULL]
    filtered_frequencies[, end_time_secs := NULL]
  }

  # and remove the auxiliary columns

  gtfs$frequencies[, c("from_within", "to_within", "within_from_to") := NULL]
  filtered_frequencies[
    ,
    c("from_within", "to_within", "within_from_to") := NULL
  ]

  return(filtered_frequencies)
}


#' @keywords internal
update_frequencies_times <- function(filtered_frequencies,
                                     from_secs,
                                     to_secs,
                                     keep) {
  # how frequencies times should be updated depends if:
  #   - exact_times is 0 or 1. if it's 0, just update start and end time to the
  #   corresponding to/from. if it's 1, we need to adhere to the headway. if
  #   exact_times doesn't exist, the behaviour is like when it's 0
  #   - keep is TRUE or FALSE. this changes whether start/end time relates to
  #   to/from.

  if (is.null(filtered_frequencies$exact_times)) {
    if (keep) {
      filtered_frequencies[
        from_within == TRUE,
        start_time := seconds_to_string(from_secs)
      ]
      filtered_frequencies[
        to_within == TRUE,
        end_time := seconds_to_string(to_secs)
      ]
    } else {
      # when keep = FALSE, there may be cases in which the the day period to
      # filter is between start and end time, so we want to preserve the
      # periods between start time and from, and between end time and to. to do
      # so, we need to identify these rows, duplicate them, and then edit one by
      # one
      filtered_frequencies <- rbind(
        filtered_frequencies,
        filtered_frequencies[to_within == TRUE & from_within == TRUE]
      )
      cols <- names(filtered_frequencies)
      filtered_frequencies[, is_duplicated := .N > 1, by = cols]

      filtered_frequencies[
        filtered_frequencies[is_duplicated == TRUE, .I[1], by = cols]$V1,
        `:=`(
          to_within = FALSE,
          is_duplicated = FALSE
        )
      ]
      filtered_frequencies[
        is_duplicated == TRUE,
        `:=`(
          from_within = FALSE,
          is_duplicated = FALSE
        )
      ]
      filtered_frequencies[, is_duplicated := NULL]

      # the rest of the filtering is pretty standard but "inverted" when
      # compared to keep = TRUE
      filtered_frequencies[
        from_within == TRUE,
        end_time := seconds_to_string(from_secs)
      ]
      filtered_frequencies[
        to_within == TRUE,
        start_time := seconds_to_string(to_secs)
      ]
    }
  } else {
    # TODO: implement update frequencies function when exact_times is present
  }

  return(filtered_frequencies)
}
