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

    gtfs$frequencies <- filter_frequencies(gtfs, from_secs, to_secs, keep)

    if (update_frequencies) {
      gtfsio::assert_field_class(
        gtfs,
        "frequencies",
        "headway_secs",
        "character"
      )
      # TODO: implement frequency adjustment based on headway_secs and 'from'
      # and 'to'
    }
  }

  return(gtfs)
}


#' @keywords internal
filter_frequencies <- function(gtfs, from_secs, to_secs, keep) {
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
      from_to_within =
        (from_secs > start_time_secs & from_secs < end_time_secs) |
        (to_secs > start_time_secs & to_secs < end_time_secs),
      within_from_to = start_time_secs >= from_secs & end_time_secs <= to_secs
    )
  ]

  # remove time-in-seconds columns before subsetting the table, otherwise it
  # will create a copy of the table and we won't be able to remove the columns
  # by reference anymore

  if (exists("created_start_secs")) {
    gtfs$frequencies[, start_time_secs := NULL]
  }

  if (exists("created_end_secs")) {
    gtfs$frequencies[, end_time_secs := NULL]
  }

  if (keep) {
    gtfs$frequencies <- gtfs$frequencies[
      from_to_within == TRUE | within_from_to == TRUE
    ]
  } else {
    gtfs$frequencies <- gtfs$frequencies[
      from_to_within == TRUE | within_from_to == FALSE
    ]
  }

  gtfs$frequencies[, c("from_to_within", "within_from_to") := NULL]

  return(gtfs$frequencies)
}
