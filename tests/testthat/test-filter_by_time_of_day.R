context("Filter by time of day: overall usage")

path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   from = "00:00:00",
                   to = "06:30:00",
                   keep = TRUE,
                   full_trips = FALSE,
                   update_frequencies = TRUE) {
  filter_by_time_of_day(gtfs, from, to, keep, full_trips, update_frequencies)
}

# most of the tests are covered in the sections that test filter_frequencies()
# and filter_stop_times() individually. this section only covers some sanity
# checks

test_that("raises errors due to incorrect input types", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(from = factor("03:30:00")))
  expect_error(tester(to = factor("06:30:00")))
  expect_error(tester(from = "05:00:00", to = "04:30:00"))
  expect_error(tester(keep = 1))
  expect_error(tester(full_trips = 1))
  expect_error(tester(update_frequencies = 1))
})

test_that("raises warning if a trip in stop_times is not listed in trips", {
  expect_warning(tester())
})

test_that("results in a dt_gtfs object", {
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")
  suppressWarnings(smaller_gtfs <- tester())
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)
  original_gtfs <- read_gtfs(path)
  gtfs <- read_gtfs(path)
  expect_identical(original_gtfs, gtfs)

  suppressWarnings(smaller_gtfs <- tester())
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$agency, NULL)
  data.table::setindex(gtfs$calendar, NULL)
  data.table::setindex(gtfs$calendar_dates, NULL)
  data.table::setindex(gtfs$fare_attributes, NULL)
  data.table::setindex(gtfs$fare_rules, NULL)
  data.table::setindex(gtfs$levels, NULL)
  data.table::setindex(gtfs$pathways, NULL)
  data.table::setindex(gtfs$routes, NULL)
  data.table::setindex(gtfs$stops, NULL)
  data.table::setindex(gtfs$transfers, NULL)
  expect_identical(original_gtfs, gtfs)
})

# frequencies filtering ---------------------------------------------------

context("Filter by time of day: filter frequencies")

path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   from = "03:30:00",
                   to = "06:30:00",
                   keep = TRUE,
                   update_frequencies = TRUE) {
  from_secs <- string_to_seconds(from)
  to_secs <- string_to_seconds(to)
  gtfstools:::filter_frequencies(
    gtfs,
    from_secs,
    to_secs,
    keep,
    update_frequencies
  )[]
}

test_that("doesn't change specified gtfs", {
  original_gtfs <- read_gtfs(path)
  expect_identical(gtfs, original_gtfs)

  result <- tester()
  expect_identical(gtfs, original_gtfs)

  # should also work when update_frequencies = FALSE

  result <- tester(update_frequencies = FALSE)
  expect_identical(gtfs, original_gtfs)

  # should also work if gtfs previously contained time-in-seconds columns (i.e.
  # these columns should not be removed if they previously existed)

  secs_gtfs <- read_gtfs(path)
  secs_gtfs$frequencies[
    ,
    `:=`(
      start_time_secs = string_to_seconds(start_time),
      end_time_secs = string_to_seconds(end_time)
    )
  ]
  original_gtfs$frequencies <- data.table::copy(secs_gtfs$frequencies)
  expect_identical(secs_gtfs, original_gtfs)
  result <- tester(secs_gtfs)
  expect_identical(secs_gtfs, original_gtfs)
})

test_that("output doesn't contain auxiliary columns", {
  aux_columns <- c(
    "from_within",
    "to_within",
    "within_from_to",
    "is_duplicated"
  )
  result <- tester()
  expect_true(
    !any(aux_columns %chin% names(result))
  )

  result <- tester(keep = FALSE)
  expect_true(
    !any(aux_columns %chin% names(result))
  )
})

test_that("filters frequencies correctly, when update_frequencies is FALSE", {
  frequencies <- gtfs$frequencies

  expect_identical(tester(update_frequencies = FALSE), frequencies[1])
  expect_identical(
    tester(from = "05:00:00", update_frequencies = FALSE),
    frequencies[1]
  )
  expect_identical(
    tester(from = "06:00:00", to = "17:00:00", update_frequencies = FALSE),
    frequencies[c(1, 2)]
  )
  expect_identical(
    tester(from = "05:30:00", to = "28:00:00", update_frequencies = FALSE),
    frequencies
  )

  # the behaviour is a bit different with keep = FALSE
  expect_identical(
    tester(update_frequencies = FALSE, keep = FALSE),
    frequencies[c(2, 3)]
  )
  expect_identical(
    tester(from = "05:00:00", update_frequencies = FALSE, keep = FALSE),
    frequencies[c(2, 3)]
  )
  expect_identical(
    tester(
      from = "06:00:00",
      to = "17:00:00",
      keep = FALSE,
      update_frequencies = FALSE
    ),
    frequencies
  )
  expect_identical(
    tester(
      from = "05:30:00",
      to = "28:00:00",
      keep = FALSE,
      update_frequencies = FALSE
    ),
    frequencies[0]
  )
})

test_that("updates frequencies correctly when exact_times doesn't exist", {
  expect_equal(tester(to = "06:29:00")$end_time, "06:29:00")
  expect_equal(tester(from = "05:31:00")$start_time, "05:31:00")

  result <- tester(from = "05:31:00", to = "07:29:00")
  expect_equal(result$start_time, c("05:31:00", "06:30:00"))
  expect_equal(result$end_time, c("06:30:00", "07:29:00"))

  # the behaviour is a bit different with keep = FALSE
  result <- tester(from = "05:31:00", to = "07:29:00", keep = FALSE)
  expect_equal(result$start_time, c("05:30:00", "07:29:00", "20:30:00"))
  expect_equal(result$end_time, c("05:31:00", "20:30:00", "28:00:00"))

  result <- tester(from = "05:35:00", to = "05:45:00", keep = FALSE)
  expect_equal(result[c(1, 4)]$start_time, c("05:30:00", "05:45:00"))
  expect_equal(result[c(1, 4)]$end_time, c("05:35:00", "06:30:00"))
})

test_that("updates frequencies correctly when exact_times exists", {
  # when exact_times = 0 the behaviour should be identical to when it doesn't
  # exist
  gtfs$frequencies[, exact_times := 0]

  expect_equal(tester(to = "06:29:00")$end_time, "06:29:00")
  expect_equal(tester(from = "05:31:00")$start_time, "05:31:00")

  result <- tester(from = "05:31:00", to = "07:29:00")
  expect_equal(result$start_time, c("05:31:00", "06:30:00"))
  expect_equal(result$end_time, c("06:30:00", "07:29:00"))

  result <- tester(from = "05:31:00", to = "07:29:00", keep = FALSE)
  expect_equal(result$start_time, c("05:30:00", "07:29:00", "20:30:00"))
  expect_equal(result$end_time, c("05:31:00", "20:30:00", "28:00:00"))

  result <- tester(from = "05:35:00", to = "05:45:00", keep = FALSE)
  expect_equal(result[c(1, 4)]$start_time, c("05:30:00", "05:45:00"))
  expect_equal(result[c(1, 4)]$end_time, c("05:35:00", "06:30:00"))

  # when exact_times = 1, start_time should be adjusted according to the headway
  gtfs$frequencies[, exact_times := 1]

  expect_equal(tester(to = "06:29:00")$end_time, "06:29:00")
  expect_equal(tester(from = "05:31:00")$start_time, "05:35:00")

  result <- tester(from = "05:31:00", to = "07:29:00")
  expect_equal(result$start_time, c("05:35:00", "06:30:00"))
  expect_equal(result$end_time, c("06:30:00", "07:29:00"))

  result <- tester(from = "05:31:00", to = "07:29:00", keep = FALSE)
  expect_equal(result$start_time, c("05:30:00", "07:30:00", "20:30:00"))
  expect_equal(result$end_time, c("05:31:00", "20:30:00", "28:00:00"))

  result <- tester(from = "05:36:00", to = "05:46:00", keep = FALSE)
  expect_equal(result[c(1, 4)]$start_time, c("05:30:00", "05:50:00"))
  expect_equal(result[c(1, 4)]$end_time, c("05:36:00", "06:30:00"))

  gtfs$frequencies[, exact_times := NULL]
})

test_that("drops entries where start_time > end_time when exact_times = 1", {
  gtfs$frequencies[, exact_times := 1]

  result <- tester(from = "05:31:00", to = "05:34:00")
  expect_true(nrow(result) == 0)
})


# stop_times filtering ----------------------------------------------------

context("Filter by time of day: filter stop_times")

path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   from = "03:30:00",
                   to = "06:30:00",
                   keep = TRUE,
                   full_trips = FALSE,
                   frequency_trips = character()) {
  from_secs <- string_to_seconds(from)
  to_secs <- string_to_seconds(to)
  gtfstools:::filter_stop_times(
    gtfs,
    from_secs,
    to_secs,
    keep,
    full_trips,
    frequency_trips
  )[]
}

test_that("doesn't change specified gtfs", {
  original_gtfs <- read_gtfs(path)
  expect_identical(gtfs, original_gtfs)

  result <- tester()
  expect_identical(gtfs, original_gtfs)

  # should also work if gtfs previously contained time-in-seconds columns (i.e.
  # these columns should not be removed if they previously existed)

  secs_gtfs <- read_gtfs(path)
  secs_gtfs$stop_times[
    ,
    `:=`(
      departure_time_secs = string_to_seconds(departure_time),
      arrival_time_secs = string_to_seconds(arrival_time)
    )
  ]
  original_gtfs$stop_times <- data.table::copy(secs_gtfs$stop_times)
  expect_identical(secs_gtfs, original_gtfs)
  result <- tester(secs_gtfs)
  expect_identical(secs_gtfs, original_gtfs)
})

test_that("filters stop_times correctly when full_trips = FALSE", {
  # building a smaller gtfs to keep a more focused test
  small_gtfs <- filter_by_trip_id(gtfs, "CPTM L07-0")

  filtered_stop_times <- tester(small_gtfs, from = "04:00:00", to = "04:48:00")
  expect_identical(filtered_stop_times, small_gtfs$stop_times[1:7])

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:00:00",
    to = "04:48:00",
    keep = FALSE
  )
  expect_identical(filtered_stop_times, small_gtfs$stop_times[8:18])

  filtered_stop_times <- tester(small_gtfs, from = "04:30:00", to = "04:48:00")
  expect_identical(filtered_stop_times, small_gtfs$stop_times[5:7])

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:30:00",
    to = "04:48:00",
    keep = FALSE
  )
  expect_identical(filtered_stop_times, small_gtfs$stop_times[c(1:4, 8:18)])
})

test_that("filters stop_times correctly when full_trips = FALSE", {
  small_gtfs <- filter_by_trip_id(gtfs, "CPTM L07-0")

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:00:00",
    to = "04:48:00",
    full_trips = TRUE
  )
  data.table::setindex(small_gtfs$stop_times, NULL)
  expect_identical(filtered_stop_times, small_gtfs$stop_times)

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:00:00",
    to = "04:48:00",
    keep = FALSE,
    full_trips = TRUE
  )
  expect_identical(filtered_stop_times, small_gtfs$stop_times[0])

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:30:00",
    to = "04:48:00",
    full_trips = TRUE
  )
  data.table::setindex(small_gtfs$stop_times, NULL)
  expect_identical(filtered_stop_times, small_gtfs$stop_times)

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:30:00",
    to = "04:48:00",
    keep = FALSE,
    full_trips = TRUE
  )
  expect_identical(filtered_stop_times, small_gtfs$stop_times[0])
})

test_that("doesn't filter trips listed in frequency_trips", {
  small_gtfs <- filter_by_trip_id(gtfs, "CPTM L07-0")

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:00:00",
    to = "04:48:00",
    frequency_trips = "CPTM L07-0"
  )
  expect_identical(filtered_stop_times, small_gtfs$stop_times)

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:00:00",
    to = "04:48:00",
    full_trips = TRUE,
    frequency_trips = "CPTM L07-0"
  )
  data.table::setindex(small_gtfs$stop_times, NULL)
  expect_identical(filtered_stop_times, small_gtfs$stop_times)

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:00:00",
    to = "04:48:00",
    keep = FALSE,
    frequency_trips = "CPTM L07-0"
  )
  expect_identical(filtered_stop_times, small_gtfs$stop_times)

  filtered_stop_times <- tester(
    small_gtfs,
    from = "04:00:00",
    to = "04:48:00",
    keep = FALSE,
    full_trips = TRUE,
    frequency_trips = "CPTM L07-0"
  )
  expect_identical(filtered_stop_times, small_gtfs$stop_times)
})

test_that("handles trips with some non specified departure/arrival times", {
  path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
  gtfs <- read_gtfs(path)

  # when only the first and last departure and arrival times are specified, the
  # function will only work "correctly" when full_trips = TRUE. when it's not,
  # it will drop all stops in which the times are not specified.

  filtered_stop_times <- tester(from = "00:06:10", to = "00:06:45")
  expect_equal(nrow(filtered_stop_times), nrow(gtfs$stop_times) - 5)

  filtered_stop_times <- tester(
    from = "00:06:10",
    to = "00:06:45",
    full_trips = TRUE
  )
  data.table::setindex(gtfs$stop_times, NULL)
  expect_identical(filtered_stop_times, gtfs$stop_times)
})
