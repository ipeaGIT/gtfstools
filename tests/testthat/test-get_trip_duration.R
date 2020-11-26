context("Get trip duration")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------


test_that("get_trip_duration raises errors due to incorrect input types/value", {

  no_class_gtfs <- gtfs
  attr(no_class_gtfs, "class") <- NULL

  expect_error(get_trip_duration(no_class_gtfs))
  expect_error(get_trip_duration(gtfs, as.factor("CPTM L07-0")))
  expect_error(get_trip_duration(gtfs, unit = min))
  expect_error(get_trip_duration(gtfs, unit = "mins"))
  expect_error(get_trip_duration(gtfs, unit = c("s", "min")))

})

test_that("get_trip_duration raises errors if gtfs doesn't have required files/fields", {

  # create gtfs with no stop_times

  no_stop_times_gtfs <- gtfs
  no_stop_times_gtfs$stop_times <- NULL

  # create gtfs with no 'trip_id' column in stop_times

  no_st_tripid_gtfs <- gtfs
  no_st_tripid_gtfs$stop_times <- data.table::copy(gtfs$stop_times)
  no_st_tripid_gtfs$stop_times[, trip_id := NULL]

  # create gtfs with no 'arrival_time' column in stop_times

  no_st_arrtime_gtfs <- gtfs
  no_st_arrtime_gtfs$stop_times <- data.table::copy(gtfs$stop_times)
  no_st_arrtime_gtfs$stop_times[, arrival_time := NULL]

  # create gtfs with no 'departure_time' column in stop_times

  no_st_deptime_gtfs <- gtfs
  no_st_deptime_gtfs$stop_times <- data.table::copy(gtfs$stop_times)
  no_st_deptime_gtfs$stop_times[, departure_time := NULL]

  expect_error(get_trip_duration(no_stop_times_gtfs))
  expect_error(get_trip_duration(no_st_tripid_gtfs))
  expect_error(get_trip_duration(no_st_arrtime_gtfs))
  expect_error(get_trip_duration(no_st_deptime_gtfs))

})

test_that("get_trip_duration calculates the duration of correct trip_id's", {

  # if trip_id = NULL all trips in stop_times have their durations calculated

  all_trip_ids <- unique(gtfs$stop_times$trip_id)
  duration_all_trip_ids <- get_trip_duration(gtfs)
  expect_equal(length(all_trip_ids), nrow(duration_all_trip_ids))

  # else only the duration of (valid) trip_ids are calculated

  selected_trip_ids <- c("CPTM L07-0", "ola")
  suppressWarnings(
    duration_selected_trip_ids <- get_trip_duration(gtfs, selected_trip_ids)
  )
  expect_equal(1, nrow(duration_selected_trip_ids))

})

test_that("get_trip_duration raises warnings if a non_existent trip_id is given", {
  expect_warning(get_trip_duration(gtfs, c("CPTM L07-0", "ola")))
  expect_warning(get_trip_duration(gtfs, "ola"))
})

test_that("get_trip_duration doesn't change given gtfs (except for stop_times index)", {

  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  durations <- get_trip_duration(gtfs, "CPTM L07-0")
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$stop_times, NULL)
  expect_identical(original_gtfs, gtfs)

})

test_that("get_trip_duration calculates duration correctly", {

  durations <- get_trip_duration(gtfs, "CPTM L07-0")
  expect_equal(durations$duration, 136)

  modified_st_gtfs <- gtfs
  modified_st_gtfs$stop_times <- data.table::copy(gtfs$stop_times)
  modified_st_gtfs$stop_times[trip_id == "CPTM L07-0", arrival_time := "24:00:00"]

  durations <- get_trip_duration(modified_st_gtfs, "CPTM L07-0")
  expect_equal(durations$duration, 1200)

})

test_that("get_trip_duration outputs duration in the correct unit", {

  seconds_durations <- get_trip_duration(gtfs, "CPTM L07-0", "s")
  minutes_durations <- get_trip_duration(gtfs, "CPTM L07-0", "min")
  hours_durations   <- get_trip_duration(gtfs, "CPTM L07-0", "h")
  days_durations    <- get_trip_duration(gtfs, "CPTM L07-0", "d")

  expect_equal(seconds_durations$duration / 60, minutes_durations$duration)
  expect_equal(seconds_durations$duration / 3600, hours_durations$duration)
  expect_equal(seconds_durations$duration / 86400, days_durations$duration)

})
