context("Get trip segment duration")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------


test_that("get_trip_segment_duration raises errors due to incorrect input types/value", {

  no_class_gtfs <- gtfs
  attr(no_class_gtfs, "class") <- NULL

  expect_error(get_trip_segment_duration(no_class_gtfs))
  expect_error(get_trip_segment_duration(gtfs, as.factor("CPTM L07-0")))
  expect_error(get_trip_segment_duration(gtfs, unit = min))
  expect_error(get_trip_segment_duration(gtfs, unit = "mins"))
  expect_error(get_trip_segment_duration(gtfs, unit = c("s", "min")))

})

test_that("get_trip_segment_duration raises errors if gtfs doesn't have required files/fields", {

  # create gtfs without 'stop_times'

  no_stop_times_gtfs <- copy_gtfs_without_file(gtfs, "stop_times")

  # create gtfs without relevant fields

  no_st_tripid_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "trip_id")
  no_st_arrtime_gtfs <- copy_gtfs_without_field(
    gtfs, "stop_times", "arrival_time"
  )
  no_st_deptime_gtfs <- copy_gtfs_without_field(
    gtfs, "stop_times", "departure_time"
  )
  no_st_stopseq_gtfs <- copy_gtfs_without_field(
    gtfs, "stop_times", "stop_sequence"
  )

  expect_error(get_trip_segment_duration(no_stop_times_gtfs))
  expect_error(get_trip_segment_duration(no_st_tripid_gtfs))
  expect_error(get_trip_segment_duration(no_st_arrtime_gtfs))
  expect_error(get_trip_segment_duration(no_st_deptime_gtfs))
  expect_error(get_trip_segment_duration(no_st_stopseq_gtfs))

})

test_that("get_trip_segment_duration calculates the duration of correct 'trip_id's", {

  # if trip_id = NULL, all trips in 'stop_times' have their duration calculated

  all_trip_ids <- unique(gtfs$stop_times$trip_id)
  all_trip_ids <- all_trip_ids[order(all_trip_ids)]

  seg_duration_all_trip_ids <- get_trip_segment_duration(gtfs)
  trip_ids_from_duration <- unique(seg_duration_all_trip_ids$trip_id)
  trip_ids_from_duration <- trip_ids_from_duration[order(trip_ids_from_duration)]

  expect_identical(all_trip_ids, trip_ids_from_duration)

  # else, only the duration of (valid) trip_ids are calculated

  selected_trip_ids <- c("CPTM L07-0", "ola")
  expect_warning(
    dur_selected_trip_ids <- get_trip_segment_duration(gtfs, selected_trip_ids)
  )
  expect_equal(unique(dur_selected_trip_ids$trip_id), "CPTM L07-0")

})

test_that("get_trip_segment_duration raises warnings if a non_existent trip_id is given", {
  expect_warning(get_trip_segment_duration(gtfs, c("CPTM L07-0", "ola")))
  expect_warning(get_trip_segment_duration(gtfs, "ola"))
})

test_that("get_trip_segment_duration outputs a data.table with adequate columns' classes", {

  segments_durations <- get_trip_segment_duration(gtfs, "CPTM L07-0")

  # result is a data.table

  expect_s3_class(segments_durations, "data.table")

  # columns' types

  expect_equal(class(segments_durations$trip_id), "character")
  expect_equal(class(segments_durations$segment), "integer")
  expect_equal(class(segments_durations$duration), "numeric")

  # should work even if no given 'trip_id's are present in 'stop_times'

  expect_warning(segments_durations <- get_trip_segment_duration(gtfs, "ola"))

  expect_s3_class(segments_durations, "data.table")

  expect_equal(class(segments_durations$trip_id), "character")
  expect_equal(class(segments_durations$segment), "integer")
  expect_equal(class(segments_durations$duration), "numeric")

})

test_that("get_trip_segment_duration outputs duration in the correct unit", {

  seconds_durations <- get_trip_segment_duration(gtfs, "CPTM L07-0", "s")
  minutes_durations <- get_trip_segment_duration(gtfs, "CPTM L07-0", "min")
  hours_durations   <- get_trip_segment_duration(gtfs, "CPTM L07-0", "h")
  days_durations    <- get_trip_segment_duration(gtfs, "CPTM L07-0", "d")

  expect_equal(seconds_durations$duration / 60, minutes_durations$duration)
  expect_equal(seconds_durations$duration / 3600, hours_durations$duration)
  expect_equal(seconds_durations$duration / 86400, days_durations$duration)

})

test_that("get_trip_segment_duration outputs the correct number of segments", {

  segments_durations <- get_trip_segment_duration(gtfs, "CPTM L07-0")

  # number of segments is equal to number of stops minus 1

  expect_equal(
    nrow(segments_durations),
    nrow(gtfs$stop_times[trip_id == "CPTM L07-0"]) - 1
  )

})

test_that("get_trip_segment_duration calculates duration correctly", {

  segments_durations <- get_trip_segment_duration(gtfs, "CPTM L07-0", "min")
  expect_identical(segments_durations$duration, rep(8, 17))

  # should also work with 24+ hours times

  modified_st_gtfs <- gtfs
  modified_st_gtfs$stop_times <- data.table::copy(gtfs$stop_times)
  modified_st_gtfs$stop_times[
    trip_id == "CPTM L07-0" & stop_sequence == 18,
    arrival_time := "24:00:00"
  ]

  segments_durations <- get_trip_segment_duration(modified_st_gtfs, "CPTM L07-0")
  expect_identical(segments_durations$duration, c(rep(8, 16), 1072))

})

test_that("get_trip_segment_duration doesn't change given gtfs (except for stop_times index)", {

  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  durations <- get_trip_segment_duration(gtfs, "CPTM L07-0")
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$stop_times, NULL)
  expect_identical(original_gtfs, gtfs)

})
