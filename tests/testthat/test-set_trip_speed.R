context("Set trip speed")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------


test_that("set_trip_speed raises errors due to incorrect input types/value", {

  no_class_gtfs <- gtfs
  attr(no_class_gtfs, "class") <- NULL

  expect_error(set_trip_speed(no_class_gtfs, "CPTM L07-0", 50))
  expect_error(set_trip_speed(gtfs, as.factor("CPTM L07-0"), 50))
  expect_error(set_trip_speed(gtfs, "CPTM L07-0", "50"))
  expect_error(set_trip_speed(gtfs, c("CPTM L07-0", "6450-51-0", "2105-10-0"), c(50, 60)))
  expect_error(set_trip_speed(gtfs, "CPTM L07-0", 50, unit = "kms/h"))
  expect_error(set_trip_speed(gtfs, "CPTM L07-0", 50, by_reference = "TRUE"))

})

test_that("set_trip_speed raises errors if gtfs doesn't have required files/fields", {

  # create gtfs without 'stop_times'

  no_stop_times_gtfs <- copy_gtfs_without_file(gtfs, "stop_times")

  # create gtfs without relevant fields

  no_st_tripid_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "trip_id")
  no_st_arrtime_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "arrival_time")
  no_st_deptime_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "departure_time")
  no_st_stopseq_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "stop_sequence")

  expect_error(set_trip_speed(no_stop_times_gtfs, "CPTM L07-0", 50))
  expect_error(set_trip_speed(no_st_tripid_gtfs, "CPTM L07-0", 50))
  expect_error(set_trip_speed(no_st_arrtime_gtfs, "CPTM L07-0", 50))
  expect_error(set_trip_speed(no_st_deptime_gtfs, "CPTM L07-0", 50))
  expect_error(set_trip_speed(no_st_stopseq_gtfs, "CPTM L07-0", 50))

})

test_that("set_trip_speed raises warnings if a non_existent trip_id is given", {
  expect_warning(set_trip_speed(gtfs, c("CPTM L07-0", "ola"), 50))
  expect_warning(set_trip_speed(gtfs, "ola", 50))
})

test_that("set_trip_speed sets the speed of correct 'trip_id's", {

  selected_trip_ids <- c("ola", "2105-10-0", "CPTM L07-0")
  expect_warning(
    new_speeds_gtfs <- set_trip_speed(gtfs, selected_trip_ids, 50)
  )

  old_gtfs_stop_times   <- gtfs$stop_times
  new_speeds_stop_times <- new_speeds_gtfs$stop_times

  # 'stop_times' entries not related to given 'trip_id's should be identical

  expect_identical(
    old_gtfs_stop_times[! trip_id %chin% selected_trip_ids],
    new_speeds_stop_times[! trip_id %chin% selected_trip_ids]
  )

  # but given 'trip_id's entries' should be different

  expect_false(identical(
    old_gtfs_stop_times[trip_id %chin% selected_trip_ids],
    new_speeds_stop_times[trip_id %chin% selected_trip_ids]
  ))

  # if no valid 'trip_id' is given, then all entries should be identical

  expect_warning(new_speeds_gtfs <- set_trip_speed(gtfs, "ola", 50))

  old_gtfs_stop_times   <- gtfs$stop_times
  new_speeds_stop_times <- new_speeds_gtfs$stop_times

  expect_identical(old_gtfs_stop_times, new_speeds_stop_times)

})

test_that("set_trip_speed calculates speeds correctly", {

  selected_trip_ids <- c("CPTM L07-0", "6450-51-0", "2105-10-0")

  new_speeds_gtfs <- set_trip_speed(gtfs, selected_trip_ids, 50)

  trips_speeds <- get_trip_speed(new_speeds_gtfs, selected_trip_ids, "shapes")

  # all speeds should be around 50 km/h (there is some rounding error)

  expect_identical(round(trips_speeds$speed, 0), rep(50, 3))

  # it should also work with distinct unit (m/s)

  new_speeds_gtfs <- set_trip_speed(gtfs, selected_trip_ids, 50, unit = "m/s")

  trips_speeds <- get_trip_speed(new_speeds_gtfs, selected_trip_ids, "shapes", "m/s")

  expect_identical(round(trips_speeds$speed, 0), rep(50, 3))

  # it should also work if distinct speeds are given

  new_speeds_gtfs <- set_trip_speed(gtfs, selected_trip_ids, c(50, 60, 70))

  trips_speeds <- get_trip_speed(new_speeds_gtfs, selected_trip_ids, "shapes")

  expect_identical(round(trips_speeds$speed, 0), c(50, 70, 60))

  # and it should also work if distinct speeds are given with distinct unit (m/s)

  new_speeds_gtfs <- set_trip_speed(gtfs, selected_trip_ids, c(50, 60, 70), unit = "m/s")

  trips_speeds <- get_trip_speed(new_speeds_gtfs, selected_trip_ids, "shapes", "m/s")

  expect_identical(round(trips_speeds$speed, 0), c(50, 70, 60))

})

test_that("set_trip_speed sets arrival_time and departure_time adequately", {

  # modify base gtfs to make sure set_trip_speed sets 'arrival_time' and
  # 'departure_time' as the same value

  modified_gtfs <- gtfs
  modified_gtfs$stop_times <- data.table::copy(gtfs$stop_times)
  modified_gtfs$stop_times[1,  arrival_time := "04:00:01"]
  modified_gtfs$stop_times[18, arrival_time := "06:16:01"]

  new_speed_gtfs      <- set_trip_speed(modified_gtfs, "CPTM L07-0", 50)
  filtered_stop_times <- new_speed_gtfs$stop_times[trip_id == "CPTM L07-0"]
  first_last_stops    <- filtered_stop_times[stop_sequence %in% c(1, 18)]
  intermediate_stops  <- filtered_stop_times[! stop_sequence %in% c(1, 18)]

  # check if first/last stops' arrival and departure time are the same and not ""

  expect_identical(first_last_stops$arrival_time, first_last_stops$departure_time)
  expect_equal(sum(first_last_stops$arrival_time == ""), 0)
  expect_equal(sum(first_last_stops$departure_time == ""), 0)

  # check if intermediate stops' arrival and departure time are all ""

  expect_equal(sum(intermediate_stops$arrival_time == ""), 16)
  expect_equal(sum(intermediate_stops$departure_time == ""), 16)

})

test_that("set_trip_speed outputs a dt_gtfs object", {

  # by_reference = FALSE

  expect_s3_class(set_trip_speed(gtfs, "CPTM L07-0", 50), "dt_gtfs")
  expect_warning(still_gtfs <- set_trip_speed(gtfs, "ola", 50))
  expect_s3_class(still_gtfs, "dt_gtfs")

  # by_reference = TRUE

  expect_s3_class(set_trip_speed(gtfs, "CPTM L07-0", 50, by_reference = TRUE), "dt_gtfs")
  expect_s3_class(gtfs, "dt_gtfs")

})

test_that("set_trip_speed 'by_reference' parameter works adequately", {

  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  # if by_reference == FALSE then the given gtfs should not be changed (other than 'shapes' and 'trips' indices)

  new_speed_gtfs <- set_trip_speed(gtfs, "CPTM L07-0", 50)
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$trips, NULL)
  data.table::setindex(gtfs$shapes, NULL)

  expect_identical(original_gtfs, gtfs)

  # if by_reference == TRUE then the given gtfs' 'stop_times' is altered

  set_trip_speed(gtfs, "CPTM L07-0", 50, by_reference = TRUE)
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$trips, NULL)
  data.table::setindex(gtfs$shapes, NULL)

  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$stop_times, NULL)

  expect_false(identical(original_gtfs, gtfs))

  # the difference is exactly the trips whose speeds have been set

  expect_identical(
    gtfs$stop_times[trip_id != "CPTM L07-0"],
    original_gtfs$stop_times[trip_id != "CPTM L07-0"]
  )

  expect_false(identical(
    gtfs$stop_times[trip_id == "CPTM L07-0"],
    original_gtfs$stop_times[trip_id == "CPTM L07-0"]
  ))

})
