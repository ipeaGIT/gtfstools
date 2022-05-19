data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------


test_that("raises errors due to incorrect input types/value", {
  no_class_gtfs <- gtfs
  attr(no_class_gtfs, "class") <- NULL
  expect_error(get_trip_speed(no_class_gtfs))
  expect_error(get_trip_speed(gtfs, as.factor("CPTM L07-0")))
  expect_error(get_trip_speed(gtfs, NA))
  expect_error(get_trip_speed(gtfs, file = c("shape", "stop_times")))
  expect_error(get_trip_speed(gtfs, unit = "km/s"))
})

test_that("raises errors if gtfs doesn't have required files/fields", {

  # create gtfs without 'stop_times'

  no_trips_gtfs <- copy_gtfs_without_file(gtfs, "trips")
  no_shapes_gtfs <- copy_gtfs_without_file(gtfs, "shapes")
  no_stops_gtfs <- copy_gtfs_without_file(gtfs, "stops")
  no_stop_times_gtfs <- copy_gtfs_without_file(gtfs, "stop_times")

  # create gtfs without relevant fields

  no_trp_tripid_gtfs <- copy_gtfs_without_field(gtfs, "trips", "trip_id")
  no_trp_shapeid_gtfs <- copy_gtfs_without_field(gtfs, "trips", "shape_id")
  no_shp_shapeid_gtfs <- copy_gtfs_without_field(gtfs, "shapes", "shape_id")
  no_shp_shapeptlat_gtfs <- copy_gtfs_without_field(
    gtfs, "shapes", "shape_pt_lat"
  )
  no_shp_shapeptlon_gtfs <- copy_gtfs_without_field(
    gtfs, "shapes", "shape_pt_lon"
  )
  no_stt_tripid_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "trip_id")
  no_stt_stopid_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "stop_id")
  no_stt_arrtime_gtfs <- copy_gtfs_without_field(
    gtfs, "stop_times", "arrival_time"
  )
  no_stt_deptime_gtfs <- copy_gtfs_without_field(
    gtfs, "stop_times", "departure_time"
  )
  no_sto_stopid_gtfs <- copy_gtfs_without_field(gtfs, "stops", "stop_id")
  no_sto_stoplat_gtfs <- copy_gtfs_without_field(gtfs, "stops", "stop_lat")
  no_sto_stoplon_gtfs <- copy_gtfs_without_field(gtfs, "stops", "stop_lon")

  # tests

  expect_error(get_trip_speed(no_trips_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_stop_times_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_shapes_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_stops_gtfs, "CPTM L07-0", "stop_times"))
  expect_error(get_trip_speed(no_trp_tripid_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_trp_shapeid_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_shp_shapeid_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_shp_shapeptlat_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_shp_shapeptlon_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_trp_tripid_gtfs, "CPTM L07-0", "stop_times"))
  expect_error(get_trip_speed(no_stt_tripid_gtfs, "CPTM L07-0", "stop_times"))
  expect_error(get_trip_speed(no_stt_stopid_gtfs, "CPTM L07-0", "stop_times"))
  expect_error(get_trip_speed(no_sto_stopid_gtfs, "CPTM L07-0", "stop_times"))
  expect_error(get_trip_speed(no_sto_stoplat_gtfs, "CPTM L07-0", "stop_times"))
  expect_error(get_trip_speed(no_sto_stoplon_gtfs, "CPTM L07-0", "stop_times"))
  expect_error(get_trip_speed(no_stt_arrtime_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_stt_deptime_gtfs, "CPTM L07-0"))
  expect_error(get_trip_speed(no_stt_arrtime_gtfs, "CPTM L07-0", "stop_times"))
  expect_error(get_trip_speed(no_stt_deptime_gtfs, "CPTM L07-0", "stop_times"))

})

test_that("raises warnings if a non_existent trip_id is given", {
  expect_warning(get_trip_speed(gtfs, c("CPTM L07-0", "ola")))
  expect_warning(get_trip_speed(gtfs, "ola"))
})

test_that("gets the speed of correct 'trip_id's", {

  # if 'trip_id' = NULL, all trips have their geometries returned

  all_trip_ids <- unique(gtfs$trips$trip_id)
  all_trip_ids <- all_trip_ids[order(all_trip_ids)]

  speeds_all_trip_ids <- get_trip_speed(gtfs)
  trip_ids_from_speeds <- unique(speeds_all_trip_ids$trip_id)
  trip_ids_from_speeds <- trip_ids_from_speeds[order(trip_ids_from_speeds)]

  expect_identical(all_trip_ids, trip_ids_from_speeds)

  # else, only the duration of (valid) trip_ids are calculated

  selected_trip_ids <- c("CPTM L07-0", "ola")
  suppressWarnings(
    speeds_selected_trip_ids <- get_trip_speed(gtfs, selected_trip_ids)
  )
  expect_equal(unique(speeds_selected_trip_ids$trip_id), "CPTM L07-0")

})

test_that("calculates the speed based on correct 'file'", {

  shape_speed <- get_trip_speed(gtfs, "CPTM L07-0")
  stop_times_speed <- get_trip_speed(gtfs, "CPTM L07-0", file = "stop_times")
  both_speed <- get_trip_speed(
    gtfs,
    "CPTM L07-0",
    file = c("shapes", "stop_times")
  )

  expect_equal(unique(shape_speed$origin_file), "shapes")
  expect_equal(unique(stop_times_speed$origin_file), "stop_times")
  expect_identical(unique(both_speed$origin_file), c("shapes", "stop_times"))

})

test_that("outputs a 'data.table' with correct column types", {

  speeds_dt <- get_trip_speed(gtfs, "CPTM L07-0")

  # result is a data.table

  expect_s3_class(speeds_dt, "data.table")

  # column types

  expect_equal(class(speeds_dt$trip_id), "character")
  expect_equal(class(speeds_dt$origin_file), "character")
  expect_equal(class(speeds_dt$speed), "numeric")

  # should work even when no given 'trip_id's given are present in the gtfs

  expect_warning(speeds_dt <- get_trip_speed(gtfs, "ola"))

  expect_s3_class(speeds_dt, "data.table")

  expect_equal(class(speeds_dt$trip_id), "character")
  expect_equal(class(speeds_dt$origin_file), "character")
  expect_equal(class(speeds_dt$speed), "numeric")

})

test_that("outputs speeds in correct unit", {

  # km/h

  kmh_speeds_dt <- get_trip_speed(gtfs, "CPTM L07-0", unit = "km/h")

  trip_id_len <- sf::st_length(get_trip_geometry(gtfs, "CPTM L07-0", "shapes"))
  trip_id_len <- as.numeric(units::set_units(trip_id_len, "km"))
  trip_id_duration <- get_trip_duration(gtfs, "CPTM L07-0", unit = "h")

  expect_equal(trip_id_len / trip_id_duration$duration, kmh_speeds_dt$speed)

  # m/s

  ms_speeds_dt <- get_trip_speed(gtfs, "CPTM L07-0", unit = "m/s")

  trip_id_len <- sf::st_length(get_trip_geometry(gtfs, "CPTM L07-0", "shapes"))
  trip_id_len <- as.numeric(units::set_units(trip_id_len, "m"))
  trip_id_duration <- get_trip_duration(gtfs, "CPTM L07-0", unit = "s")

  expect_equal(trip_id_len / trip_id_duration$duration, ms_speeds_dt$speed)

  # km/h = 3.6 * m/s

  expect_equal(kmh_speeds_dt$speed, ms_speeds_dt$speed * 3.6)

})

test_that("doesn't change given gtfs", {
  # (except for 'stop_times' and 'shapes' indices)

  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  speeds <- get_trip_speed(gtfs, "CPTM L07-0", c("shapes", "stop_times"))
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$shapes, NULL)
  data.table::setindex(gtfs$stop_times, NULL)
  expect_identical(original_gtfs, gtfs)

})

# issue #35
test_that("warnings works properly", {
  # should raise warning if trip_id is specified and doesn't exist (and should
  # not calculate speed, instead of returning NA)
  gtfs$stop_times <- gtfs$stop_times[trip_id != "CPTM L07-0"]
  expect_warning(
    speed <- get_trip_speed(gtfs, "CPTM L07-0")
  )
  expect_true(nrow(speed) == 0)

  # but if trip_id is not specified, it should not raise a warning
  expect_silent(speed <- get_trip_speed(gtfs))
  expect_true(!any("CPTM L07-0" %chin% speed$trip_id))
  expect_true(all(gtfs$stop_times$trip_id %chin% speed$trip_id))
})
