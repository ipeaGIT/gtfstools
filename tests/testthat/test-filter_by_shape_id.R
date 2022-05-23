spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
spo_shapes <- c("17846", "68962")

ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)
ggl_shapes <- "A_shp"


# tests -------------------------------------------------------------------


test_that("raises error due to incorrect input types", {
  expect_error(filter_by_shape_id(unclass(spo_gtfs), spo_shapes))
  expect_error(filter_by_shape_id(spo_gtfs, factor(spo_shapes)))
  expect_error(filter_by_shape_id(spo_gtfs, NA))
  expect_error(filter_by_shape_id(spo_gtfs, spo_shapes, keep = "TRUE"))
  expect_error(filter_by_shape_id(spo_gtfs, spo_shapes, NA))
})

test_that("results in a dt_gtfs object", {
  # a dt_gtfs object is a list with "dt_gtfs" and "gtfs" classes
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")
  smaller_gtfs <- filter_by_shape_id(spo_gtfs, spo_shapes)
  expect_s3_class(smaller_gtfs, dt_gtfs_class, exact = TRUE)
  expect_type(smaller_gtfs, "list")

  # all objects inside a dt_gtfs are data.tables
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)

  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- filter_by_shape_id(gtfs, spo_shapes)
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$agency, NULL)
  data.table::setindex(gtfs$calendar, NULL)
  data.table::setindex(gtfs$frequencies, NULL)
  data.table::setindex(gtfs$routes, NULL)
  data.table::setindex(gtfs$stop_times, NULL)
  data.table::setindex(gtfs$stops, NULL)
  expect_identical(original_gtfs, gtfs)
})

test_that("'shape_id' and 'keep' arguments work correctly", {
  smaller_keeping <- filter_by_shape_id(ggl_gtfs, ggl_shapes)
  expect_true(all(smaller_keeping$shapes$shape_id %chin% ggl_shapes))
  expect_true(all(smaller_keeping$trips$shape_id %chin% ggl_shapes))

  smaller_not_keeping <- filter_by_shape_id(ggl_gtfs, ggl_shapes, keep = FALSE)
  expect_true(!any(smaller_not_keeping$shapes$shape_id %chin% ggl_shapes))
  expect_true(!any(smaller_not_keeping$trips$shape_id %chin% ggl_shapes))
})

test_that("the function filters berlin's gtfs correctly", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)
  ber_shapes <- c("14", "2")

  smaller_ber <- filter_by_shape_id(ber_gtfs, ber_shapes)

  # shapes
  expect_true(all(smaller_ber$shapes$shape_id %chin% ber_shapes))

  # trips
  relevant_trips <- ber_gtfs$trips[shape_id %chin% ber_shapes]$trip_id
  expect_true(all(smaller_ber$trips$trip_id %chin% relevant_trips))

  # calendar and calendar_dates
  relevant_services <- c("1", "4", "8")
  expect_true(all(smaller_ber$calendar$service_id %chin% relevant_services))
  expect_true(
    all(smaller_ber$calendar_dates$service_id %chin% relevant_services)
  )

  # routes
  relevant_routes <- c("1921_700", "1920_700")
  expect_true(all(smaller_ber$routes$route_id %chin% relevant_routes))

  # agency
  relevant_agency <- "92"
  expect_true(smaller_ber$agency$agency_id == relevant_agency)

  # stop_times
  expect_true(all(smaller_ber$stop_times$trip_id %chin% relevant_trips))

  # stops
  relevant_stops <- unique(
    ber_gtfs$stop_times[trip_id %chin% relevant_trips]$stop_id
  )
  relevant_stops <- get_parent_station(ber_gtfs, relevant_stops)$stop_id
  expect_true(all(smaller_ber$stops$stop_id %chin% relevant_stops))
})

test_that("the function filters sao paulo's gtfs correctly", {
  smaller_spo <- filter_by_shape_id(spo_gtfs, spo_shapes)

  # shapes
  expect_true(all(smaller_spo$shapes$shape_id %chin% spo_shapes))

  # trips
  relevant_trips <- c("CPTM L07-0", "6450-51-0")
  expect_true(all(smaller_spo$trips$trip_id %chin% relevant_trips))

  # calendar and calendar_dates
  relevant_services <- c("USD", "U__")
  expect_true(all(smaller_spo$calendar$service_id %chin% relevant_services))

  # routes
  relevant_routes <- c("CPTM L07", "6450-51")
  expect_true(all(smaller_spo$routes$route_id %chin% relevant_routes))

  # agency
  relevant_agency <- "1"
  expect_true(all(smaller_spo$agency$agency_id %chin% relevant_agency))

  # stop_times
  expect_true(all(smaller_spo$stop_times$trip_id %chin% relevant_trips))

  # stops
  relevant_stops <- unique(
    spo_gtfs$stop_times[trip_id %chin% relevant_trips]$stop_id
  )
  expect_true(all(smaller_spo$stop_times$stop_id %chin% relevant_stops))

  # frequencies
  expect_true(all(smaller_spo$frequencies$trip_id %chin% relevant_trips))
})

test_that("the function filters google's gtfs correctly", {
  smaller_ggl <- filter_by_shape_id(ggl_gtfs, ggl_shapes)

  # expect smaller_ggl to be identical to ggl_gtfs, because shapes only contain
  # one shape_id and trips doesn't contain a shape_id - i.e. none of the filters
  # that are triggered by trip_id happen
  expect_identical(ggl_gtfs, smaller_ggl)

  # adding a shape_id column to increase test coverage
  # since the included shape is not equal to ggl_shapes, all tables (but shapes
  # and agency) will be empty
  ggl_gtfs$trips[, shape_id := "wrong_shape"]
  smaller_ggl <- filter_by_shape_id(ggl_gtfs, ggl_shapes)

  # shapes
  expect_true(all(smaller_ggl$shapes$shape_id %chin% ggl_shapes))

  #agency
  relevant_agency <- "agency001"
  expect_true(all(smaller_ggl$agency$agency_id %chin% relevant_agency))

  # the rest
  expect_true(nrow(smaller_ggl$trips) == 0)
  expect_true(nrow(smaller_ggl$calendar) == 0)
  expect_true(nrow(smaller_ggl$calendar_dates) == 0)
  expect_true(nrow(smaller_ggl$routes) == 0)
  expect_true(nrow(smaller_ggl$fare_rules) == 0)
  expect_true(nrow(smaller_ggl$fare_attributes) == 0)
  expect_true(nrow(smaller_ggl$stop_times) == 0)
  expect_true(nrow(smaller_ggl$stops) == 0)
  expect_true(nrow(smaller_ggl$levels) == 0)
  expect_true(nrow(smaller_ggl$pathway) == 0)
  expect_true(nrow(smaller_ggl$transfers) == 0)
  expect_true(nrow(smaller_ggl$frequencies) == 0)
})

test_that("behaves correctly when shape_id = character(0)", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)

  # if keep = TRUE, gtfs should be empty
  empty <- filter_by_shape_id(ber_gtfs, character(0))
  n_rows <- vapply(empty, nrow, FUN.VALUE = integer(1))
  expect_true(all(n_rows == 0))

  # if keep = FALSE, gtfs should remain unchanged
  # this is actually not true because the calendar, calendar_dates and agency
  # tables contain ids not listed in the routes and trips tables, which and up
  # removed anyway (I like this behaviour, so not considering a bug)
  full <- filter_by_shape_id(ber_gtfs, character(0), keep = FALSE)
  modified_ber <- read_gtfs(ber_path)
  modified_ber$calendar <- modified_ber$calendar[
    service_id %in% modified_ber$trips$service_id
  ]
  modified_ber$calendar_dates <- modified_ber$calendar_dates[
    service_id %in% modified_ber$trips$service_id
  ]
  modified_ber$agency <- modified_ber$agency[
    agency_id %in% modified_ber$routes$agency_id
  ]
  expect_identical(modified_ber, full)
})
