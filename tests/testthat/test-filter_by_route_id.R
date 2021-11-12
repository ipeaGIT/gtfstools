context("Filter by route_id")


# setup -------------------------------------------------------------------


spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
spo_routes <- c("6450-51", "CPTM L11")

ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)
ggl_routes <- c("A", "TSW")


# tests -------------------------------------------------------------------


test_that("raises error due to incorrect input types", {
  expect_error(filter_by_route_id(unclass(spo_gtfs), spo_routes))
  expect_error(filter_by_route_id(spo_gtfs, factor(spo_routes)))
  expect_error(filter_by_route_id(spo_gtfs, spo_routes, keep = "TRUE"))
})

test_that("results in a dt_gtfs object", {
  # a dt_gtfs object is a list with "dt_gtfs" and "gtfs" classes
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")
  smaller_gtfs <- filter_by_route_id(spo_gtfs, spo_routes)
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")

  # all objects inside a dt_gtfs are data.tables
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)

  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- filter_by_route_id(gtfs, spo_routes)
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$agency, NULL)
  data.table::setindex(gtfs$calendar, NULL)
  data.table::setindex(gtfs$frequencies, NULL)
  data.table::setindex(gtfs$shapes, NULL)
  data.table::setindex(gtfs$stop_times, NULL)
  data.table::setindex(gtfs$stops, NULL)
  expect_identical(original_gtfs, gtfs)
})

test_that("'route_id' and 'keep' arguments work correctly", {
  smaller_ggl_keeping <- filter_by_route_id(ggl_gtfs, ggl_routes)
  expect_true(all(smaller_ggl_keeping$routes$route_id %chin% ggl_routes))
  expect_true(all(smaller_ggl_keeping$trips$route_id %chin% ggl_routes))
  expect_true(all(smaller_ggl_keeping$fare_rules$route_id %chin% ggl_routes))

  smaller_ggl_not_keeping <- filter_by_route_id(ggl_gtfs, ggl_routes, keep = FALSE)
  expect_true(!any(smaller_ggl_not_keeping$routes$route_id %chin% ggl_routes))
  expect_true(!any(smaller_ggl_not_keeping$trips$route_id %chin% ggl_routes))
  expect_true(
    !any(smaller_ggl_not_keeping$fare_rules$route_id %chin% ggl_routes)
  )
})

test_that("it doesn't throw warnings because of missing stations in 'stops'", {
  # this would be caused if 'stop_times' mentioned a stop not listed in 'stops'.
  # the current implementation suppress this eventual warning, but perhaps we
  # could throw a meaningful warning in the future
  expect_silent(filter_by_route_id(ggl_gtfs, ggl_routes))
})

test_that("the function filters berlin's gtfs correctly", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)
  ber_routes <- c("1923_700", "1922_3")

  smaller_ber <- filter_by_route_id(ber_gtfs, ber_routes)

  # routes
  expect_true(nrow(smaller_ber$routes) == 2)
  expect_true(all(smaller_ber$routes$route_id %chin% ber_routes))

  # agency
  expect_true(nrow(smaller_ber$agency) == 1)
  expect_true(smaller_ber$agency$agency_id == "92")

  # trips
  expect_identical(
    smaller_ber$trips,
    ber_gtfs$trips[route_id %chin% ber_routes]
  )

  # shapes
  expect_true(all(smaller_ber$shapes$shape_id %chin% as.character(15:20)))

  # calendar and calendar_dates
  relevant_services <- unique(
    ber_gtfs$trips[route_id %chin% ber_routes]$service_id
  )
  expect_true(all(smaller_ber$calendar$service_id %chin% relevant_services))
  expect_true(
    all(smaller_ber$calendar_dates$service_id %chin% relevant_services)
  )

  # stop_times
  relevant_trips <- unique(ber_gtfs$trips[route_id %chin% ber_routes]$trip_id)
  expect_true(all(smaller_ber$stop_times$trip_id %chin% relevant_trips))

  # stops
  relevant_stops <- unique(
    ber_gtfs$stop_times[trip_id %chin% relevant_trips]$stop_id
  )
  relevant_stops <- get_parent_station(ber_gtfs, relevant_stops)$stop_id
  expect_true(all(smaller_ber$stops$stop_id %chin% relevant_stops))
})

test_that("the function filters sao paulo's gtfs correctly", {
  smaller_spo <- filter_by_route_id(spo_gtfs, spo_routes)

  # routes
  expect_true(nrow(smaller_spo$routes) == 2)
  expect_true(all(smaller_spo$routes$route_id %chin% spo_routes))

  # agency
  expect_true(all(smaller_spo$agency$agency_id == "1"))

  # trips
  relevant_trips <- c("CPTM L11-0", "CPTM L11-1", "6450-51-0")
  expect_true(all(smaller_spo$trips$trip_id %chin% relevant_trips))

  # shapes
  relevant_shapes <- c("17854", "17855", "68962")
  expect_true(all(smaller_spo$shapes$shape_id %chin% relevant_shapes))

  # calendar
  relevant_services <- c("USD", "U__")
  expect_true(all(smaller_spo$calendar$service_id %chin% relevant_services))

  # stop_times
  expect_true(all(smaller_spo$stop_times$trip_id %chin% relevant_trips))

  # stops
  relevant_stops <- unique(
    spo_gtfs$stop_times[trip_id %chin% relevant_trips]$stop_id
  )
  expect_true(all(smaller_spo$stops$stop_id %chin% relevant_stops))

  # frequencies
  expect_true(all(smaller_spo$frequencies$trip_id %chin% relevant_trips))
})

test_that("the function filters google's gtfs correctly", {
  smaller_ggl <- filter_by_route_id(ggl_gtfs, ggl_routes)

  # routes
  expect_true(all(smaller_ggl$routes$route_id %chin% ggl_routes))

  # agency
  expect_true(smaller_ggl$agency$agency_id == "agency001")

  # trips
  relevant_trips <- c("AWE1", "AWE2")
  expect_true(all(smaller_ggl$trips$trip_id %chin% relevant_trips))

  # shapes - no rows because trips doesn't have a shape_id column
  expect_true(nrow(smaller_ggl$shapes) == 0)

  # calendar
  relevant_services <- "WE"
  expect_true(all(smaller_ggl$calendar$service_id %chin% relevant_services))
  expect_true(
    all(smaller_ggl$calendar_dates$service_id %chin% relevant_services)
  )

  # stop_times
  expect_true(all(smaller_ggl$stop_times$trip_id %chin% relevant_trips))

  # stops - no rows because none of the stop_ids listed are in stop_times
  expect_true(nrow(smaller_ggl$stops) == 0)

  # frequencies
  expect_true(all(smaller_ggl$frequencies$trip_id %chin% relevant_trips))

  # levels, pathways and transfers - no rows because stops doesn't have any rows
  expect_true(nrow(smaller_ggl$levels) == 0)
  expect_true(nrow(smaller_ggl$pathways) == 0)
  expect_true(nrow(smaller_ggl$transfers) == 0)

  # fare_rules
  expect_true(all(smaller_ggl$fare_rules$route_id %chin% ggl_routes))

  # fare_attributes - no rows because none of the fare_ids listed are in
  # fare_rules
  expect_true(nrow(smaller_ggl$fare_attributes) == 0)
})
