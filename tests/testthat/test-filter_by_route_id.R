spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
spo_routes <- c("6450-51", "CPTM L11")

ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)
ggl_routes <- c("A", "TSW")

tester <- function(gtfs = spo_gtfs, route_id = spo_routes, keep = TRUE) {
  filter_by_route_id(gtfs, route_id, keep)
}

# tests -------------------------------------------------------------------

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(spo_gtfs)))

  expect_error(tester(route_id = factor(spo_routes)))
  expect_error(tester(route_id = NA))

  expect_error(tester(keep = "TRUE"))
  expect_error(tester(keep = NA))
})

test_that("results in a dt_gtfs object", {
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")

  smaller_gtfs <- tester()
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)

  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- tester(gtfs, spo_routes)
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
  smaller_keeping <- tester()
  expect_true(all(smaller_keeping$routes$route_id %chin% spo_routes))
  expect_true(all(smaller_keeping$trips$route_id %chin% spo_routes))
  expect_true(all(smaller_keeping$fare_rules$route_id %chin% spo_routes))

  smaller_not_keeping <- tester(keep = FALSE)
  expect_true(!any(smaller_not_keeping$routes$route_id %chin% spo_routes))
  expect_true(!any(smaller_not_keeping$trips$route_id %chin% spo_routes))
  expect_true(!any(smaller_not_keeping$fare_rules$route_id %chin% spo_routes))
})

test_that("it doesn't throw warnings because of missing stations in 'stops'", {
  # this would be caused if 'stop_times' mentioned a stop not listed in 'stops'.
  # the current implementation suppress this eventual warning, but perhaps we
  # could throw a meaningful warning in the future
  expect_silent(tester(ggl_gtfs, ggl_routes))
})

test_that("the function filters berlin's gtfs correctly", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)
  ber_routes <- c("1923_700", "1922_3")

  smaller_ber <- tester(ber_gtfs, ber_routes)

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
  smaller_spo <- tester()

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
  smaller_ggl <- tester(ggl_gtfs, ggl_routes)

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

test_that("behaves correctly when route_id = character(0)", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)

  # if keep = TRUE, gtfs should be empty
  empty <- tester(ber_gtfs, character(0))
  n_rows <- vapply(empty, nrow, FUN.VALUE = integer(1))
  expect_true(all(n_rows == 0))

  # if keep = FALSE, gtfs should remain unchanged
  # this is actually not true because the calendar, calendar_dates and agency
  # tables contain ids not listed in the routes and trips tables, which and up
  # removed anyway (I like this behaviour, so not considering a bug)
  full <- tester(ber_gtfs, character(0), keep = FALSE)
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

test_that("agency table is kept intact if routes not list agency_id", {
  agencyless <- read_gtfs(spo_path)
  agencyless$routes[, agency_id := NULL]

  filtered_agencyless <- tester(agencyless)

  expect_identical(agencyless$agency, filtered_agencyless$agency)
})
