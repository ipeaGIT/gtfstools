ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)
ggl_service <- "WE"

tester <- function(gtfs = ggl_gtfs, service_id = ggl_service, keep = TRUE) {
  filter_by_service_id(gtfs, service_id, keep)
}

# tests -------------------------------------------------------------------

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(ggl_gtfs), service_id))

  expect_error(tester(service_id = factor(ggl_service)))
  expect_error(tester(service_id = NA))

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

  original_gtfs <- read_gtfs(ggl_path)
  gtfs <- read_gtfs(ggl_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- tester(gtfs, ggl_service)
  expect_equal(original_gtfs, gtfs, check.attributes = FALSE)
})

test_that("'service_id' and 'keep' arguments work correctly", {
  smaller_gtfs_keeping <- tester()
  expect_true(all(smaller_gtfs_keeping$trips$service_id %in% ggl_service))
  expect_true(all(smaller_gtfs_keeping$calendar$service_id %in% ggl_service))
  expect_true(
    all(smaller_gtfs_keeping$calendar_dates$service_id %in% ggl_service)
  )

  smaller_gtfs_not_keeping <- tester(keep = FALSE)
  expect_true(nrow(smaller_gtfs_not_keeping$trips) == 0)
  expect_true(
    !any(smaller_gtfs_not_keeping$calendar$service_id %in% ggl_service)
  )
  expect_true(
    !any(smaller_gtfs_not_keeping$calendar_dates$service_id %in% ggl_service)
  )
})

test_that("the function filters berlin's gtfs correctly", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)
  ber_services <- c("3", "24")

  smaller_ber <- tester(ber_gtfs, ber_services)

  # calendar
  expect_true(all(smaller_ber$calendar$service_id %chin% ber_services))

  # calendar_dates
  expect_true(all(smaller_ber$calendar_dates$service_id %chin% ber_services))

  # trips
  expect_true(all(smaller_ber$trips$service_id %chin% ber_services))

  # shapes
  relevant_shapes <- ber_gtfs$trips[service_id %chin% ber_services]$shape_id
  expect_true(all(smaller_ber$shapes$shape_id %chin% relevant_shapes))

  # routes
  relevant_routes <- ber_gtfs$trips[service_id %chin% ber_services]$route_id
  expect_true(all(smaller_ber$routes$route_id %chin% relevant_routes))

  # agency
  relevant_agency <- "92"
  expect_true(smaller_ber$agency$agency_id == relevant_agency)

  # stop_times
  relevant_trips <- ber_gtfs$trips[service_id %chin% ber_services]$trip_id
  expect_true(all(smaller_ber$stop_times$trip_id %chin% relevant_trips))

  # stops
  relevant_stops <- unique(
    ber_gtfs$stop_times[trip_id %chin% relevant_trips]$stop_id
  )
  relevant_stops <- get_parent_station(ber_gtfs, relevant_stops)$stop_id
  expect_true(all(smaller_ber$stops$stop_id %chin% relevant_stops))
})

test_that("the function filters google's gtfs correctly", {
  smaller_ggl <- tester()

  # calendar
  expect_true(all(smaller_ggl$calendar$service_id %chin% ggl_service))

  # calendar_dates
  expect_true(all(smaller_ggl$calendar_dates$service_id %chin% ggl_service))

  # trips
  expect_true(all(smaller_ggl$trips$service_id %chin% ggl_service))

  # shapes - empty dt because trips doesn't contain shape_id column
  expect_true(nrow(smaller_ggl$shapes) == 0)

  # routes
  relevant_routes <- "A"
  expect_true(all(smaller_ggl$routes$route_id %chin% relevant_routes))

  # fare_rules and fare_attributes are empty
  expect_true(nrow(smaller_ggl$fare_rules) == 0)
  expect_true(nrow(smaller_ggl$fare_attributes) == 0)

  # agency
  relevant_agency <- "agency001"
  expect_true(smaller_ggl$agency$agency_id == relevant_agency)

  # stop_times
  relevant_trips <- "AWE1"
  expect_true(all(smaller_ggl$stop_times$trip_id %chin% relevant_trips))

  # stops is empty because of relevant trip
  expect_true(nrow(smaller_ggl$stops) == 0)
  expect_true(nrow(smaller_ggl$transfers) == 0)
  expect_true(nrow(smaller_ggl$pathways) == 0)
  expect_true(nrow(smaller_ggl$levels) == 0)
})

test_that("behaves correctly when service_id = character(0)", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)

  # if keep = TRUE, gtfs should be empty
  empty <- tester(ber_gtfs, character(0))
  n_rows <- vapply(empty, nrow, FUN.VALUE = integer(1))
  expect_true(all(n_rows == 0))

  # if keep = FALSE, gtfs should remain unchanged
  # this is actually not true because the calendar, calendar_dates and agency
  # tables contain ids not listed in the routes and trips tables, which end up
  # removed anyway (I like this behaviour, so not considering a bug)
  full <- tester(ber_gtfs, character(0), keep = FALSE)
  modified_ber <- read_gtfs(ber_path)
  modified_ber$agency <- modified_ber$agency[
    agency_id %in% modified_ber$routes$agency_id
  ]
  expect_identical(modified_ber, full)
})

test_that("agency table is kept intact if routes not list agency_id", {
  spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
  spo_service <- "USD"

  agencyless <- read_gtfs(spo_path)
  agencyless$routes[, agency_id := NULL]

  filtered_agencyless <- tester(agencyless, spo_service)

  expect_identical(agencyless$agency, filtered_agencyless$agency)
})
