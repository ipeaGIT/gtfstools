ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
ber_gtfs <- read_gtfs(ber_path)
ber_agency <- "92"

ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)
ggl_agency <- "agency001"

tester <- function(gtfs = ber_gtfs, agency_id = ber_agency, keep = TRUE) {
  filter_by_agency_id(gtfs, agency_id, keep)
}



# tests -------------------------------------------------------------------

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(ber_gtfs)))

  expect_error(tester(agency_id = factor(ber_agency)))
  expect_error(tester(agency_id = NA))

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

  original_gtfs <- read_gtfs(ber_path)
  gtfs <- read_gtfs(ber_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- tester(gtfs, ber_agency)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})

test_that("'agency_id' and 'keep' arguments work correctly", {
  smaller_keeping <- tester(ggl_gtfs, ggl_agency)
  expect_true(all(smaller_keeping$agency$agency_id %chin% ggl_agency))
  expect_true(all(smaller_keeping$routes$agency_id %chin% ggl_agency))
  expect_true(all(smaller_keeping$fare_attributes$agency_id %chin% ggl_agency))
  expect_true(all(smaller_keeping$attributions$agency_id %chin% ggl_agency))

  smaller_dropping <- tester(ggl_gtfs, ggl_agency, keep = FALSE)
  expect_true(!any(smaller_dropping$agency$agency_id %chin% ggl_agency))
  expect_true(!any(smaller_dropping$routes$agency_id %chin% ggl_agency))
  expect_true(
    !any(smaller_dropping$fare_attributes$agency_id %chin% ggl_agency)
  )
  expect_true(!any(smaller_dropping$attributions$agency_id %chin% ggl_agency))
})

test_that("the function filters berlin's gtfs correctly", {
  smaller_ber <- tester()

  # agency
  expect_true(nrow(smaller_ber$agency) == 1)
  expect_true(smaller_ber$agency$agency_id == ber_agency)

  # routes
  expect_true(all(smaller_ber$routes$agency_id %chin% ber_agency))
  relevant_routes <- unique(smaller_ber$routes$route_id)

  # fare_rules
  expect_true(all(smaller_ber$fare_rules$route_id %chin% relevant_routes))

  # trips
  expect_true(all(smaller_ber$trips$route_id %chin% relevant_routes))
  relevant_shapes <- unique(smaller_ber$trips$shape_id)
  relevant_services <- unique(smaller_ber$trips$service_id)
  relevant_trips <- unique(smaller_ber$trips$trip_id)

  # shapes
  expect_true(all(smaller_ber$shapes$shape_id %chin% relevant_shapes))

  # calendar and calendar_dates
  expect_true(all(smaller_ber$calendar$service_id %chin% relevant_services))
  expect_true(
    all(smaller_ber$calendar_dates$service_id %chin% relevant_services)
  )

  # stop_times
  expect_true(all(smaller_ber$stop_times$trip_id %chin% relevant_trips))

  # stops
  relevant_stops <- unique(
    ber_gtfs$stop_times[trip_id %chin% relevant_trips]$stop_id
  )
  relevant_stops <- get_parent_station(ber_gtfs, relevant_stops)$stop_id
  expect_true(all(smaller_ber$stops$stop_id %chin% relevant_stops))
})

test_that("the function filters google's gtfs correctly", {
  smaller_ggl <- tester(ggl_gtfs, ggl_agency)

  # agency
  expect_true(smaller_ggl$agency$agency_id == ggl_agency)

  # attributions
  expect_true(all(smaller_ggl$attributions$agency_id == ggl_agency))

  # fare_attributes
  expect_true(all(smaller_ggl$fare_attributes$agency_id == ggl_agency))

  # routes
  expect_true(all(smaller_ggl$routes$agency_id %chin% ggl_agency))
  relevant_routes <- unique(smaller_ggl$routes$route_id)

  # fare_rules - none of the route_ids listed are in routes
  expect_true(nrow(smaller_ggl$fare_rules) == 0)

  # trips
  expect_true(all(smaller_ggl$trips$route_id %chin% relevant_routes))
  relevant_shapes <- unique(smaller_ggl$trips$shape_id)
  relevant_services <- unique(smaller_ggl$trips$service_id)
  relevant_trips <- unique(smaller_ggl$trips$trip_id)

  # shapes - none of the shape_ids listed are in trips
  expect_true(nrow(smaller_ggl$shapes) == 0)

  # calendar and calendar_dates
  expect_true(all(smaller_ggl$calendar$service_id %chin% relevant_services))
  expect_true(
    all(smaller_ggl$calendar_dates$service_id %chin% relevant_services)
  )

  # stop_times
  expect_true(all(smaller_ggl$stop_times$trip_id %chin% relevant_trips))

  # stops - none of the stop_ids listed are in stop_times
  expect_true(nrow(smaller_ggl$stops) == 0)

  # transfers - no rows either
  expect_true(nrow(smaller_ggl$transfers) == 0)

  # pathways - no rows either
  expect_true(nrow(smaller_ggl$pathways) == 0)

  # levels - no rows either
  expect_true(nrow(smaller_ggl$levels) == 0)

  # frequencies
  expect_true(all(smaller_ggl$frequencies$trip_id %chin% relevant_trips))
})



test_that("behaves correctly when agency_id = character(0)", {
  # if keep = TRUE, gtfs should be empty
  empty <- tester(agency_id = character(0))
  n_rows <- vapply(empty, nrow, FUN.VALUE = integer(1))
  expect_true(all(n_rows == 0))

  # if keep = FALSE, gtfs should remain unchanged
  # this is actually not true because the calendar, calendar_dates and agency
  # tables contain ids not listed in the routes and trips tables, which end up
  # removed anyway (I like this behaviour, so not considering a bug)
  full <- tester(ber_gtfs, character(0), keep = FALSE)
  modified_ber <- read_gtfs(ber_path)
  modified_ber$calendar <- modified_ber$calendar[
    service_id %in% modified_ber$trips$service_id
  ]
  modified_ber$calendar_dates <- modified_ber$calendar_dates[
    service_id %in% modified_ber$trips$service_id
  ]
  expect_identical(modified_ber, full)
})
