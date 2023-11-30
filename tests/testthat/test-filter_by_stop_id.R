spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
spo_stops <- c("18848", "940004157")

tester <- function(gtfs = spo_gtfs,
                   stop_id = spo_stops,
                   keep = TRUE,
                   full_trips = TRUE) {
  filter_by_stop_id(gtfs, stop_id, keep, full_trips)
}

# tests -------------------------------------------------------------------

# full_trips = TRUE

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(spo_gtfs)))

  expect_error(tester(stop_id = factor(spo_stops)))
  expect_error(tester(stop_id = NA))

  expect_error(tester(keep = "TRUE"))
  expect_error(tester(keep = c(TRUE, TRUE)))
  expect_error(tester(keep = NA))

  expect_error(tester(full_trips = "TRUE"))
  expect_error(tester(full_trips = c(TRUE, TRUE)))
  expect_error(tester(full_trips = NA))
})

test_that("full_trips = TRUE is deprecated", {
  expect_warning(tester(), class = "deprecated_full_trips_filter")
})

test_that("results in a dt_gtfs object", {
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")

  suppressWarnings(
    smaller_gtfs <- tester(),
    classes = "deprecated_full_trips_filter"
  )
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  suppressWarnings(
    smaller_gtfs <- tester(gtfs),
    classes = "deprecated_full_trips_filter"
  )
  expect_equal(original_gtfs, gtfs, check.attributes = FALSE)
})

test_that("'stop_id' and 'keep' arguments work correctly", {
  relevant_trips <- spo_gtfs$stop_times[stop_id %chin% spo_stops]$trip_id

  suppressWarnings(
    smaller_gtfs_keeping <- tester(),
    classes = "deprecated_full_trips_filter"
  )
  expect_true(all(smaller_gtfs_keeping$trips$trip_id %in% relevant_trips))

  suppressWarnings(
    smaller_gtfs_not_keeping <- tester(keep = FALSE),
    classes = "deprecated_full_trips_filter"
  )
  expect_true(!any(smaller_gtfs_not_keeping$trips$trip_id %in% relevant_trips))
})

# full_trips = FALSE

tester2 <- function(...) tester(..., full_trips = FALSE)

test_that("raises error due to incorrect input types", {
  expect_error(tester2(unclass(spo_gtfs)))

  expect_error(tester2(stop_id = factor(spo_stops)))
  expect_error(tester2(stop_id = NA))

  expect_error(tester2(keep = "TRUE"))
  expect_error(tester2(keep = c(TRUE, TRUE)))
  expect_error(tester2(keep = NA))
})

test_that("results in a dt_gtfs object", {
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")

  smaller_gtfs <- tester2()
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)

  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- tester2(gtfs)
  expect_equal(original_gtfs, gtfs, check.attributes = FALSE)
})

test_that("'stop_id' and 'keep' arguments work correctly", {
  smaller_keeping <- tester2()
  expect_true(all(smaller_keeping$stops$stop_id %chin% spo_stops))
  expect_true(all(smaller_keeping$stop_times$stop_id %chin% spo_stops))

  smaller_not_keeping <- tester2(keep = FALSE)
  expect_true(!any(smaller_not_keeping$stops$stop_id %chin% spo_stops))
  expect_true(!any(smaller_not_keeping$stop_times$stop_id %chin% spo_stops))
})

test_that("the function filters berlin's gtfs correctly", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)
  ber_stops <- c("100000710203", "100000410202")

  smaller_ber <- tester2(ber_gtfs, ber_stops)

  # stops
  expect_true(nrow(smaller_ber$stops) == 2)
  expect_true(all(smaller_ber$stops$stop_id %chin% ber_stops))

  # stop_times
  expect_true(all(smaller_ber$stop_times$stop_id %chin% ber_stops))

  # trips
  relevant_trips <- smaller_ber$stop_times$trip_id
  expect_true(all(smaller_ber$trips$trip_id %chin% relevant_trips))

  # shapes
  relevant_shapes <- smaller_ber$trips$shape_id
  expect_true(all(smaller_ber$shapes$shape_id %chin% relevant_shapes))

  # calendar and calendar_dates
  relevant_services <- smaller_ber$trips$service_id
  expect_true(all(smaller_ber$calendar$service_id %chin% relevant_services))
  expect_true(
    all(smaller_ber$calendar_dates$service_id %chin% relevant_services)
  )

  # routes
  relevant_routes <- smaller_ber$trips$route_id
  expect_true(all(smaller_ber$routes$route_id %chin% relevant_routes))

  # agency
  relevant_agency <- unique(smaller_ber$routes$agency_id)
  expect_true(smaller_ber$agency$agency_id == relevant_agency)
})

test_that("the function filters sao paulo's gtfs correctly", {
  smaller_spo <- tester2()

  # stops
  expect_true(nrow(smaller_spo$stops) == 2)
  expect_true(all(smaller_spo$stops$stop_id %chin% spo_stops))

  # stop_times
  expect_true(all(smaller_spo$stop_times$stop_id %chin% spo_stops))

  # trips and frequencies
  relevant_trips <- smaller_spo$stop_times$trip_id
  expect_true(all(smaller_spo$trips$trip_id %chin% relevant_trips))
  expect_true(all(smaller_spo$frequencies$trip_id %chin% relevant_trips))

  # shapes
  relevant_shapes <- smaller_spo$trips$shape_id
  expect_true(all(smaller_spo$shapes$shape_id %chin% relevant_shapes))

  # calendar
  relevant_services <- smaller_spo$trips$service_id
  expect_true(all(smaller_spo$calendar$service_id %chin% relevant_services))

  # routes
  relevant_routes <- smaller_spo$trips$route_id
  expect_true(all(smaller_spo$routes$route_id %chin% relevant_routes))

  # agency
  relevant_agency <- unique(smaller_spo$routes$agency_id)
  expect_true(unique(smaller_spo$agency$agency_id) == relevant_agency)
})

test_that("the function filters google's gtfs correctly", {
  ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
  ggl_gtfs <- read_gtfs(ggl_path)
  ggl_stops <- c("S1", "N1", "N2", "S6", "S7")

  smaller_ggl <- tester2(ggl_gtfs, ggl_stops)

  # stops, stop_times, pathways and transfers
  expect_true(all(smaller_ggl$stops$stop_id %chin% ggl_stops))
  expect_true(all(smaller_ggl$stop_times$stop_id %chin% ggl_stops))
  expect_true(
    all(
      smaller_ggl$transfers$from_stop_id %chin% ggl_stops &
        smaller_ggl$transfers$to_stop_id %chin% ggl_stops
    )
  )
  expect_true(
    all(
      smaller_ggl$pathways$from_stop_id %chin% ggl_stops &
        smaller_ggl$pathways$to_stop_id %chin% ggl_stops
    )
  )

  # levels
  relevant_levels <- smaller_ggl$stops$level_id
  expect_true(all(smaller_ggl$level$level_id %chin% relevant_levels))

  # trips and frequencies
  relevant_trips <- smaller_ggl$stop_times$trip_id
  expect_true(all(smaller_ggl$trips$trip_id %chin% relevant_trips))
  expect_true(all(smaller_ggl$frequencies$trip_id %chin% relevant_trips))

  # shapes
  relevant_shapes <- smaller_ggl$trips$shape_id
  expect_true(all(smaller_ggl$shapes$shape_id %chin% relevant_shapes))

  # calendar and calendar_dates
  relevant_services <- smaller_ggl$trips$service_id
  expect_true(all(smaller_ggl$calendar$service_id %chin% relevant_services))
  expect_true(
    all(smaller_ggl$calendar_dates$service_id %chin% relevant_services)
  )

  # routes
  relevant_routes <- smaller_ggl$trips$route_id
  expect_true(all(smaller_ggl$routes$route_id %chin% relevant_routes))

  # fare_rules and fare_attributes are empty
  expect_true(nrow(smaller_ggl$fare_rules) == 0)
  expect_true(nrow(smaller_ggl$fare_attributes) == 0)

  # agency
  relevant_agency <- "agency001"
  expect_true(unique(smaller_ggl$agency$agency_id) == relevant_agency)
})

test_that("behaves correctly when stop_id = character(0)", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)

  # if keep = TRUE, gtfs should be empty
  empty <- tester2(ber_gtfs, character(0))
  n_rows <- vapply(empty, nrow, FUN.VALUE = integer(1))
  expect_true(all(n_rows == 0))

  # if keep = FALSE, gtfs should remain unchanged
  # this is actually not true because the calendar, calendar_dates and agency
  # tables contain ids not listed in the routes and trips tables, which end up
  # removed anyway (I like this behaviour, so not considering a bug)
  full <- tester2(ber_gtfs, character(0), keep = FALSE)
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
