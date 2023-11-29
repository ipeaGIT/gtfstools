spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
relevant_stops <- c("18848", "940004157")

tester <- function(gtfs = spo_gtfs,
                   stop_id = relevant_stops,
                   keep = TRUE,
                   full_trips = TRUE) {
  filter_by_stop_id(gtfs, stop_id, keep, full_trips)
}

# tests -------------------------------------------------------------------

# full_trips = TRUE

test_that("raises error due to incorrect input types", {
  expect_error(tester(unclass(spo_gtfs)))

  expect_error(tester(stop_id = factor(relevant_stops)))
  expect_error(tester(stop_id = NA))

  expect_error(tester(keep = "TRUE"))
  expect_error(tester(keep = c(TRUE, TRUE)))
  expect_error(tester(keep = NA))

  expect_error(tester(full_trips = "TRUE"))
  expect_error(tester(full_trips = c(TRUE, TRUE)))
  expect_error(tester(full_trips = NA))
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

  smaller_gtfs <- tester(gtfs)
  expect_false(identical(original_gtfs, gtfs))
  data.table::setindex(gtfs$stops, NULL)
  data.table::setindex(gtfs$stop_times, NULL)
  expect_identical(original_gtfs, gtfs)
})

test_that("'stop_id' and 'keep' arguments work correctly", {
  relevant_trips <- gtfs$stop_times[stop_id %chin% stop_ids]$trip_id

  smaller_gtfs_keeping <- tester()
  expect_true(all(smaller_gtfs_keeping$trips$trip_id %in% relevant_trips))

  smaller_gtfs_not_keeping <- tester(keep = FALSE)
  expect_true(!any(smaller_gtfs_not_keeping$trips$trip_id %in% relevant_trips))
})
