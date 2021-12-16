context("Convert frequencies to stop_times")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)
trip_id <- "CPTM L07-0"

tester <- function(gtfs = get("gtfs", envir = parent.frame()), trip_id = NULL) {
  frequencies_to_stop_times(gtfs, trip_id)
}


# tests -------------------------------------------------------------------


test_that("raises errors due to incorrect input types/value", {
  no_class_gtfs <- structure(gtfs, class = NULL)
  expect_error(tester(no_class_gtfs))
  expect_error(tester(trip_id = as.factor(trip_id)))
})

test_that("raises warning if a non-existent trip_id is specified", {
  expect_warning(tester(trip_id = "a"))
  expect_warning(tester(trip_id = c("a", trip_id)))
})
