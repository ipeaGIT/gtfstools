path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(path)
weekday <- c("monday", "sunday")


# tests -------------------------------------------------------------------
# the large majority of tests are already conducted inside
# filter_by_service_id(), which filter_by_weekday() calls. only a few sanity
# checks are included here


test_that("raises error due to incorrect input types", {
  expect_error(filter_by_weekday(unclass(gtfs), weekday))
  expect_error(filter_by_weekday(gtfs, factor(weekday)))
  expect_error(filter_by_weekday(gtfs, "bad_day"))
  expect_error(filter_by_weekday(gtfs, weekday, combine = factor("or")))
  expect_error(filter_by_weekday(gtfs, weekday, combine = "oie"))
  expect_error(filter_by_weekday(gtfs, weekday, keep = "TRUE"))
  expect_error(filter_by_weekday(gtfs, weekday, keep = NA))
})

test_that("results in a dt_gtfs object", {
  # a dt_gtfs object is a list with "dt_gtfs" and "gtfs" classes
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")
  smaller_gtfs <- filter_by_weekday(gtfs, weekday)
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")

  # all objects inside a dt_gtfs are data.tables
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)

  original_gtfs <- read_gtfs(path)
  gtfs <- read_gtfs(path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- filter_by_weekday(gtfs, weekday)
  expect_equal(original_gtfs, gtfs, ignore_attr = TRUE)
})

test_that("'weekday', 'combine' and 'keep' arguments work correctly", {
  smaller_gtfs_or_keeping <- filter_by_weekday(gtfs, weekday)
  relevant_services <- c("USD", "U__", "US_", "_SD", "__D")
  expect_true(
    all(smaller_gtfs_or_keeping$calendar$service_id %in% relevant_services)
  )

  smaller_gtfs_or_dropping <- filter_by_weekday(gtfs, weekday, keep = FALSE)
  relevant_services <- c("_S_")
  expect_true(
    all(smaller_gtfs_or_dropping$calendar$service_id %in% relevant_services)
  )

  smaller_gtfs_and_keeping <- filter_by_weekday(gtfs, weekday, combine = "and")
  relevant_services <- c("USD")
  expect_true(
    all(smaller_gtfs_and_keeping$calendar$service_id %in% relevant_services)
  )

  smaller_gtfs_and_dropping <- filter_by_weekday(
    gtfs,
    weekday,
    combine = "and",
    keep = FALSE
  )
  relevant_services <- c("U__", "US_", "_SD", "__D", "_S_")
  expect_true(
    all(smaller_gtfs_and_dropping$calendar$service_id %in% relevant_services)
  )
})
