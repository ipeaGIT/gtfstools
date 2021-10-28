context("Remove duplicated entries")

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)

test_that("raises errors due to incorrect input", {
  no_class_gtfs <- unclass(gtfs)
  expect_error(remove_duplicates(no_class_gtfs), regexp = "'dt_gtfs'")
})

test_that("outputs a dt_gtfs object", {
  expect_s3_class(remove_duplicates(gtfs), "dt_gtfs")
})

test_that("removes duplicates", {
  nrows1 <- vapply(
    gtfs,
    function(dt) nrow(dt[, .N, by = names(dt)]),
    integer(1)
  )
  nrows2 <- vapply(remove_duplicates(gtfs), nrow, integer(1))
  expect_identical(nrows1, nrows2)

  # and a sanity check
  # remove duplicates from gtfs first, because otherwise the original gtfs would
  # have some duplicated entries and the unduplicated bigger wouldn't
  unique_gtfs <- remove_duplicates(gtfs)
  bigger_gtfs <- merge_gtfs(unique_gtfs, unique_gtfs)
  expect_identical(unique_gtfs, remove_duplicates(bigger_gtfs))
})

test_that("doesn't change original gtfs object", {
  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  unique_gtfs <- remove_duplicates(gtfs)
  expect_identical(original_gtfs, gtfs)
})
