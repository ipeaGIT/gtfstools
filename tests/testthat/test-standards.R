path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
standard_gtfs <- gtfsio::import_gtfs(path)
standard_gtfs$feed_info[, feed_start_date := 20230428L]
standard_gtfs$feed_info[, feed_end_date := 20231231L]
gtfstools_gtfs <- convert_from_standard(standard_gtfs)


# convert_from_standard() tests -------------------------------------------


test_that("convert_from_standard only works on objects with gtfs class", {
  expect_error(convert_from_standard(unclass(standard_gtfs)))
})

test_that("convert_from_standard doesn't change original gtfs object", {
  original_gtfs <- gtfsio::import_gtfs(path)
  original_gtfs$feed_info[, feed_start_date := 20230428L]
  original_gtfs$feed_info[, feed_end_date := 20231231L]

  expect_silent(invisible(convert_from_standard(standard_gtfs)))
  expect_identical(original_gtfs, standard_gtfs)
})

test_that("convert_from_standard works correctly", {
  expect_s3_class(gtfstools_gtfs$calendar_dates$date, "Date")
  expect_s3_class(gtfstools_gtfs$calendar$start_date, "Date")
  expect_s3_class(gtfstools_gtfs$calendar$end_date, "Date")
  expect_s3_class(gtfstools_gtfs$feed_info$feed_start_date, "Date")
  expect_s3_class(gtfstools_gtfs$feed_info$feed_end_date, "Date")
})


# convert_to_standard() tests ---------------------------------------------


test_that("convert_to_standard only works on 'dt_gtfs's", {
  expect_error(convert_to_standard(unclass(gtfstools_gtfs)))
})

test_that("convert_to_standard doesn't change original gtfs object", {
  another_gtfstools_gtfs <- convert_from_standard(standard_gtfs)

  expect_silent(invisible(convert_to_standard(gtfstools_gtfs)))
  expect_identical(gtfstools_gtfs, another_gtfstools_gtfs)
})

test_that("convert_to_standard works correctly", {
  restandard_gtfs <- convert_to_standard(gtfstools_gtfs)

  expect_identical(restandard_gtfs, standard_gtfs)
})
