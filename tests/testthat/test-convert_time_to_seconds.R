context("Convert time to seconds")

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   file = NULL,
                   by_reference = FALSE) {
  convert_time_to_seconds(gtfs, file, by_reference)
}

test_that("raises errors due to incorrect input types/value", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(file = "shapes"))
  expect_error(tester(file = 2))
  expect_error(tester(by_reference = "TRUE"))

})

test_that("raises errors if gtfs doesn't have stop_times and frequencies", {
  no_st_gtfs <- copy_gtfs_without_file(gtfs, "stop_times")
  no_st_freq_gtfs <- copy_gtfs_without_file(no_st_gtfs, "frequencies")
  expect_error(tester(no_st_freq_gtfs))
})

test_that("correct fields are converted", {
  both_converted <- tester()
  expect_true(
    all(
      c("departure_time_secs", "arrival_time_secs") %chin%
        names(both_converted$stop_times)
    )
  )
  expect_true(
    all(
      c("start_time_secs", "end_time_secs") %chin%
        names(both_converted$frequencies)
    )
  )

  stop_times_converted <- tester(file = "stop_times")
  expect_identical(gtfs$frequencies, stop_times_converted$frequencies)
  expect_true(
    all(
      c("departure_time_secs", "arrival_time_secs") %chin%
        names(stop_times_converted$stop_times)
    )
  )

  freq_converted <- tester(file = "frequencies")
  expect_identical(gtfs$stop_times, freq_converted$stop_times)
  expect_true(
    all(
      c("start_time_secs", "end_time_secs") %chin%
        names(freq_converted$frequencies)
    )
  )
})

test_that("output is a gtfs object when by_reference = FALSE", {
  converted_gtfs <- tester()
  expect_s3_class(converted_gtfs, "dt_gtfs")
})

test_that("original gtfs object remains unchanged when by_reference = FALSE", {
  original_gtfs <- read_gtfs(data_path)
  converted_gtfs <- tester()
  expect_identical(original_gtfs, gtfs)
})

test_that("original gtfs object is changed when by_reference = TRUE", {
  original_gtfs <- read_gtfs(data_path)
  tester(by_reference = TRUE)
  expect_false(identical(original_gtfs, gtfs))

  expect_true(
    all(
      c("departure_time_secs", "arrival_time_secs") %chin%
        names(gtfs$stop_times)
    )
  )
  expect_true(
    all(c("start_time_secs", "end_time_secs") %chin% names(gtfs$frequencies))
  )
})
