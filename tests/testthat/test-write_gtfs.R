context("Write GTFS")

# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)
gtfs$extra_file <- gtfs$agency

no_stop_times_gtfs <- gtfs
no_stop_times_gtfs$stop_times <- NULL

temp_file <- tempfile(pattern = "gtfs", fileext = ".zip")

specified_files <- c(
  "agency", "stops", "routes", "trips", "stop_times", "calendar",
  "calendar_dates", "fare_attributes", "fare_rules", "shapes", "frequencies",
  "transfers", "pathways", "levels", "feed_info", "translations", "attributions"
)

required_files <- c(
  "agency", "stops", "routes", "trips", "stop_times", "calendar"
)

optional_files <- setdiff(specified_files, required_files)


# tests -------------------------------------------------------------------


test_that("raises errors due to incorrect input types", {

  no_class_gtfs <- gtfs
  attr(no_class_gtfs, "class") <- NULL

  expect_error(write_gtfs(no_class_gtfs, temp_file))
  expect_error(write_gtfs(gtfs))
  expect_error(write_gtfs(gtfs, as.factor(temp_file)))
  expect_error(write_gtfs(gtfs, tempfile(pattern = "gtfs")))
  expect_error(write_gtfs(gtfs, temp_file, optional = "TRUE"))
  expect_error(write_gtfs(gtfs, temp_file, extra = "TRUE"))
  expect_error(write_gtfs(gtfs, temp_file, overwrite = "TRUE"))
  expect_error(write_gtfs(gtfs, temp_file, quiet = "TRUE"))
  expect_error(write_gtfs(gtfs, temp_file, warnings = "TRUE"))

})

test_that("raises an errors if file exists and should not be overwritten", {
  invisible(file.create(temp_file))
  expect_error(write_gtfs(gtfs, temp_file, overwrite = FALSE))
})

test_that("outputs a .zip file and invisibly returns the provided gtfs", {
  unlink(temp_file)
  expect_false(file.exists(temp_file))
  expect_s3_class(write_gtfs(gtfs, temp_file), "dt_gtfs")
  expect_true(file.exists(temp_file))
})

test_that("raises messages and warnings adequately", {
  expect_silent(write_gtfs(gtfs, temp_file))
  expect_silent(write_gtfs(no_stop_times_gtfs, temp_file, warnings = FALSE))
  expect_message(write_gtfs(gtfs, temp_file, quiet = FALSE))
  expect_warning(write_gtfs(no_stop_times_gtfs, temp_file, warnings = TRUE))
})

test_that("outputs a gtfs with the desired files", {

  # check if all items are written by default

  write_gtfs(gtfs, temp_file)
  files_in_gtfs <- zip::zip_list(temp_file)$filename
  expect_equal(length(files_in_gtfs), length(names(gtfs)))

  # check if extra file is not written when extra = FALSE

  write_gtfs(gtfs, temp_file, extra = FALSE)
  files_in_gtfs <- zip::zip_list(temp_file)$filename
  expect_equal(
    setdiff(names(gtfs), sub(".txt", "", files_in_gtfs)),
    "extra_file"
  )

  # check if optional files are not written when optional = FALSE

  write_gtfs(gtfs, temp_file, optional = FALSE)
  files_in_gtfs <- zip::zip_list(temp_file)$filename
  expect_equal(sum(files_in_gtfs %in% optional_files), 0)

  # check if only required files are written when extra = optional = FALSE

  write_gtfs(gtfs, temp_file, optional = FALSE, extra = FALSE)
  files_in_gtfs <- zip::zip_list(temp_file)$filename
  expect_equal(
    sum(sub(".txt", "", files_in_gtfs) %in% required_files),
    length(files_in_gtfs)
  )

})

test_that("doesn't change original gtfs (only validation_result attribute)", {

  no_validation_gtfs <- gtfs
  attr(no_validation_gtfs, "validation_result") <- NULL
  pre_write_no_validation_gtfs <- no_validation_gtfs

  written_gtfs <- write_gtfs(no_validation_gtfs, temp_file)

  expect_identical(no_validation_gtfs, pre_write_no_validation_gtfs)
  expect_false(identical(no_validation_gtfs, written_gtfs))

  # the difference between written_gtfs and no_validation_gtfs is the
  # validation_result

  validation_result <- attr(written_gtfs, "validation_result")
  attr(no_validation_gtfs, "validation_result") <- validation_result

  expect_identical(no_validation_gtfs, written_gtfs)

})

test_that("writes dates as YYYYMMDD", {

  write_gtfs(gtfs, temp_file)

  temp_dir <- tempdir()
  zip::unzip(
    temp_file,
    files = "calendar.txt",
    exdir = temp_dir,
    overwrite = TRUE
  )

  written_calendar_path <- file.path(temp_dir, "calendar.txt")
  written_calendar      <- data.table::fread(
    written_calendar_path,
    colClasses = "character"
  )

  expect_equal(
    sum(grepl("\\d{8}", written_calendar$start_date)),
    length(written_calendar$start_date)
  )

  expect_equal(
    sum(grepl("\\d{8}", written_calendar$end_date)),
    length(written_calendar$start_date)
  )

})
