context("Utilities")

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)

no_class_gtfs <- gtfs
attr(no_class_gtfs, "class") <- NULL


# check_gtfs_file_exists --------------------------------------------------


test_that("check_gtfs_file_exists raises errors due to incorrect input types", {
  expect_error(check_gtfs_file_exists(no_class_gtfs, "stop_times"))
  expect_error(check_gtfs_file_exists(gtfs, factor("stop_times")))
})

test_that("check_gtfs_file_exists outputs TRUE if files exist and an error message if not", {
  expect_true(check_gtfs_file_exists(gtfs, "stop_times"))
  expect_type(
    check_gtfs_file_exists(gtfs, "non_existent file"), "character"
  )
  expect_type(
    check_gtfs_file_exists(gtfs, c("non_existent file", "stop_times")),
    "character"
  )
  expect_type(
    check_gtfs_file_exists(gtfs, c("non_existent file", "hello_world")),
    "character"
  )
})


# check_gtfs_field_exists -------------------------------------------------


test_that("check_gtfs_field_exists raises errors due to incorrect input types", {
  expect_error(
    check_gtfs_field_exists(no_class_gtfs, "stop_times", "arrival_time")
  )
  expect_error(
    check_gtfs_field_exists(gtfs, factor("stop_times"), "arrival_time")
  )
  expect_error(
    check_gtfs_field_exists(gtfs, "stop_times", factor("arrival_time"))
  )
})

test_that("check_gtfs_field_exists outputs TRUE if file and fields exist and an error message if not", {
  expect_true(check_gtfs_field_exists(gtfs, "stop_times", "arrival_time"))
  expect_type(
    check_gtfs_field_exists(gtfs, "stop_times", "non-existent field"), "character"
  )
  expect_type(
    check_gtfs_field_exists(
      gtfs,
      "stop_times",
      c("arrival_time", "non-existent field")
    ),
    "character"
  )
  expect_type(
    check_gtfs_field_exists(
      gtfs,
      "stop_times",
      c("arrival_time", "non-existent field", "hello_world")
    ),
    "character"
  )
})

test_that("check_gtfs_field_exists raises an error if the file doesn't exist", {
  expect_error(check_gtfs_field_exists(gtfs, "non_existent file", "arrival_time"))
})


# string_to_seconds -------------------------------------------------------


test_that("string_to_seconds raises errors due to incorrect input types", {
  expect_error(string_to_seconds(factor("25:00:00")))
  expect_error(string_to_seconds(NULL))
})

test_that("string_to_seconds outputs NA if NA is given", {
  expect_identical(string_to_seconds(NA_character_), NA_integer_)
})

test_that("string_to_seconds calculates seconds adequately", {
  expect_identical(string_to_seconds("00:00:00"), 0L)
  expect_identical(string_to_seconds("25:00:00"), 90000L)
  expect_identical(string_to_seconds("01:01:01"), 3661L)
})


# seconds_to_string -------------------------------------------------------


test_that("seconds_to_string raises errors due to incorrect input types", {
  expect_error(seconds_to_string(1000))
  expect_error(seconds_to_string("1000"))
})

test_that("seconds_to_string outputs '' if NA is given", {
  expect_equal(seconds_to_string(NA), "")
})

test_that("seconds_to_string generates strings correctly", {
  expect_identical(seconds_to_string(0L), "00:00:00")
  expect_identical(seconds_to_string(90000L), "25:00:00")
  expect_identical(seconds_to_string(3661L), "01:01:01")
})


# copy_gtfs_without_file --------------------------------------------------


test_that("copy_gtfs_without_file raises errors due to incorrect input types", {
  expect_error(copy_gtfs_without_file(no_class_gtfs, "shapes"))
  expect_error(copy_gtfs_without_file(gtfs, as.factor("shapes")))
})

test_that("copy_gtfs_without_file raises errors if non-existent file is given", {
  expect_error(copy_gtfs_without_file(gtfs, "non-existent file"))
})

test_that("copy_gtfs_without_file outputs a gtfs without given file", {
  gtfs_copy <- copy_gtfs_without_file(gtfs, "shapes")
  expect_s3_class(gtfs_copy, "dt_gtfs")
  expect_identical(names(gtfs_copy), names(gtfs)[names(gtfs) != "shapes"])
})


# copy_gtfs_without_field -------------------------------------------------


test_that("copy_gtfs_without_field raises errors due to incorrect input types", {
  expect_error(copy_gtfs_without_file(no_class_gtfs, "shapes", "shape_id"))
  expect_error(copy_gtfs_without_file(gtfs, as.factor("shapes"), "shape_id"))
  expect_error(copy_gtfs_without_file(gtfs, "shapes", as.factor("shape_id")))
})

test_that("copy_gtfs_without_field raises errors if non-existent file/field is given", {
  expect_error(copy_gtfs_without_file(gtfs, "non-existent file", "shape_id"))
  expect_error(copy_gtfs_without_file(gtfs, "shapes", "non-existent field"))
})

test_that("copy_gtfs_without_field outputs a gtfs without the given field", {
  gtfs_copy <- copy_gtfs_without_field(gtfs, "shapes", "shape_id")
  expect_s3_class(gtfs_copy, "dt_gtfs")
  expect_identical(
    names(gtfs_copy$shapes),
    names(gtfs$shapes)[names(gtfs$shapes) != "shape_id"]
  )
})

test_that("copy_gtfs_without_field outputs doesn't change original file", {
  shapes_before <- data.table::copy(gtfs$shapes)
  gtfs_copy <- copy_gtfs_without_field(gtfs, "shapes", "shape_id")
  shapes_after <- data.table::copy(gtfs$shapes)
  expect_identical(shapes_before, shapes_after)
})





