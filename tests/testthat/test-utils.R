data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)
no_class_gtfs <- unclass(gtfs)


# string_to_seconds -------------------------------------------------------


context("String to seconds")

test_that("raises errors due to incorrect input types", {
  expect_error(string_to_seconds(factor("25:00:00")))
  expect_error(string_to_seconds(NULL))
})

test_that("outputs NA if empty string or NA is given", {
  expect_identical(string_to_seconds(""), NA_integer_)
  expect_identical(string_to_seconds(NA_character_), NA_integer_)
})

test_that("calculates seconds adequately", {
  expect_identical(string_to_seconds("00:00:00"), 0L)
  expect_identical(string_to_seconds("25:00:00"), 90000L)
  expect_identical(string_to_seconds("01:01:01"), 3661L)
})


# seconds_to_string -------------------------------------------------------


context("Seconds to string")

test_that("raises errors due to incorrect input types", {
  expect_error(seconds_to_string(1000))
  expect_error(seconds_to_string("1000"))
})

test_that("outputs '' if NA is given", {
  expect_equal(seconds_to_string(NA), "")
})

test_that("generates strings correctly", {
  expect_identical(seconds_to_string(0L), "00:00:00")
  expect_identical(seconds_to_string(90000L), "25:00:00")
  expect_identical(seconds_to_string(3661L), "01:01:01")
})


# copy_gtfs_without_file --------------------------------------------------


context("Copy GTFS without file")

test_that("raises errors due to incorrect input types", {
  expect_error(copy_gtfs_without_file(no_class_gtfs, "shapes"))
  expect_error(copy_gtfs_without_file(gtfs, as.factor("shapes")))
})

test_that("raises errors if non-existent file is given", {
  expect_error(copy_gtfs_without_file(gtfs, "non-existent file"))
})

test_that("outputs a gtfs without given file", {
  gtfs_copy <- copy_gtfs_without_file(gtfs, "shapes")
  expect_s3_class(gtfs_copy, "dt_gtfs")
  expect_identical(names(gtfs_copy), names(gtfs)[names(gtfs) != "shapes"])
})


# copy_gtfs_without_field -------------------------------------------------


context("Copy GTFS without field")

test_that("raises errors due to incorrect input types", {
  expect_error(copy_gtfs_without_file(no_class_gtfs, "shapes", "shape_id"))
  expect_error(copy_gtfs_without_file(gtfs, as.factor("shapes"), "shape_id"))
  expect_error(copy_gtfs_without_file(gtfs, "shapes", as.factor("shape_id")))
})

test_that("raises errors if non-existent file/field is given", {
  expect_error(copy_gtfs_without_file(gtfs, "non-existent file", "shape_id"))
  expect_error(copy_gtfs_without_file(gtfs, "shapes", "non-existent field"))
})

test_that("outputs a gtfs without the given field", {
  gtfs_copy <- copy_gtfs_without_field(gtfs, "shapes", "shape_id")
  expect_s3_class(gtfs_copy, "dt_gtfs")
  expect_identical(
    names(gtfs_copy$shapes),
    names(gtfs$shapes)[names(gtfs$shapes) != "shape_id"]
  )
})

test_that("outputs doesn't change original file", {
  shapes_before <- data.table::copy(gtfs$shapes)
  gtfs_copy <- copy_gtfs_without_field(gtfs, "shapes", "shape_id")
  shapes_after <- data.table::copy(gtfs$shapes)
  expect_identical(shapes_before, shapes_after)
})


# copy_gtfs_diff_field_class ----------------------------------------------


context("Copy GTFS with a field of a different class")

test_that("raises errors due to incorrect input types", {
  expect_error(
    copy_gtfs_diff_field_class(no_class_gtfs, "stops", "stop_id", "factor")
  )
  expect_error(
    copy_gtfs_diff_field_class(gtfs, as.factor("stops"), "stop_id", "factor")
  )
  expect_error(
    copy_gtfs_diff_field_class(gtfs, "stops", as.factor("stop_id"), "factor")
  )
  expect_error(
    copy_gtfs_diff_field_class(gtfs, "stops", "stop_id", as.factor("factor"))
  )
})

test_that("raises errors if non-existent file/field is given", {
  expect_error(
    copy_gtfs_diff_field_class(gtfs, "wrong_file", "stop_id", "factor")
  )
  expect_error(
    copy_gtfs_diff_field_class(gtfs, "stops", "wrong_field", "factor")
  )
})

test_that("outputs a gtfs with field of desired class", {

  # converting to factor

  gtfs_copy <- copy_gtfs_diff_field_class(gtfs, "stops", "stop_id", "factor")
  expect_s3_class(gtfs_copy, "dt_gtfs")
  expect_vector(gtfs$stops$stop_id, character(0))
  expect_s3_class(gtfs_copy$stops$stop_id, "factor")

  # and to character

  gtfs_copy <- copy_gtfs_diff_field_class(
    gtfs,
    "stops",
    "stop_lat",
    "character"
  )
  expect_s3_class(gtfs_copy, "dt_gtfs")
  expect_vector(gtfs$stops$stop_lat, numeric(0))
  expect_vector(gtfs_copy$stops$stop_lat, character(0))

})

test_that("function call doesn't change original file", {
  stops_before <- data.table::copy(gtfs$stops)
  gtfs_copy <- copy_gtfs_diff_field_class(gtfs, "stops", "stop_id", "factor")
  stops_after <- data.table::copy(gtfs$stops)
  expect_identical(stops_before, stops_after)
})
