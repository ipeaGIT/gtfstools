data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)

shapes_sf <- convert_shapes_to_sf(gtfs, shape_id = c("17846", "17847", "17848"))

tester <- function(sf_shapes = shapes_sf,
                   shape_id = NULL,
                   calculate_distance = FALSE) {
  convert_sf_to_shapes(sf_shapes, shape_id, calculate_distance)
}

test_that("raises errors due to incorrect input types", {
  expect_error(tester("a"))
  expect_error(tester(sf::st_cast(shapes_sf, "POINT")))
  expect_error(tester(sf::st_transform(shapes_sf, 4674)))

  expect_error(tester(shape_id = 1))
  expect_error(tester(shape_id = NA_character_))

  expect_error(tester(calculate_distance = "TRUE"))
  expect_error(tester(calculate_distance = c(TRUE, TRUE)))
  expect_error(tester(calculate_distance = NA))
})

test_that("convert correct shapes", {
  # if 'shape_id' is NULL (default), all shapes are converted
  shapes <- tester()
  expect_true(all(shapes_sf$shape_id %chin% shapes$shape_id))

  # else only (valid) shapes are converted
  shape_ids <- c("17846", "ola")
  suppressWarnings(shapes <- tester(shape_id = shape_ids))
  expect_true(all(shapes$shape_id == "17846"))
})

test_that("raises warnings if some of the specified ids don't exist", {
  expect_warning(tester(shape_id = c("17846", "ola")))
  expect_warning(tester(shape_id = "ola"))
})

test_that("returns empty dt with empty sf and shape_id as inputs", {
  empty_sf_result <- tester(shapes_sf[0, ])
  expect_true(nrow(empty_sf_result) == 0)

  empty_shape_id_result <- tester(shape_id = character(0))
  expect_true(nrow(empty_shape_id_result) == 0)
})

test_that("returns a data.table with columns of correct class", {
  result <- tester()
  expect_s3_class(result, "data.table")
  expect_type(result$shape_id, "character")
  expect_type(result$shape_pt_lon, "double")
  expect_type(result$shape_pt_lat, "double")
  expect_type(result$shape_pt_sequence, "integer")

  # should work even if shape_id = character(0)

  result <- tester(shape_id = character(0))
  expect_s3_class(result, "data.table")
  expect_type(result$shape_id, "character")
  expect_type(result$shape_pt_lon, "double")
  expect_type(result$shape_pt_lat, "double")
  expect_type(result$shape_pt_sequence, "integer")

  # and both cases above should work when additional columns are present

  larger_sf <- shapes_sf
  larger_sf$extra_col <- c(1L, 2L, 3L)

  result <- tester(larger_sf)
  expect_s3_class(result, "data.table")
  expect_type(result$shape_id, "character")
  expect_type(result$shape_pt_lon, "double")
  expect_type(result$shape_pt_lat, "double")
  expect_type(result$shape_pt_sequence, "integer")
  expect_type(result$extra_col, "integer")

  result <- tester(larger_sf, shape_id = character(0))
  expect_s3_class(result, "data.table")
  expect_type(result$shape_id, "character")
  expect_type(result$shape_pt_lon, "double")
  expect_type(result$shape_pt_lat, "double")
  expect_type(result$shape_pt_sequence, "integer")
  expect_type(result$extra_col, "integer")
})

test_that("calculate_distance calculates shape_dist_traveled", {
  result <- tester(calculate_distance = TRUE)
  expect_type(result$shape_dist_traveled, "double")

  # works when shape_id = character(0)

  result <- tester(shape_id = character(0), calculate_distance = TRUE)
  expect_type(result$shape_dist_traveled, "double")
})

test_that("calculated coords and distances are correct", {
  filtered_original_shapes <- gtfs$shapes[
    shape_id %in% c("17846", "17847", "17848")
  ]

  shapes_from_sf <- tester()

  expect_identical(
    filtered_original_shapes[
      ,
      .(shape_id, shape_pt_lon, shape_pt_lat, shape_pt_sequence)
    ],
    shapes_from_sf
  )

  # distances in original gtfs don't start at 0 and are not really trustworthy,
  # so not checking that for now
})
