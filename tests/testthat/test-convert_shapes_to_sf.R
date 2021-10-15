context("Convert shapes to sf")


# setup -------------------------------------------------------------------

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------

test_that("raises errors due to incorrect input types", {
  no_class_gtfs <- unclass(gtfs)

  expect_error(convert_shapes_to_sf(no_class_gtfs))
  expect_error(convert_shapes_to_sf(gtfs, shape_id = as.factor("17846")))
  expect_error(convert_shapes_to_sf(gtfs, crs = "4326"))

  wrong_types_gtfs <- read_gtfs(data_path)
  wrong_types_gtfs$shapes[, shape_id := as.factor(shape_id)]
  expect_error(convert_shapes_to_sf(wrong_types_gtfs))

  wrong_types_gtfs$shapes[
    ,
    `:=`(
      shape_id = as.character(shape_id),
      shape_pt_lon = as.character(shape_pt_lon)
    )
  ]
  expect_error(convert_shapes_to_sf(wrong_types_gtfs))

  wrong_types_gtfs$shapes[
    ,
    `:=`(
      shape_pt_lon = as.numeric(shape_pt_lon),
      shape_pt_lat = as.character(shape_pt_lat)
    )
  ]
  expect_error(convert_shapes_to_sf(wrong_types_gtfs))

  wrong_types_gtfs$shapes[
    ,
    `:=`(
      shape_pt_lat = as.numeric(shape_pt_lon),
      shape_pt_sequence = as.character(shape_pt_sequence)
    )
  ]
  expect_error(convert_shapes_to_sf(wrong_types_gtfs))
})

test_that("convert correct shapes", {
  # if 'shape_id' is NULL (default), all shapes are converted
  shapes_sf <- convert_shapes_to_sf(gtfs)
  expect_true(
    all(gtfs$shapes$shape_id %chin% shapes_sf$shape_id)
  )

  # else only (valid) shapes are converted
  shape_ids <- c("17846", "ola")
  suppressWarnings(
    shapes_sf <- convert_shapes_to_sf(gtfs, shape_id = shape_ids)
  )
  expect_identical(shapes_sf$shape_id, "17846")
})

test_that("raises warnings/erros if invalid stop_ids are passed", {
  # raises warning if a invalid stop_id is passed
  expect_warning(convert_shapes_to_sf(gtfs, shape_id = c("17846", "ola")))

  # throws error if all passed stop_ids are invalid
  expect_error(convert_shapes_to_sf(gtfs, shape_id = "ola"))
})

test_that("returns a LINESTRING sf with correct crs", {
  # outputs a LINESTRING sf
  shapes_sf <- convert_shapes_to_sf(gtfs)
  expect_s3_class(shapes_sf, "sf")
  expect_s3_class(shapes_sf$geometry, "sfc_LINESTRING")

  # crs is 4326 by default
  expect_identical(sf::st_crs(shapes_sf), sf::st_crs(4326))

  # which can be changed with 'crs' argument
  shapes_sf <- convert_shapes_to_sf(gtfs, crs = 4674)
  expect_identical(sf::st_crs(shapes_sf), sf::st_crs(4674))
})

test_that("doesn't change passed gtfs object (only the index of gtfs$shapes)", {
  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  shapes_sf <- convert_shapes_to_sf(gtfs)

  # 'shape_id' is set as gtfs$shapes index because of data.table subset
  data.table::setindex(gtfs$shapes, NULL)
  expect_identical(original_gtfs, gtfs)
})
