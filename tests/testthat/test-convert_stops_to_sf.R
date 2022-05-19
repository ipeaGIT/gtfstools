data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------

test_that("raises errors due to incorrect input types", {
  no_class_gtfs <- unclass(gtfs)

  expect_error(convert_stops_to_sf(no_class_gtfs))
  expect_error(convert_stops_to_sf(gtfs, stop_id = as.factor("18848")))
  expect_error(convert_stops_to_sf(gtfs, stop_id = NA))
  expect_error(convert_stops_to_sf(gtfs, crs = "4326"))
  expect_error(convert_stops_to_sf(gtfs, crs = NA))

  wrong_types_gtfs <- read_gtfs(data_path)
  wrong_types_gtfs$stops[, stop_id := as.factor(stop_id)]
  expect_error(convert_stops_to_sf(wrong_types_gtfs))

  wrong_types_gtfs$stops[
    ,
    `:=`(
      stop_id = as.character(stop_id),
      stop_lon = as.character(stop_lon)
    )
  ]
  expect_error(convert_stops_to_sf(wrong_types_gtfs))

  wrong_types_gtfs$stops[
    ,
    `:=`(
      stop_lon = as.numeric(stop_lon),
      stop_lat = as.character(stop_lat)
    )
  ]
  expect_error(convert_stops_to_sf(wrong_types_gtfs))
})

test_that("convert correct stops", {
  # if 'stop_id' is NULL (default), all stops are converted
  stops_sf <- convert_stops_to_sf(gtfs)
  expect_identical(stops_sf$stop_id, gtfs$stops$stop_id)

  # else only (valid) stops are converted
  stop_ids <- c("18848", "ola")
  suppressWarnings(stops_sf <- convert_stops_to_sf(gtfs, stop_id = stop_ids))
  expect_identical(stops_sf$stop_id, "18848")
})

test_that("raises warnings/erros if invalid stop_ids are passed", {
  # raises warning if a invalid stop_id is passed
  expect_warning(convert_stops_to_sf(gtfs, stop_id = c("18848", "ola")))
  expect_warning(convert_stops_to_sf(gtfs, stop_id = c("ola")))
  expect_silent(convert_stops_to_sf(gtfs, character(0)))
})

test_that("returns a POINT sf with correct crs", {
  # outputs a POINT sf
  stops_sf <- convert_stops_to_sf(gtfs)
  expect_s3_class(stops_sf, "sf")
  expect_s3_class(stops_sf$geometry, "sfc_POINT")

  # crs is 4326 by default
  expect_identical(sf::st_crs(stops_sf), sf::st_crs(4326))

  # which can be changed with 'crs' argument
  stops_sf <- convert_stops_to_sf(gtfs, crs = 4674)
  expect_identical(sf::st_crs(stops_sf), sf::st_crs(4674))

  # works even if none of the specified stops exist/character(0) is given
  suppressWarnings(
    stops_sf <- convert_stops_to_sf(gtfs, "ola")
  )
  expect_s3_class(stops_sf, "sf")
  expect_s3_class(stops_sf$geometry, "sfc_POINT")

  stops_sf <- convert_stops_to_sf(gtfs, character(0))
  expect_s3_class(stops_sf, "sf")
  expect_s3_class(stops_sf$geometry, "sfc_POINT")
})

test_that("doesn't change passed gtfs object (only the index of gtfs$stops)", {
  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  stops_sf <- convert_stops_to_sf(gtfs)

  # 'stop_id' is set as gtfs$stops index because of data.table subset
  data.table::setindex(gtfs$stops, NULL)
  expect_identical(original_gtfs, gtfs)

  # the same is true when it returns an empty sf
  stops_sf <- convert_stops_to_sf(gtfs, character(0))
  data.table::setindex(gtfs$stops, NULL)
  expect_identical(original_gtfs, gtfs)
})
