context("Get trip geometry")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------


test_that("get_trip_geometry raises errors due to incorrect input types/value", {

  no_class_gtfs <- gtfs
  attr(no_class_gtfs, "class") <- NULL

  expect_error(get_trip_geometry(no_class_gtfs))
  expect_error(get_trip_geometry(gtfs, as.factor("CPTM L07-0")))
  expect_error(get_trip_geometry(gtfs, file = c("shapes", "stops")))
  expect_error(get_trip_geometry(gtfs, crs = "4674"))

})

test_that("get_trip_duration raises errors if gtfs doesn't have required files/fields", {

  # create gtfs objects without relevant files

  no_trips_gtfs <- copy_gtfs_without_file(gtfs, "trips")
  no_shapes_gtfs <- copy_gtfs_without_file(gtfs, "shapes")
  no_stop_times_gtfs <- copy_gtfs_without_file(gtfs, "stop_times")
  no_stops_gtfs <- copy_gtfs_without_file(gtfs, "stops")

  # create gtfs objects without relevant fields

  no_trp_tripid_gtfs <- copy_gtfs_without_field(gtfs, "trips", "trip_id")
  no_trp_shapeid_gtfs <- copy_gtfs_without_field(gtfs, "trips", "shape_id")

  no_shp_shapeid_gtfs <- copy_gtfs_without_field(gtfs, "shapes", "shape_id")
  no_shp_shapelat_gtfs <- copy_gtfs_without_field(gtfs, "shapes", "shape_pt_lat")
  no_shp_shapelon_gtfs <- copy_gtfs_without_field(gtfs, "shapes", "shape_pt_lon")
  no_shp_shapeseq_gtfs <- copy_gtfs_without_field(gtfs, "shapes", "shape_pt_sequence")

  no_stt_tripid_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "trip_id")
  no_stt_stopid_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "stop_id")
  no_stt_stopseq_gtfs <- copy_gtfs_without_field(gtfs, "stop_times", "stop_sequence")

  no_sts_stopid_gtfs <- copy_gtfs_without_field(gtfs, "stops", "stop_id")
  no_sts_stoplat_gtfs <- copy_gtfs_without_field(gtfs, "stops", "stop_lat")
  no_sts_stoplon_gtfs <- copy_gtfs_without_field(gtfs, "stops", "stop_lon")

  # 'shapes'-based geometries

  expect_error(get_trip_geometry(no_trips_gtfs, "CPTM L07-0", file = "shapes"))
  expect_error(get_trip_geometry(no_shapes_gtfs, "CPTM L07-0", file = "shapes"))
  expect_error(get_trip_geometry(no_trp_tripid_gtfs, "CPTM L07-0", file = "shapes"))
  expect_error(get_trip_geometry(no_trp_shapeid_gtfs, "CPTM L07-0", file = "shapes"))
  expect_error(get_trip_geometry(no_shp_shapeid_gtfs, "CPTM L07-0", file = "shapes"))
  expect_error(get_trip_geometry(no_shp_shapelat_gtfs, "CPTM L07-0", file = "shapes"))
  expect_error(get_trip_geometry(no_shp_shapelon_gtfs, "CPTM L07-0", file = "shapes"))
  expect_error(get_trip_geometry(no_shp_shapeseq_gtfs, "CPTM L07-0", file = "shapes"))

  expect_s3_class(
    get_trip_geometry(no_stop_times_gtfs, "CPTM L07-0", file = "shapes"),
    "sf"
  )
  expect_s3_class(
    get_trip_geometry(no_stops_gtfs, "CPTM L07-0", file = "shapes"),
    "sf"
  )

  # 'stop_times'-based geometries

  expect_error(get_trip_geometry(no_trips_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_stop_times_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_stops_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_trp_tripid_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_stt_tripid_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_stt_stopid_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_stt_stopseq_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_sts_stopid_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_sts_stoplat_gtfs, "CPTM L07-0", file = "stop_times"))
  expect_error(get_trip_geometry(no_sts_stoplon_gtfs, "CPTM L07-0", file = "stop_times"))

  expect_s3_class(
    get_trip_geometry(no_shapes_gtfs, "CPTM L07-0", file = "stop_times"),
    "sf"
  )
  expect_s3_class(
    get_trip_geometry(no_trp_shapeid_gtfs, "CPTM L07-0", file = "stop_times"),
    "sf"
  )

})

test_that("get_trip_geometry returns the geometries of correct 'trip_id's", {

  # if 'trip_id' = NULL, all trips have their geometries returned

  all_trip_ids <- unique(gtfs$trips$trip_id)
  all_trip_ids <- all_trip_ids[order(all_trip_ids)]

  geometries_all_trip_ids <- get_trip_geometry(gtfs)
  trip_ids_from_geom <- unique(geometries_all_trip_ids$trip_id)
  trip_ids_from_geom <- trip_ids_from_geom[order(trip_ids_from_geom)]

  expect_identical(all_trip_ids, trip_ids_from_geom)

  # else, only the duration of (valid) trip_ids are calculated

  selected_trip_ids <- c("CPTM L07-0", "ola")
  suppressWarnings(
    geom_selected_trip_ids <- get_trip_geometry(gtfs, selected_trip_ids)
  )
  expect_equal(unique(geom_selected_trip_ids$trip_id), "CPTM L07-0")

})

test_that("get_trip_geometry raises warnings if a non_existent trip_id is given", {
  expect_warning(get_trip_geometry(gtfs, c("CPTM L07-0", "ola")))
})

test_that("get_trip_geometry returns the geometries created by the given 'file'", {

  shape_geom <- get_trip_geometry(gtfs, "CPTM L07-0", file = "shapes")
  stop_times_geom <- get_trip_geometry(gtfs, "CPTM L07-0", file = "stop_times")
  both_geom <- get_trip_geometry(gtfs, "CPTM L07-0")

  expect_equal(unique(shape_geom$origin_file), "shapes")
  expect_equal(unique(stop_times_geom$origin_file), "stop_times")
  expect_identical(unique(both_geom$origin_file), c("shapes", "stop_times"))

})

test_that("get_trip_geometry outputs an 'sf' object with correct crs", {

  # crs is WGs by default

  point <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)

  sf_geom <- get_trip_geometry(gtfs, c("CPTM L07-0"))
  expect_s3_class(sf_geom, "sf")
  expect_identical(sf::st_crs(sf_geom), sf::st_crs(point))

  # 'crs' can be an integer or crs object

  point <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4674)

  sf_geom <- get_trip_geometry(gtfs, c("CPTM L07-0"), crs = 4674)
  expect_s3_class(sf_geom, "sf")
  expect_identical(sf::st_crs(sf_geom), sf::st_crs(point))

  sf_geom <- get_trip_geometry(gtfs, c("CPTM L07-0"), crs = sf::st_crs(point))
  expect_s3_class(sf_geom, "sf")
  expect_identical(sf::st_crs(sf_geom), sf::st_crs(point))

  # should work even when all 'trip_id's given are not present in the gtfs

  # default

  point <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)

  expect_warning(sf_geom <- get_trip_geometry(gtfs, c("ola")))
  expect_s3_class(sf_geom, "sf")
  expect_identical(sf::st_crs(sf_geom), sf::st_crs(point))

  # different crs

  point <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4674)

  expect_warning(sf_geom <- get_trip_geometry(gtfs, c("ola"), crs = 4674))
  expect_s3_class(sf_geom, "sf")
  expect_identical(sf::st_crs(sf_geom), sf::st_crs(point))

  expect_warning(sf_geom <- get_trip_geometry(gtfs, c("ola"), crs = sf::st_crs(point)))
  expect_s3_class(sf_geom, "sf")
  expect_identical(sf::st_crs(sf_geom), sf::st_crs(point))

})

test_that("get_trip_geometry outputs an 'sf' object with correct columns' types", {

  sf_geom <- get_trip_geometry(gtfs, "CPTM L07-0")

  # result is an sf

  expect_s3_class(sf_geom, "sf")

  # columns' types

  expect_equal(class(sf_geom$trip_id), "character")
  expect_equal(class(sf_geom$origin_file), "character")
  expect_identical(class(sf_geom$geometry), c("sfc_LINESTRING", "sfc"))

  # should work even when all 'trip_id's given are not present in the gtfs
  # (geometry class changes)

  expect_warning(sf_geom <- get_trip_geometry(gtfs, "ola"))

  expect_s3_class(sf_geom, "sf")

  expect_equal(class(sf_geom$trip_id), "character")
  expect_equal(class(sf_geom$origin_file), "character")
  expect_identical(class(sf_geom$geometry), c("sfc_GEOMETRY", "sfc"))

})

test_that("get_trip_geometry doesn't change given gtfs (except for 'stop_times', 'shapes' and 'trips' indices)", {

  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  sf_geom <- get_trip_geometry(gtfs, "CPTM L07-0")
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$shapes, NULL)
  data.table::setindex(gtfs$stop_times, NULL)
  data.table::setindex(gtfs$trips, NULL)
  expect_identical(original_gtfs, gtfs)

})
