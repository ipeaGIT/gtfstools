context("Get parent station")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)


# tests -------------------------------------------------------------------


test_that("raises errors due to incorrect input types/value", {

  no_class_gtfs <- unclass(gtfs)

  expect_error(get_parent_station(no_class_gtfs))
  expect_error(get_parent_station(gtfs, as.factor("N1")))

})

test_that("raises errors if reqrd fields do not exist or have the right type", {

  # create gtfs without 'stop_times'

  no_stops_gtfs <- copy_gtfs_without_file(gtfs, "stops")

  # create gtfs without relevant fields

  no_stp_id_gtfs <- copy_gtfs_without_field(gtfs, "stops", "stop_id")
  no_prt_st_gtfs <- copy_gtfs_without_field(gtfs, "stops", "parent_station")

  # create gtfs with fields of wrong class

  wrong_stp_id_gtfs <- copy_gtfs_diff_field_class(
    gtfs,
    "stops",
    "stop_id",
    "factor"
  )
  wrong_prt_st_gtfs <- copy_gtfs_diff_field_class(
    gtfs,
    "stops",
    "parent_station",
    "factor"
  )

  expect_error(get_parent_station(no_stops_gtfs, "N1"))
  expect_error(get_parent_station(no_stp_id_gtfs, "N1"))
  expect_error(get_parent_station(no_prt_st_gtfs, "N1"))
  expect_error(get_parent_station(wrong_stp_id_gtfs, "N1"))
  expect_error(get_parent_station(wrong_prt_st_gtfs, "N1"))

})

test_that("raises warnings if a non_existent stop_id is passed", {
  expect_warning(get_parent_station(gtfs, c("N1", "ola")))
  expect_warning(get_parent_station(gtfs, "ola"))
})

test_that("outputs a data.table with adequate columns' classes", {

  parents <- get_parent_station(gtfs, "N1")

  # result is a data.table

  expect_s3_class(parents, "data.table")

  # columns' types

  expect_vector(parents$stop_id, character(0))
  expect_vector(parents$parent_station, character(0))

  # should work even if no given 'stop_id's are present in 'stops'

  expect_warning(parents <- get_parent_station(gtfs, "ola"))

  expect_s3_class(parents, "data.table")

  expect_vector(parents$stop_id, character(0))
  expect_vector(parents$parent_station, character(0))

})

test_that("returns parents correctly", {
  # when none of the passed stops have parents, all parents will be "" and
  # the data.table will have as many as rows as the number of stop_ids passed
  parents <- get_parent_station(gtfs, "F12")
  expect_identical(
    parents,
    data.table::data.table(stop_id = "F12", parent_station = "")
  )

  # if they have parents, their parents' parents will be searched recursively
  parents <- get_parent_station(gtfs, "B1")
  expect_identical(
    parents,
    data.table::data.table(
      stop_id = c("B1", "F12S", "F12"),
      parent_station = c("F12S", "F12", "")
    )
  )

  # and non-existent ids are not included in the result
  expect_warning(parents <- get_parent_station(gtfs, "ola"))
  expect_identical(
    parents,
    data.table::data.table(
      stop_id = character(0),
      parent_station = character(0)
    )
  )
})

test_that("doesn't change original gtfs", {
  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  parents <- get_parent_station(gtfs, "B1")
  expect_identical(original_gtfs, gtfs)
})

# issue #33
test_that("unlisted parent_stations do not introduce NAs", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)
  ber_shapes <- c("14", "2")

  smaller_ber <- filter_shape_id(ber_gtfs, ber_shapes)

  parents <- get_parent_station(ber_gtfs, smaller_ber$stop_times$stop_id)
  expect_false(any(is.na(parents$stop_id)))
  expect_false(any(is.na(parents$parent_station)))
})
