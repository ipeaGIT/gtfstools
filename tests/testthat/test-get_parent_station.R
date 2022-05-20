data_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)
stop_id <- "N1"

tester <- function(gtfs = get("gtfs", envir = parent.frame()), stop_id = NULL) {
  get_parent_station(gtfs, stop_id)
}

test_that("raises errors due to incorrect input types/value", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(gtfs, as.factor("N1")))
  expect_error(tester(gtfs, NA))
})

test_that("raises errors if reqrd fields do not exist or have the right type", {
  no_stops_gtfs <- copy_gtfs_without_file(gtfs, "stops")

  no_stp_id_gtfs <- copy_gtfs_without_field(gtfs, "stops", "stop_id")
  no_prt_st_gtfs <- copy_gtfs_without_field(gtfs, "stops", "parent_station")

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

  expect_error(tester(no_stops_gtfs), class = "missing_required_file")
  expect_error(tester(no_stp_id_gtfs), class = "missing_required_field")
  expect_error(tester(no_prt_st_gtfs), class = "missing_required_field")
  expect_error(tester(wrong_stp_id_gtfs), class = "wrong_class_field")
  expect_error(tester(wrong_prt_st_gtfs), class = "wrong_class_field")
})

test_that("raises warnings if a non_existent stop_id is passed", {
  expect_warning(tester(stop_id = c("N1", "ola")))
  expect_warning(tester(stop_id = "ola"))
})

test_that("outputs a data.table with adequate columns' classes", {
  parents <- tester()
  expect_s3_class(parents, "data.table")
  expect_type(parents$stop_id, "character")
  expect_type(parents$parent_station, "character")

  # should work even if no given 'stop_id's are present in 'stops'

  expect_warning(parents <- tester(stop_id = "ola"))
  expect_s3_class(parents, "data.table")
  expect_type(parents$stop_id, "character")
  expect_type(parents$parent_station, "character")

  # and when stop_id = character(0)

  parents <- tester(stop_id = character(0))
  expect_s3_class(parents, "data.table")
  expect_type(parents$stop_id, "character")
  expect_type(parents$parent_station, "character")
})

test_that("returns parents of correct stops", {
  parents <- tester()
  expect_true(all(gtfs$stops$stop_id %chin% parents$stop_id))

  parents <- tester(stop_id = stop_id)
  expect_identical(parents$stop_id, c("N1", "F12"))
})

test_that("returns parents correctly", {
  # when none of the passed stops have parents, all parents will be "" and
  # the data.table will have as many as rows as the number of stop_ids passed
  parents <- tester(stop_id = "F12")
  expect_identical(
    parents,
    data.table::data.table(stop_id = "F12", parent_station = "")
  )

  # if they have parents, their parents' parents will be searched recursively
  parents <- tester(stop_id = "B1")
  expect_identical(
    parents,
    data.table::data.table(
      stop_id = c("B1", "F12S", "F12"),
      parent_station = c("F12S", "F12", "")
    )
  )

  # and non-existent ids are not included in the result
  expect_warning(parents <- tester(stop_id = "ola"))
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

  parents <- tester(stop_id = "B1")
  expect_identical(original_gtfs, gtfs)
})

# issue #33
test_that("unlisted parent_stations do not introduce NAs", {
  ber_path <- system.file("extdata/ber_gtfs.zip", package = "gtfstools")
  ber_gtfs <- read_gtfs(ber_path)
  ber_shapes <- c("14", "2")

  smaller_ber <- filter_by_shape_id(ber_gtfs, ber_shapes)

  parents <- tester(ber_gtfs, smaller_ber$stop_times$stop_id)
  expect_false(any(is.na(parents$stop_id)))
  expect_false(any(is.na(parents$parent_station)))
})
