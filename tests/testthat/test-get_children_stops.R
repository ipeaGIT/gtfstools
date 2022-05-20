data_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()), stop_id = NULL) {
  get_children_stops(gtfs, stop_id)
}

test_that("raises errors due to incorrect input types/value", {
  expect_error(tester(unclass(gtfs)))
  expect_error(tester(stop_id = as.factor("N1")))
  expect_error(tester(stop_id = NA))
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
  children <- tester(stop_id = "F12")
  expect_s3_class(children, "data.table")
  expect_vector(children$stop_id, character(0))
  expect_vector(children$child_id, character(0))

  # should work even if no given 'stop_id's are present in 'stops'
  expect_warning(children <- tester(stop_id = "ola"))
  expect_s3_class(children, "data.table")
  expect_vector(children$stop_id, character(0))
  expect_vector(children$child_id, character(0))

  # and when stop_id = character(0)
  expect_s3_class(children, "data.table")
  expect_vector(children$stop_id, character(0))
  expect_vector(children$child_id, character(0))
})

test_that("returns children of correct stops", {
  children <- tester()
  expect_true(all(gtfs$stops$stop_id %chin% children$stop_id))

  children <- tester(stop_id = "F12S")
  expect_identical(unique(children$stop_id), c("F12S", "B1", "B3"))
})

test_that("returns children correctly", {
  # when none of the passed stops have children, all children will be "" and
  # the data.table will have as many as rows as the number of stop_ids passed
  children <- tester(stop_id = c("B1", "B3"))
  expect_identical(
    children,
    data.table::data.table(stop_id = c("B1", "B3"), child_id = "")
  )

  # if they have children, their children's children will be searched
  # recursively
  children <- tester(stop_id = "F12S")
  expect_identical(
    children,
    data.table::data.table(
      stop_id = c("F12S", "F12S", "B1", "B3"),
      child_id = c("B1", "B3", "", "")
    )
  )

  # non-existent ids are not included in the result
  expect_warning(children <- tester(stop_id = c("B1", "ola")))
  expect_identical(
    children,
    data.table::data.table(stop_id = "B1", child_id = "")
  )

  # stop_id = character() returns empty data.table
  children <- tester(stop_id = character())
  expect_identical(
    children,
    data.table::data.table(stop_id = character(), child_id = character())
  )
})

test_that("doesn't change original gtfs", {
  original_gtfs <- read_gtfs(data_path)
  gtfs <- read_gtfs(data_path)
  expect_identical(original_gtfs, gtfs)

  children <- tester(stop_id = "B1")
  expect_identical(original_gtfs, gtfs)
})
