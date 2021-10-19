context("Filter by route_id")


# setup -------------------------------------------------------------------


spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
spo_gtfs <- read_gtfs(spo_path)
spo_routes <- c("6450-51", "CPTM L11")

ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)
ggl_routes <- c("A", "TSW")


# tests -------------------------------------------------------------------


test_that("raises error due to incorrect input types", {
  expect_error(filter_route_id(unclass(spo_gtfs), spo_routes))
  expect_error(filter_route_id(spo_gtfs, factor(spo_routes)))
  expect_error(filter_route_id(spo_gtfs, spo_routes, keep = "TRUE"))
})

test_that("results in a dt_gtfs object", {
  # a dt_gtfs object is a list with "dt_gtfs" and "gtfs" classes
  dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")
  smaller_gtfs <- filter_route_id(spo_gtfs, spo_routes)
  expect_s3_class(smaller_gtfs, dt_gtfs_class)
  expect_type(smaller_gtfs, "list")

  # all objects inside a dt_gtfs are data.tables
  invisible(lapply(smaller_gtfs, expect_s3_class, "data.table"))
})

test_that("doesn't change given gtfs", {
  # (except for some tables' indices)

  original_gtfs <- read_gtfs(spo_path)
  gtfs <- read_gtfs(spo_path)
  expect_identical(original_gtfs, gtfs)

  smaller_gtfs <- filter_route_id(gtfs, spo_routes)
  expect_false(identical(original_gtfs, gtfs))

  data.table::setindex(gtfs$agency, NULL)
  data.table::setindex(gtfs$calendar, NULL)
  data.table::setindex(gtfs$frequencies, NULL)
  data.table::setindex(gtfs$shapes, NULL)
  data.table::setindex(gtfs$stop_times, NULL)
  data.table::setindex(gtfs$stops, NULL)
  expect_identical(original_gtfs, gtfs)
})

test_that("'route_id' and 'keep' arguments work correctly", {
  smaller_ggl_keeping <- filter_route_id(ggl_gtfs, ggl_routes)
  expect_true(any(smaller_ggl_keeping$routes$route_id %chin% ggl_routes))
  expect_true(any(smaller_ggl_keeping$trips$route_id %chin% ggl_routes))
  expect_true(any(smaller_ggl_keeping$fare_rules$route_id %chin% ggl_routes))

  smaller_ggl_not_keeping <- filter_route_id(ggl_gtfs, ggl_routes, keep = FALSE)
  expect_true(!any(smaller_ggl_not_keeping$routes$route_id %chin% ggl_routes))
  expect_true(!any(smaller_ggl_not_keeping$trips$route_id %chin% ggl_routes))
  expect_true(
    !any(smaller_ggl_not_keeping$fare_rules$route_id %chin% ggl_routes)
  )
})

test_that("it doesn't throw warnings because of missing stations in 'stops'", {
  # this would be caused if 'stop_times' mentioned a stop not listed in 'stops'.
  # the current implementation suppress this eventual warning, but perhaps we
  # could throw a meaningful warning in the future
  expect_silent(filter_route_id(ggl_gtfs, ggl_routes))
})

# TODO: add tests with berlin's gtfs to make sure the correct agencies are being
# kept
# TODO: add tests to make sure the subsets are conducted correctly

test_that("filter_route_id - expected behavior", {

  # check dimensions
  subset1 <- filter_route_id(
    spo_gtfs,
    route_id = c("6450-51", "CPTM L11"),
    keep=TRUE
  )
  expect_equal(nrow(subset1$routes), 2)
  expect_equal(nrow(subset1$trips), 3)
  expect_equal(nrow(subset1$shapes), 1654)

  # check dimensions
  subset2 <- filter_route_id(spo_gtfs, route_id=c("6450-51", "CPTM L11"), keep=FALSE)
  expect_equal(nrow(subset2$routes), 17)
  expect_equal(nrow(subset2$trips), 33)
  expect_equal(nrow(subset2$shapes), 10641)

  })
