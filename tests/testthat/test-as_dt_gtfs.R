data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfstools_gtfs <- read_gtfs(data_path)

dt_gtfs_class <- c("dt_gtfs", "gtfs", "list")

# as_dt_gtfs.gtfs() -------------------------------------------------------

test_that("works correctly for gtfs-classed objects", {
  gtfs <- gtfsio::import_gtfs(data_path)

  converted_gtfs <- as_dt_gtfs(gtfs)

  expect_identical(converted_gtfs, gtfstools_gtfs)
})

test_that("doesn't change original gtfs-classed object", {
  gtfs <- gtfsio::import_gtfs(data_path)
  original_gtfs <- gtfsio::import_gtfs(data_path)

  converted_gtfs <- as_dt_gtfs(gtfs)

  expect_identical(class(converted_gtfs), dt_gtfs_class)
  expect_identical(gtfs, original_gtfs)
})

# as_dt_gtfs.tidygtfs() ---------------------------------------------------

# simulating a tidygtfs without depending on tidytransit. times will be
# converted to seconds after midnight (which is basically how {hms} handles them
# internally) and data.tables will be converted to data.frames

tidygtfs <- read_gtfs(data_path)
tidygtfs$stop_times[
  ,
  `:=`(
    arrival_time = string_to_seconds(arrival_time),
    departure_time = string_to_seconds(departure_time)
  )
]
tidygtfs$frequencies[
  ,
  `:=`(
    start_time = string_to_seconds(start_time),
    end_time = string_to_seconds(end_time)
  )
]
invisible(lapply(tidygtfs, data.table::setDF))
tidygtfs$. <- list(internal_table = data.frame(a = 1:2, b = 2:3))
class(tidygtfs) <- c("tidygtfs", "gtfs")

test_that("doesn't change original tidygtfs-classed objects", {
  original_gtfs <- tidygtfs

  converted_gtfs <- as_dt_gtfs(tidygtfs)

  expect_identical(original_gtfs, tidygtfs)
})

test_that("works correctly for tidygtfs-classed objects", {
  gtfstools_like_gtfs <- gtfstools_gtfs
  gtfstools_like_gtfs$. <- list(
    internal_table = data.table::data.table(a = 1:2, b = 2:3)
  )

  converted_gtfs <- as_dt_gtfs(tidygtfs)

  expect_identical(class(converted_gtfs), dt_gtfs_class)
  expect_identical(converted_gtfs, gtfstools_like_gtfs)
})

test_that("converts sf shapes in tidygtfs-classed objects, if present", {
  sf_shapes <- convert_shapes_to_sf(
    gtfstools_gtfs,
    shape_id = c("17846", "17847")
  )

  tidygtfs_sf <- tidygtfs
  tidygtfs_sf$shapes <- sf_shapes

  converted_gtfs <- as_dt_gtfs(tidygtfs_sf)

  expect_s3_class(converted_gtfs$shapes, "data.table")
  expect_true(!is.null(converted_gtfs$shapes$shape_pt_lon))
  expect_true(!is.null(converted_gtfs$shapes$shape_pt_lat))
  expect_true(!is.null(converted_gtfs$shapes$shape_pt_sequence))
})

# as_dt_gtfs.list() -------------------------------------------------------

# list elements are converted to data.table, but no changes are made to the
# elements' columns, as we don't have any prior information on how they are
# formatted

list_gtfs <- unclass(tidygtfs)

test_that("works correctly for lists", {
  converted_gtfs <- as_dt_gtfs(list_gtfs)

  obj_names <- names(converted_gtfs)
  no_dot_names <- setdiff(obj_names, ".")

  invisible(
    lapply(
      converted_gtfs[no_dot_names],
      function(i) expect_s3_class(i, "data.table")
    )
  )
  invisible(
    lapply(
      converted_gtfs$.,
      function(i) expect_s3_class(i, "data.table")
    )
  )

  expect_identical(class(converted_gtfs), dt_gtfs_class)

  # time columns should remain as integers, since they weren't converted

  expect_type(converted_gtfs$stop_times$departure_time, "integer")
  expect_type(converted_gtfs$stop_times$arrival_time, "integer")
  expect_type(converted_gtfs$frequencies$start_time, "integer")
  expect_type(converted_gtfs$frequencies$end_time, "integer")
})
