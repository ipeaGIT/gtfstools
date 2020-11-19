context("Read GTFS")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs_url  <- "https://github.com/ipeaGIT/gtfstools/raw/master/inst/extdata/spo_gtfs.zip"

gtfs <- read_gtfs(data_path)

# create gtfs with empty table

empty_table_gtfs <- gtfs
empty_table_gtfs$trips <- empty_table_gtfs$trips[trip_id == "def_no_trip"]
temp_file <- tempfile(pattern = "gtfs", fileext = ".zip")
on.exit(file.remove(temp_file))
write_gtfs(empty_table_gtfs, temp_file)
empty_table_gtfs <- read_gtfs(temp_file)

# create gtfs with empty file

temp_dir <- file.path(tempdir(), "test_gtfsdir")
on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
zip::unzip(temp_file, exdir = temp_dir, overwrite = TRUE)
file.remove(file.path(temp_dir, "trips.txt"))
file.create(file.path(temp_dir, "trips.txt"))
zip::zipr(temp_file, file.path(temp_dir, list.files(temp_dir)))
empty_file_gtfs <- read_gtfs(temp_file)


# tests -------------------------------------------------------------------


test_that("read_gtfs raises errors due to incorrect input types", {
  expect_error(read_gtfs(as.factor(data_path)))
  expect_error(read_gtfs(data_path, files = NA))
  expect_error(read_gtfs(data_path, files = as.factor("stop_times")))
  expect_error(read_gtfs(data_path, quiet = "TRUE"))
  expect_error(read_gtfs(data_path, warnings = "TRUE"))
})

test_that("read_gtfs results in a gtfs object", {

  # a gtfs object is a list with "gtfs" class

  expect_s3_class(gtfs, "gtfs")
  expect_s3_class(empty_table_gtfs, "gtfs")
  expect_s3_class(empty_file_gtfs, "gtfs")
  expect_type(gtfs, "list")
  expect_type(empty_table_gtfs, "list")
  expect_type(empty_file_gtfs, "list")

  # every object within list is a dt, even if it's originally an empty table/file

  invisible(lapply(gtfs, function(i) expect_s3_class(i, "data.table")))
  invisible(lapply(empty_table_gtfs, function(i) expect_s3_class(i, "data.table")))
  invisible(lapply(empty_file_gtfs, function(i) expect_s3_class(i, "data.table")))

})
