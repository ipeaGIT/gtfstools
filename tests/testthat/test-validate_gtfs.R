testthat::skip_if_offline() # calls skip_on_cran()

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs_url <- "https://github.com/ipeaGIT/gtfstools/raw/master/inst/extdata/spo_gtfs.zip"
gtfs <- read_gtfs(data_path)
trip_id <- "CPTM L07-0"
output_dir <- tempfile("validate_gtfs_tests")
validator_dir <- tempfile()
dir.create(validator_dir)
validator <- download_validator(validator_dir)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   output_path = output_dir,
                   validator_path = validator,
                   overwrite = TRUE,
                   html_preview = TRUE,
                   quiet = TRUE) {
  validate_gtfs(
    gtfs,
    output_path,
    validator_path,
    overwrite,
    html_preview,
    quiet
  )
}

test_that("raises error due to incorrect input", {
  invalid_file <- tempfile()
  file.create(invalid_file)

  expect_error(tester(gtfs = unclass(gtfs)))
  expect_error(tester(gtfs = invalid_file))
  expect_error(tester(gtfs = dirname(gtfs_url)))
  expect_error(tester(gtfs = "hehe"))

  expect_error(tester(output_path = 1))
  expect_error(tester(output_path = "oi/ola"))
  expect_error(tester(output_path = c("oi", "oi")))

  expect_error(tester(validator_path = 1))
  expect_error(tester(validator_path = c("R", "R")))
  expect_error(tester(validator_path = tempdir()))
  expect_error(tester(validator_path = invalid_file))

  expect_error(tester(overwrite = 1))
  expect_error(tester(overwrite = c(TRUE, TRUE)))
  expect_error(tester(overwrite = NA))

  expect_error(tester(html_preview = 1))
  expect_error(tester(html_preview = c(TRUE, TRUE)))
  expect_error(tester(html_preview = NA))

  expect_error(tester(quiet = 1))
  expect_error(tester(quiet = c(TRUE, TRUE)))
  expect_error(tester(quiet = NA))
})

test_that("doesn't overwrite existing results with overwrite = FALSE", {
  tmpdir <- tempdir()
  create_test_delete <- function(file) {
    file_path <- file.path(tmpdir, file)
    file.create(file_path)
    expect_error(tester(output_path = tmpdir, overwrite = FALSE))
    file.remove(file_path)
  }
  create_test_delete("report.html")
  create_test_delete("report.json")
  create_test_delete("system_errors.json")
  create_test_delete("validation_stdout.txt")
  create_test_delete("validation_stderr.txt")
})
