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
                   pretty_json = FALSE,
                   quiet = TRUE) {
  validate_gtfs(
    gtfs,
    output_path,
    validator_path,
    overwrite,
    html_preview,
    pretty_json,
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

  expect_error(tester(pretty_json = 1))
  expect_error(tester(pretty_json = c(TRUE, TRUE)))
  expect_error(tester(pretty_json = NA))

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

test_that("errors if validator_basename is not gtfs-validator-vX.Y.Z.jar", {
  invalid_validator_path <- sub("\\.jar$", "", validator)
  expect_error(tester(validator_path = invalid_validator_path))
})

validation_works <- function(input, validator_version = "latest") {
  validation_dir <- tempfile(paste0("validator_", validator_version))
  validator_path <- download_validator(tempdir(), validator_version)
  validator_numeric_version <- gtfstools:::parse_validator_version(
    validator_path
  )

  validation_result <- tester(
    input,
    validation_dir,
    validator_path = validator_path,
    html_preview = FALSE
  )

  if (validator_numeric_version >= numeric_version("3.1.0")) {
    expect_true(file.exists(file.path(validation_dir, "report.html")))
  }

  if (validator_numeric_version < numeric_version("3.1.0")) {
    expect_true(file.exists(file.path(validation_dir, "validation_stdout.txt")))
  }

  expect_true(file.exists(file.path(validation_dir, "report.json")))
  expect_true(file.exists(file.path(validation_dir, "system_errors.json")))
  expect_true(file.exists(file.path(validation_dir, "validation_stderr.txt")))
  expect_identical(validation_result, normalizePath(validation_dir))

  return(invisible(TRUE))
}

test_that("validation works with the 3 types of input (url, path, object)", {
  validation_works(gtfs)
  validation_works(data_path)
  validation_works(gtfs_url)
})

test_that("all versions of the validation work", {
  available_versions <- c(
    "latest",
    "3.1.0",
    "3.0.1",
    "3.0.0"
  )

  for (version in available_versions) {
    expect_true(validation_works(data_path, version))
  }
})
