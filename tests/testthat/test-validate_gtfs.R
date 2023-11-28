testthat::skip_if_offline() # calls skip_on_cran()

available_versions <- c(
  "latest",
  "4.2.0",
  "4.1.0",
  "4.0.0",
  "3.1.1",
  "3.1.0",
  "3.0.1",
  "3.0.0"
)

data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs_url <- "https://github.com/ipeaGIT/gtfstools/raw/master/inst/extdata/spo_gtfs.zip"
gtfs <- read_gtfs(data_path, encoding = "UTF-8")
gtfsio_gtfs <- gtfsio::import_gtfs(data_path, encoding = "UTF-8")
gtfs_dir <- tempfile("gtfs")

write_gtfs(gtfs, gtfs_dir, as_dir = TRUE)

output_dir <- tempfile("validate_gtfs_tests")
validator <- download_validator(tempdir())

tester <- function(gtfs = data_path,
                   output_path = output_dir,
                   validator_path = validator,
                   overwrite = TRUE,
                   html_preview = TRUE,
                   pretty_json = FALSE,
                   quiet = TRUE,
                   n_threads = 1) {
  validate_gtfs(
    gtfs,
    output_path,
    validator_path,
    overwrite,
    html_preview,
    pretty_json,
    quiet,
    n_threads
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

  expect_error(tester(n_threads = "1"))
  expect_error(tester(n_threads = 0))
  expect_error(tester(n_threads = Inf))
  expect_error(tester(n_threads = c(1, 1)))
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

validation_works <- function(input,
                             validator_version = "latest",
                             pretty_json = FALSE) {
  validation_dir <- tempfile(paste0("validator_", validator_version))
  validator_path <- download_validator(tempdir(), validator_version)
  validator_numeric_version <- gtfstools:::parse_validator_version(
    validator_path
  )

  validation_result <- tester(
    input,
    validation_dir,
    validator_path = validator_path,
    html_preview = FALSE,
    pretty_json = pretty_json
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

  return(invisible(validation_result))
}

get_result_json <- function(validation_dir) {
  json_report_path <- file.path(validation_dir, "report.json")
  json_report <- jsonlite::fromJSON(json_report_path)

  # validator v4.2.0 introduces three summary fields that vary based on the
  # validation time, output directory and input file. we remove these just to
  # make sure we are comparing the actual validation content

  json_report$summary$validatedAt <- NULL
  json_report$summary$gtfsInput <- NULL
  json_report$summary$outputDirectory <- NULL

  return(json_report)
}

test_that("works with the 4 types of input (url, path, dir, object)", {
  obj_dir <- validation_works(gtfs)
  gtfsio_obj_dir <- validation_works(gtfsio_gtfs)
  path_dir <- validation_works(data_path)
  url_dir <- validation_works(gtfs_url)
  dir_dir <- validation_works(gtfs_dir)

  # and their validation report should be the same
  # (results from dir are in a different order)

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    obj_result <- get_result_json(obj_dir)
    gtfsio_obj_result <- get_result_json(gtfsio_obj_dir)
    path_result <- get_result_json(path_dir)
    url_result <- get_result_json(url_dir)
    dir_result <- get_result_json(dir_dir)

    expect_identical(obj_result, gtfsio_obj_result)
    expect_identical(obj_result, path_result)
    expect_identical(obj_result, url_result)

    ordered_results <- function(result) {
      notices <- result$notices$sampleNotices
      notices <- lapply(notices, data.table::setDT)
      notices[[1]] <- notices[[1]][order(filename)]
      notices[[4]] <- notices[[4]][order(filename)]
    }
    expect_identical(ordered_results(obj_result), ordered_results(dir_result))
  }
})

test_that("all versions of the validation work", {
  for (version in available_versions) {
    validation_works(data_path, version)
  }
})

test_that("pretty_json works with all functions and results are the same", {
  pretty_results <- vapply(
    available_versions,
    function(v) validation_works(data_path, v, pretty_json = TRUE),
    character(1)
  )

  non_pretty_results <- vapply(
    available_versions,
    function(v) validation_works(data_path, v, pretty_json = FALSE),
    character(1)
  )

  json_as_character_length <- function(validation_dir) {
    json_path <- file.path(validation_dir, "report.json")
    json_content <- readLines(json_path)
    json_length <- length(json_content)
    return(json_length)
  }

  suppressWarnings({
    pretty_jsons <- vapply(pretty_results, json_as_character_length, integer(1))
    non_pretty_jsons <- vapply(
      non_pretty_results,
      json_as_character_length,
      integer(1)
    )
  })

  expect_true(all(pretty_jsons > 1))
  expect_true(all(non_pretty_jsons == 1))

  # their content should be identical though

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    pretty_jsons_parsed <- lapply(pretty_results, get_result_json)
    non_pretty_jsons_parsed <- lapply(non_pretty_results, get_result_json)

    expect_identical(pretty_jsons_parsed, non_pretty_jsons_parsed)
  }
})

test_that("quiet arg works correctly", {
  expect_silent(tester(gtfs, quiet = TRUE, html_preview = FALSE))
  expect_silent(tester(data_path, quiet = TRUE, html_preview = FALSE))
  expect_silent(tester(gtfs_url, quiet = TRUE, html_preview = FALSE))
  expect_silent(tester(gtfs_dir, quiet = TRUE, html_preview = FALSE))

  expect_message(tester(gtfs, quiet = FALSE, html_preview = FALSE))
  expect_message(tester(data_path, quiet = FALSE, html_preview = FALSE))
  capture.output(
    expect_message(tester(gtfs_url, quiet = FALSE, html_preview = FALSE)),
    type = "message"
  )
  expect_message(tester(gtfs_dir, quiet = FALSE, html_preview = FALSE))
})

test_that("n_threads arg works correctly", {
  get_used_threads <- function(result, version) {
    relevant_file <- file.path(result, "validation_stderr.txt")

    running_info <- readLines(relevant_file)
    thread_info <- running_info[grepl("thread", running_info)]

    info_pos <- regexpr("\\d", thread_info)
    n_threads <- as.integer(substring(thread_info, info_pos, info_pos))

    return(n_threads)
  }

  for (version in available_versions) {
    validator_path <- download_validator(tempdir(), version = version)
    result <- tester(
      validator_path = validator_path,
      n_threads = 1,
      html_preview = FALSE
    )
    expect_equal(get_used_threads(result), 1)
  }

  for (version in available_versions) {
    validator_path <- download_validator(tempdir(), version = version)
    result <- tester(
      validator_path = validator_path,
      n_threads = 2,
      html_preview = FALSE
    )
    expect_equal(get_used_threads(result), 2)
  }
})
