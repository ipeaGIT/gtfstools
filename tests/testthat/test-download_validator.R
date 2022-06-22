available_versions <- c(
  "latest",
  "3.1.0",
  "3.0.0",
  "2.0.0",
  "1.4.0",
  "1.3.1",
  "1.3.0",
  "1.2.1",
  "1.2.0",
  "1.1.0",
  "1.0.1",
  "1.0.0"
)
tmpdir <- tempfile("gtfsvalidator")
dir.create(tmpdir)

tester <- function(path = tmpdir, version = "latest", quiet = TRUE) {
  download_validator(path, version, quiet)
}

test_that("raises error due to incorrect input types", {
  expect_error(tester(path = "non_existent_dir"))
  expect_error(tester(path = 1))
  expect_error(tester(path = c(tmpdir, tmpdir)))

  expect_error(tester(version = 3))
  expect_error(tester(version = c("latest", "latest")))
  expect_error(tester(version = "a"))

  expect_error(tester(quiet = 1))
  expect_error(tester(quiet = c(TRUE, TRUE)))
  expect_error(tester(quiet = NA))
})

testthat::skip_if_offline() # calls skip_on_cran()

test_that("validator is saved to path", {
  expect_identical(list.files(tmpdir), character(0))
  tester()
  expect_true(length(list.files(tmpdir)) == 1)
  invisible(file.remove(list.files(tmpdir, full.names = TRUE)))
})

test_that("correct version is downloaded", {
  # latest defaults to highest version, else downloads specified version
  tester(version = "latest")
  expect_true(grepl("gtfs-validator-v3\\.1\\.0\\.jar$", list.files(tmpdir)))

  tester(version = "3.0.0")
  expect_true(
    any(grepl("gtfs-validator-v3\\.0\\.0\\.jar$", list.files(tmpdir)))
  )
  invisible(file.remove(list.files(tmpdir, full.names = TRUE)))
})

test_that("returns normalized path to validator", {
  normalized_path <- tester()

  manually_built_path <- file.path(tmpdir, list.files(tmpdir))
  expect_identical(normalized_path, normalizePath(manually_built_path))
})

test_that("quiet argument is respected", {
  expect_silent(tester())

  download_messages <- tempfile("download_messages", fileext = ".txt")
  expect_message(
    utils::capture.output(
      tester(quiet = FALSE),
      file = download_messages,
      type = "message"
    ),
    label = "Downloading https:\\/\\/.*\\.jar to .*\\.jar\\."
  )

  curl_messages <- readLines(download_messages)
  expect_true(any(grepl("\\[\\d*%\\] Downloaded \\d* bytes", curl_messages)))
})

test_that("all versions can be downloaded", {
  old_option <- options(GTFSTOOLS_PARSE_RESPONSE = FALSE)
  on.exit(options(old_option), add = TRUE)

  status_codes <- vapply(
    available_versions,
    function(v) tester(version = v),
    integer(1)
  )
  expect_true(all(status_codes == 200))
})
