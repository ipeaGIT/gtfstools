available_versions <- c(
  "latest",
  "3.1.0",
  "3.0.1",
  "3.0.0"
)
tmpdir <- tempfile("gtfsvalidator")
dir.create(tmpdir)

tester <- function(path = tmpdir,
                   version = "latest",
                   force = FALSE,
                   quiet = TRUE) {
  download_validator(path, version, force, quiet)
}

test_that("raises error due to incorrect input types", {
  expect_error(tester(path = "non_existent_dir"))
  expect_error(tester(path = 1))
  expect_error(tester(path = c(tmpdir, tmpdir)))

  expect_error(tester(version = 3))
  expect_error(tester(version = c("latest", "latest")))
  expect_error(tester(version = "a"))

  expect_error(tester(force = 1))
  expect_error(tester(force = c(TRUE, TRUE)))
  expect_error(tester(force = NA))

  expect_error(tester(quiet = 1))
  expect_error(tester(quiet = c(TRUE, TRUE)))
  expect_error(tester(quiet = NA))
})

testthat::skip_if_offline() # calls skip_on_cran()

test_that("validator is saved to path", {
  expect_identical(list.files(tmpdir), character(0))
  tester()
  expect_true(length(list.files(tmpdir)) == 1)
})

test_that("correct version is downloaded", {
  # latest defaults to highest version, else downloads specified version
  tester(version = "latest")
  expect_true(grepl("gtfs-validator-v3\\.1\\.0\\.jar$", list.files(tmpdir)))

  tester(version = "3.0.0")
  expect_true(
    any(grepl("gtfs-validator-v3\\.0\\.0\\.jar$", list.files(tmpdir)))
  )
})

test_that("returns normalized path to validator", {
  normalized_path <- tester(version = "3.1.0")
  manually_built_path <- file.path(tmpdir, "gtfs-validator-v3.1.0.jar")
  expect_identical(normalized_path, normalizePath(manually_built_path))
})

test_that("force argument works", {
  download_time <- system.time(tester(force = TRUE))
  cache_time <- system.time(tester())
  expect_true(download_time["elapsed"] > cache_time["elapsed"])
})

test_that("quiet argument is respected", {
  # first when not hitting the cache

  expect_silent(tester(force = TRUE))

  download_messages <- tempfile("download_messages", fileext = ".txt")
  expect_message(
    utils::capture.output(
      tester(force = TRUE, quiet = FALSE),
      file = download_messages,
      type = "message"
    ),
    label = "Downloading https:\\/\\/.*\\.jar to .*\\.jar\\."
  )

  curl_messages <- readLines(download_messages)
  expect_true(any(grepl("\\[\\d*%\\] Downloaded \\d* bytes", curl_messages)))

  # and then when using cached validator

  expect_silent(tester())
  expect_message(
    tester(quiet = FALSE),
    label = "^Using previously downloaded validator"
  )
})

test_that("all versions can be downloaded", {
  paths <- vapply(
    available_versions,
    function(v) tester(tempdir(), version = v),
    character(1)
  )
  expect_true(paths["latest"] == paths["3.1.0"])

  non_latest <- paths[-1]
  manually_built <- file.path(
    tempdir(),
    paste0("gtfs-validator-v", names(non_latest), ".jar")
  )
  expect_true(all(manually_built == non_latest))
})
