path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(path)

context("Filter by day period: overall usage")


# frequencies filtering ---------------------------------------------------

context("Filter by day period: filter frequencies")

path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(path)

tester <- function(gtfs = get("gtfs", envir = parent.frame()),
                   from = "03:30:00",
                   to = "06:30:00",
                   keep = TRUE) {
  from_secs <- string_to_seconds(from)
  to_secs <- string_to_seconds(to)
  gtfstools:::filter_frequencies(gtfs, from_secs, to_secs, keep)[]
}

test_that("doesn't change specified gtfs", {
  original_gtfs <- read_gtfs(path)
  expect_identical(gtfs, original_gtfs)

  result <- tester()

  expect_identical(gtfs, original_gtfs)

  # should also work if gtfs previously contained time-in-seconds columns (i.e.
  # these columns should not be removed if they previously existed)

  secs_gtfs <- read_gtfs(path)
  secs_gtfs$frequencies[
    ,
    `:=`(
      start_time_secs = string_to_seconds(start_time),
      end_time_secs = string_to_seconds(end_time)
    )
  ]
  original_gtfs$frequencies <- data.table::copy(secs_gtfs$frequencies)
  expect_identical(secs_gtfs, original_gtfs)
  result <- tester(secs_gtfs)
  expect_identical(secs_gtfs, original_gtfs)
})

test_that("output doesn't contain auxiliary columns", {
  result <- tester()
  expect_true(
    !any(c("from_within", "to_within", "within_from_to") %chin% names(result))
  )
})

test_that("filters frequencies correctly", {
  frequencies <- gtfs$frequencies

  expect_identical(tester(), frequencies[1])
  expect_identical(tester(from = "05:00:00"), frequencies[1])
  expect_identical(
    tester(from = "06:00:00", to = "17:00:00"),
    frequencies[c(1, 2)]
  )
  expect_identical(tester(from = "05:30:00", to = "28:00:00"), frequencies)

  expect_identical(tester(keep = FALSE), frequencies[c(2, 3)])
  expect_identical(
    tester(from = "05:00:00", keep = FALSE),
    frequencies[c(2, 3)]
  )
  expect_identical(
    tester(from = "06:00:00", to = "17:00:00", keep = FALSE),
    frequencies
  )
  expect_identical(
    tester(from = "05:30:00", to = "28:00:00", keep = FALSE),
    frequencies[0]
  )
})
