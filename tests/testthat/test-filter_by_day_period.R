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
                   keep = TRUE,
                   update_frequencies = FALSE) {
  from_secs <- string_to_seconds(from)
  to_secs <- string_to_seconds(to)
  gtfstools:::filter_frequencies(
    gtfs,
    from_secs,
    to_secs,
    keep,
    update_frequencies
  )[]
}

test_that("doesn't change specified gtfs", {
  original_gtfs <- read_gtfs(path)
  expect_identical(gtfs, original_gtfs)

  result <- tester()
  expect_identical(gtfs, original_gtfs)

  # should also work when update_frequencies = TRUE

  result <- tester(update_frequencies = TRUE)
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
  aux_columns <- c(
    "from_within",
    "to_within",
    "within_from_to",
    "is_duplicated"
  )
  result <- tester()
  expect_true(
    !any(aux_columns %chin% names(result))
  )

  result <- tester(keep = FALSE, update_frequencies = TRUE)
  expect_true(
    !any(aux_columns %chin% names(result))
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

  # the behaviour is a bit different with keep = FALSE
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

test_that("updates frequencies correctly when exact_times doesn't exist", {
  expect_equal(
    tester(to = "06:29:00", update_frequencies = TRUE)$end_time,
    "06:29:00"
  )
  expect_equal(
    tester(from = "05:31:00", update_frequencies = TRUE)$start_time,
    "05:31:00"
  )

  result <- tester(
    from = "05:31:00",
    to = "07:29:00",
    update_frequencies = TRUE
  )
  expect_equal(result$start_time, c("05:31:00", "06:30:00"))
  expect_equal(result$end_time, c("06:30:00", "07:29:00"))

  # the behaviour is a bit different with keep = FALSE
  result <- tester(
    from = "05:31:00",
    to = "07:29:00",
    update_frequencies = TRUE,
    keep = FALSE
  )
  expect_equal(result$start_time, c("05:30:00", "07:29:00", "20:30:00"))
  expect_equal(result$end_time, c("05:31:00", "20:30:00", "28:00:00"))

  result <- tester(
    from = "05:35:00",
    to = "05:45:00",
    update_frequencies = TRUE,
    keep = FALSE
  )
  expect_equal(result[c(1, 4)]$start_time, c("05:30:00", "05:45:00"))
  expect_equal(result[c(1, 4)]$end_time, c("05:35:00", "06:30:00"))
})

test_that("updates frequencies correctly when exact_times exists", {
  # when exact_times = 0 the behaviour should be identical to when it doesn't
  # exist
  gtfs$frequencies[, exact_times := 0]

  expect_equal(
    tester(to = "06:29:00", update_frequencies = TRUE)$end_time,
    "06:29:00"
  )
  expect_equal(
    tester(from = "05:31:00", update_frequencies = TRUE)$start_time,
    "05:31:00"
  )

  result <- tester(
    from = "05:31:00",
    to = "07:29:00",
    update_frequencies = TRUE
  )
  expect_equal(result$start_time, c("05:31:00", "06:30:00"))
  expect_equal(result$end_time, c("06:30:00", "07:29:00"))

  result <- tester(
    from = "05:31:00",
    to = "07:29:00",
    update_frequencies = TRUE,
    keep = FALSE
  )
  expect_equal(result$start_time, c("05:30:00", "07:29:00", "20:30:00"))
  expect_equal(result$end_time, c("05:31:00", "20:30:00", "28:00:00"))

  result <- tester(
    from = "05:35:00",
    to = "05:45:00",
    update_frequencies = TRUE,
    keep = FALSE
  )
  expect_equal(result[c(1, 4)]$start_time, c("05:30:00", "05:45:00"))
  expect_equal(result[c(1, 4)]$end_time, c("05:35:00", "06:30:00"))

  # when exact_times = 1, start_time should be adjusted according to the headway
  gtfs$frequencies[, exact_times := 1]

  expect_equal(
    tester(to = "06:29:00", update_frequencies = TRUE)$end_time,
    "06:29:00"
  )
  expect_equal(
    tester(from = "05:31:00", update_frequencies = TRUE)$start_time,
    "05:35:00"
  )

  result <- tester(
    from = "05:31:00",
    to = "07:29:00",
    update_frequencies = TRUE
  )
  expect_equal(result$start_time, c("05:35:00", "06:30:00"))
  expect_equal(result$end_time, c("06:30:00", "07:29:00"))

  result <- tester(
    from = "05:31:00",
    to = "07:29:00",
    update_frequencies = TRUE,
    keep = FALSE
  )
  expect_equal(result$start_time, c("05:30:00", "07:30:00", "20:30:00"))
  expect_equal(result$end_time, c("05:31:00", "20:30:00", "28:00:00"))

  result <- tester(
    from = "05:36:00",
    to = "05:46:00",
    update_frequencies = TRUE,
    keep = FALSE
  )
  expect_equal(result[c(1, 4)]$start_time, c("05:30:00", "05:50:00"))
  expect_equal(result[c(1, 4)]$end_time, c("05:36:00", "06:30:00"))

  gtfs$frequencies[, exact_times := NULL]
})

test_that("drops entries where start_time > end_time when exact_times = 1", {
  gtfs$frequencies[, exact_times := 1]

  result <- tester(
    from = "05:31:00",
    to = "05:34:00",
    update_frequencies = TRUE
  )
  expect_true(nrow(result) == 0)
})
