ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
ggl_gtfs <- read_gtfs(ggl_path)

test_that("filter_fare_rules_from_zone_id() works correctly", {
  test_gtfs <- ggl_gtfs
  test_gtfs$fare_rules <- data.table::copy(ggl_gtfs$fare_rules)

  test_gtfs$fare_rules <- rbind(
    test_gtfs$fare_rules,
    data.table::data.table(
      fare_id = "d",
      route_id = "GRT",
      origin_id = "",
      destination_id = "",
      contains_id = c("2", "4")
    )
  )

  test_gtfs$fare_rules <- rbind(
    test_gtfs$fare_rules,
    data.table::data.table(
      fare_id = "e",
      route_id = "GRT",
      origin_id = "",
      destination_id = "",
      contains_id = c("1", "2", "4")
    )
  )

  filtered_gtfs_keeping <- filter_fare_rules_from_zone_id(
    test_gtfs,
    c("2", "4"),
    `%chin%`
  )
  expect_identical(
    filtered_gtfs_keeping$fare_rules,
    data.table::data.table(
      fare_id = c("a", "a", "d", "d"),
      route_id = c("GRT", "GRJ", "GRT", "GRT"),
      origin_id = c("2", "4", "", ""),
      destination_id = c("4", "2", "", ""),
      contains_id = c("", "", "2", "4")
    )
  )

  filtered_gtfs_dropping <- filter_fare_rules_from_zone_id(
    test_gtfs,
    c("2", "4"),
    Negate(`%chin%`)
  )
  expect_identical(
    filtered_gtfs_dropping$fare_rules,
    data.table::data.table(
      fare_id = c(rep("a", 6), "b", "c"),
      route_id = c("TSW", "TSE", "GRT", "GRJ", "SVJ", "JSV", "GRT", "GRT"),
      origin_id = c(rep("1", 6), "3", ""),
      destination_id = c(rep("1", 6), "3", ""),
      contains_id = c(rep("", 7), "6")
    )
  )
})
