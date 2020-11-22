context("Validate GTFS")


# setup -------------------------------------------------------------------


data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
gtfs <- read_gtfs(data_path)

full_validation <- validate_gtfs(gtfs)
partial_validation_1 <- validate_gtfs(gtfs, "stop_times")
partial_validation_2 <- validate_gtfs(gtfs, c("stop_times", "agency"))

extra_file_gtfs <- gtfs
extra_file_gtfs$extra_file <- extra_file_gtfs$calendar
extra_file_validation <- validate_gtfs(extra_file_gtfs)

extra_field_gtfs <- gtfs
extra_field_gtfs$calendar <- data.table::copy(gtfs$calendar)
extra_field_gtfs$calendar[, extra_field := "ola"]
extra_field_validation <- validate_gtfs(extra_field_gtfs)

specified_files <- c(
  "agency", "stops", "routes", "trips", "stop_times", "calendar",
  "calendar_dates", "fare_attributes", "fare_rules", "shapes", "frequencies",
  "transfers", "pathways", "levels", "feed_info", "translations", "attributions"
)

required_files <- c(
  "agency", "stops", "routes", "trips", "stop_times", "calendar"
)

# files' fields

agency_field <- c(
  "agency_id", "agency_name", "agency_url", "agency_timezone", "agency_lang",
  "agency_phone", "agency_fare_url", "agency_email"
)

stops_field <- c(
  "stop_id", "stop_code", "stop_name", "stop_desc", "stop_lat", "stop_lon",
  "zone_id", "stop_url", "location_type", "parent_station", "stop_timezone",
  "wheelchair_boarding", "level_id", "platform_code"
)

routes_field <- c(
  "route_id", "agency_id", "route_short_name", "route_long_name", "route_desc",
  "route_type", "route_url", "route_color", "route_text_color",
  "route_sort_order", "continuous_pickup", "continuous_drop_off"
)

trips_field <- c(
  "route_id", "service_id", "trip_id", "trip_headsign", "trip_short_name",
  "direction_id", "block_id", "shape_id", "wheelchair_accessible",
  "bikes_allowed"
)

stop_times_field <- c(
  "trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence",
  "stop_headsign", "pickup_type", "drop_off_type", "continuous_pickup",
  "continuous_drop_off", "shape_dist_traveled", "timepoint"
)

calendar_field <- c(
  "service_id", "monday", "tuesday", "wednesday", "thursday", "friday",
  "saturday", "sunday", "start_date", "end_date"
)

calendar_dates_field <- c("service_id", "date", "exception_type")

fare_attributes_field <- c(
  "agency_id", "fare_id", "price", "currency_type", "payment_method",
  "transfers", "transfer_duration"
)

fare_rules_field <- c(
  "fare_id", "route_id", "origin_id", "destination_id", "contains_id"
)

shapes_field <- c(
  "shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence",
  "shape_dist_traveled"
)

frequencies_field <- c(
  "trip_id", "start_time", "end_time", "headway_secs", "exact_times"
)

transfers_field <- c(
  "from_stop_id", "to_stop_id", "transfer_type", "min_transfer_time"
)

pathways_field <- c(
  "pathway_id", "from_stop_id", "to_stop_id", "pathway_mode", "is_bidirectional",
  "length", "traversal_time", "stair_count", "max_slope", "min_width",
  "signposted_as", "reversed_signposted_as"
)

levels_field <- c("level_id", "level_index", "level_name")

feed_info_field <- c(
  "feed_publisher_name", "feed_publisher_url", "feed_lang", "feed_start_date",
  "feed_end_date", "feed_version", "feed_contact_email", "feed_contact_url"
)

translations_field <- c(
  "table_name", "field_name", "language", "translation", "record_id",
  "record_sub_id", "field_value"
)

attributions_field <- c(
  "attribution_id", "agency_id", "route_id", "trip_id", "organization_name",
  "is_producer", "is_operator", "is_authority", "attribution_url",
  "attribution_email", "attribution_phone"
)


# tests -------------------------------------------------------------------


test_that("validate_gtfs raises errors due to incorrect input types", {

  no_class_gtfs <- gtfs
  attr(no_class_gtfs, "class") <- NULL

  expect_error(validate_gtfs(no_class_gtfs))
  expect_error(validate_gtfs(gtfs, files = NA))
  expect_error(validate_gtfs(gtfs, files = as.factor("stop_times")))
  expect_error(validate_gtfs(gtfs, quiet = "TRUE"))
  expect_error(validate_gtfs(gtfs, warnings = "TRUE"))

})

test_that("validate_gtfs raises error due to non-existent/mistyped supplied file in gtfs", {
  expect_error(validate_gtfs(gtfs, files = "agency.txt"))
  expect_error(validate_gtfs(gtfs, files = "non-existent-file"))
})

test_that("validate_gtfs results in a data.table", {
  expect_s3_class(validate_gtfs(gtfs), "data.table")
})

test_that("validate_gtfs validates the correct files", {

  validated_files <- unique(full_validation$file)
  expect_equal(sum(validated_files %in% specified_files), 17)

  validated_files <- unique(partial_validation_1$file)
  expect_true(validated_files == "stop_times")

  validated_files <- unique(partial_validation_2$file)
  expect_equal(sum(validated_files %in% c("stop_times", "agency")), 2)

})

test_that("validate_gtfs validates all fields from desired files", {

  full_validation <- validate_gtfs(gtfs)
  validated_files <- unique(full_validation$file)
  invisible(lapply(
    validated_files,
    function (i) {
      supposed_fields <- get(paste0(i, "_field"))
      expect_equal(
        sum(full_validation[file == i]$field %in% supposed_fields),
        length(supposed_fields)
      )
    }
  ))

  partial_validation_1 <- validate_gtfs(gtfs, "stop_times")
  supposed_fields <- stop_times_field
  expect_equal(
    sum(partial_validation_1$field %in% supposed_fields),
    length(supposed_fields)
  )

  partial_validation_2 <- validate_gtfs(gtfs, c("stop_times", "agency"))
  validated_files <- unique(partial_validation_2$file)
  invisible(lapply(
    validated_files,
    function (i) {
      supposed_fields <- get(paste0(i, "_field"))
      expect_equal(
        sum(full_validation[file == i]$field %in% supposed_fields),
        length(supposed_fields)
      )
    }
  ))

})

test_that("validate_gtfs recognize extra files and fields as extra", {

  # extra file

  expect_equal(
    sum(extra_file_validation[file == "extra_file"]$file_spec == "ext"),
    length(extra_file_validation[file == "extra_file"]$field)
  )
  expect_equal(
    sum(extra_file_validation[file == "extra_file"]$field_spec == "ext"),
    length(extra_file_validation[file == "extra_file"]$field)
  )
  expect_equal(
    sum(extra_file_validation[file == "extra_file"]$file_provided_status == TRUE),
    length(extra_file_validation[file == "extra_file"]$field)
  )
  expect_equal(
    sum(extra_file_validation[file == "extra_file"]$field_provided_status == TRUE),
    length(extra_file_validation[file == "extra_file"]$field)
  )

  # extra field

  expect_equal(
    extra_field_validation[file == "calendar" & field == "extra_field"]$field_spec,
    "ext"
  )

})
