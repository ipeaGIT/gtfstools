context("Merge GTFS")


# setup -------------------------------------------------------------------


spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")

spo_gtfs <- read_gtfs(spo_path)
ggl_gtfs <- read_gtfs(ggl_path)


# tests -------------------------------------------------------------------


test_that("merge_gtfs raises errors due to incorrect input types", {
  expect_error(merge_gtfs("spo_gtfs", ggl_gtfs))
  expect_error(merge_gtfs(spo_gtfs, "ggl_gtfs"))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, files = 1))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, quiet = "TRUE"))
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, warnings = "TRUE"))
})

test_that("merge_gtfs raises errors/warnings due to unavailable files passed to 'files'", {

  # should throw a warning if a specified file doesn't exist
  expect_warning(
    merge_gtfs(spo_gtfs, ggl_gtfs, files = c("shapes", "ola", "oie"))
  )

  # but should not throw a warning if warnings = FALSE
  expect_silent(
    merge_gtfs(
      spo_gtfs,
      ggl_gtfs,
      files = c("shapes", "ola", "oie"),
      warnings = FALSE
    )
  )

  # should throw an error when none of the specified files exist
  expect_error(merge_gtfs(spo_gtfs, ggl_gtfs, files = c("ola", "oie")))

})

test_that("merge_gtfs raises messages adequately", {
  expect_silent(merge_gtfs(spo_gtfs, ggl_gtfs))
  expect_message(merge_gtfs(spo_gtfs, ggl_gtfs, quiet = FALSE))
})

test_that("merge_gtfs results in a GTFS object", {

  # should work when files = NULL
  expect_s3_class(merge_gtfs(spo_gtfs, ggl_gtfs), "dt_gtfs")
  expect_s3_class(merge_gtfs(list(spo_gtfs, ggl_gtfs)), "dt_gtfs")

  # and should also work when files = something else
  expect_s3_class(merge_gtfs(spo_gtfs, ggl_gtfs, files = "shapes"), "dt_gtfs")
  expect_s3_class(
    merge_gtfs(list(spo_gtfs, ggl_gtfs), files = "shapes"),
    "dt_gtfs"
  )

  # even if a non-existent file is passed to 'files' (but not all of them)
  expect_warning(
    merged <- merge_gtfs(spo_gtfs, ggl_gtfs, files = c("shapes", "ola", "oie"))
  )
  expect_s3_class(merged, "dt_gtfs")

})

test_that("merge_gtfs results in a validated GTFS object (and only correct files are validated)", {

  # all files are validated if files = NULL

  all_possible_files <- gsub(".txt", "", names(gtfs_metadata))

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)
  expect_true("validation_result" %in% names(attributes(merged_gtfs)))

  validated_files <- unique(attr(merged_gtfs, "validation_result")$file)

  expect_identical(all_possible_files, validated_files)

  # only selected files are validated if files = something else

  merged_gtfs_small <- merge_gtfs(spo_gtfs, ggl_gtfs, files = "shapes")

  validated_files <- unique(attr(merged_gtfs_small, "validation_result")$file)

  expect_identical(validated_files, "shapes")

})

test_that("merge_gtfs merges the adequate 'files'", {

  # when files = NULL all files from all gtfs should be merged

  spo_names <- names(spo_gtfs)
  ggl_names <- names(ggl_gtfs)
  all_names <- c(spo_names, ggl_names)
  all_names <- unique(all_names[order(all_names)])

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)
  merged_gtfs_names <- names(merged_gtfs)[order(names(merged_gtfs))]

  expect_identical(all_names, merged_gtfs_names)

  # should also work if GTFS objects are passed as list

  merged_gtfs_list <- merge_gtfs(list(spo_gtfs, ggl_gtfs))
  merged_gtfs_lnames <- names(merged_gtfs_list)[order(names(merged_gtfs_list))]

  expect_identical(all_names, merged_gtfs_lnames)

  # when files = something else, only the specified files should be merged

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, files = "shapes")
  expect_identical(names(merged_gtfs), "shapes")

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, files = c("shapes", "stops"))
  expect_identical(names(merged_gtfs), c("shapes", "stops"))

  # should work even if a non-existant file is passed to 'files'

  expect_warning(
    merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs, files = c("shapes", "oie"))
  )
  expect_identical(names(merged_gtfs), "shapes")

})

test_that("merge_gtfs bind the rows of each GTFS object adequately", {

  merged_gtfs       <- merge_gtfs(spo_gtfs, ggl_gtfs)
  merged_gtfs_names <- names(merged_gtfs)

  # each data.table in the final GTFS object should be the "sum" of the
  # data.tables of same name in the original GTFS objs

  for (filename in merged_gtfs_names){

    expect_equal(
      nrow(merged_gtfs[[filename]]),
      nrow(rbind(spo_gtfs[[filename]], ggl_gtfs[[filename]], fill = TRUE))
    )

  }

  # but rbind.data.table() fill = TRUE results in some character columns with a
  # NA_character_ where a "" is expected
  # test if after replacing these values the data.tables are identical

  for (filename in merged_gtfs_names){

    merged_by_hand <- rbind(
      spo_gtfs[[filename]],
      ggl_gtfs[[filename]],
      fill = TRUE
    )
    col_classes    <- vapply(merged_by_hand, class, character(1))
    is_char        <- which(col_classes == "character")

    for (col in is_char) {

      data.table::set(
        merged_by_hand,
        i = which(is.na(merged_by_hand[[col]])),
        j = col,
        value = ""
      )

    }

    expect_equal(
      nrow(merged_gtfs[[filename]]),
      nrow(merged_by_hand)
    )

  }

})

test_that("merge_gtfs does not change the original GTFS objects", {

  spo_gtfs <- original_spo_gtfs <- read_gtfs(spo_path)
  ggl_gtfs <- original_ggl_gtfs <- read_gtfs(ggl_path)

  merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)

  expect_identical(spo_gtfs, original_spo_gtfs)
  expect_identical(ggl_gtfs, original_ggl_gtfs)

})
