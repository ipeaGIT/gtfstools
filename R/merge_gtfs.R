#' Merge GTFS files
#'
#' Combines many GTFS objects into a single one.
#'
#' @param ... GTFS objects to be merged. Each argument can either be a GTFS or
#' a list of GTFS objects.
#' @param files A character vector listing the GTFS tables to be merged. If
#' `NULL` (the default), all tables are merged.
#' @param warnings Whether to display warning messages (defaults to `TRUE`).
#' @param prefix Either a logical or a character vector (defaults to `FALSE`).
#'
#' @return A GTFS object in which each table is a combination (by row) of the
#' tables from the specified GTFS objects.
#'
#' @examples
#' spo_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' ggl_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
#'
#' spo_gtfs <- read_gtfs(spo_path)
#' names(spo_gtfs)
#'
#' ggl_gtfs <- read_gtfs(ggl_path)
#' names(ggl_gtfs)
#'
#' merged_gtfs <- merge_gtfs(spo_gtfs, ggl_gtfs)
#' names(merged_gtfs)
#'
#' # use a list() to programatically merge many GTFS objects
#' merged_gtfs <- merge_gtfs(list(spo_gtfs, ggl_gtfs))
#'
#' @export
merge_gtfs <- function(..., files = NULL, warnings = TRUE, prefix = FALSE) {
  checkmate::assert_logical(warnings)
  checkmate::assert_character(files, null.ok = TRUE)
  checkmate::assert_character(files, null.ok = TRUE)

  # store gtfs objects in a list and unlist eventual lists of gtfs

  gtfs_objects <- list(...)

  is_list_of_gtfs <- vapply(
    gtfs_objects,
    function(list_element) {
      is_gtfs_object <- vapply(
        list_element,
        function(i) checkmate::test_class(i, "dt_gtfs"),
        logical(1)
      )
      all(is_gtfs_object)
    },
    logical(1)
  )

  gtfs_objects <- append(
    gtfs_objects[!is_list_of_gtfs],
    unlist(gtfs_objects[is_list_of_gtfs], recursive = FALSE)
  )

  # check if all objects passed in ... are GTFS objects

  invisible(
    lapply(gtfs_objects, checkmate::assert_class, "dt_gtfs", .var.name = "...")
  )

  # check if prefix is either a logical or a character vector with length equal
  # to the number of provided gtfs objects

  gtfs_count <- length(gtfs_objects)

  checkmate::assert(
    checkmate::check_logical(prefix, len = 1),
    checkmate::check_character(prefix, len = gtfs_count),
    combine = "or"
  )

  if (is.logical(prefix)) {
    list_of_prefixes <- as.character(seq.int(gtfs_count))
  } else {
    list_of_prefixes <- prefix
  }

  # only merge the tables specified by 'files'
  # if 'files' is NULL, then merge all possible files.
  # if not, check if the GTFS to be merged contain the files.
  # * if some of them are missing, throw a warning.
  # * if all of them are missing, throw an error.

  files_within_gtfs <- unlist(lapply(gtfs_objects, names))
  files_within_gtfs <- unique(files_within_gtfs)
  files_to_merge <- files_within_gtfs

  if (!is.null(files)) {
    invalid_files <- files[! files %chin% files_within_gtfs]
    files_to_merge <- files[! files %chin% invalid_files]

    if (!identical(invalid_files, character(0))) {
      if (!identical(files_to_merge, character(0)) & warnings) {
        warning(
          "None of the GTFS to be merged contain the following files: ",
          paste0("'", invalid_files, "'", collapse = ", ")
        )
      } else if (identical(files_to_merge, character(0))) {
        stop(
          "None of the GTFS to be merged contain the files passed to 'files'"
        )
      }
    }
  }

  # merge specified files using rbindlist - loop over each file to merge, then
  # use rbindlist to bind all correspondent data.tables to a single data.table.

  merged_gtfs <- lapply(
    files_to_merge,
    function(file) {
      data.table::rbindlist(
        lapply(gtfs_objects, function(gtfs_object) gtfs_object[[file]]),
        fill = TRUE,
        idcol = "origin_gtfs"
      )
    }
  )

  # using fill = TRUE in data.table::rbindlist introduces some NA_character.
  # change those to "" to keep consistency (empty character entries are read as
  # "" by data.table::fread)

  for (dt in seq_along(merged_gtfs)) {
    # determine the character columns in each data.table

    col_classes <- vapply(merged_gtfs[[dt]], class, character(1))
    charac_cols <- which(col_classes == "character")

    # then substitute NA by ""

    for (col in charac_cols) {
      data.table::set(
        merged_gtfs[[dt]],
        i = which(is.na(merged_gtfs[[dt]][[col]])),
        j = col,
        value = ""
      )
    }
  }

  # if prefix is TRUE or a list of prefixes, then add the prefixes to the ids.
  # some fields that end with "_id" should not be changed (e.g. direction_id in
  # trips, which is actually an enum, not an id).

  unchanged_ids <- c("direction_id")

  if (!isFALSE(prefix)) {
    for (dt in merged_gtfs) {
      # determine the id columns (or columns that refer to ids) in each table.
      # direction_id, from trips, ends with _id but is an enum and should not be
      # changed.
      # field_value, from translations, doesn't end with _id but refers to an
      # id, thus should be changed.

      id_cols <- names(dt)[(grepl("_id$", names(dt)))]
      id_cols <- setdiff(id_cols, "direction_id")
      if (!is.null(dt[["field_value"]])) id_cols <- c(id_cols, "field_value")

      # then add the prefix to the id value

      for (col in id_cols) {
        dt[
          get(col) != "",
          eval(col) := paste0(list_of_prefixes[origin_gtfs], "_", get(col))
        ]
      }
    }
  }

  for (dt in merged_gtfs) dt[, origin_gtfs := NULL]

  # create a gtfs object from the current list of gtfs tables in merged_gtfs

  names(merged_gtfs) <- files_to_merge
  merged_gtfs <- gtfsio::new_gtfs(merged_gtfs, "dt_gtfs")

  return(merged_gtfs)
}
