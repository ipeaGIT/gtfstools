#' Merge GTFS files
#'
#' Combines many GTFS file into a single one and validates the resulting object.
#'
#' @param ... GTFS objects, as created by \code{\link{read_gtfs}}, to be merged.
#'   Each argument can either be a GTFS or a list of GTFS objects.
#' @param files A character vector listing the GTFS text files (i.e. the ones
#'   represented by \code{data.table}s) to be merged. If \code{NULL} (the
#'   default) all files are merged.
#' @param quiet Whether to hide log messages (defaults to TRUE).
#' @param warnings Whether to display warning messages (defaults to TRUE).
#'
#' @return Returns a GTFS object, with an updated \code{validation_result}
#'   attribute, in which each \code{data.table} is the combination (by row) of
#'   \code{data.table}s with the same name from the GTFS objects passed in
#'   \code{...}.
#'
#' @section Details:
#' Please note that this function does not disambiguate \code{ids} that may be
#' repeated within different GTFS objects. Please let us know if you'd like to
#' see this feature implemented.
#'
#' @seealso \code{\link{validate_gtfs}}
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
merge_gtfs <- function(..., files = NULL, quiet = TRUE, warnings = TRUE) {

  # input checking - gtfs objects are checked below

  checkmate::assert_logical(quiet)
  checkmate::assert_logical(warnings)
  checkmate::assert_character(files, null.ok = TRUE)

  # store gtfs objects in a list and unlist eventual lists of gtfs

  gtfs_objects <- list(...)

  is_list_gtfs <- vapply(
    gtfs_objects,
    function(i) checkmate::test_class(i, "list"),
    logical(1)
  )

  gtfs_objects <- append(
    gtfs_objects[!is_list_gtfs],
    unlist(gtfs_objects[is_list_gtfs], recursive = FALSE)
  )

  # check if all objects passed in ... are GTFS objects

  invisible(
    lapply(gtfs_objects, checkmate::assert_class, "dt_gtfs", .var.name = "...")
  )

  # only merge and validate the data.tables specified by 'files'
  # if 'files' is NULL, then merge and validate all possible files
  # if not, check if the GTFS to be merged contain the files.
  # * if some of them are missing, throw a warning.
  # * if all of them are missing, throw an error

  files_within_gtfs <- unlist(lapply(gtfs_objects, names))
  files_within_gtfs <- unique(files_within_gtfs)

  if (is.null(files)) {

    files_to_merge    <- files_within_gtfs
    files_to_validate <- files

  } else {

    invalid_files  <- files[! files %chin% files_within_gtfs]
    files_to_merge <- files_to_validate <- files[! files %chin% invalid_files]

    if (!identical(invalid_files, character(0))) {

      if (!identical(files_to_merge, character(0)) & warnings) {

        warning(
          paste0(
            "None of the GTFS to be merged contain the following files: ",
            paste0("'", invalid_files, "'", collapse = ", ")
          )
        )

      } else if (identical(files_to_merge, character(0))) {

        stop(
          "None of the GTFS to be merged contain the files passed to 'files'"
        )

      }

    }

  }

  # merge specified files using rbindlist - loop over each file to merge, than
  # use rbindlist to bind all correspondent data.tables to a single data.table

  merged_gtfs <- lapply(
    files_to_merge,
    function(txt_file) {
      data.table::rbindlist(
        lapply(gtfs_objects, function(gtfs_file) gtfs_file[[txt_file]]),
        fill = TRUE
      )
    }
  )
  names(merged_gtfs) <- files_to_merge
  class(merged_gtfs) <- "dt_gtfs"

  # using fill = TRUE in data.table::rbindlist introduces some NA_character_
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

  # validate final gtfs

  merged_gtfs <- validate_gtfs(merged_gtfs, files_to_validate, quiet, warnings)

  return(merged_gtfs)

}
