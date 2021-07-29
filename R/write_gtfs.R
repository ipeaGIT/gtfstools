#' Write GTFS files
#'
#' Writes GTFS objects as GTFS `.zip` files.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param path The path to the `.zip` file in which the feed should be written
#'   to.
#' @param files A character vector containing the name of the elements to be
#'   written to the feed. If `NULL` (the default), all elements inside the GTFS
#'   object are written.
#' @param standard_only Whether to write only standard files and fields
#'   (defaults to `FALSE`, which doesn't drop extra files and fields).
#' @param as_dir Whether to write the feed as a directory, instead of a `.zip`
#'   file (defaults to `FALSE`, which means that the field is written as a zip
#'   file).
#' @param overwrite Whether to overwrite existing `.zip` file (defaults to
#'   `TRUE`).
#' @param quiet Whether to hide log messages and progress bars (defaults to
#'   `TRUE`).
#'
#' @return Invisibly returns the same GTFS object passed to the `gtfs`
#' parameter.
#'
#' @family io functions
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' tmp_dir <- file.path(tempdir(), "tmpdir")
#' dir.create(tmp_dir)
#' list.files(tmp_dir)
#'
#' tmp_file <- tempfile(pattern = "gtfs", tmpdir = tmp_dir, fileext = ".zip")
#' write_gtfs(gtfs, tmp_file)
#' list.files(tmp_dir)
#'
#' gtfs_all_files <- read_gtfs(tmp_file)
#' names(gtfs_all_files)
#'
#' write_gtfs(gtfs, tmp_file, files = "stop_times")
#' gtfs_stop_times <- read_gtfs(tmp_file)
#' names(gtfs_stop_times)
#'
#' @export
write_gtfs <- function(gtfs,
                       path,
                       files = NULL,
                       standard_only = FALSE,
                       as_dir = FALSE,
                       overwrite = TRUE,
                       quiet = TRUE) {

  # inputs are more thoroughly checked on gtfsio::export_gtfs()

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_path_for_output(
    path,
    overwrite = overwrite,
    extension = "zip"
  )
  checkmate::assert_character(files, null.ok = TRUE)
  checkmate::assert_logical(standard_only)
  checkmate::assert_logical(as_dir)
  checkmate::assert_logical(overwrite)

  # convert relevant fields to standard types and write result using {gtfsio}

  std_gtfs <- convert_to_standard(gtfs)
  gtfsio::export_gtfs(
    std_gtfs,
    path = path,
    files = files,
    standard_only = standard_only,
    as_dir = as_dir,
    overwrite = overwrite,
    quiet = quiet
  )

  # return object passed to 'gtfs' invisibly

  return(invisible(gtfs))

}
