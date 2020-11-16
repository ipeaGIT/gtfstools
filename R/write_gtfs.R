#' Write GTFS files
#'
#' Writes in-memory GTFS objects as GTFS \code{.zip} files. Conditionally
#' includes optional and extra \code{.txt} files (check \code{\link{validate_gtfs}}
#' documentation to check what are optional/extra files).
#'
#' @param gtfs A GTFS object as created by \code{read_gtfs}.
#' @param path The path to the \code{.zip} file in which the feed should be
#'   written to.
#' @param optional Whether to write optional \code{.txt}. Defaults to TRUE.
#' @param extra Whether to write extra \code{.txt}. Defaults to TRUE.
#' @param overwrite Whether to overwrite existing \code{.zip} file. Defaults to
#'   TRUE.
#'
#' @return Invisibly returns the provided GTFS object.
#'
#' @seealso \code{\link{validate_gtfs}}
#'
#' @export
write_gtfs <- function(gtfs, path, optional = TRUE, extra = TRUE, overwrite = TRUE) {

  checkmate::assert_class(gtfs, "gtfs")
  checkmate::assert_path_for_output(path, overwrite = overwrite, extension = "zip")
  checkmate::assert_logical(optional)
  checkmate::assert_logical(extra)
  checkmate::assert_logical(overwrite)

  # write files to temporary folder - remove temp_dir on exit

  temp_dir <- file.path(tempdir(), "gtfsdir")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # figure out what files should be written

  files_in_gtfs <- names(gtfs)

  validation_result <- attr(gtfs, "validation_result")
  files_index <- validation_result[, .I[1], keyby = file]$V1
  files_specs <- stats::setNames(
    validation_result$file_spec[files_index],
    validation_result$file[files_index]
  )

  files_to_write <- files_in_gtfs
  optional_files <- files_to_write[files_specs[files_to_write] == "opt"]
  extra_files    <- files_to_write[files_specs[files_to_write] == "ext"]

  if (!optional) files_to_write <- files_to_write[! files_to_write %in% optional_files]
  if (!extra)    files_to_write <- files_to_write[! files_to_write %in% extra_files]

  for (file in files_to_write) {

    dt <- gtfs[[file]]
    col_classes <- vapply(dt, function(i) class(i)[1], character(1))

    # format dates back to YYYYMMDD

    date_cols <- names(which(col_classes == "Date"))

    if (length(date_cols) > 0) {

      dt <- data.table::copy(dt)
      dt[, (date_cols) := lapply(.SD, format, "%Y%m%d"), .SDcols = date_cols]

    }

    # format hms back to HH:MM:SS

    time_cols <- names(which(col_classes == "hms"))

    if (length(time_cols) > 0) {

      dt <- data.table::copy(dt)
      dt[, (time_cols) := lapply(.SD, as.character), .SDcols = time_cols]

    }

    # write files

    file_path <- file.path(temp_dir, paste0(file, ".txt"))

    data.table::fwrite(dt, file_path)

  }

  # zip files

  zip::zipr(path, file.path(temp_dir, paste0(files_to_write, ".txt")))

  return(invisible(gtfs))

}
