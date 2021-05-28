#' Read and validate GTFS files
#'
#' Reads GTFS text files from either a local \code{.zip} file or an URL and
#' validates them against GTFS specifications.
#'
#' @param path The path to a GTFS \code{.zip} file.
#' @param files A character vector containing the text files to be read from the
#'   GTFS (without the \code{.txt} extension). If \code{NULL} (the default) all
#'   existing files are read.
#' @param quiet Whether to hide log messages and progress bars (defaults to
#'   TRUE).
#' @param warnings Whether to display warning messages (defaults to TRUE).
#'
#' @return A GTFS object: a \code{list} of \code{data.table}s in which each
#'   index represents a GTFS text file. In case of parsing failures (e.g. files
#'   with more/less columns than specified in the file header), the function
#'   throws an error detailing where such failures occurred.
#'
#' @section Details:
#' The column types of each \code{data.table} in the final GTFS object conform
#' as closely as possible to the
#' \href{https://developers.google.com/transit/gtfs/reference}{Google's Static GTFS Reference}.
#' Exceptions are date-related columns (such as \code{calendar.txt}'s
#' \code{start_date} and \code{end_date}, for example), which are converted to
#' \code{Date} objects, instead of being kept as \code{integer}s, allowing for
#' easier data manipulation. These columns are converted back to
#' \code{integer}s when writing the GTFS object to a \code{.zip} file using
#' \code{\link{write_gtfs}}.
#'
#' @seealso \code{\link{validate_gtfs}}
#'
#' @examples
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#' names(gtfs)
#'
#' gtfs <- read_gtfs(data_path, files = c("trips", "stop_times"))
#' names(gtfs)
#'
#' @export
read_gtfs <- function(path, files = NULL, quiet = TRUE, warnings = TRUE) {

  checkmate::assert_string(path)
  checkmate::assert_logical(quiet)
  checkmate::assert_logical(warnings)

  # check if path is an url. if so, download the gtfs from it

  if (grepl('http[s]?://.*', path)) {

    temp_file <- tempfile(pattern = "gtfs", fileext = ".zip")

    utils::download.file(path, temp_file, method = "auto", quiet = quiet)

    if (!quiet) message(paste0("File downloaded to ", normalizePath(temp_file)))

    path <- temp_file

  }

  # unzip files to temporary folder

  checkmate::assert_file_exists(path, extension = "zip")

  files_in_gtfs <- zip::zip_list(path)$filename

  if (is.null(files)) {

    files_to_read <- files_in_gtfs

  } else {

    checkmate::assert_names(files, subset.of = sub(".txt", "", files_in_gtfs))

    files_to_read <- paste0(files, ".txt")

  }

  temp_dir <- file.path(tempdir(), "gt_gtfsdir")
  unlink(temp_dir, recursive = TRUE)
  zip::unzip(path, files = files_to_read, exdir = temp_dir, overwrite = TRUE)

  if (!quiet) {
    message(
      paste0(
        "Unzipped the following files to directory ",
        normalizePath(temp_dir),
        ":\n",
        paste0("> ", files_to_read, collapse = "\n")
      )
    )
  }

  # read files into list and assign GTFS class

  gtfs <- lapply(files_to_read, read_files, temp_dir, quiet)
  gtfs <- stats::setNames(gtfs, sub(".txt", "", files_to_read))
  class(gtfs) <- c("dt_gtfs", "gtfs")

  # check if any parsing warnings were thrown by data.table::fread
  # if so, thrown an error detailing them

  files_class <- lapply(gtfs, class)
  has_warning <- vapply(files_class, function(i) "warning" %in% i, logical(1))

  if (any(has_warning)) {

    gtfs_warnings <- gtfs[has_warning]
    gtfs_warnings <- lapply(gtfs_warnings, extract_warning_message)

    # compose error message

    general_description <- paste0(
      "Parsing failures while reading the following file(s): ",
      paste0("'", names(gtfs_warnings), "'", collapse = ", ")
    )

    detailed_problems <- vapply(
      seq.int(length(gtfs_warnings)),
      function(i) paste0("'", names(gtfs_warnings)[i], "': ", gtfs_warnings[i]),
      character(1)
    )
    detailed_problems <- paste(detailed_problems, collapse = "\n")

    error_message <- paste0(general_description, "\n", detailed_problems)

    stop(error_message)

  }

  # if not, validate given GTFS structure against specifications

  gtfs <- validate_gtfs(gtfs, files, quiet, warnings)

  return(gtfs)

}



#' Read GTFS text files
#'
#' Reads a text file from the main zip file.
#'
#' @param file The name of the file (with \code{.txt} extension) to be read.
#' @param temp_dir The path to the temporary folder where the GTFS was unzipped.
#' @param quiet Whether to hide log messages and progress bars (defaults to
#'   TRUE).
#'
#' @return Either a data.table containing the desired file or a log message if
#'   any parsing warnings were thrown.
#'
#' @noRd
read_files <- function(file, temp_dir, quiet) {

  # uses internal data gtfs_metadata - check data-raw/gtfs_metadata.R

  file_metadata <- gtfs_metadata[[file]]

  if (!quiet) message(paste0("Reading ", file))

  # if metadata is null then file is undocumented. read everything as character

  if (is.null(file_metadata)) {

    if (!quiet) {

      message(
        paste0(
          "  File ",
          file,
          ".txt not recognized. Trying to read it as a csv."
        )
      )

    }

    full_dt <- data.table::fread(
      file.path(temp_dir, file),
      colClasses = "character",
      showProgress = !quiet
    )

  } else {

    # read first row to know what columns to read

    sample_dt <- suppressWarnings(
      data.table::fread(file.path(temp_dir, file), nrows = 1)
    )

    # if file is completely empty (without even a header) return NULL data.table

    if (ncol(sample_dt) == 0) {

      if (!quiet) message("  File is empty. Returning a NULL data.table")
      return(data.table::data.table(NULL))

    }

    # read full file, specify column classes according to which columns should
    # be read

    col_to_read <- names(sample_dt)
    col_classes <- file_metadata$coltype[col_to_read]

    # substitute NA elements in col_classes with undocumented column names and
    # "character" as the default column type

    extra_col <- setdiff(col_to_read, names(col_classes))
    col_classes[is.na(col_classes)] <- "character"
    names(col_classes)[is.na(names(col_classes))] <- extra_col

    # try reading the file. if any parsing warning is thrown, return the warning
    # message to convert it in an error

    full_dt <- tryCatch(
      data.table::fread(file.path(temp_dir, file), select = col_classes),
      warning = function(w) w
    )

  }

  return(full_dt)

}



#' Extract warning message
#'
#' Extracts the warning message as a string from a warning log.
#'
#' @param warning_log Warning log generated by \code{data.table::fread}.
#'
#' @return A string containing details on the parsing failure.
#'
#' @noRd
extract_warning_message <- function(warning_log) {

  warning_log <- as.character(warning_log)

  possible_warnings <- c(
    paste0(
      "Detected \\d+ column names but the data has \\d+ columns ",
      "\\(i\\.e\\. invalid file\\)\\."
    ),
    "Detected \\d+ column names but the data has \\d+ columns.",
    "Stopped early on line \\d+\\. Expected \\d+ fields but found \\d+\\.",
    "Discarded single-line footer: <<.+>>"
  )

  warning_message <- regmatches(
    warning_log,
    regexpr(
      paste(possible_warnings, collapse = "|"),
      warning_log
    )
  )

  return(warning_message)

}
