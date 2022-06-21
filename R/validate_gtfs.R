#' Validate GTFS feed
#'
#' Uses MobilityData's [GTFS
#' validator](https://github.com/MobilityData/gtfs-validator) to perform a GTFS
#' business rule validation. The results are available as an HTML report and in
#' JSON format. Please check the complete set of rules used in the validation
#' [here](https://github.com/MobilityData/gtfs-validator/blob/master/RULES.md).
#'
#' @param gtfs The path to the GTFS feed to be validated.
#' @param output_path The path to the directory that the validator will create
#'   and in which the results will be saved to.
#' @param validator_path The path to the GTFS validator, previously downloaded
#'   with [download_validator()].
#' @param html_preview Whether to show HTML report in a viewer, such as RStudio
#'   or a browser. Defaults to `TRUE` (only works on interactive sessions).
#'
#' @return Invisibly returns the normalized path to the directory where the
#'   validation results were saved to.
#'
#' @family validation
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' output_path <- tempfile("validation_result")
#' validator_path <- download_validator(tempdir())
#' gtfs <- read_gtfs(data_path)
#'
#' validate_gtfs(gtfs, output_path, validator_path)
#' list.files(output_path)
#'
#' # works with feeds saved to disk
#' new_output_path <- tempfile("new_validation_result")
#' validate_gtfs(data_path, new_output_path, validator_path)
#' list.files(new_output_path)
#'
#' # and with feeds pointed by an url
#' newer_output_path <- tempfile("newer_validation_result")
#' gtfs_url <- "https://github.com/ipeaGIT/gtfstools/raw/master/inst/extdata/spo_gtfs.zip"
#' validate_gtfs(gtfs_url, newer_output_path, validator_path)
#' list.files(newer_output_path)
#'
#' @export
validate_gtfs <- function(gtfs,
                          output_path,
                          validator_path,
                          html_preview = TRUE) {
  assert_java_version()
  checkmate::assert_path_for_output(output_path)
  checkmate::assert_file_exists(validator_path)
  checkmate::assert_logical(html_preview, any.missing = FALSE, len = 1)

  if (!inherits(gtfs, "dt_gtfs")) {
    if (!checkmate::test_string(gtfs)) {
      stop(
        "Assertion on 'gtfs' failed: Must either be a GTFS object (with ",
        "dt_gtfs class), a path to a local GTFS feed or an URL to a feed."
      )
    }
    is_url <- grepl("^http[s]?\\:\\/\\/\\.*", gtfs)
    if (!(file.exists(gtfs) || is_url)) {
      stop(
        "Assertion on 'gtfs' failed: Must either be a GTFS object (with ",
        "dt_gtfs class), a path to a local GTFS feed or an URL to a feed."
      )
    }
  }

  command_flags <- c("-jar", validator_path, "-o", output_path)

  if (inherits(gtfs, "dt_gtfs")) {
    gtfs_path <- tempfile("gtfs", fileext = ".zip")
    write_gtfs(gtfs, gtfs_path)
    command_flags <- c(command_flags, "-i", gtfs_path)
  } else if (!is_url) {
    command_flags <- c(command_flags, "-i", gtfs)
  } else {
    command_flags <- c(command_flags, "-u", gtfs)
  }

  call_output <- processx::run("java", command_flags)

  if (call_output$stdout != "") {
    writeLines(
      call_output$stdout,
      con = file.path(output_path, "validation_stdout.txt")
    )
  }

  if (call_output$stderr != "") {
    writeLines(
      call_output$stderr,
      con = file.path(output_path, "validation_stderr.txt")
    )
  }

  if (interactive() && html_preview) {
    html_path <- file.path(output_path, "report.html")
    utils::browseURL(html_path)
  }

  return(invisible(normalizePath(output_path)))
}

assert_java_version <- function() {
  informative_message <- paste0(
    "Please install Java version 11 or higher to run the validator.\n",
    "You can download Java 11 from https://jdk.java.net/java-se-ri/11."
  )
  java_version_output <- tryCatch(
    processx::run("java", "-version"),
    error = function(cnd) cnd
  )

  if (inherits(java_version_output, "error")) {
    stop("Could not find Java on the system path. ", informative_message)
  }

  full_java_version <- strsplit(java_version_output$stderr, "\"")[[1]][2]
  java_version <- strsplit(full_java_version, "_")[[1]][1]
  java_version <- numeric_version(java_version)

  if (java_version < numeric_version("11.0.0")) {
    stop(
      "You seem to have Java version ", full_java_version, " installed. ",
      informative_message
    )
  }

  return(invisible(TRUE))
}
