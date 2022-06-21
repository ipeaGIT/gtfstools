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
#'
#' @return Invisibly returns the normalized path to the directory where the
#'   validation results were saved to.
#'
#' @family validation
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' gtfs_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' output_path <- tempfile("validation_result")
#' validator_path <- download_validator(tempdir())
#'
#' validate_gtfs(gtfs_path, output_path, validator_path)
#' @export
validate_gtfs <- function(gtfs, output_path, validator_path) {
  assert_java_version()

  call_output <- processx::run(
    "java",
    c("-jar", validator_path, "-i", gtfs, "-o", output_path)
  )

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
