#' Download MobilityData's GTFS validator
#'
#' Downloads MobilityData's command line tool to validate GTFS feeds.
#'
#' @param path The directory where the validator should be saved to.
#' @param version The version of the validator that should be downloaded.
#'   Defaults to `"latest"`, but accepts version numbers as strings (i.e. to
#'   download version v3.0.0 please enter `"3.0.0"`). Please check
#'   [MobilityData/gtfs-validator
#'   releases](https://github.com/MobilityData/gtfs-validator/releases) for the
#'   full set of available versions.
#' @param quiet Whether to hide log messages and progress bars (defaults to
#'   `TRUE`).
#'
#' @return Invisibly returns the normalized path to the downloaded validator.
#'
#' @family validation
#'
#' @examplesIf identical(tolower(Sys.getenv("NOT_CRAN")), "true")
#' path <- tempdir()
#' dir.create(path)
#'
#' download_validator(path)
#'
#' # specifying a specific version
#' download_validator(path, version = "3.1.0")
#'
#' @export
download_validator <- function(path, version = "latest", quiet = TRUE) {
  available_versions <- c(
    "latest",
    "3.1.0",
    "3.0.0",
    "2.0.0",
    "1.4.0",
    "1.3.1",
    "1.3.0",
    # "1.2.2", # .jar not available on release
    "1.2.1",
    "1.2.0",
    "1.1.0",
    "1.0.1",
    "1.0.0"
  )

  checkmate::assert(
    checkmate::check_string(path),
    checkmate::check_directory_exists(path),
    combine = "and"
  )
  checkmate::assert(
    checkmate::check_string(version),
    checkmate::check_names(version, subset.of = available_versions),
    combine = "and"
  )
  checkmate::assert_logical(quiet, any.missing = FALSE, len = 1)

  if (version == "latest") {
    version <- setdiff(available_versions, "latest")[1]
  }

  validator_url <- get_validator_url(version, available_versions)

  parse_content <- getOption("GTFSTOOLS_PARSE_RESPONSE", default = TRUE)
  if (!parse_content) {
    response <- curl::curl_fetch_memory(validator_url)
    return(response$status_code)
  }

  validator_basename <- paste0("gtfs-validator-v", version)
  output_file <- file.path(path, validator_basename)
  if (!quiet) message("Downloading ", validator_url, " to ", output_file, ".")

  curl::curl_download(validator_url, destfile = output_file, quiet = quiet)
  return(invisible(normalizePath(output_file)))
}

get_validator_url <- function(version, available_versions) {
  base_url <- "https://github.com/MobilityData/gtfs-validator/releases/"

  release_url <- paste0(base_url, "download/v", version, "/")
  cli_basename <- paste0("gtfs-validator-", version, "-cli.jar")

  if (version == "1.0.0") {
    cli_basename <- "gtfs-validator.jar"
  } else if (numeric_version(version) < numeric_version("3.1.0")) {
    cli_basename <- sub("gtfs-validator-", "gtfs-validator-v", cli_basename)

    if (numeric_version(version) > numeric_version("1.2.2")) {
      cli_basename <- sub("-cli", "_cli", cli_basename)
    } else if (numeric_version(version) < numeric_version("1.2.2")) {
      cli_basename <- sub("-cli", "", cli_basename)
    }
  }

  validator_url <- paste0(release_url, cli_basename)

  return(validator_url)
}