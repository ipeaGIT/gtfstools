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
#' @examples
#' path <- tempfile()
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
    "1.2.2",
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

  base_url <- "https://github.com/MobilityData/gtfs-validator/releases/"

  if (version == "latest") {
    release_url <- paste0(base_url, "latest")
  } else {
    release_url <- paste0(base_url, "tag/v", version)
  }

  validator_url <- get_validator_url(release_url)

  do_download <- as.logical(Sys.getenv("DOWNLOAD_VALIDATOR", unset = TRUE))
  if (!do_download) {
    download_request <- curl::curl_fetch_memory(validator_url)
    return(download_request$status_code)
  }

  output_file <- file.path(path, basename(validator_url))
  if (!quiet) message("Downloading ", validator_url, " to ", output_file, ".")

  curl::curl_download(validator_url, destfile = output_file, quiet = quiet)
  return(invisible(normalizePath(path)))
}

get_validator_url <- function(release_url) {
  # we have to inspect the url first to get a "real" version from 'latest'.
  # this way we don't need to manually update the list of versions every time a
  # new version is released (people would still be able to get the latest
  # version, at least)

  request <- curl::curl_fetch_memory(release_url)

  versioned_url <- request$url
  versioned_download_url <- sub("\\/tag\\/", "\\/download\\/", versioned_url)

  string_length <- nchar(versioned_download_url)
  url_version <- substring(
    versioned_download_url,
    first = regexpr("\\d+\\.\\d+\\.\\d+", versioned_download_url),
    last = string_length
  )
  cli_basename <- paste0("gtfs-validator-", url_version, "-cli.jar")
  validator_url <- file.path(versioned_download_url, cli_basename)

  if (numeric_version(url_version) < numeric_version("3.1.0")) {
    validator_url <- sub("-cli", "_cli", validator_url)
    validator_url <- sub("gtfs-validator-", "gtfs-validator-v", validator_url)
  }

  return(validator_url)
}
