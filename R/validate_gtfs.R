#' Validate GTFS file
#'
#' Validates the GTFS file against GTFS specifications and raises warnings if
#' required files/fields are not found.
#'
#' @param gtfs A GTFS object as created by \code{read_gtfs}.
#' @param files A character vector containing the text files to be validated
#'   against the GTFS specification (without the \code{.txt} extension). If
#'   \code{NULL} (the default) the provided GTFS is validated against all
#'   possible GTFS text files.
#' @param quiet Whether to display warning messages (the default) or not.
#'
#' @return A data.table containing the validation summary of all possible fields
#'   from the specified files.
#'
#' @export
validate_gtfs <- function(gtfs, files, quiet) {

  gtfs_metadata <- get_gtfs_meta()

  checkmate::assert_class(gtfs, "gtfs")

  # if any files have been specified in read_gtfs, only validate those

  if (is.null(files)) {
    files_to_validate <- names(gtfs_metadata)
  } else {
    files_to_validate <- paste0(files, ".txt")
  }

  # build validation dt for each file

  validation_result <- lapply(files_to_validate, function(filename) {

    file_metadata <- gtfs_metadata[[filename]]
    file          <- sub(".txt", "", filename)

    # if metadata is null then file is undocumented. validate it as an "extra" file
    # https://developers.google.com/transit/gtfs/reference

    if (is.null(file_metadata)) {

      file_provided_status  <- TRUE
      file_spec             <- "ext"
      field                 <- names(gtfs[[file]])
      field_spec            <- "ext"
      field_provided_status <- TRUE

    } else {

      file_provided_status  <- file %in% names(gtfs)
      file_spec             <- file_metadata$file_spec
      field                 <- file_metadata$field
      field_spec            <- file_metadata$field_spec[field]
      field_provided_status <- field %in% names(gtfs[[file]])

    }

    data.table::data.table(
      file,
      file_spec,
      file_provided_status,
      field,
      field_spec,
      field_provided_status
    )

  })

  validation_result <- data.table::rbindlist(validation_result)

  # checks for the missing calendar.txt exception
  # see https://developers.google.com/transit/gtfs/reference/#calendar_datestxt
  # if the exception is true then calendar.txt is set to optional

  if (! "calendar" %in% names(gtfs)) {

    validation_result[file == "calendar", file_spec := "opt"]
    validation_result[file == "calendar_dates", file_spec := "req"]

  }

  # checks for validation status and details

  validation_result[, `:=`(validation_status = "ok", validation_details = NA_character_)]

  # if file is not provided and is required, mark as a problem

  validation_result[
    !file_provided_status & file_spec == "req",
    `:=`(validation_status = "problem", validation_details = "missing_req_file")
  ]

  # if file is not provided and is optional, mark as a info

  validation_result[
    !file_provided_status & file_spec == "opt",
    `:=`(validation_status = "info", validation_details = "missing_opt_file")
  ]

  # if file is provided but misses a required field, mark as a problem

  validation_result[
    file_provided_status & !field_provided_status & field_spec == "req",
    `:=`(validation_status = "problem", validation_details = "missing_req_field")
  ]

  # if file is provided but misses a optional field, mark as a info

  validation_result[
    file_provided_status & !field_provided_status & field_spec == "opt",
    `:=`(validation_status = "info", validation_details = "missing_opt_field")
  ]

  # raises warnings if problems are found

  files_problems <- validation_result[validation_details == "missing_req_file"]

  if (nrow(files_problems) >= 1 & !quiet) {

    warning(
      paste0(
        "Invalid feed. Missing required file(s): ",
        paste(paste0(unique(files_problems$file), ".txt"), collapse = ", ")
      )
    )

  }

  fields_problems <- validation_result[validation_details == "missing_req_field"]

  if (nrow(fields_problems) >= 1 & !quiet) {

    problematic_files <- unique(fields_problems$file)

    problematic_fields <- unlist(
      lapply(
        problematic_files,
        function(i) paste(fields_problems[file == i]$field, collapse = ", ")
      )
    )

    warning(
      paste0(
        "Invalid feed. ",
        paste0(
          "Missing required field(s) in ", problematic_files, ": ", problematic_fields,
          collapse = ". "
        )
      )
    )

  }

  return(validation_result)

}
