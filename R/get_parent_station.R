#' Get parent stations recursively
#'
#' Returns the (recursive) parent stations of each specified \code{stop_id}.
#' Recursive in this context means it returns all parents' parents (i.e. first
#' parents, then parents' parents, and then their parents, and so on).
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param stop_id A string vector including the \code{stop_id}s to have their
#'   parents returned.
#'
#' @return A \code{data.table} containing the \code{stop_id}s and their
#'   \code{parent_station}s. If a stop doesn't have a parent, its
#'   \code{parent_station} is \code{""}.
#'
#' @examples
#' data_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
#'
#' gtfs <- read_gtfs(data_path)
#'
#' parents <- get_parent_station(gtfs, "N3")
#' parents
#'
#' parents <- get_parent_station(gtfs, c("B1", "B2"))
#' parents
#'
#' @export
get_parent_station <- function(gtfs, stop_id) {

  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(stop_id)

  # check if required files and fields exist and are of correct type

  gtfsio::assert_fields_types(
    gtfs,
    "stops",
    c("stop_id", "parent_station"),
    rep("character", 2)
  )

  # create a "relational" vector, whose names are the stop ids and values are
  # the parent stations

  parents <- gtfs$stops$parent_station
  names(parents) <- gtfs$stops$stop_id

  # raise warning if passed 'stop_id' doesn't exist in 'stops'
  # and remove invalid ids from 'stop_id'

  invalid_stop_id <- stop_id[! stop_id %chin% gtfs$stops$stop_id]

  if (!identical(invalid_stop_id, character(0))) {

    warning(
      paste0(
        "'stops' doesn't contain the following stop_id(s): "),
      paste0("'", invalid_stop_id, "'", collapse = ", ")
    )

  }

  stop_id <- setdiff(stop_id, invalid_stop_id)

  # create the data.table where parent stops will be stored

  result <- data.table::data.table(
    stop_id = stop_id,
    parent_station = rep(NA_character_, length(stop_id))
  )

  # get the parent stations of each stop

  do_check <- TRUE

  while (do_check) {

    result[is.na(parent_station), parent_station := parents[stop_id]]

    # when a stop not listed in stops (in the stop_id field) is used to subset
    # the parents vector, it introduces a NA_character_ in the parent_station
    # columns. substitute NAs by "" because of that
    result[is.na(parent_station), parent_station := ""]

    # keep checking for recursive parents if new parents were found in this
    # iteration

    do_check <- FALSE

    found_parents <- setdiff(result$parent_station, "")

    if (!all(found_parents %chin% result$stop_id)) {

      new_parents <- unique(setdiff(found_parents, result$stop_id))

      result <- rbind(
        result,
        data.table::data.table(
          stop_id = new_parents,
          parent_station = NA_character_
        )
      )

      do_check <- TRUE

    }

  }

  return(result[])

}
