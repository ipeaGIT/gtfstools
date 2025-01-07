#' Get children stops recursively
#'
#' Returns the (recursive) children stops of each specified `stop_id`.
#' Recursive in this context means it returns all children's children (i.e.
#' first children, then children's children, and then their children, and so
#' on).
#'
#' @template gtfs
#' @param stop_id A string vector including the `stop_id`s to have their
#'   children returned. If `NULL` (the default), the function returns the
#'   children of every `stop_id` in the GTFS.
#'
#' @return A `data.table` containing the `stop_id`s and their children'
#'   `stop_id`s. If a stop doesn't have a child, its correspondent `child_id`
#'   entry is marked as `""`.
#'
#' @examples
#' \dontshow{
#'   old_dt_threads <- data.table::setDTthreads(1)
#'   on.exit(data.table::setDTthreads(old_dt_threads), add = TRUE)
#' }
#' data_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' children <- get_children_stops(gtfs)
#' head(children)
#'
#' # use the stop_id argument to control which stops are analyzed
#' children <- get_children_stops(gtfs, stop_id = c("F12S", "F12N"))
#' children
#'
#' @export
get_children_stops <- function(gtfs, stop_id = NULL) {
  gtfs <- assert_and_assign_gtfs_object(gtfs)
  checkmate::assert_character(stop_id, null.ok = TRUE, any.missing = FALSE)
  gtfsio::assert_field_class(
    gtfs,
    "stops",
    c("stop_id", "parent_station"),
    rep("character", 2)
  )

  # select stop_ids to identify parents and raise warning if a given stop_id
  # doesn't exist in 'stops'

  if (!is.null(stop_id)) {
    invalid_stop_id <- stop_id[! stop_id %chin% gtfs$stops$stop_id]

    if (!identical(invalid_stop_id, character(0))) {
      warning(
        paste0(
          "'stops' doesn't contain the following stop_id(s): "),
        paste0("'", invalid_stop_id, "'", collapse = ", ")
      )
    }

    stop_id <- setdiff(stop_id, invalid_stop_id)
  } else {
    stop_id <- gtfs$stops$stop_id
  }

  # recursively find children

  parents <- gtfs$stops$parent_station
  names(parents) <- gtfs$stops$stop_id

  result <- data.table::data.table(
    stop_id = stop_id,
    checked = rep(FALSE, length(stop_id))
  )

  do_check <- TRUE

  while (do_check) {
    result[
      checked == FALSE,
      `:=`(
        children_list = lapply(
          stop_id,
          function(stop) names(parents[parents == stop])
        ),
        checked = TRUE
      )
    ]

    found_children <- unique(unlist(result$children_list))
    do_check <- FALSE

    if (!all(found_children %chin% result$stop_id)) {
      new_children <- setdiff(found_children, result$stop_id)

      result <- rbind(
        result,
        data.table::data.table(
          stop_id = new_children,
          checked = FALSE,
          children_list = list()
        )
      )

      do_check <- TRUE
    }
  }

  result[
    ,
    children_list := lapply(
      children_list,
      function(children) {
        if (identical(children, character(0))) {
          ""
        } else {
          children
        }
      }
    )
  ]


  # if stop_id == character(0) (be it because it was specified like so or
  # because none of the specified stop_ids were valid), the unlist() call below
  # would fail because data.table wouldn't be able to infer the column type

  if (identical(result$children_list, list())) {
    result[, children_list := character()]
  }
  result <- result[, .(child_id = unlist(children_list)), by = stop_id]

  return(result[])
}
