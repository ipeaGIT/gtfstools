#' Get children stops recursively
#'
#' Returns the (recursive) children stops of each specified `stop_id`.
#' Recursive in this context means it returns all children's children (i.e.
#' first children, then children's children, and then their children, and so
#' on).
#'
#' @param gtfs A GTFS object.
#' @param stop_id A string vector including the `stop_id`s to have their
#' children returned.
#'
#' @return A `data.table` containing the `stop_id`s and their children'
#' `stop_id`s. If a stop doesn't have a child, its `child_id` is `""`.
#'
#' @examples
#' data_path <- system.file("extdata/ggl_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#' children <- get_children_stops(gtfs, "F12")
#' children
#'
#' children <- get_children_stops(gtfs, c("F12S", "F12N"))
#' children
#'
#' @export
get_children_stops <- function(gtfs, stop_id) {
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_character(stop_id)
  gtfsio::assert_field_class(
    gtfs,
    "stops",
    c("stop_id", "parent_station"),
    rep("character", 2)
  )

  # raise warning if one of the specified stop_ids doesn't exist in 'stops'.
  # also drop invalid ids

  invalid_ids <- stop_id[! stop_id %chin% gtfs$stops$stop_id]

  if (!identical(invalid_ids, character(0))) {
    warning(
      "'stops' doesn't contain the following stop_id(s): ",
      paste0("'", invalid_ids, "'", collapse = ", ")
    )
  }

  relevant_stops <- setdiff(stop_id, invalid_ids)

  # recursively find children

  parents <- gtfs$stops$parent_station
  names(parents) <- gtfs$stops$stop_id

  result <- data.table::data.table(
    stop_id = relevant_stops,
    checked = rep(FALSE, length(relevant_stops))
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
