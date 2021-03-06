#' @title Filter GTFS data by route ids
#'
#' @description
#' Filter a GTFS data by its route ids, subsetting routes
#' and trips. It also removes the unnecessary stop_times, shapes, frequencies
#' (if exist in a feed), and stops accordingly.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param route_ids A vector of route ids belonging to the routes of the
#'                 gtfs data. A `route_id`can be either a string or a number,
#'                 depending on the GTF input.
#' @param keep Logical. Whether the passed `route_ids` should be kept (Defaults
#'             to `TRUE`), or whether they shoud be droped.
#'
#' @return Returns a GTFS object with filtered routes, trips, shapes, stops,
#'         stop_times and frequencies.
#'
#' @examples
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#'# filter: Keep selected routes
#' subset1 <- filter_route_id(gtfs, route_ids=c("6450-51", "CPTM L11"), keep = T)
#'
#'# filter: Drop selected routes
#' subset2 <- filter_route_id(gtfs, route_ids=c("6450-51", "CPTM L11"), keep = F)
#'
#' @export
filter_route_id <- function(gtfs, route_ids, keep = TRUE) {

  ## input checking
  checkmate::assert_logical(keep)
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_vector(route_ids)
  checkmate::assert_data_table(gtfs$routes, .var.name= 'gtfs$routes')
    ## I like this check below because it has a more informative error message
    # if(is.null(gtfs$routes)) stop("GTFS data does not have routes")


  ## filter operator
  `%ffilter%` <- `%chin%`
  if (keep==FALSE) { `%ffilter%` <- Negate(`%chin%`) }


  ## start filtering gtfs
  # routes and trips
  gtfs$routes <- subset(gtfs$routes, route_id %ffilter% route_ids)
  gtfs$trips <- subset(gtfs$trips, route_id %ffilter% route_ids)

  # shapes
  shape_ids <- unique(gtfs$trips$shape_id)
  gtfs$shapes <- subset(gtfs$shapes, shape_id %chin% shape_ids)

  # stop_times
  trip_ids <- unique(gtfs$trips$trip_id)
  gtfs$stop_times <- subset(gtfs$stop_times, trip_id %chin% trip_ids)

  # frequencies
  if(!is.null(gtfs$frequencies)){
    gtfs$frequencies <- subset(gtfs$frequencies, trip_id %chin% trip_ids)
    }

  # stops
  stop_ids <- unique(gtfs$stop_times$stop_id)
  gtfs$stops <- subset(gtfs$stops, stop_id %chin% stop_ids)

  return(gtfs)
}


