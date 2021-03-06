#' @title Filter GTFS data by route type (transport mode)
#'
#' @description
#' Filter a GTFS data byits route type (transport mode), subsetting routes
#' and trips. It also removes the unnecessary stop_times, shapes, frequencies
#' (if exist in a feed), and stops accordingly.
#'
#' @param gtfs A GTFS object as created by \code{\link{read_gtfs}}.
#' @param route_types A vector of route types belonging to the routes of the
#'                    GTFS data. Each transport mode is associated with a
#'                    `route_type` number. See Details.
#' @param keep Logical. Whether the passed `route_ids` should be kept (Defaults
#'             to `TRUE`), or whether they should be dropped.
#'
#' @return Returns a GTFS object with filtered routes, trips, shapes, stops,
#'         stop_times and frequencies.
#'
#' @details
#' Route types:
#' |**`route_type`**|**Transport mode**| Note |
#' |-----|-----|-----|
#' |0  | Tram, Streetcar, Light rail | Any light rail or street level system within a metropolitan area |
#' |1  | Subway, Metro | Any underground rail system within a metropolitan area |
#' |2  | Rail | Used for intercity or long-distance travel |
#' |3  | Bus | Used for short- and long-distance bus routes |
#' |4  | Ferry | Used for short- and long-distance boat service |
#' |5  | Cable tram | Used for street-level rail cars where the cable runs beneath the vehicle, e.g., cable car in San Francisco |
#' |6  | Aerial lift, suspended cable car (e.g., gondola lift, aerial tramway) | Cable transport where cabins, cars, gondolas or open chairs are suspended by means of one or more cables |
#' |7  | Funicular | Any rail system designed for steep inclines |
#' |11 |  Trolleybus | Electric buses that draw power from overhead wires using poles |
#' |12 |  Monorail | Railway in which the track consists of a single rail or a beam |
#'
#' @examples
#' # read gtfs
#' data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
#' gtfs <- read_gtfs(data_path)
#'
#'# filter: Keep selected route types
#' subset1 <- filter_route_type(gtfs, route_types=c(2, 3), keep = T)
#'
#'# filter: Drop selected route types
#' subset2 <- filter_route_type(gtfs, route_types=c(2, 3), keep = F)
#'
#' @family filter functions
#' @export
filter_route_type <- function(gtfs, route_types, keep = TRUE) {

  ## input checking
  checkmate::assert_logical(keep)
  checkmate::assert_class(gtfs, "dt_gtfs")
  checkmate::assert_vector(route_types)
  checkmate::assert_numeric(route_types)
  checkmate::assert_data_table(gtfs$routes, .var.name= 'gtfs$routes')
    ## I like this check below because it has a more informative error message
    # if(is.null(gtfs$routes)) {stop("GTFS data does not have routes")}

  # chech route_types values
  all_modes <- c(0,1,2,3,4,5,6,7,1,12)
  if( !all(route_types %in% all_modes)){
     stop(paste0("One of the route_types input does not exist. Values must be
                 among the following:", paste(all_modes, collapse = ", ")))
    }



  ## filter operator
  `%ffilter%` <- `%in%`
  if (keep==FALSE) { `%ffilter%` <- Negate(`%in%`) }

  ## start filtering gtfs
  # routes
  gtfs$routes <- subset(gtfs$routes, route_type %ffilter% route_types)

  # trips
  route_ids <- unique(gtfs$routes$route_id)
  gtfs$trips <- subset(gtfs$trips, route_id %chin% route_ids)

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


