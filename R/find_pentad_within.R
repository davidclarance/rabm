#' Find a specific species or a group of species from a species list
#'
#' @param coordinates A dataframe of coordinate with
#'
#' @return list of pentad string
#'
#' @export
#'
#' @examples
#'
#' # get pentad for a list of coordinate
#'
#' coordinates <- data.frame(lat = c(38.9, 38.9, 41, 41, 38.9), lon = c(-5, -2.5, -2.5, -5, -5))
#' pentads <- find_pentad_within(coordinates)
#'
#'
find_pentad_within <- function(coordinates,
                               plotting=FALSE) {

  Check <- ArgumentCheck::newArgCheck()
  if (typeof(coordinates) != 'list') {
    ArgumentCheck::addError(msg = "`coordinates` needs to be a dataframe/list",
                            argcheck = Check)
  }
  if (!("lat" %in% colnames(coordinates))) {
    ArgumentCheck::addError(msg = "`coordinates` needs to have a column `lat`",
                            argcheck = Check)
  }
  if (!("lon" %in% colnames(coordinates))) {
    ArgumentCheck::addError(msg = "`coordinates` needs to have a column `lon`",
                            argcheck = Check)
  }
  if ( !is.logical(plotting) ) {
    ArgumentCheck::addError(msg = "`plotting` needs to be `TRUE` or `FALSE`",
                            argcheck = Check)
  }
  ArgumentCheck::finishArgCheck(Check)

  # Rounding to the grid resolution (0.2°)
  format_pentad <- function(x) {
    sign(x)*floor(abs(x)/0.2)*0.2
  }

  # Generate a list of all possible pentads covering the extremum of the coordinates queries
  pentads_possible<-list()
  i <- 1
  for (i_lat in seq( format_pentad(min(coordinates$lat)), format_pentad(max(coordinates$lat)), 0.2)){
    for (i_lon in seq( format_pentad(min(coordinates$lon)), format_pentad(max(coordinates$lon)), 0.2)){
      pentads_possible[[i]] <- sp::Polygons(list(
        sp::Polygon( cbind(
          c(i_lat, i_lat+0.2, i_lat+0.2, i_lat, i_lat),
          c(i_lon, i_lon, i_lon+0.2, i_lon+0.2, i_lon)
          ))
      ), i)
      i <- i + 1
    }
  }

  # Create the SpatialPolygons object of the possible pentads
  pentads_possible_poly <- sp::SpatialPolygons(pentads_possible)

  # Create the SpatialPolygons object of coordinates queries
  coordinates_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coordinates)), 1)))

  # Check which possible pentads overlap with the coordinate
  pentads_id <- !is.na(sp::over(pentads_possible_poly, coordinates_poly))

  # Select only the pentads which are inside
  pentads_inside <- pentads_possible_poly[pentads_id]


  pentads_coord <- sp::coordinates(pentads_inside)+0.1

  pid <- sapply(slot(pentads_inside, "polygons"), function(x) slot(x, "ID"))
  p.df <- data.frame( ID=1:length(pentads_inside), row.names = pid)
  p <- sp::SpatialPolygonsDataFrame(pentads_inside, p.df)
  p@data$pentad_name <- find_pentad(data.frame(lat=pentads_coord[,1],lon=pentads_coord[,2]))



  if (plotting){
    leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(data=p,
                           label = ~paste0(pentad_name)) %>%
      leaflet::addPolygons(data=coordinates_poly, fill = FALSE, color = 'red')
  }

  p@data$pentad_name
}






