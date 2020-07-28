#' Find the GPS coordinate of pentad code
#'
#' @param pentad_codes A list of pentad codes
#'
#' @return list of pentad code string
#'
#' @export
#'
#' @examples
#'
#' # get pentad for a list of coordinate
#' coordinates <- data.frame(lon = -122.335167, lat = 47.608013)
#' pentad <- find_pentad(coordinates)
#'
#' coordinates <- data.frame(lon = c(-10,10,10,-10),lat = c(10,-10,10,-10))
#' pentad <- find_pentad(coordinates)
#'
#'
find_pentad_coordinates <- function(pentad_codes, plotting=FALSE) {

  Check <- ArgumentCheck::newArgCheck()
  #if (typeof(pentad_codes) != 'list') {
  #  ArgumentCheck::addError(msg = "`pentad` needs to be a list",
  #                          argcheck = Check)
  #}
  ArgumentCheck::finishArgCheck(Check)


  coordinates <- data.frame(pentad_codes = pentad_codes) %>%
    dplyr::mutate(letter = substr(pentad_codes, 5, 5)) %>%
    dplyr::mutate(
      lat = ifelse(
        (letter == '_' || letter == 'a'),
        -as.numeric(substr(pentad_codes, 1, 4)) / 100,
        as.numeric(substr(pentad_codes, 1, 4)) / 100),
      lon = ifelse(
        (letter == 'a' || letter == 'b'),
        -as.numeric(substr(pentad_codes, 6, 9)) / 100,
        as.numeric(substr(pentad_codes, 6, 9)) / 100)
      )

  if (plotting){
    pentads_poly<-list()
    for (i in 1:nrow(coordinates)){
      pentads_poly[[i]] <- sp::Polygons(list(
        sp::Polygon( cbind(
          c(coordinates[i,'lat'], coordinates[i,'lat']+0.2, coordinates[i,'lat']+0.2, coordinates[i,'lat'], coordinates[i,'lat']),
          c(coordinates[i,'lon'], coordinates[i,'lon'], coordinates[i,'lon']+0.2, coordinates[i,'lon']+0.2, coordinates[i,'lon'])
        ))
      ), i)
    }
    pentads_poly <- sp::SpatialPolygons(pentads_poly)
    pid <- sapply(slot(pentads_poly, "polygons"), function(x) slot(x, "ID"))
    p.df <- data.frame( ID=1:length(pentads_poly), row.names = pid)
    p <- sp::SpatialPolygonsDataFrame(pentads_poly, p.df)
    p@data$pentad_code <- pentad_codes


    p %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(label = ~paste0(pentad_code))
  }

  coordinates %>%
    dplyr::select(lat, lon)
}
