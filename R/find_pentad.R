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
#' coordinates <- data.frame(lon = -122.335167, lat = 47.608013)
#' pentad <- find_pentad(coordinates)
#'
#' coordinates <- data.frame(lon = c(-10,10,10,-10),lat = c(10,-10,10,-10))
#' pentad <- find_pentad(coordinates)
#'
#'
find_pentad <- function(coordinates) {

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
  ArgumentCheck::finishArgCheck(Check)


  format_pentad <- function(x) {
    formatC(floor(abs(x+0.0001)/0.2)*0.2*100, width=4, flag="0")
  }
  format_letter <- function(lat,lon){
    if (abs(lon)>100){
      stop("Longitude cannot be less than -100 or greater than 100")
    }
    if (lat < 0 & lon > 0){
      letter <- '_'
    } else if (lat < 0 & lon < 0) {
      letter <- 'a'
    } else if (lat > 0  & lon < 0) {
      letter <- 'b'
    } else if (lat > 0 & lon > 0) {
      letter <- 'c'
    }
    letter
  }

  coordinates %>% rowwise() %>%
    mutate(lat = ifelse(is.factor(lat), as.numeric(levels(lat)), lat)) %>%
    mutate(lon = ifelse(is.factor(lon), as.numeric(levels(lon)), lon)) %>%
    mutate(letter = format_letter(lat,lon)) %>%
    mutate(pentad = paste(format_pentad(lat),letter,format_pentad(lon), sep='')) %>%
    pull()

}






