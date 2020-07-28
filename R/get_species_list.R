#' Get species list for a country
#'
#' @param country Full name for country
#'
#' @return Dataframe with the species ids, common names and scientific names of species in the defined country. Not when a species is split, the old species ID is discontinued and two (or more) new species ids are created.
#'
#' @export
#'
#' @examples
#'
#' # get species list for Nigerian
#'
#' get_species_list(country = "Nigeria")
#'
#'
get_species_list <- function(country) {

  country <- tolower(country)

  valid_countries <- c("kenya", "southafrica", "nigeria", "botswana", "namibia", "zimbabwe", "lesotho", "swaziland", "mozambique")

  if(! (country %in% valid_countries))  {
    stop(message =  paste("Sorry; currently this call is only supported for: ", paste(valid_countries, collapse = ', ')))
  }

  readr::read_csv(
    glue::glue(
      "http://api.adu.org.za/sabap2/v2/monthly/species/country/{country}?format=csv"
    ),
    col_types = readr::cols_only(
      Spp = readr::col_integer(),
      Common_group = readr::col_character(),
      Common_species = readr::col_character(),
      Genus = readr::col_character(),
      Species = readr::col_character()
    )
  )
}

