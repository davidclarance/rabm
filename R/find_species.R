#' Find a specific species or a group of species from a species list
#'
#' @param species_list The output of `get_species_list`
#' @param species_names A character or a vector of characters to search for in a species list,
#'
#' @return A subset of the output of `species_list` that produces the best match for the `species_names` provided.
#' @export
#'
#' @examples
#'
#' # get species list for kenya
#' kenya_list <- get_species_list("kenya")
#'
#' # specify the species you want to search for
#' interesting_species <- c("drongo", "sunbird")
#'
#' # find interesting species in the species list
#' find_species(species_list = kenya_list, species_name = interesting_species)
#'
#' @import magrittr
find_species <- function(species_list, species_names) {
  df_to_search <- species_list %>%
    dplyr::mutate(SearchColumn = as.character(tolower(
      glue::glue(
        "{Common_group} {Common_species} {Genus} {Species} {Common_species} {Common_group} {Species} {Genus}"
      )
    )))


  species_names %>%
    purrr::map_df( ~ dplyr::filter(
      df_to_search,
      stringr::str_detect(df_to_search$SearchColumn, tolower(.x))
    ))
}






