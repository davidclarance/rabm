



#' Extract data for one or multiple species
#'
#' @param species_ids A character or a vector of characters that represent the id or ids of the species. Example: observer_numbers = '10723' or observer_numbers = c('10723', '40147')
#' @param start_date A character representing the start date from which you want to query the database. It must be in the YYYY-MM-DD format. Defaults to 1 Jan,2019. Example: '2019-01-26'.
#' @param end_date A character representing the end date to which you want to query the database. It must be in the YYYY-MM-DD format. Defaults to 1 Feb, 2019. Example: '2019-02-26'.
#' @param region_type Can take one of the following types: country, province, pentad, group, qdgc. Default is 'country'
#' @param region_ids A character or a vector of characters that represent the id or ids of the locations. Instantiation of (i.e. co-defined with) `region_type`. If the `region_type` = 'country', then the `region_id` must be one of the following: 'kenya', 'southafrica', 'nigeria', 'botswana', 'namibia', 'zimbabwe', 'lesotho', 'swaziland', 'mozambique'. If `region_type` = 'province', then ... . If `region_type` = 'pentad', then  `region_id` is the pentad code (e.g. '3355_1825'). If `region_type` = 'group', then  `region_id` is the group code (e.g. 'BBD'). If `region_type` = 'qdgc', then  `region_id` is the qdgccode (e.g. '???'). Default is 'kenya'.
#' @param return_type There are two options, 'data' and 'count'. 'data' is the actual records submitted to the africa bird map. 'count' return a count of the rows. Default is 'data'
#'
#' @export
#'
#' @return Depending on the return_format, the function will return a dataframe with the following:
#' 1. `return_type = data` A tidy dataframe is returned with the records that fall within the specification defined.
#' 2. `return_type = count` A tidy dataframe is returns with the number of rows that will be produced if the return_type was to be specified as 'data'.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Extract data for a single specie (variable sunbird) and location (Kenya)
#'
#' extract_species(
#' species_ids = '762',
#' start_date = '2019-01-01',
#' end_date = '2019-02-01',
#' region_type = 'country',
#' region_ids = 'kenya',
#' return_type = 'data',
#' )
#'
#'
#' # Extract data for multiple species (variable sunbird and collared sunbird) and location
#' extract_species(species_ids = c('762', '771'),
#' start_date = '2019-01-01',
#' end_date = '2019-02-01',
#' region_type = 'pentads',
#' region_ids = c('0115_3640', '0140_3640'),
#' return_type = 'data',
#' )
#'
#'
#' }
#'
#'
extract_species <- function(species_ids,
                            start_date = '2019-01-01',
                            end_date = '2019-12-01',
                            region_type = 'country',
                            region_ids = 'kenya',
                            return_type = 'data') {
  # Check arguments
  Check <- ArgumentCheck::newArgCheck()
  if (is.na(as.Date(start_date, format = "%Y-%m-%d"))) {
    ArgumentCheck::addError(msg = "'start_date' is incorrectly specified. Please use the YYYY-MM-DD format",
                            argcheck = Check)
  }
  if (is.na(as.Date(end_date, format = "%Y-%m-%d"))) {
    ArgumentCheck::addError(msg = "'end_date' is incorrectly specified. Please use the YYYY-MM-DD format",
                            argcheck = Check)
  }
  if (!is.character(region_type) ||
      !region_type %in% c('country', 'province', 'pentad', 'group', 'qdgc')) {
    ArgumentCheck::addError(msg = "'region_type' is incorrectly specified. Please use one of the following: 'country', 'province', 'pentad', 'group', 'qdgc'",
                            argcheck = Check)
  }
  if (!is.character(return_type) ||
      !return_type %in% c('data', 'count')) {
    ArgumentCheck::addError(msg = "'end_date' is incorrectly specified. Please use 'data' or 'count'",
                            argcheck = Check)
  }
  ArgumentCheck::finishArgCheck(Check)


  # pull many observer numbers
  a <- purrr::map_df(species_ids, function(species_id) {
    purrr::map_df(region_ids, function(region_id) {
      pull_single_specie_location(species_id,
                                  start_date,
                                  end_date,
                                  region_type,
                                  region_id,
                                  return_type)
    })
  })

}
