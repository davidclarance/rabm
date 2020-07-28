#' Extract all data of one observer at one location
#'
#' @param observer_id A character that represent the ADU number that you want to query the database for. Example: observer_id = '10723'
#' @param start_date A character representing the start date from which you want to query the database. It must be in the YYYY-MM-DD format. Defaults to 1 Jan,2019. Example: '2019-01-26'.
#' @param end_date A character representing the end date to which you want to query the database. It must be in the YYYY-MM-DD format. Defaults to 1 Feb, 2019. Example: '2019-02-26'.
#' @param region_type Can take one of the following types: country, province, pentad, group, qdgc. Default is 'country'
#' @param region_id A character that represent the id of the single location. Instantiation of (i.e. co-defined with) `region_type`. If the `region_type` = 'country', then the `region_id` must be one of the following: 'kenya', 'southafrica', 'nigeria', 'botswana', 'namibia', 'zimbabwe', 'lesotho', 'swaziland', 'mozambique'. If `region_type` = 'province', then ... . If `region_type` = 'pentad', then  `region_id` is the pentad code (e.g. '3355_1825'). If `region_type` = 'group', then  `region_id` is the group code (e.g. 'BBD'). If `region_type` = 'qdgc', then  `region_id` is the qdgccode (e.g. '???'). Default is 'kenya'.
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
#' pull_single_observer_location(
#' observer_id = '762',
#' start_date = '2019-01-01',
#' end_date = '2019-02-01',
#' region_type = 'country',
#' region_id = 'kenya',
#' return_type = 'data',
#' )
#'
#' }
#'
#'
pull_single_observer_location <- function(observer_id,
                                        start_date,
                                        end_date,
                                        region_type,
                                        region_id,
                                        return_type) {
  # Check Arguments of function
  Check <- ArgumentCheck::newArgCheck()
  if (length(observer_id) > 1 || is.na(as.numeric(observer_id))) {
    ArgumentCheck::addError(
      msg = "'observer_id' is incorrectly specified. Please use a single numeric or character of numeric",
      argcheck = Check
    )
  }
  if (length(region_id) > 1) {
    ArgumentCheck::addError(
      msg = "'region_id' is incorrectly specified. Please use a single character",
      argcheck = Check
    )
  }
  ArgumentCheck::finishArgCheck(Check)


  # Convert to lower case
  region_type = tolower(region_type)
  region_id = tolower(region_id)
  return_type = tolower(return_type)


  # get number of records to be returned
  return_data_count <- readr::read_csv(
    glue::glue(
      "http://api.adu.org.za/sabap2/v2/R/{start_date}/{end_date}/{region_type}/{region_id}/count/observers/{species_id}?format=csv"
    ),
    col_types = readr::cols()
  )$records

  if (return_data_count > 250000) {
    stop(message = "Your query returns more than 250,000 records. The API does not allow more than 250,000 records to be drawn at once. Please split your query.")
  }

  if (return_data_count > 0) {
    print(glue::glue("Pulling data for {species_id} and for location {region_id}"))
    readr::read_csv(
      glue::glue(
        "http://api.adu.org.za/sabap2/v2/R/{start_date}/{end_date}/{region_type}/{region_id}/{return_type}/observers/{species_id}?format=csv"
      ),
      col_types = readr::cols()
    ) %>% add_column(region_id = region_id)
  }
}
