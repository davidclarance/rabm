

#' Extract data for one or multiple observers
#'
#' @param observer_numbers A character or a vector of characters that represent the ADU number or numbers that you want to query the database for. Example: observer_numbers = '10723' or observer_numbers = c('10723', '40147')
#' @param start_date A character representing the start date from which you want to query the database. It must be in the YYYY-MM-DD format. Defaults to 1 Jan,2019. Example: '2019-01-26'.
#' @param end_date A character representing the end date to which you want to query the database. It must be in the YYYY-MM-DD format. Defaults to 1 Feb, 2019. Example: '2019-02-26'.
#' @param region_type Can take one of the following types: country, pentad, group, qdgc. Default is 'country'
#' @param region_id Has to be co-defined with `region_type`. That is, it is an instantiation of the `region_type`. For instance if the `region_type` = 'country', then the `region_id` = 'kenya'. Or if `region_type` = 'pentad', then  `region_id` = '3355_1825'. Default is 'kenya'.
#' @param return_type There are two options, 'data' and 'count'. 'data' is the actual records submitted to the africa bird map. 'count' return a count of the rows. Default is 'data'.
#' @param return_format Default s 'CSV', though the API default is 'JSON'
#'
#' @return Depending on the return_format, the function will return a dataframe with the following:
#' 1. `return_type = data` A tidy dataframe is returned with the records that fall within the specification defined.
#' 2. `return_type = count` A tidy dataframe is returns with the number of rows that will be produced if the return_type was to be specified as 'data'.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Extract data with single observers.
#'
#' extract_observers(
#' observer_numbers = 10723,
#' start_date = '2019-01-01',
#' end_date = '2019-02-01',
#' region_type = 'country',
#' region_id = 'kenya',
#' return_type = 'data',
#' return_format = 'CSV'
#' )
#'
#'
#' # Extract data for multiple observers
#' extract_observers(observer_numbers = c('10723', '40147'),
#' start_date = '2019-01-01',
#' end_date = '2019-02-01',
#' region_type = 'country',
#' region_id = 'kenya',
#' return_type = 'data',
#' return_format = 'CSV'
#' )
#'
#'
#' }
#'
#'
extract_observers <- function(observer_numbers,
                              start_date = '2019-01-01',
                              end_date = '2019-02-01',
                              region_type = 'country',
                              region_id = 'kenya',
                              return_type = 'data',
                              return_format = 'CSV') {

  # requirements on inputs

  if (is.na(as.Date(start_date, format = "%Y-%m-%d"))) {
    stop(message = "start_date is incorrectly specified. Please use the YYYY-MM-DD format")

  }


  if (is.na(as.Date(end_date, format = "%Y-%m-%d"))) {
    stop(message = "end_date is incorrectly specified. Please use the YYYY-MM-DD format")

  }


  region_type = tolower(region_type)
  region_id = tolower(region_id)
  return_type = tolower(return_type)


  # get number of records to be returned

  all_observers = paste(observer_numbers, collapse = ",")

  return_data_count <- readr::read_csv(
    glue::glue(
      "http://api.adu.org.za/sabap2/v2/R/{start_date}/{end_date}/{region_type}/{region_id}/count/observers/{all_observers}?format={return_format}"
    ),
    col_types = readr::cols()
  )

  if (return_data_count$records[1] > 250000) {
    stop(message = "Your query returns more than 250,000 records. The API does not allow more than 250,000 records to be drawn at once. Please split your query.")

  }



  # pull for single observer number
  pull_for_observer <- function(observer_number) {
    print(glue::glue("Pulling data for {observer_number}"))
    readr::read_csv(
      glue::glue(
        "http://api.adu.org.za/sabap2/v2/R/{start_date}/{end_date}/{region_type}/{region_id}/{return_type}/observers/{observer_number}?format={return_format}"
      ),
      col_types = readr::cols()
    )
  }


  # pull many observer numbers
  purrr::map_df(observer_numbers, pull_for_observer)


}

