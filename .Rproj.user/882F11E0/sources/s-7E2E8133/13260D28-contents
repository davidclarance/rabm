#' Export data from the Africa Bird Map
#'
#' Pull data from the African Bird Map fit for reporting rate and other analyses. This function is temporary and will be made redundant. It is unsafe and is only for demonstrative purposes.
#'
#' The function requires a username and password in plain text. This is not how the final function will look. The final function will call the RStudio API for a safe transaction. This is just for demonstrative purposes.
#'
#'
#' @param username The citizen scientist's bird map email
#' @param user_id The citizen scientist's bird map user number (usually 5 digits)
#' @param password The citizen scientist's password.
#' @param species_id The species_id for which data is extracted. A complete list of species name and ids are available on the Kenya Bird Map website.
#' @export
#' @return A dataframe with every list submitted to Africa Bird Map (includes all countries).
#' @examples
#'
#' \dontrun{
#'
#'# Extract data for the African Black-shouldered kite.
#'
#' extract_data(username = "dclarance@gmail.com" ,user_id = 40664, password = "abcd", species_id = 103s)
#'
#' }
#'
#'
extract_data <- function(username,
                         user_id,
                         password,
                         species_id){

  # put check to see if path contains .csv

  password=digest(password, "md5", serialize = FALSE)

  api_key ='32859abe3dc684a266846283cbd2264d'

  ip='127.0.0.1'

  #Species ref number
  spp=species_id

  #login to the database
  login_url=paste('http://api.adu.org.za/validation/user/login?API_KEY=',api_key,'&sourceIP=',ip,'&userid=',user_id,'&user_email=',username,'&passid=',password, sep="")
  login <- RJSONIO::fromJSON(login_url)

  print(login)

  #extracting the data
  url <- paste('http://api.adu.org.za/sabap2/v2/cards/species/',
               login$registered$status[3],'/',spp,'?format=csv',sep="")

  myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

  mydata <- read_csv(myfile)

  mydata

}
