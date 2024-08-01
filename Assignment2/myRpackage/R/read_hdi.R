#' Read and Process Human Development Indicators Data
#'
#' This function reads the Human Development Indicators data from a CSV file,
#' processes the data by setting appropriate column types.
#' @param file A character string specifying the path to the CSV file.
#' @return A data.frame object.
#' @examples
#' file <- system.file("extdata", "hdro_indicators_chn.csv", package = "myRpackage")
#' hdi_data <- read_hdi(file)
#' @export
read_hdi <- function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  data <- data[-1, ] # Remove the first row
  data$year <- as.integer(data$year)
  data$value <- as.numeric(data$value)
  data$country_code <- as.factor(data$country_code)
  data$country_name <- as.factor(data$country_name)
  data$indicator_id <- as.factor(data$indicator_id)
  data$indicator_name <- as.factor(data$indicator_name)
  data$index_id <- as.factor(data$index_id)
  data$index_name <- as.factor(data$index_name)
  return(data)
}
