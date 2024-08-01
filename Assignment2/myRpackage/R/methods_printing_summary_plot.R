#' Print method for hdi_data
#'
#' @param x An object of class `hdi_data`.
#' @param ... Additional arguments (currently unused).
#' @export
print.hdi_data <- function(x, ...) {
  cat("Human Development Indicators Data\n")
  print(head(x$data), ...)
}

#' Summary method for hdi_data
#'
#' @param object An object of class `hdi_data`.
#' @param ... Additional arguments (currently unused).
#' @export
summary.hdi_data <- function(object, ...) {
  cat("Summary of Human Development Indicators Data\n")
  summary(object$data, ...)
}

#' Plot method for hdi_data
#'
#' @param x An object of class `hdi_data`.
#' @param ... Additional arguments for the plot function.
#' @export
plot.hdi_data <- function(x, ...) {
  library(ggplot2)
  ggplot(x$data, aes(x = year, y = value, color = country_name)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Human Development Index Over Time", x = "Year", y = "HDI Value")
}
