#' Title
#'
#' @param x 
#' @param name 
#' @param value 
#' @param where 
#'
#' @return
#'
#' @examples
add_col <- function(x, name, value, where = ncol(x) + 1) {
  if (name %in% names(x)) {
    x[[name]] <- value
    x
  } else {
    df <- setNames(data.frame(value), name)
    insert_into(x, df, where = where)
  }
}



