#' Calculate Mean Ignoring NAs and Non-Numeric Values
#'
#' @param x A vector containing numeric and/or non-numeric values.
#'
#' @return The mean of the numeric values in the vector.
#' @export
#'
#' @examples
#' mean_na(c(1, 2, "a", NA, 4, 5, "b", 5))
mean_na <- function(x) {
  numeric_values <- as.numeric(x)
  return(mean(numeric_values, na.rm = TRUE))
}


