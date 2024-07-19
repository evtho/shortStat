#' Calculate Mode Ignoring NAs and Non-Numeric Values
#'
#' @param x A vector containing numeric and/or non-numeric values.
#'
#' @return The mode of the numeric values in the vector.
#' @export
#'
#' @examples
#' mode_na(c(1, 2, "a", NA, 4, 5, "b", 5))
mode_na <- function(x) {
  numeric_values <- as.numeric(x)
  numeric_values <- numeric_values[!is.na(numeric_values)]
  uniq_x <- unique(numeric_values)
  return(uniq_x[which.max(tabulate(match(numeric_values, uniq_x)))])
}
