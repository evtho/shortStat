#' Summary Statistics Ignoring NAs and Non-Numeric Values
#'
#' @param x A vector containing numeric and/or non-numeric values.
#'
#' @return A list with the mean, median, standard deviation, minimum, and maximum of the numeric values in the vector.
#' @export
#' @importFrom stats median sd
#' @examples summary_stats(c(1, 2, "a", NA, 4, 5, "b", 5))
summary_stats <- function(x) {
  numeric_values <- as.numeric(x)
  numeric_values <- numeric_values[!is.na(numeric_values)]
  summary <- list(
    mean = mean(numeric_values, na.rm = TRUE),
    median = median(numeric_values, na.rm = TRUE),
    sd = sd(numeric_values, na.rm = TRUE),
    min = min(numeric_values, na.rm = TRUE),
    max = max(numeric_values, na.rm = TRUE)
  )
  return(summary)
}
