#' Normalize Sex/Gender Values
#'
#' @param x  A character vector containing sex/gender values.
#'
#' @return A character vector with normalized sex/gender values.
#' @export
#'
#' @examples
#' uniform_sex(c("M", "D", "F", "W", "m", "d", "f", "w"))
uniform_sex <- function(x) {
  x <- as.character(x)

  valid_values <- c("M", "D", "F", "W", "m", "d", "f", "w")
  if (!all(x %in% valid_values)) {
    stop("Input vector must only contain valid sex/gender values (M, D, F, W, m, d, f, w).")
  }

  # Normalize values
  x[x == "M"] <- "m"
  x[x == "D"] <- "d"
  x[x == "F"] <- "f"
  x[x == "W" | x == "w"] <- "f"

  return(x)
}
