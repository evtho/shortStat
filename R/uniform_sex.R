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
  x <- tolower(as.character(x))

  valid_values <- c("m", "d", "f", "w")
  if (!all(x %in% valid_values)) {
    stop("Input vector must only contain valid sex/gender values (M, D, F, W, m, d, f, w).")
  }

  x[x == "w"] <- "f"

  return(x)
}

