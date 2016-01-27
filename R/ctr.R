#' Mean center variable
#' @param x A numeric vector
#' @return A mean centered vector
#' @export
ctr <- function(x, ...) {
  x - mean(x, na.rm = TRUE)
}
