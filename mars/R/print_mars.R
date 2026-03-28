#' @export
print.mars <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(coef(x))
  invisible(x)
}
