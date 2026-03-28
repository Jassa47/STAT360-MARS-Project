#' @export
summary.mars <- function(object, ...) {
  Bfuncs  <- object$Bfuncs
  x_names <- object$x_names
  B_names <- names(object$B)

  cat("Basis functions:\n")
  for (m in seq_along(Bfuncs)) {
    bf <- Bfuncs[[m]]
    cat("  ", B_names[m], ":\n", sep = "")
    if (is.null(bf)) {
      cat("    Intercept\n")
    } else {
      for (i in seq_len(nrow(bf))) {
        cat(sprintf("  Component %d:  variable %s; sign %d; knot at %.4f\n",
                    i, x_names[bf[i,"v"]], as.integer(bf[i,"s"]), bf[i,"t"]))
      }
    }
  }
  cat("\n")

  lm_sum <- summary.lm(object, ...)
  print(lm_sum)
  invisible(lm_sum)
}
