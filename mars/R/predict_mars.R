#' @export
predict.mars <- function(object, newdata, ...) {
  if (missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  } else {
    X <- as.matrix(newdata[, object$x_names, drop = FALSE])
    B <- make_B(X, object$Bfuncs)
    colnames(B) <- names(object$B)   # ← add this
  }
  beta <- object$coefficients
  drop(B %*% beta)
}
#' @keywords internal
make_B <- function(X, Bfuncs) {
  N <- nrow(X)
  M <- length(Bfuncs)
  B <- matrix(1, nrow = N, ncol = M)

  for (m in seq_along(Bfuncs)) {
    bf <- Bfuncs[[m]]
    if (!is.null(bf)) {
      for (i in seq_len(nrow(bf))) {
        s      <- bf[i, "s"]
        v      <- bf[i, "v"]
        t      <- bf[i, "t"]
        B[, m] <- B[, m] * h(X[, v], s, t)
      }
    }
  }

  colnames(B) <- names(Bfuncs)
  B
}
