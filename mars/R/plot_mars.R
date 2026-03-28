#' @export
plot.mars <- function(x, ...) {
  object  <- x
  Bfuncs  <- object$Bfuncs
  x_names <- object$x_names
  B_names <- names(object$B)

  get_range <- function(v) {
    ts <- unlist(lapply(Bfuncs, function(bf) {
      if (!is.null(bf) && nrow(bf) > 0) bf[bf[,"v"] == v, "t"] else NULL
    }))
    if (length(ts) == 0) return(c(-1, 1))
    span <- diff(range(ts))
    if (span == 0) span <- 1
    c(min(ts) - 0.5*span, max(ts) + 0.5*span)
  }

  for (m in seq_along(Bfuncs)) {
    bf <- Bfuncs[[m]]
    if (is.null(bf) || nrow(bf) == 0) next

    n_vars <- length(unique(bf[,"v"]))
    bname  <- B_names[m]

    if (n_vars == 1) {
      v    <- bf[1,"v"]
      rng  <- get_range(v)
      xseq <- seq(rng[1], rng[2], length.out = 300)
      bvals <- rep(1, 300)
      for (k in seq_len(nrow(bf)))
        bvals <- bvals * h(xseq, bf[k,"s"], bf[k,"t"])
      plot(xseq, bvals, type="l", lwd=2,
           xlab=x_names[v],
           ylab=paste0(bname,"(",x_names[v],")"),
           main=paste0("Basis function ", bname), ...)

    } else if (n_vars == 2) {
      vs <- unique(bf[,"v"])
      v1 <- vs[1]; v2 <- vs[2]
      xseq1 <- seq(get_range(v1)[1], get_range(v1)[2], length.out=50)
      xseq2 <- seq(get_range(v2)[1], get_range(v2)[2], length.out=50)
      zmat <- outer(xseq1, xseq2, FUN=function(a,b) {
        bval <- rep(1, length(a))
        for (k in seq_len(nrow(bf))) {
          xvk  <- if (bf[k,"v"]==v1) a else b
          bval <- bval * h(xvk, bf[k,"s"], bf[k,"t"])
        }
        bval
      })
      graphics::persp(xseq1, xseq2, zmat,
                      xlab=x_names[v1], ylab=x_names[v2], zlab=bname,
                      main=paste0("Basis function ", bname),
                      theta=30, phi=20, col="lightblue", shade=0.5, ...)
    }
  }
  invisible(NULL)
}
