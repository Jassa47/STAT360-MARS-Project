new_mars.control <- function(control) {
  structure(control, class = "mars.control")
}

validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax),
            is.numeric(control$d),
            is.logical(control$trace))
  if (control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2L
  }
  if (control$Mmax %% 2 > 0) {
    control$Mmax <- 2L * ceiling(control$Mmax / 2L)
    warning("Mmax should be an even integer. Reset it to ", control$Mmax)
  }
  control
}

mars.control <- function(Mmax = 2, d = 3, trace = FALSE) {
  Mmax    <- as.integer(Mmax)
  control <- list(Mmax = Mmax, d = d, trace = trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}

h <- function(x, s, t) {
  pmax(0, s * (x - t))
}

init_B <- function(N, Mmax) {
  B        <- as.data.frame(matrix(0, nrow = N, ncol = Mmax + 1))
  B[, 1]   <- 1
  names(B) <- c("B0", paste0("B", 1:Mmax))
  B
}

split_points <- function(xv, Bm) {
  out <- sort(unique(xv[Bm > 0]))
  out[-length(out)]
}

LOF <- function(form, data, control) {
  ff     <- lm(form, data)
  RSS    <- sum(residuals(ff)^2)
  N      <- nrow(data)
  M      <- length(coef(ff)) - 1
  Ctilde <- sum(hatvalues(ff)) + control$d * M
  RSS * N / (N - Ctilde)^2
}
fwd_stepwise <- function(y, x, control = mars.control()) {
  N    <- length(y)
  n    <- ncol(x)
  Mmax <- control$Mmax

  B      <- init_B(N, Mmax)
  Bfuncs <- vector("list", length = Mmax + 1)
  Bfuncs[[1]] <- NULL

  for (i in 1:(Mmax / 2)) {
    M          <- 2 * i - 1
    lof_best   <- Inf
    split_best <- NULL

    for (m in 1:M) {
      if (is.null(Bfuncs[[m]])) {
        svars <- 1:n
      } else {
        svars <- setdiff(1:n, Bfuncs[[m]][, "v"])
      }

      if (control$trace) cat("M", M, "m", m, "svars", svars, "\n")

      for (v in svars) {
        tt <- split_points(x[, v], B[, m])
        if (length(tt) == 0) next

        for (t in tt) {
          Bnew <- data.frame(B[, 1:M],
                             Btem1 = B[, m] * h(x[, v],  1, t),
                             Btem2 = B[, m] * h(x[, v], -1, t))
          gdat <- data.frame(y = y, Bnew)
          lof  <- LOF(y ~ ., gdat, control)

          if (lof < lof_best) {
            lof_best   <- lof
            split_best <- c(m = m, v = v, t = t)
          }
        }
      }
    }

    mstar <- unname(split_best["m"])
    vstar <- unname(split_best["v"])
    tstar <- unname(split_best["t"])

    if (control$trace)
      cat(sprintf("  best (m,v,t,lof): (%d,%d,%.4f,%.4f)\n",
                  mstar, vstar, tstar, lof_best))

    B[, M + 1] <- B[, mstar] * h(x[, vstar], -1, tstar)
    B[, M + 2] <- B[, mstar] * h(x[, vstar],  1, tstar)

    Bfuncs[[M + 1]] <- rbind(Bfuncs[[mstar]], c(s = -1, v = vstar, t = tstar))
    Bfuncs[[M + 2]] <- rbind(Bfuncs[[mstar]], c(s =  1, v = vstar, t = tstar))
  }

  colnames(B) <- paste0("B", 0:Mmax)
  list(y = y, B = as.data.frame(B), Bfuncs = Bfuncs)
}
bwd_stepwise <- function(fwd, control) {
  y      <- fwd$y
  B      <- fwd$B
  Bfuncs <- fwd$Bfuncs
  Ncols  <- ncol(B)

  J_star   <- 1:Ncols
  lof_star <- LOF(y ~ . - 1,
                  data = cbind(y = y, B[, J_star, drop = FALSE]),
                  control)
  J_best   <- J_star

  for (M in Ncols:2) {
    b_best <- Inf
    K_best <- NULL

    for (m in 2:M) {
      K   <- J_star[-m]
      dat <- cbind(y = y, B[, K, drop = FALSE])
      lof <- LOF(y ~ . - 1, data = dat, control)

      if (lof < b_best) {
        b_best <- lof
        K_best <- K
      }
      if (lof < lof_star) {
        lof_star <- lof
        J_best   <- K
      }
    }
    J_star <- K_best
  }

  list(y      = y,
       B      = B[, J_best, drop = FALSE],
       Bfuncs = Bfuncs[J_best])
}

