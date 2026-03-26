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
  B        <- as.data.frame(matrix(NA_real_, nrow = N, ncol = Mmax + 1))
  B[, 1]   <- 1
  names(B) <- c("B0", paste0("B", 1:Mmax))
  B
}

split_points <- function(xv, Bm) {
  out <- sort(unique(xv[Bm > 0]))
  out[-length(out)]
}
