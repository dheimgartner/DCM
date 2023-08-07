#' Simulate latent variable
#'
#' @param X
#' @param betas
#' @param taus
#' @param zetas
#' @param n
#'
#' @return
#' @export
#'
#' @examples
#' n <- 10e3
#' X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
#' betas <- c(beta1 = 1, beta2 = 2)
#' taus <- c(tau1 = -1, tau2 = 1)
#' zetas <- c(zeta1 = 1, zeta2 = 2)
#' simulate_lv(X, betas, taus, zetas, n)
simulate_lv <- function(X, betas, taus, zetas, n) {
  out <- list()
  lv <- simulate_structural(X = X, betas = betas, n = n)
  mm <- simulate_measurements(lv = lv, taus = taus, zetas = zetas, n = n)
  dat <- data.frame(
    X, mm
  )
  out$lv <- lv
  out$obs <- dat
  out
}

simulate_structural <- function(X, betas, n) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  lv <- X %*% betas + rnorm(n)
  lv
}

simulate_measurements <- function(lv, taus, zetas, n) {
  if (!is.matrix(lv)) {
    lv <- as.matrix(lv)
  }
  measurements <- lv %*% zetas + rlogis(n)
  measurements <- apply(measurements, 2, function(x) {
    y <- cut(x, breaks = c(-Inf, taus, Inf))
    levels(y) <- 1:(length(taus) + 1)
    as.numeric(as.character(y))
  })
  colnames(measurements) <- paste0("ind", 1:ncol(measurements))

  measurements
}
