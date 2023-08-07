#' Simulate ordered logit data
#'
#' Can be combined with `simulate_lv()` to simulate hybrid choice data
#'
#' @param X data.frame (or matrix)
#' @param betas named coef vector
#' @param taus named thresholds
#' @param n number of obs
#'
#' @return
#' @export
#'
#' @examples
#' n <- 10e3
#' X <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
#' betas <- c(beta1 = 1, beta2 = 2)
#' taus <- c(tau1 = -1, tau2 = 1) # will generate 3 choices
#' simulate_ol(X, betas, taus, n)
simulate_ol <- function(X, betas, taus, n) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  error <- rlogis(n)
  utility <- X %*% betas + error
  choice <- cut(utility, breaks = c(-Inf, taus, Inf))
  levels(choice) <- 1:(length(taus) + 1)
  choice <- as.numeric(as.character(choice))
  dat <- data.frame(
    X, choice
  )
  dat
}
