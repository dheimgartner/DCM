#' @export
simulate_mnl <- function(beta = c(ASC1 = 0, ASC2 = 1, beta11 = 0.5, beta12 = -0.5, beta21 = 2, beta22 = 3), n = 1000, seed = 0) {
  set.seed(seed)
  simulated_data <- list()

  k <- 2 # two alternatives
  simulated_data$beta <- beta

  x11 <- rnorm(n) # does not need to be normal!
  x12 <- rnorm(n)
  x21 <- rnorm(n)
  x22 <- rnorm(n)

  utilities <- matrix(NA, nrow = n, ncol = k)
  utilities[, 1] <- beta["ASC1"] + beta["beta11"] * x11 + beta["beta12"] * x12 + ordinal::rgumbel(n)
  utilities[, 2] <- beta["ASC2"] + beta["beta21"] * x21 + beta["beta22"] * x22 + ordinal::rgumbel(n)

  random_matrix <- matrix(runif(length(utilities)), nrow = nrow(utilities))

  probabilities <- t(apply(utilities, 1, function(u) exp(u) / sum(exp(u))))
  choice <- max.col(probabilities)
  simulated_data$data <- data.frame(x11, x12, x21, x22, choice)

  return(simulated_data)
}
