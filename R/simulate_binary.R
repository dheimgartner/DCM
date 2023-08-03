#' @export
simulate_binary <- function(beta = c(ASC = 0, beta1 = 1, beta2 = 2), n = 1000, seed = 0) {
  set.seed(seed)
  simulated_data <- list()

  simulated_data$beta <- beta

  x1 <- rnorm(n) # does not need to be normal!
  x2 <- rnorm(n)

  latent <- beta["ASC"] + beta["beta1"] * x1 + beta["beta2"] * x2
  choice <- as.numeric(latent + rlogis(n) > 0)

  simulated_data$data <- data.frame(x1, x2, choice)

  return(simulated_data)
}
