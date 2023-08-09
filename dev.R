library(tidyverse)
library(plotly)

n_draws <- 1000
n_rows <- nrow(dat)
draws_matrix <- matrix(rlogis(n_draws * n_rows), nrow = n_rows, ncol = n_draws)

counter <- 0
loglik <- function(param, epsilon = 1e-10) {
  # 10000 x 1
  latent <- param["ASC"] + param["beta1"] * dat$x1 + param["beta2"] * dat$x2

  # 10000 x n_draws
  indicator_ <- latent + draws_matrix

  indicator <- (indicator_ > 0)
  P_1n <- apply(indicator, 1, mean)
  P_1n <- pmax(pmin(P_1n, 1 - epsilon), epsilon)
  P_0n <- 1 - P_1n

  y_1n <- dat$choice
  y_0n <- 1 - y_1n

  ll <- sum(y_1n * log(P_1n) + y_0n * log(P_0n))
  if (counter %% 100 == 0)
    cat("\n", counter, "\n")
  counter <<- counter + 1
  cat("=")

  ll
}

beta1 <- seq(0, 3, 0.1)
beta2 <- seq(1, 4, 0.1)
betas <- expand.grid(beta1, beta2)
coefs <- data.frame(asc = 0, beta1 = betas[, 1], beta2 = betas[, 2])

coefs <- coefs %>%
  rowwise() %>%
  mutate(p = list(p(c(asc, beta1, beta2)))) %>%
  ungroup() %>%
  mutate(ll = unlist(map(p, ~ loglik(.x)))) %>%
  select(beta1, beta2, ll)

plotly::plot_ly(data = coefs, x = ~beta1, y = ~beta2, z = ~ll) %>%
  add_trace(type = "mesh3d", intensity = ~ll)

