devtools::load_all()

rm(list = ls())


sim_dat <- simulate_binary(n = 10000)
beta <- sim_dat$beta
dat <- sim_dat$data

fit <- glm(choice ~ x1 + x2, data = dat, family = binomial(link = "logit"))
summary(fit)


# Closed-form solution ----------------------------------------------------
loglik <- function(param) {
  latent <- param["ASC"] + param["beta1"] * dat$x1 + param["beta2"] * dat$x2
  exp <- exp(latent)
  P_1n <- exp / (1 + exp)
  P_0n <- 1 - P_1n

  y_1n <- dat$choice
  y_0n <- 1 - y_1n

  ll <- sum(y_1n * log(P_1n) + y_0n * log(P_0n))
  cat(ll, "\n")
  ll
}

p <- function(v) {
  v <- setNames(v, c("ASC", "beta1", "beta2"))
  v
}

param <- p(c(1, 1, 1))
m <- maxLik::maxLik(loglik, start = param, method = "BFGS")
summary(m)


hessian_function <- function(param) numDeriv::hessian(loglik, param)
m <- maxLik::maxLik(loglik, start = param, method = "BFGS", hess = hessian_function)
summary(m)

# Simulation (draws internal) ---------------------------------------------
loglik <- function(param, epsilon = 1e-10) {
  n_draws <- 1000
  latent <- param["ASC"] + param["beta1"] * dat$x1 + param["beta2"] * dat$x2

  set.seed(0)

  P_1n_ <- matrix(NA, nrow = nrow(dat), ncol = n_draws)
  for (i in seq(n_draws)) {
    # take a new random draw for each individual!
    error <- rlogis(nrow(dat))
    P_1n_[, i] <- as.numeric(latent + error > 0)
  }

  # average
  P_1n <- apply(P_1n_, 1, mean)
  P_1n <- pmax(pmin(P_1n, 1 - epsilon), epsilon)
  P_0n <- 1 - P_1n

  y_1n <- dat$choice
  y_0n <- 1 - y_1n

  ll <- sum(y_1n * log(P_1n) + y_0n * log(P_0n))
  cat(ll, "\n")
  ll
}

p <- function(v) {
  v <- setNames(v, c("ASC", "beta1", "beta2"))
  v
}

loglik(param)
loglik(p(c(0, 1, 2)))
m <- maxLik::maxLik(loglik, start = param, method = "BFGS")
summary(m)



# Simulation (draws matrix) -----------------------------------------------
n_draws <- 1000
n_rows <- nrow(dat)
draws_matrix <- matrix(rlogis(n_draws * n_rows), nrow = n_rows, ncol = n_draws)

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
  cat(ll, "\n")
  ll
}

p <- function(v) {
  v <- setNames(v, c("ASC", "beta1", "beta2"))
  v
}

# loglik clearly makes sense
loglik(p(c(0, 0, 0)))
loglik(p(c(1, 1, 1)))
loglik(p(c(1, 2, 3)))
loglik(param)
m <- maxLik::maxLik(loglik, start = param, method = "BFGS")
summary(m)

# providing gradient and hessian
grad <- function(betas) numDeriv::grad(loglik, betas)
hess <- function(betas) numDeriv::hessian(loglik, betas)
m <- maxLik::maxLik(loglik, start = param, method = "BFGS", grad = grad, hess = hess)
summary(m)


x <- seq(from = 0.0001, to = 0.9999, by = 0.01)
y <- log(x)
plot(x, y)


