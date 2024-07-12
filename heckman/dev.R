library(Rcpp)
library(mvtnorm)

evalCpp("R::rnorm(0, 1)")
evalCpp("Rcpp::rnorm(0, 1)")

evalCpp("R::pmvnorm(1)")

