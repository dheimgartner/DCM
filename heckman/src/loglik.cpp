#include <Rcpp.h>
using namespace Rcpp;

// Define a function to calculate the log likelihood
// This function will be called from R
// 'param' and 'dat' are passed as Rcpp::NumericVector and Rcpp::DataFrame
// 'I1', 'I2', 'I3', 'n' are assumed to be defined previously
// You may need to adjust the argument types and inputs based on your actual data
// Please include the necessary Rcpp headers in your C++ file
// [[Rcpp::export]]
double loglik(NumericVector param, DataFrame dat, NumericVector I1, NumericVector I2, NumericVector I3, int n) {

  // Unpack 'param' into separate variables
  double alpha = param["alpha"];
  double gamma = param["gamma"];
  double rho = param["rho"];
  double a1 = param["a1"];
  double a2 = param["a2"];

  // Calculate 'gammaX' and 'alphaZ'
  NumericVector X = dat["X"];
  NumericVector Z = dat["Z"];
  NumericVector gammaX = gamma * X;
  NumericVector alphaZ = alpha * Z;

  // Define the covariance matrix 'sigma'
  NumericMatrix sigma(2, 2);
  sigma(0, 0) = 1;
  sigma(0, 1) = -rho;
  sigma(1, 0) = -rho;
  sigma(1, 1) = 1;

  // Create a vector 'n_p_inf' with Inf values
  NumericVector n_p_inf(n, R_PosInf);

  // Calculate 'pt0', 'a1', and 'a2'
  NumericVector pt0 = 1 - R::pnorm(gammaX);
  NumericVector a1_vector = R::pmnorm(cbind(gammaX, a1 - alphaZ), sigma);
  NumericVector a2_vector = R::pmnorm(cbind(gammaX, a2 - alphaZ), sigma);

  // Calculate 'pt1_j1', 'pt1_j2', and 'pt1_j3'
  NumericVector pt1_j1 = a1_vector; // below
  NumericVector pt1_j2 = a2_vector - a1_vector; // between
  NumericVector pt1_j3 = R::pmnorm(cbind(gammaX, n_p_inf), sigma) - a2_vector; // above

  // Calculate 'selection' and 'observation'
  NumericVector t = dat["t"];
  NumericVector selection = (1 - t) * log(pt0);
  NumericVector observation = t * (I1 * log(pt1_j1) + I2 * log(pt1_j2) + I3 * log(pt1_j3));

  // Calculate the log likelihood 'll'
  double ll = sum(selection + observation);

  return ll;
}
