sim_friedman = function(n = 500, p = 200, sigma = 1) {
  x = matrix(rnorm(n*p), ncol = p)
  y0 = 10 * sin(pi * x[, 1] * x[, 2]) + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
  y = y0 + rnorm(n) * sigma
  list(x = x, y = y)
}

#'
#' @import MASS
sim_checkerboard = function(n = 500, p = 200, sigma = 1) {
  Sigma = outer(1:p, 1:p, function(j, k) 0.9^abs(j - k))
  mu = rep(0, p)
  x = mvrnorm(n, mu, Sigma)
  y0 = 2 * x[, 50] * x[, 100] + 2 * x[, 150] * x[, 200]
  y = y0 + rnorm(n) * sigma
  list(x = x, y = y)
}

sim_linear = function(n = 500, p = 200, sigma = 1) {
  Sigma = outer(1:p, 1:p, function(j, k) 0.5^abs(j-k) + 0.2 * (j != k) )
  mu = rep(0, p)
  x = MASS::mvrnorm(n, mu, Sigma)
  y0 = 2 * x[, 50] + 2 * x[, 100] + 4 * x[, 150]
  y = y0 + rnorm(n) *sigma
  list(x = x, y = y)
}
