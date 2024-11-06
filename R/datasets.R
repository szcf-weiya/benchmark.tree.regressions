gen_x = function(n = 500, p = 200, structure = "indep",
                 rho = 0.9, # used for ar1
                 rho1 = 0.5, rho2 = 0.2 # used for ar1+
                 ) {
  if (structure == "indep") {
    x = matrix(rnorm(n*p), ncol = p)
  } else if (structure == "ar1") {
    Sigma = outer(1:p, 1:p, function(j, k) rho^abs(j - k))
    mu = rep(0, p)
    x = MASS::mvrnorm(n, mu, Sigma)
  } else if (structure == "ar1+") {
    Sigma = outer(1:p, 1:p, function(j, k) rho1^abs(j-k) + rho2 * (j != k) )
    mu = rep(0, p)
    x = MASS::mvrnorm(n, mu, Sigma)
  } else if (structure == "factor") {
    k = floor(p / 5)
    stopifnot(5 * k == p)
    Fmat = matrix(rnorm(k * n), ncol = n)
    rawB = rbind(diag(nrow = k), diag(nrow = k), diag(nrow = k), diag(nrow = k), diag(nrow = k))
    B = rawB[sample(1:nrow(rawB), nrow(rawB), replace = F), ]
    x = t(B %*% Fmat) + matrix(rnorm(n*p, sd = 0.1*sqrt(k)), ncol = p)
    x = scale(x)
  } else {
    warning("not supported structure = '", structure, "', use the independent X instead\n")
    x = matrix(rnorm(n*p), ncol = p)
  }
  x
}

## used in Linero (2018)
sim_friedman = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  y0 = 10 * sin(pi * x[, 1] * x[, 2]) + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
  y = y0 + rnorm(n) * sigma
  list(x = x, y = y)
}

## used in Linero (2018)
sim_checkerboard = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  y0 = 2 * x[, 50] * x[, 100] + 2 * x[, 150] * x[, 200]
  y = y0 + rnorm(n) * sigma
  list(x = x, y = y)
}

## used in Linero (2018)
sim_linear = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  y0 = 2 * x[, 50] + 2 * x[, 100] + 4 * x[, 150]
  y = y0 + rnorm(n) *sigma
  list(x = x, y = y)
}

## used in He and Hahn (2023)
sim_max = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  y0 = apply(x, 1, function(x) max(x[1:3]))
  y = y0 + rnorm(n) * sigma
}

