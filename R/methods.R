# baseline
mean_fit = function(x, y, xtest) {
  rep(mean(y), nrow(xtest))
}

bart_fit = function(x, y, xtest, ntree = 200) {
  fit = BART::gbart(x, y, ntree = ntree, ndpost = 1000L, nskip = 100L)
  ypred = predict(fit, xtest)
  colMeans(ypred)
}

dbarts_fit = function(x, y, xtest, ntree = 200) {
  fit = dbarts::bart(x, y, ntree = ntree, keeptrees = TRUE, ndpost = 1000L, nskip = 100L,
                     sigest = ifelse(ncol(x) >= nrow(x) - 1, 1.0, NA)) # a temp workaround for p >= n-1 (see #15)
  ypred = predict(fit, xtest)
  colMeans(ypred)
}

xbart_fit = function(x, y, xtest, num_trees = 100, num_sweeps = 40, burnin = 15, mtry = "p") {
  if (mtry == "p")
    n_mtry = ncol(x)
  else if (mtry == "sqrtp")
    n_mtry = floor(sqrt(ncol(x)))
  else
    n_mtry = floor(ncol(x)/3)
  fit = XBART::XBART(as.matrix(y), x, num_trees = num_trees, num_sweeps = num_sweeps, burnin = burnin, mtry = n_mtry)
  ypred = predict(fit, xtest)
  rowMeans(ypred)
}

mars_fit = function(x, y, xtest, degree = 1, df.correct = FALSE, nrep = 10) {
  fit = earth::earth(x, y, degree = degree)
  if (df.correct) {
    df.res = earth.dof.patch::correct_df(fit, N = nrep)
    fit = earth::earth(x, y, degree = degree, penalty = df.res$penalty)
  }
  ypred = predict(fit, xtest)
  ypred
}

xgboost_fit = function(x, y, xtest, nrounds = 100, early_stopping_rounds = NULL) {
  fit = xgboost::xgboost(data = x, label = y, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, verbose = 0)
  ypred = predict(fit, xtest)
  ypred
}

# renv::install('https://github.com/catboost/catboost/releases/download/v1.2.7/catboost-R-linux-x86_64-1.2.7.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
carboost_fit = function(x, y, xtest, num_trees = 1000) {
  train_pool = catboost::catboost.load_pool(data = x, label = y)
  test_pool = catboost::catboost.load_pool(xtest)
  model = catboost::catboost.train(train_pool, NULL, params = list(iterations = num_trees,
                                                                   verbose = 100))
  ypred = catboost::catboost.predict(model, test_pool)
  ypred
}

ranger_fit = function(x, y, xtest, num.trees = 500) {
  df = data.frame(x, y)
  # use the standard practice: mtry = [p / 3]
  fit = ranger::ranger(dependent.variable.name = "y", data = df, num.trees = num.trees, mtry = floor(ncol(x) / 3))
  ypred = predict(fit, data.frame(xtest))
  ypred$predictions
}

rf_fit = function(x, y, xtest, ntree = 500) {
  df = data.frame(x, y)
  fit = randomForest::randomForest(y ~ ., data = df, ntree = ntree)
  ypred = predict(fit, data.frame(xtest))
  ypred
}
