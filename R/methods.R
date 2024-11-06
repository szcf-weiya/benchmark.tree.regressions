bart_fit = function(x, y, xtest) {
  fit = BART::gbart(x, y)
  ypred = predict(fit, xtest)
  colMeans(ypred)
}

xbart_fit = function(x, y, xtest, num_trees = 10, num_sweeps = 10) {
  fit = XBART::XBART(as.matrix(y), x, num_trees = num_trees, num_sweeps = num_sweeps)
  ypred = predict(fit, xtest)
  rowMeans(ypred)
}

mars_fit = function(x, y, xtest, degree = 1, df.correct = FALSE) {
  fit = earth::earth(x, y, degree = degree)
  if (df.correct) {
    df.res = earth.dof.patch::correct_df(fit)
    fit = earth::earth(x, y, degree = degree, penalty = df.res$penalty)
  }
  ypred = predict(fit, xtest)
  ypred
}

xgboost_fit = function(x, y, xtest) {
  fit = xgboost::xgboost(data = x, label = y, nrounds = 100)
  ypred = predict(fit, xtest)
  ypred
}

ranger_fit = function(x, y, xtest, num.trees = 500) {
  df = data.frame(x, y)
  # use the standard practice: mtry = [p / 3]
  fit = ranger::ranger(y ~ ., data = df, num.trees = num.trees, mtry = floor(ncol(x) / 3))
  ypred = predict(fit, data.frame(xtest))
  ypred$predictions
}

rf_fit = function(x, y, xtest, ntree = 500) {
  df = data.frame(x, y)
  fit = randomForest::randomForest(y ~ ., data = df, ntree = ntree)
  ypred = predict(fit, data.frame(xtest))
  ypred
}
