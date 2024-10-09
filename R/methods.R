#' Fit BART model
#' @import BART
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
