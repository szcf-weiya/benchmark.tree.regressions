source("R/methods.R")

set.seed(1234)

x_train <- cbind(
  signal = seq_len(8),
  curved_signal = seq_len(8)^2,
  constant_in_train = rep(0, 8)
)
y_train <- c(1.0, 1.4, 1.8, 2.1, 2.7, 3.0, 3.4, 3.9)
x_test <- cbind(
  signal = c(9, 10),
  curved_signal = c(81, 100),
  constant_in_train = c(1, 2)
)

invisible(capture.output(
  predictions <- bart_fit(x_train, y_train, x_test, ntree = 5)
))

stopifnot(
  is.numeric(predictions),
  length(predictions) == nrow(x_test),
  all(is.finite(predictions))
)

cat("BART constant-column regression test passed\n")
