evaluate = function(x, y, fun_method, nfold = 5, seed = 1234, ...) {
  set.seed(seed)
  folds = createFolds(y, k = nfold, returnTrain = TRUE)
  errs = numeric(nfold)
  start_time = Sys.time()
  for (i in 1:nfold) {
    idx_train = folds[[i]]
    xtrain = x[idx_train, ]
    ytrain = y[idx_train]
    xtest = x[-idx_train, ]
    ytest = y[-idx_train]
    ypred = fun_method(xtrain, ytrain, xtest, ...)
    errs[i] = mean((ypred - ytest)^2)
  }
  end_time = Sys.time()
  cverr = mean(errs)
  runtime = as.numeric(end_time - start_time)
  list(cverr = cverr, runtime = runtime)
}

benchmark = function(arr_data, arr_methods, ns = c(500), ps = c(200)) {
  ndata = length(arr_data)
  nmethod = length(arr_methods)
  names.method = names(arr_methods)
  if (is.null(names.method)) names.method = arr_methods
  df = matrix(0, ncol = 6, nrow = 0)
  for (i in 1:ndata) {
    for (n in ns) {
      for (p in ps) {
        data = get(arr_data[i])(n = n, p = p)
        for (m in 1:nmethod) {
          method = arr_methods[m]
          tmp = evaluate(data$x, data$y, get(method))
          df = rbind(df, c(arr_data[i], n, p, names.method[m], unlist(tmp)))
        }
      }
    }
  }
  df = as.data.frame(df)
  colnames(df) = c("data.model", "n", "p", "method", "cv.error", "runtime")
  df$n = as.integer(df$n)
  df$p = as.integer(df$p)
  df$cv.error = as.numeric(df$cv.error)
  df$runtime = as.numeric(df$runtime)
  # res = list()
  # for (i in 1:ndata) {
  #   res[[arr_data[i]]] = list()
  #   for (n in ns) {
  #     res[[i]][[as.character(n)]] = list()
  #     for (p in ps) {
  #       res[[i]][[as.character(n)]][[as.character(p)]] = list()
  #       data = get(arr_data[i])(n = n, p = p)
  #       for (method in arr_methods) {
  #         tmp = evaluate(data$x, data$y, get(method))
  #         res[[i]][[as.character(n)]][[as.character(p)]][[method]] = tmp
  #         df = rbind(df, c(arr_data[i], n, p, method, unlist(tmp)))
  #       }
  #     }
  #   }
  # }
  return(df)
}

scripts = function() {
  df = benchmark(c("sim_friedman", "sim_checkerboard"), c(`BART` = "bart_fit", `XBART` = "xbart_fit"), ns = c(100, 200, 500, 1000), ps = c(200, 400, 600))
  saveRDS(df, "benchmark-tree-regressions/res.rds")
}
