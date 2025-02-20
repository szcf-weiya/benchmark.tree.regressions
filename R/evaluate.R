evaluate = function(x, y, fun_method, nfold = 5, seed = 1234, paras = NULL) {
  set.seed(seed)
  folds = caret::createFolds(y, k = nfold, returnTrain = TRUE)
  errs = numeric(nfold)
  start_time = Sys.time()
  for (i in 1:nfold) {
    idx_train = folds[[i]]
    xtrain = x[idx_train, ]
    ytrain = y[idx_train]
    xtest = x[-idx_train, ]
    ytest = y[-idx_train]
    paras$x = xtrain
    paras$y = ytrain
    paras$xtest = xtest
    ypred = do.call(fun_method, paras)
    errs[i] = mean((ypred - ytest)^2)
  }
  end_time = Sys.time()
  cverr = mean(errs)
  runtime = as.numeric(difftime(end_time, start_time, units = "secs"))
  list(cverr = cverr, runtime = runtime)
}

benchmark = function(arr_data, arr_methods, arr_paras, arr_structures = c("indep"), ns = c(500), ps = c(200)) {
  ndata = length(arr_data)
  nmethod = length(arr_methods)
  names.method = names(arr_methods)
  if (is.null(names.method)) names.method = arr_methods
  df = matrix(0, ncol = 7, nrow = 0)
  for (i in 1:ndata) {
    if (grepl("^sim_", arr_data[i])) { # simulation data
      for (n in ns) {
        for (p in ps) {
          for (s in arr_structures) {
            data = get(arr_data[i])(n = n, p = p, structure = s)
            for (m in 1:nmethod) {
              method = arr_methods[m]
              cat("\n\n==== n =", n, "p =", p, "s =", s, "method =", method, "====\n\n")
              tmp = tryCatch(evaluate(data$x, data$y, get(method), paras = arr_paras[[m]]),
                             error = function(e) list(NA, NA))
              df = rbind(df, c(arr_data[i], s, n, p, names.method[m], unlist(tmp)))
            }
          }
        }
      }
    } else { # real data
      data = get(arr_data[i])()
      for (m in 1:nmethod) {
        method = arr_methods[m]
        tmp = evaluate(data$x, data$y, get(method), paras = arr_paras[[m]])
        df = rbind(df, c(arr_data[i], "", nrow(data$x), ncol(data$x), names.method[m], unlist(tmp)))
      }
    }
  }
  df = as.data.frame(df)
  colnames(df) = c("data.model", "structure", "n", "p", "method", "cv.error", "runtime")
  df$n = as.integer(df$n)
  df$p = as.integer(df$p)
  df$cv.error = as.numeric(df$cv.error)
  df$runtime = as.numeric(df$runtime)
  df$method = factor(df$method, levels = names.method)
  return(df)
}

para.benchmark = function(arr_data, arr_methods, arr_paras, arr_structures = c("indep"), ns = c(500), ps = c(200), ncores = 10) {
  ndata = length(arr_data)
  nmethod = length(arr_methods)
  names.method = names(arr_methods)
  if (is.null(names.method)) names.method = arr_methods
  df = matrix(0, ncol = 7, nrow = 0)
  single_benchmark = function(i, n, p, s) {
    df = matrix(0, ncol = 7, nrow = nmethod)
    if (grepl("^sim_", arr_data[i])) {
      data = get(arr_data[i])(n = n, p = p, structure = s)
    } else {
      data = get(arr_data[i])()
      n = nrow(data$x)
      p = ncol(data$x)
    }
    for (m in 1:nmethod) {
      method = arr_methods[m]
      tmp = evaluate(data$x, data$y, get(method), paras = arr_paras[[m]])
      df[m, ] = c(arr_data[i], s, n, p, names.method[m], unlist(tmp))
    }
    return(df)
  }
  indices = expand.grid(i = 1:ndata, n = ns, p = ps, s = arr_structures, stringsAsFactors = FALSE)
  res = pbmcapply::pbmclapply(1:nrow(indices), function(idx) {
    i = indices[idx, "i"]
    n = indices[idx, "n"]
    p = indices[idx, "p"]
    s = indices[idx, "s"]
    single_benchmark(i, n, p, s)
  }, mc.cores = ncores)
  df = do.call(rbind, res)
  df = as.data.frame(df)
  colnames(df) = c("data.model", "structure", "n", "p", "method", "cv.error", "runtime")
  df$n = as.integer(df$n)
  df$p = as.integer(df$p)
  df$cv.error = as.numeric(df$cv.error)
  df$runtime = as.numeric(df$runtime)
  df$method = factor(df$method, levels = names.method)
  return(df)
}


scripts = function() {
  lst_methods = c("bart_fit", rep("xbart_fit", 3), rep("mars_fit", 4))
  names(lst_methods) = c("BART",
                         "XBART_10_10", "XBART_5_10", "XBART_10_5",
                         "MARS_d1", "MARS_d2", "MARS_d1_df", "MARS_d2_df")
  lst_methods_paras = list(NULL,
                           list(num_trees = 10, num_sweeps = 10),
                           list(num_trees = 5, num_sweeps = 10),
                           list(num_trees = 10, num_sweeps = 5),
                           list(degree = 1, df.correct = FALSE),
                           list(degree = 2, df.correct = FALSE),
                           list(degree = 1, df.correct = TRUE),
                           list(degree = 2, df.correct = TRUE))
  stopifnot(length(lst_methods) == length(lst_methods_paras))
  df = benchmark(c("sim_friedman", "sim_checkerboard"),
                 lst_methods, lst_methods_paras,
                 ns = c(100, 200, 500, 1000), ps = c(200, 400, 600, 800)
                 )
  saveRDS(df, "benchmark-tree-regressions/res2.rds")
}
