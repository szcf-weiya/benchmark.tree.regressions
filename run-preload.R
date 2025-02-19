source("R/datasets.R")
source("R/evaluate.R")
source("R/methods.R")

lst_methods = c(rep("bart_fit", 3),
                rep("dbarts_fit", 3),
                rep("xbart_fit", 2),
                rep("mars_fit", 4),
                rep("ranger_fit", 3),
                rep("rf_fit", 3),
                rep("xgboost_fit", 1))
names_lst_methods = c("BART_50", "BART_100", "BART_200",
                      "dbarts_50", "dbarts_100", "dbarts_200",
                      "XBART_10_40", "XBART_100_40", #"XBART_500_40", #"XBART_200_20", "XBART_200_80",
                       "MARS_d1", "MARS_d1_df", "MARS_d2", "MARS_d2_df",
                      "ranger_50", "ranger_100", "ranger_200",
                       "randomForest_50", "randomForest_100", "randomForest_200",
                      # "XGBoost_100_NULL",
                      "XGBoost_100_3")
stopifnot(length(lst_methods) == length(names_lst_methods))
names(lst_methods) = names_lst_methods
lst_methods_paras = list(list(ntree = 50), list(ntree = 100), list(ntree = 200),
                         list(ntree = 50), list(ntree = 100), list(ntree = 200),
                         list(num_trees = 10, num_sweeps = 40, burnin = 15, mtry = "sqrtp"),
                         list(num_trees = 100, num_sweeps = 40, burnin = 15, mtry = "sqrtp"),
                         #list(num_trees = 500, num_sweeps = 40, burnin = 15),
                         #list(num_trees = 200, num_sweeps = 20, burnin = 15),
                         #list(num_trees = 200, num_sweeps = 80, burnin = 15),
                         list(degree = 1, df.correct = FALSE),
                         list(degree = 1, df.correct = TRUE, nrep = 20),
                         list(degree = 2, df.correct = FALSE),
                         list(degree = 2, df.correct = TRUE, nrep = 20),
                         list(num.trees = 50), list(num.trees = 100), list(num.trees = 200),
                         list(ntree = 50), list(ntree = 100), list(ntree = 200),
                         #list(nrounds = 1000, early_stopping_rounds = NULL),
                         list(nrounds = 1000, early_stopping_rounds = 3))
stopifnot(length(lst_methods) == length(lst_methods_paras))
