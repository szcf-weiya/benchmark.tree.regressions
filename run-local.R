source("R/datasets.R")
source("R/evaluate.R")
source("R/methods.R")

lst_methods = c(rep("bart_fit", 1),
                rep("xbart_fit", 1),
                rep("mars_fit", 4),
                rep("ranger_fit", 3),
                rep("rf_fit", 3),
                rep("xgboost_fit", 1))
names_lst_methods = c("BART_100", #"BART_200", "BART_500",
                      "XBART_100_40", #"XBART_200_40", #"XBART_500_40", #"XBART_200_20", "XBART_200_80",
                      "MARS_d1", "MARS_d2", "MARS_d1_df", "MARS_d2_df",
                      "ranger_100", "ranger_200", "ranger_500",
                      "randomForest_100", "randomForest_200", "randomForest_500",
                      # "XGBoost_100_NULL",
                      "XGBoost_100_3")
stopifnot(length(lst_methods) == length(names_lst_methods))
names(lst_methods) = names_lst_methods
lst_methods_paras = list(list(ntree = 100), #list(ntree = 200), list(ntree = 500),
                         list(num_trees = 100, num_sweeps = 40, burnin = 15),
                         #list(num_trees = 200, num_sweeps = 40, burnin = 15),
                         #list(num_trees = 500, num_sweeps = 40, burnin = 15),
                         #list(num_trees = 200, num_sweeps = 20, burnin = 15),
                         #list(num_trees = 200, num_sweeps = 80, burnin = 15),
                         list(degree = 1, df.correct = FALSE),
                         list(degree = 2, df.correct = FALSE),
                         list(degree = 1, df.correct = TRUE),
                         list(degree = 2, df.correct = TRUE),
                         list(num.trees = 100), list(num.trees = 200), list(num.trees = 500),
                         list(ntree = 100), list(ntree = 200), list(ntree = 500),
                         #list(nrounds = 1000, early_stopping_rounds = NULL),
                         list(nrounds = 1000, early_stopping_rounds = 3))
stopifnot(length(lst_methods) == length(lst_methods_paras))


df = para.benchmark(c("sim_friedman", "sim_checkerboard", "sim_linear", "sim_max"),
               lst_methods, lst_methods_paras,
               arr_structures = c("indep", "ar1", "ar1+", "factor"),
               ns = c(100, 200, 500, 1000),
               ps = c(200, 400, 600, 800),
               ncores = 10
)
saveRDS(df, "benchmark-tree-regressions/res-hpc.rds")
