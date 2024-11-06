source("R/datasets.R")
source("R/evaluate.R")
source("R/methods.R")

lst_methods = c("bart_fit", rep("xbart_fit", 1), rep("mars_fit", 4), rep("ranger_fit", 2), rep("rf_fit", 2), rep("xgboost_fit"))
names(lst_methods) = c("BART",
                       "XBART", #"XBART_10_10", "XBART_5_10", "XBART_10_5",
                       "MARS_d1", "MARS_d2", "MARS_d1_df", "MARS_d2_df",
                       "ranger_500", "ranger_1000",
                       "randomForest_500", "randomForest_1000",
                       "XGBoost")
lst_methods_paras = list(NULL,
                         list(num_trees = 10, num_sweeps = 10),
                         #list(num_trees = 5, num_sweeps = 10),
                         #list(num_trees = 10, num_sweeps = 5),
                         list(degree = 1, df.correct = FALSE),
                         list(degree = 2, df.correct = FALSE),
                         list(degree = 1, df.correct = TRUE),
                         list(degree = 2, df.correct = TRUE),
                         list(num.trees = 500), list(num.trees = 1000),
                         list(ntree = 500), list(ntree = 1000),
                         NULL)
stopifnot(length(lst_methods) == length(lst_methods_paras))

df = benchmark(c("sim_friedman", "sim_checkerboard", "sim_linear", "sim_max"),
               lst_methods, lst_methods_paras,
               arr_structures = c("indep", "ar1", "ar1+", "factor"),
               ns = c(500),
               ps = c(200)
)
saveRDS(df, "benchmark-tree-regressions/res-action.rds")
