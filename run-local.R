source("R/datasets.R")
source("R/evaluate.R")
source("R/methods.R")

lst_methods = c("bart_fit", rep("xbart_fit", 1), rep("mars_fit", 4), rep("ranger_fit", 3), rep("xgboost_fit"))
names(lst_methods) = c("BART",
                       "XBART", #"XBART_10_10", "XBART_5_10", "XBART_10_5",
                       "MARS_d1", "MARS_d2", "MARS_d1_df", "MARS_d2_df",
                       "RF_100", "RF_500", "RF_1000",
                       "XGBoost")
lst_methods_paras = list(NULL,
                         list(num_trees = 10, num_sweeps = 10),
                         #list(num_trees = 5, num_sweeps = 10),
                         #list(num_trees = 10, num_sweeps = 5),
                         list(degree = 1, df.correct = FALSE),
                         list(degree = 2, df.correct = FALSE),
                         list(degree = 1, df.correct = TRUE),
                         list(degree = 2, df.correct = TRUE),
                         list(num.trees = 100),
                         list(num.trees = 500),
                         list(num.trees = 1000),
                         NULL)
stopifnot(length(lst_methods) == length(lst_methods_paras))

df = para.benchmark(c("sim_friedman", "sim_checkerboard", "sim_linear"),
               lst_methods, lst_methods_paras,
               ns = c(100, 200, 1000),
               ps = c(200, 400, 600, 800),
               ncores = 10
)
saveRDS(df, "benchmark-tree-regressions/res-hpc.rds")


## use a subset for debug
debug_small = function() {
  lst_methods = c(#"bart_fit",
                  rep("xbart_fit", 1),
                  #rep("mars_fit", 4),
                  rep("ranger_fit", 1), rep("xgboost_fit"))
  names(lst_methods) = c(
                         #"BART",
                         "XBART", #"XBART_10_10", "XBART_5_10", "XBART_10_5",
                         #"MARS_d1", "MARS_d2", "MARS_d1_df", "MARS_d2_df",
                         "RF_100", #"RF_500", "RF_1000",
                         "XGBoost")
  lst_methods_paras = list(#NULL,
                           list(num_trees = 10, num_sweeps = 10),
                           #list(num_trees = 5, num_sweeps = 10),
                           #list(num_trees = 10, num_sweeps = 5),
                           #list(degree = 1, df.correct = FALSE),
                           #list(degree = 2, df.correct = FALSE),
                           #list(degree = 1, df.correct = TRUE),
                           #list(degree = 2, df.correct = TRUE),
                           list(num.trees = 100),
                           #list(num.trees = 500),
                           #list(num.trees = 1000),
                           NULL)
  stopifnot(length(lst_methods) == length(lst_methods_paras))
  df = para.benchmark(c("sim_friedman"),
                 lst_methods, lst_methods_paras,
                 ns = c(100, 200),
                 ps = c(200)
  )
}
