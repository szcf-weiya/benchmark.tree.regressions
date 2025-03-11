source("R/datasets.R")
source("R/evaluate.R")
source("R/methods.R")

lst_methods = c(rep("bart_fit", 1),
                rep("dbarts_fit", 1),
                rep("xbart_fit", 1),
                rep("mars_fit", 4),
                rep("ranger_fit", 1),
                rep("rf_fit", 1),
                rep("xgboost_fit", 1),
                "mean_fit")
names_lst_methods = c("BART_100", #"BART_200", "BART_500",
                      "dbarts_100",
                      "XBART_100_40", #"XBART_200_40", #"XBART_500_40", #"XBART_200_20", "XBART_200_80",
                      "MARS_d1", "MARS_d1_df", "MARS_d2", "MARS_d2_df",
                      "ranger_100", #"ranger_200", "ranger_500",
                      "randomForest_100", #"randomForest_200", "randomForest_500",
                      # "XGBoost_100_NULL",
                      "XGBoost_100_3",
                      "Baseline_mean")
stopifnot(length(lst_methods) == length(names_lst_methods))
names(lst_methods) = names_lst_methods
lst_methods_paras = list(list(ntree = 100), #list(ntree = 200), list(ntree = 500),
                         list(ntree = 100),
                         list(num_trees = 100, num_sweeps = 40, burnin = 15, mtry = "sqrtp"),
                         #list(num_trees = 200, num_sweeps = 40, burnin = 15),
                         #list(num_trees = 500, num_sweeps = 40, burnin = 15),
                         #list(num_trees = 200, num_sweeps = 20, burnin = 15),
                         #list(num_trees = 200, num_sweeps = 80, burnin = 15),
                         list(degree = 1, df.correct = FALSE),
                         list(degree = 1, df.correct = TRUE, nrep = 20),
                         list(degree = 2, df.correct = FALSE),
                         list(degree = 2, df.correct = TRUE, nrep = 20),
                         list(num.trees = 100), #list(num.trees = 200), list(num.trees = 500),
                         list(ntree = 100), #list(ntree = 200), list(ntree = 500),
                         #list(nrounds = 1000, early_stopping_rounds = NULL),
                         list(nrounds = 1000, early_stopping_rounds = 3),
                         NULL)
stopifnot(length(lst_methods) == length(lst_methods_paras))


#df = para.benchmark(c("sim_friedman", "sim_checkerboard", "sim_linear", "sim_max", "sim_singleIndex"),
#               lst_methods, lst_methods_paras,
#               arr_structures = c("indep", "ar1", "ar1+", "factor"),
#               ns = c(100, 200, 500, 1000),
#               ps = c(20, 50, 100, 200),
#               ncores = 10
#)
#saveRDS(df, "benchmark-tree-regressions/res-hpc.rds")

# some issue with para.benchmark for local (TODO)
df_real = benchmark(c("real_BostonHousing", "real_CaliforniaHousing",
                      "real_CASP", "real_Energy",
                      "real_AirQuality", "real_BiasCorrection",
                      "real_ElectricalStability", "real_GasTurbine",
                      "real_ResidentialBuilding", "real_LungCancerGenomic",
                      "real_StructureActivity", "real_BloodBrain",
                      "real_GSE65904"),
                    lst_methods, lst_methods_paras,
                    arr_structures = c(""),
                    ns = c(0),
                    ps = c(0)
                    #ncores = 10
)

saveRDS(df_real, "benchmark-tree-regressions/res-hpc-real.rds")
