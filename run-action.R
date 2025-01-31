source("run-preload.R")
df = benchmark(c("sim_friedman", "sim_checkerboard", "sim_linear", "sim_max"),
               lst_methods, lst_methods_paras,
               arr_structures = c("indep", "ar1", "ar1+", "factor"),
               ns = c(500),
               ps = c(200)
)
saveRDS(df, "benchmark-tree-regressions/res-action.rds")

df_real = benchmark(c("real_CASP", "real_Energy"),
               lst_methods, lst_methods_paras,
               arr_structures = c(""),
               ns = c(0),
               ps = c(0)
)
saveRDS(df_real, "benchmark-tree-regressions/res-action-real.rds")
