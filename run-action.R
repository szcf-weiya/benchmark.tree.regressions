source("run-preload.R")
df = benchmark(c("sim_friedman", "sim_checkerboard", "sim_linear", "sim_max"),
               lst_methods, lst_methods_paras,
               arr_structures = c("indep", "ar1", "ar1+", "factor"),
               ns = c(500),
               ps = c(200)
)
saveRDS(df, "benchmark-tree-regressions/res-action.rds")
