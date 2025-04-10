source("run-preload.R")
df = para.benchmark(c("sim_friedman", "sim_checkerboard", "sim_linear", "sim_max", "sim_singleIndex"),
               lst_methods, lst_methods_paras,
               arr_structures = c("indep", "ar1", "ar1+", "factor"),
               ns = c(100),
               ps = c(20),
               ncores = 4
)
saveRDS(df, "benchmark-tree-regressions/res-action.rds")

exclude_real_data = c("GSE65904")
lst_funcs = paste0("real_", setdiff(names(lst_real_data), exclude_real_data))

df_real = benchmark(lst_funcs,
               lst_methods, lst_methods_paras,
               arr_structures = c(""),
               ns = c(0),
               ps = c(0)
)
saveRDS(df_real, "benchmark-tree-regressions/res-action-real.rds")
