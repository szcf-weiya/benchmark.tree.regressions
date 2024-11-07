source("run-preload.R")
df = para.benchmark(c("sim_friedman", "sim_checkerboard", "sim_linear", "sim_max"),
               lst_methods, lst_methods_paras,
               arr_structures = c("indep", "ar1", "ar1+", "factor"),
               ns = c(100, 200, 500, 1000),
               ps = c(200, 400, 600, 800),
               ncores = 10
)
saveRDS(df, "benchmark-tree-regressions/res-hpc.rds")
