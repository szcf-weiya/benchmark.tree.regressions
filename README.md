# Benchmarking Tree Regressions

The respiratory is partially inspired by [benchopt](https://github.com/benchopt/benchopt), but aimed for a more R-user-friendly, easily-hacking, more statistically benchmarking. 

The current respiratory is mainly benchmarking various tree-based regressions, but the structure is much more flexible for other tasks. If you find this structure useful, please feel free to use it as **a template** for your benchmarking analysis.

## Structure

### Source Files

The source files are stored in [R/](R/), which contains three files

- [datasets.R](R/datasets.R): each dataset (either simulated or real dataset) is defined as a function
- [methods.R](R/methods.R): each method is defined as a function and with tuning parameters
- [evaluate.R](R/evaluate.R): evaluate each method (with particular parameter) on each dataset, and report the CV error and running time (the criteria can be customized)

### Shiny App

To interactively display the results, we adopt the Shiny App, and the related files are in the folder [benchmark-tree-regressions/](benchmark-tree-regressions/)

Note that a typical shiny app is server-based. With the help of `shinylive`, we deploy the shiny website on the GitHub pages.

## :rocket: Run Locally

The repository used `renv` to manage the compatible R packages. It is also better to use the same R version 4.1.2 in case.

Then one can install the dependencies of R package via

```r
> renv::restore()  # Restores packages from the lockfile (if using renv)
```

Next, you can check the scripts [`run-local.R`](run-local.R) and [`run-action.R`](run-action.R).

Due to the limited computational resources, [`run-action.R`](run-action.R) take shorter time to finish.

### Yale Mccelary HPC

Specifically, the current local results are run on the Yale Mccelary HPC.

```bash
$ module load R/4.2.0-foss-2020b
$ R
> renv::restore() # restore packages from the lockfile
> system.time({source("run-local.R")})
      user     system    elapsed
348314.125   5609.474  41027.405
```
