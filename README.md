# Benchmarking Tree Regressions

The respiratory is partially inspired by [benchopt](https://github.com/benchopt/benchopt), but aimed for a more R-user-friendly, easily-hacking, more statistically benchmarking. 

## :evergreen_tree: What is the Repo Structure?

### Source Files

The source files are stored in [R/](R/), which contains three files

#### [datasets.R](R/datasets.R): each dataset (either simulated or real dataset) is defined as a function

For the simulation data, we consider

- different covariance structure of the design matrix
  - Independent
  - AR(1)
  - AR(1)+
  - Factor
- different data generating model
  - Friedman
  - Checkerboard
  - Linear
  - Max

For the real datasets, we consider 

| Data | Description | n | p | URL  | 
|:----:|:------:|:--:|:--:|:-----------:|
| CASP | Physicochemical Properties of Protein Tertiary Structure | 45730 | 9 | [:link:](https://archive.ics.uci.edu/dataset/265/physicochemical+properties+of+protein+tertiary+structure) |
| Energy | Appliances Energy Prediction | 19735 | 27 | [:link:](https://archive.ics.uci.edu/dataset/374/appliances+energy+prediction) |
| AirQuality | Air Quality | 9357 | 12 | [:link:](https://archive.ics.uci.edu/dataset/360/air+quality) |
| BiasCorrection | Bias correction of numerical prediction model temperature forecast | 7590 | 21 | [:link:](https://archive.ics.uci.edu/dataset/514/bias+correction+of+numerical+prediction+model+temperature+forecast) |
| ElectricalStability | Electrical Grid Stability Simulated Data | 10000 | 12 | [:link:](https://archive.ics.uci.edu/dataset/471/electrical+grid+stability+simulated+data) |
| GasTurbine | Gas Turbine CO and NOx Emission Data Set | 36733 | 10 | [:link:](https://archive.ics.uci.edu/dataset/551/gas+turbine+co+and+nox+emission+data+set) |
| ResidentialBuilding | Residential Building | 372 | 107 | [:link:](https://archive.ics.uci.edu/dataset/437/residential+building+data+set) |
| LungCancerGenomic | Lung cancer genomic data from the Chemores Cohort Study | 123 | 945 | [:link:](https://github.com/jedazard/PRIMsrc/blob/master/data/Real.2.rda) |
| StructureActivity | Qualitative Structure Activity Relationships (triazines) | 186 | 60 | [:link:](https://archive.ics.uci.edu/dataset/85/qualitative+structure+activity+relationships) |
| BloodBrain | Blood Brain Barrier Data `data("BloodBrain", package = "caret")` | 208 | 134 | [:link:](https://github.com/topepo/caret/blob/master/pkg/caret/data/BloodBrain.RData) |
| GSE65904 | Whole-genome expression analysis of melanoma tumor biopsies from a population-based cohort. | 210 | 47323 | [:link:](https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65904) |

#### [methods.R](R/methods.R): each method is defined as a function and with tuning parameters

Here are the method we considered:

| Method | Software |
|:------:|:--------:|
| Bayesian Additive Regression Trees (BART) | [![](https://img.shields.io/badge/R-BART-blue)](https://cran.r-project.org/web/packages/BART/index.html) |
| Accelerated BART (XBART) | [![](https://img.shields.io/badge/R-XBART-blue)](https://github.com/JingyuHe/XBART) | 
| Random Forests | [![](https://img.shields.io/badge/R-randomForest-blue)](https://cran.r-project.org/web/packages/randomForest/index.html) |
| XGBoost | [![](https://img.shields.io/badge/R-xgboost-blue)](https://cran.r-project.org/web/packages/xgboost/index.html) |
| Multivariate Adaptive Regression Splines (MARS) |[![](https://img.shields.io/badge/R-earth-blue)](https://cran.r-project.org/web/packages/earth/index.html) |

#### [evaluate.R](R/evaluate.R): evaluate each method (with particular parameter) on each dataset, and report the CV error and running time (the criteria can be customized)

### Shiny App

To interactively display the results, we adopt the Shiny App, and the related files are in the folder [benchmark-tree-regressions/](benchmark-tree-regressions/)

Note that a typical shiny app is server-based. With the help of `shinylive`, we deploy the shiny website on the GitHub pages <https://hohoweiya.xyz/benchmark.tree.regressions/>.

## :rocket: How to Run Locally?

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
518757.236   6565.216  59712.296
```

## :notebook: Use the Repo as a Template

The current respiratory is mainly benchmarking various tree-based regressions, but the structure is much more flexible for other tasks. If you find this structure useful, please feel free to use it as **a template** for your benchmarking analysis via the link [:link:](https://github.com/new?template_name=benchmark.tree.regressions&template_owner=szcf-weiya).
