# Benchmarking Tree Regressions

[![:robot: Bot for New CSV data](https://github.com/szcf-weiya/benchmark.tree.regressions/actions/workflows/new-csv.yaml/badge.svg)](https://github.com/szcf-weiya/benchmark.tree.regressions/actions/workflows/new-csv.yaml)

The respiratory is partially inspired by [benchopt](https://github.com/benchopt/benchopt), but aimed for a more R-user-friendly, easily-hacking, more statistically benchmarking. 

## :exclamation::exclamation::robot: Bot `@new-csv` :exclamation::exclamation:

This repository supports an automated GitHub Action bot that listens for issues with the title 

```shell
@new-csv DataName DataURL Idx_of_Y
```

The bot will create a new commit and open a pull request to add new data for benchmarking automatically. For example, [Issue #39](https://github.com/szcf-weiya/benchmark.tree.regressions/issues/39) triggers [Pull Request #38](https://github.com/szcf-weiya/benchmark.tree.regressions/pull/38), which adds the [abalone](https://raw.githubusercontent.com/jbrownlee/Datasets/refs/heads/master/abalone.csv) dataset for benchmarking.

```shell
@new-csv abalone https://raw.githubusercontent.com/jbrownlee/Datasets/refs/heads/master/abalone.csv 9
```

The response variable is the 9-th column, so `Idx_of_Y = 9`. (Currently, assume no header column names.)

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
| BostonHousing | Housing values and other information about Boston census tracts. | 506 | 13 | [:link:](https://github.com/JWarmenhoven/ISLR-python/blob/master/Notebooks/Data/Boston.csv) |
| CaliforniaHousing | Aggregated housing data from each of 20,640 neighborhoods (1990 census block groups) in California. | 20640 | 8 | [:link:](https://github.com/szcf-weiya/ESL-CN/tree/master/data/Housing) |
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

### MyServer@ZJU

Specifically, the current local results are run on the Yale Mccelary HPC.

```bash
$ R
> renv::restore() # restore packages from the lockfile
> system.time({source("run-local.R")})
     user    system   elapsed 
207084.40  56119.33 107226.75 
```

## How to Contribute?

**Automated Way:** open an issue with title `@new-csv DataName URL Idx_of_Y`

<details><summary>Step by Step: Add a real data</summary>
<p>

1. add data info into `lst_real_data` in `datasets.R`, and write out a meta data

```r
real.data.meta = df_data_meta()
saveRDS(real.data.meta, file = "benchmark-tree-regressions/real-data-meta.rds")
```

2. run `print_to_readme()` to update the table in this `README.md` file
  
3. add the preparation step for the data (starting from downloading the data) as a function `real_XXX` in `datasets.R`

4. update the list of real data `benchmark-tree-regressions/choices.real.data.R`

</p>
</details> 




## :notebook: Use the Repo as a Template

The current respiratory is mainly benchmarking various tree-based regressions, but the structure is much more flexible for other tasks. If you find this structure useful, please feel free to use it as **a template** for your benchmarking analysis via the link [:link:](https://github.com/new?template_name=benchmark.tree.regressions&template_owner=szcf-weiya).
