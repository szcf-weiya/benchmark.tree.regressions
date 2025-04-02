library(readxl)
gen_x = function(n = 500, p = 200, structure = "indep",
                 rho = 0.9, # used for ar1
                 rho1 = 0.5, rho2 = 0.2 # used for ar1+
                 ) {
  if (structure == "indep") {
    x = matrix(rnorm(n*p), ncol = p)
  } else if (structure == "ar1") {
    Sigma = outer(1:p, 1:p, function(j, k) rho^abs(j - k))
    mu = rep(0, p)
    x = MASS::mvrnorm(n, mu, Sigma)
  } else if (structure == "ar1+") {
    Sigma = outer(1:p, 1:p, function(j, k) rho1^abs(j-k) + rho2 * (j != k) )
    mu = rep(0, p)
    x = MASS::mvrnorm(n, mu, Sigma)
  } else if (structure == "factor") {
    k = floor(p / 5)
    stopifnot(5 * k == p)
    Fmat = matrix(rnorm(k * n), ncol = n)
    rawB = rbind(diag(nrow = k), diag(nrow = k), diag(nrow = k), diag(nrow = k), diag(nrow = k))
    B = rawB[sample(1:nrow(rawB), nrow(rawB), replace = F), ]
    x = t(B %*% Fmat) + matrix(rnorm(n*p, sd = 0.1*sqrt(k)), ncol = p)
    x = scale(x)
  } else {
    warning("not supported structure = '", structure, "', use the independent X instead\n")
    x = matrix(rnorm(n*p), ncol = p)
  }
  x
}

## used in Linero (2018)
sim_friedman = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  y0 = 10 * sin(pi * x[, 1] * x[, 2]) + 20 * (x[, 3] - 0.5)^2 + 10 * x[, 4] + 5 * x[, 5]
  y = y0 + rnorm(n) * sigma
  list(x = x, y = y)
}

## used in Linero (2018)
sim_checkerboard = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  # originally, Linero used 50, 100, 150, 200
  y0 = 2 * x[, 1] * x[, 2] + 2 * x[, 3] * x[, 4]
  y = y0 + rnorm(n) * sigma
  list(x = x, y = y)
}

## used in Linero (2018)
sim_linear = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  # originally, Linero used 50, 100, 150
  y0 = 2 * x[, 1] + 2 * x[, 2] + 4 * x[, 3]
  y = y0 + rnorm(n) *sigma
  list(x = x, y = y)
}

## used in He and Hahn (2023)
sim_max = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  y0 = apply(x, 1, function(x) max(x[1:3]))
  y = y0 + rnorm(n) * sigma
  list(x = x, y = y)
}

# used in He and Hahn (2023)
sim_singleIndex = function(n = 500, p = 200, sigma = 1, structure = "indep") {
  x = gen_x(n, p, structure = structure)
  gamma = -1.5 + ( c(1:10) - 1 ) / 3
  a = rowSums(sapply(1:10, function(i) (x[, i] - gamma[i])^2 ))
  y0 = 10 * sqrt(a) + sin(5 * a)
  y = y0 + rnorm(n) * sigma
  list(x = x, y = y)
}

# collection of real dataset
lst_real_data = list(
  BostonHousing = c("Housing values and other information about Boston census tracts.",
                "https://github.com/JWarmenhoven/ISLR-python/blob/master/Notebooks/Data/Boston.csv",
                "https://github.com/JWarmenhoven/ISLR-python/raw/refs/heads/master/Notebooks/Data/Boston.csv"),
  CaliforniaHousing = c("Aggregated housing data from each of 20,640 neighborhoods (1990 census block groups) in California.",
                        "https://github.com/szcf-weiya/ESL-CN/tree/master/data/Housing",
                        "https://github.com/szcf-weiya/ESL-CN/raw/refs/heads/master/data/Housing/cadata.txt"),
  CASP = c("Physicochemical Properties of Protein Tertiary Structure",
           "https://archive.ics.uci.edu/dataset/265/physicochemical+properties+of+protein+tertiary+structure",
           "https://archive.ics.uci.edu/static/public/265/physicochemical+properties+of+protein+tertiary+structure.zip"),
  Energy = c("Appliances Energy Prediction",
             "https://archive.ics.uci.edu/dataset/374/appliances+energy+prediction",
             "https://archive.ics.uci.edu/static/public/374/appliances+energy+prediction.zip"),
  AirQuality = c("Air Quality",
                 "https://archive.ics.uci.edu/dataset/360/air+quality",
                 "https://archive.ics.uci.edu/static/public/360/air+quality.zip"),
  BiasCorrection = c("Bias correction of numerical prediction model temperature forecast",
                     "https://archive.ics.uci.edu/dataset/514/bias+correction+of+numerical+prediction+model+temperature+forecast",
                     "https://archive.ics.uci.edu/static/public/514/bias+correction+of+numerical+prediction+model+temperature+forecast.zip"),
  ElectricalStability = c("Electrical Grid Stability Simulated Data",
                          "https://archive.ics.uci.edu/dataset/471/electrical+grid+stability+simulated+data",
                          "https://archive.ics.uci.edu/static/public/471/electrical+grid+stability+simulated+data.zip"),
  GasTurbine = c("Gas Turbine CO and NOx Emission Data Set",
                 "https://archive.ics.uci.edu/dataset/551/gas+turbine+co+and+nox+emission+data+set",
                 "https://archive.ics.uci.edu/static/public/551/gas+turbine+co+and+nox+emission+data+set.zip"),
  ResidentialBuilding = c("Residential Building",
                          "https://archive.ics.uci.edu/dataset/437/residential+building+data+set",
                          "https://archive.ics.uci.edu/static/public/437/residential+building+data+set.zip"),
  LungCancerGenomic = c("Lung cancer genomic data from the Chemores Cohort Study",
                        "https://github.com/jedazard/PRIMsrc/blob/master/data/Real.2.rda",
                        "https://github.com/jedazard/PRIMsrc/raw/refs/heads/master/data/Real.2.rda"),
  StructureActivity = c("Qualitative Structure Activity Relationships (triazines)",
                        "https://archive.ics.uci.edu/dataset/85/qualitative+structure+activity+relationships",
                        "https://archive.ics.uci.edu/static/public/85/qualitative+structure+activity+relationships.zip"),
  BloodBrain = c("Blood Brain Barrier Data `data(\"BloodBrain\", package = \"caret\")`",
                 "https://github.com/topepo/caret/blob/master/pkg/caret/data/BloodBrain.RData",
                 "https://github.com/topepo/caret/blob/master/pkg/caret/data/BloodBrain.RData"),
  GSE65904 = c("Whole-genome expression analysis of melanoma tumor biopsies from a population-based cohort",
               "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65904",
               "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE65904")
)

print_to_readme = function() {
  n = length(lst_real_data)
  data_names = names(lst_real_data)
  dims = sapply(names(lst_real_data), function(x) dim(get(paste0("real_", x))()$x))
  for (i in 1:n) {
    #cat("|", data_names[i], "|", lst_real_data[[i]][1], "|", paste0("[![](https://img.shields.io/badge/UCI-", data_names[i], "-blue)](",  lst_real_data[[i]][2], ")"), "|\n")
    cat("|", data_names[i], "|", lst_real_data[[i]][1], "|", dims[1, i], "|", dims[2, i], "|", paste0("[:link:](",  lst_real_data[[i]][2], ")"), "|\n")
  }
}

df_data_meta = function() {
  n = length(lst_real_data)
  data_names = names(lst_real_data)
  dims = sapply(names(lst_real_data), function(x) dim(get(paste0("real_", x))()$x))
  data.frame(
    Data = data_names,
    Description = sapply(1:n, function(i) lst_real_data[[i]][1]),
    n = dims[1, ],
    p = dims[2, ],
    URL = sapply(1:n, function(i) paste0("<a target='_blank' href='", lst_real_data[[i]][2], "'>&#128279;</a>"))
  )
}
# real.data.meta = df_data_meta()
# saveRDS(real.data.meta, file = "benchmark-tree-regressions/real-data-meta.rds")
prepare_data = function(prefix = "./") {
  if (!dir.exists(prefix)) dir.create(prefix)
  n = length(lst_real_data)
  folders = names(lst_real_data)
  for (i in 1:n) {
    destfile = paste0(prefix, folders[i], ".zip")
    if (!file.exists(destfile)) download.file(lst_real_data[[i]][3], destfile = destfile)
    unzip(destfile, exdir = paste0(prefix, folders[i]))
  }
}

download_with_retry <- function(url, destfile, max_attempts = 5, wait_time = 5) {
  attempt <- 0
  success <- FALSE

  while (attempt < max_attempts && !success) {
    attempt <- attempt + 1
    h <- curl::new_handle()

    # Check if a partial file exists and resume if needed
    if (file.exists(destfile)) {
      downloaded_size <- file.info(destfile)$size
      if (downloaded_size > 0) {
        curl::handle_setheaders(h, "Range" = paste0("bytes=", downloaded_size, "-"))
      }
    }

    # Set timeout options to handle bad network
    curl::handle_setopt(h, connecttimeout = 30, timeout = 120)  # Adjust as needed

    try({
      curl::curl_download(url, destfile, handle = h)
      success <- TRUE  # If no error, mark as successful
    }, silent = TRUE)

    if (!success) {
      message("Download failed, retrying... (Attempt ", attempt, " of ", max_attempts, ")")
      cat("url = ", url, " dest = ", destfile, "\n")
      Sys.sleep(wait_time)  # Wait before retrying
    }
  }

  if (!success) {
    stop("Download failed after multiple attempts.")
  }
}

download.data = function(prefix = "./real_data/", dataname = "CASP") {
  zipfile = paste0(prefix, dataname, ".zip")
  destfolder = paste0(prefix, dataname, "/")
  if (!dir.exists(destfolder))
    dir.create(destfolder, recursive = T)
  if (length(list.files(destfolder)) == 0) {
    if (!file.exists(zipfile)) {
      #download.file(lst_real_data[[dataname]][3], destfile = zipfile)
      download_with_retry(lst_real_data[[dataname]][3], zipfile)
    }
    unzip(zipfile, exdir = destfolder)
  }
}

# https://archive.ics.uci.edu/dataset/265/physicochemical+properties+of+protein+tertiary+structure
real_CASP = function(prefix = "./real_data/") {
  download.data(prefix = prefix, dataname = "CASP")
  destfile = paste0(prefix, "CASP/", "CASP.csv")
  df = read.csv(destfile)
  list(x = model.matrix(~ . - 1, df[, 2:ncol(df)]), y = df[, 1])
}

# https://archive.ics.uci.edu/dataset/374/appliances+energy+prediction
real_Energy = function(prefix = "./real_data/") {
  download.data(prefix = prefix, dataname = "Energy")
  destfile = file.path(prefix, "Energy/", "energydata_complete.csv")
  df = read.csv(destfile)
  # for simplicity, we exclude the date
  # NOTE: in the original data repo (https://github.com/LuisM78/Appliances-energy-prediction-data), they proposed to derive new features from the week status
  list(x = model.matrix(~. - 1, df[, 3:ncol(df)]), y = df[, 2])
}

# https://archive.ics.uci.edu/dataset/360/air+quality
real_AirQuality = function(prefix = "./real_data/") {
  download.data(prefix = prefix, dataname = "AirQuality")
  destfile = file.path(prefix, "AirQuality/", "AirQualityUCI.xlsx")
  df = as.data.frame(read_xlsx(destfile))
  # question: which one is the response?
  list(x = model.matrix(~ . - 1, df[, 4:ncol(df)]), y = df[, 3])
}

# https://archive.ics.uci.edu/dataset/514/bias+correction+of+numerical+prediction+model+temperature+forecast
real_BiasCorrection = function(prefix = "./real_data/") {
  download.data(prefix = prefix, dataname = "BiasCorrection")
  destfile = file.path(prefix, "BiasCorrection/", "Bias_correction_ucl.csv")
  df = read.csv(destfile)
  # rm NA values
  df = df[complete.cases(df[, 2:ncol(df)]),] # exclude the first column
  # take the Next_Tmax as output, (or Next_Tmin)
  list(x = model.matrix(~. - 1, df[, 3:(ncol(df) - 2)]),
       y = df[, ncol(df) - 1])
}

# https://archive.ics.uci.edu/dataset/471/electrical+grid+stability+simulated+data
real_ElectricalStability = function(prefix = "./real_data/") {
  download.data(prefix = prefix, dataname = "ElectricalStability")
  destfile = file.path(prefix, "ElectricalStability/", "Data_for_UCI_named.csv")
  df = read.csv(destfile)
  list(x = model.matrix(~. - 1, df[, 1:(ncol(df) - 2)]),
       y = df[, ncol(df) - 1])
}

# https://archive.ics.uci.edu/dataset/551/gas+turbine+co+and+nox+emission+data+set
real_GasTurbine = function(prefix = "./real_data/") {
  download.data(prefix = prefix, dataname = "GasTurbine")
  destfile = file.path(prefix, "GasTurbine/", "gt_2011.csv")
  df = read.csv(destfile)
  for (i in 2012:2015) {
    destfile = file.path(prefix, "GasTurbine/", paste0("gt_", i, ".csv"))
    tmp = read.csv(destfile)
    df = rbind(df, tmp)
  }
  list(x = model.matrix(~. - 1, df[, -8]),
       y = df[, 8]) # TEY as output
}

# https://archive.ics.uci.edu/dataset/437/residential+building+data+set
real_ResidentialBuilding = function(prefix = "./real_data/") {
  download.data(prefix = prefix, dataname = "ResidentialBuilding")
  destfile = file.path(prefix, "ResidentialBuilding/", "Residential-Building-Data-Set.xlsx")
  df = as.data.frame(suppressMessages(read_xlsx(destfile, skip = 1)));
  list(x = model.matrix(~. - 1, df[, 1:(ncol(df) - 2)]),
       y = df[, ncol(df) - 1])
}

# https://rdrr.io/cran/PRIMsrc/man/Real.2-data.html
# https://github.com/jedazard/PRIMsrc/blob/master/data/Real.2.rda
real_LungCancerGenomic = function(prefix = "./real_data/") {
  destfolder = file.path(prefix, "LungCancerGenomic/")
  destfile = file.path(destfolder, "Real.2.rda")
  if (!file.exists(destfile)) {
    if (!dir.exists(destfolder))
      dir.create(destfolder)
    #download.file("https://github.com/jedazard/PRIMsrc/raw/refs/heads/master/data/Real.2.rda", destfile = destfile)
    download_with_retry("https://github.com/jedazard/PRIMsrc/raw/refs/heads/master/data/Real.2.rda", destfile)
  }
  load(destfile)
  list(x = model.matrix(~. - 1, Real.2[, -1]),
       y = Real.2[, 1])
}

real_StructureActivity = function(prefix = "./real_data/") {
  download.data(prefix = prefix, dataname = "StructureActivity")
  tarfile = file.path(prefix, "/StructureActivity/drug_data")
  tarfolder = file.path(prefix, "/StructureActivity/")
  datafolder = file.path(tarfolder, "data/triazines/reg/")
  target_train_file = file.path(datafolder, paste0("tr60_", 0, ".dat"))
  target_test_file = file.path(datafolder, paste0("te60_", 0, ".dat"))
  if (!(file.exists(target_test_file) && file.exists(target_test_file))) {
    system(paste0("tar xf ", tarfile, " -C ", tarfolder))
    system(paste0("gzip -d -f ", tarfolder, "data/triazines/reg/*.Z"))
  }
  tr0 = read.table(target_train_file)
  te0 = read.table(target_test_file)
  t0 = rbind(tr0, te0) # I have verified sort(t1$V1) == sort(t0$V1)
  list(x = model.matrix(~. - 1, t0[, -1]), y = t0[, 1])
}

real_BloodBrain = function() {
  data("BloodBrain", package = "caret")
  list(x = model.matrix(~. - 1, bbbDescr), y = logBBB)
}

real_BostonHousing = function(prefix = "./real_data/") {
  destfolder = file.path(prefix, "BostonHousing/")
  destfile = file.path(destfolder, "Boston.csv")
  if (!file.exists(destfile)) {
    if (!dir.exists(destfolder))
      dir.create(destfolder, recursive = T)
    download_with_retry(lst_real_data[["BostonHousing"]][3], destfile)
  }
  df = read.csv(file.path(prefix, "BostonHousing/Boston.csv"))
  list(x = model.matrix(~. - 1, df[, -ncol(df)]), # introduce dummy variable for categorical variables
       y = log(df$medv))
}

real_CaliforniaHousing = function(prefix = "./real_data/") {
  destfolder = file.path(prefix, "CaliforniaHousing/")
  destfile = file.path(destfolder, "cadata.txt")
  if (!file.exists(destfile)) {
    if (!dir.exists(destfolder))
      dir.create(destfolder, recursive = T)
    download_with_retry(lst_real_data[["CaliforniaHousing"]][3], destfile)
  }
  df = read.table(file.path(prefix, "CaliforniaHousing/cadata.txt"), skip = 27)
  list(x = model.matrix(~. - 1, df[, -1]),
       y = log(df[, 1]))
}

real_GSE65904 = function() {
  gse = GEOquery::getGEO("GSE65904")
  exprs_matrix <- Biobase::exprs(gse[[1]])
  pheno_data <- Biobase::pData(gse[[1]])
  y = as.numeric(pheno_data$`disease specific survival in days:ch1`)
  # remove NA
  idx = !is.na(y)
  y = y[idx]
  x = t(exprs_matrix[, idx])
  x = log2(x + 1) # normalize
  list(x = x, y = y)
}


## ---- generate via generate_new_csv_function ---- ##
real_abalone = function(prefix = "./real_data/") {
  destfolder = file.path(prefix, "abalone")
  destfile = file.path(destfolder, "data.csv")
  if (!file.exists(destfile)) {
    if (!dir.exists(destfolder))
      dir.create(destfolder, recursive = T)
    download_with_retry("https://raw.githubusercontent.com/jbrownlee/Datasets/refs/heads/master/abalone.csv", destfile)
  }
  # assume no header
  # header name is separate in this dataset repo: https://github.com/jbrownlee/Datasets
  df = read.csv(destfile, header = FALSE)
  list(x = model.matrix(~. -1, df[, -9]),
       y = df[,9])
}
          

## ---- generate via generate_new_csv_function ---- ##
real_WineQualityRed = function(prefix = "./real_data/") {
  destfolder = file.path(prefix, "WineQualityRed")
  destfile = file.path(destfolder, "data.csv")
  if (!file.exists(destfile)) {
    if (!dir.exists(destfolder))
      dir.create(destfolder, recursive = T)
    download_with_retry("https://raw.githubusercontent.com/jbrownlee/Datasets/refs/heads/master/winequality-red.csv", destfile)
  }
  # assume no header
  # header name is separate in this dataset repo: https://github.com/jbrownlee/Datasets
  df = read.csv(destfile, header = FALSE)
  list(x = model.matrix(~. -1, df[, -12]),
       y = df[,12])
}
          