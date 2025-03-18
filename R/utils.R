generate_new_csv_function = function(DataName, DataURL, DataY) {
  DataY = as.integer(DataY)
  sprintf('

## ---- generate via generate_new_csv_function ---- ##
real_%s = function(prefix = "./real_data/") {
  destfolder = file.path(prefix, "%s")
  destfile = file.path(destfolder, "data.csv")
  if (!file.exists(destfile)) {
    if (!dir.exists(destfolder))
      dir.create(destfolder, recursive = T)
    download_with_retry("%s", destfile)
  }
  # assume no header
  # header name is separate in this dataset repo: https://github.com/jbrownlee/Datasets
  df = read.csv(destfile, header = FALSE)
  list(x = model.matrix(~. -1, df[, -%d]),
       y = df[,%d])
}
          ', DataName, DataName, DataURL, DataY, DataY)

}
