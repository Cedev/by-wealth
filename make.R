
make <- function(target, dependencies, definition) {
  target_dir <- dirname(target)
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  if (!file.exists(target) || file.mtime(target) < max(sapply(dependencies, file.mtime))) {
    definition(target, dependencies)
  }
}

make.csv <- function(target, dependencies, definition) {
  make(target, dependencies, function(t, ds) {
    data <- definition(ds)
    write.csv(data, target, row.names=FALSE)
  })
  return(readr::read_csv(target))
}

make.parquet <- function(target, dependencies, definition) {
  make(target, dependencies, function(t, ds) {
    data <- definition(ds)
    arrow::write_parquet(data, t)
  })
  return(arrow::read_parquet(target))
}