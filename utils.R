source('libs.R')

# from R documentation
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
erf.inv <- function (x) qnorm((1 + x)/2)/sqrt(2)

justFuckingCall <- function(f, args, ...) do.call(f, c(args, list(...)))

fill <- function(v, x) {
  filled <- replicate(length(v), x)
  dim(filled) <- dim(v)
  return(filled)
}

scale1 <- function(x) scale(x, center=FALSE, scale=colSums(x))

r_squared <- function(predicted, observed) 1 - sum((observed - predicted)^2)/sum((observed-mean(observed))^2)

pivot_matrix <- function(df, row_name, col_name, values_from) {
  row_name <- enquo(row_name)
  col_name <- enquo(col_name)
  values_from <- enquo(values_from)
  
  pivotted <- df |>
    group_by(!!row_name, !!col_name) |>
    arrange(!!values_from) |>
    summarize(value=sum(!!values_from)) |>
    pivot_wider(names_from=!!col_name, values_from=value, values_fill = 0, names_sort = TRUE) |>
    ungroup() |>
    arrange(!!row_name)
  
  m <- as.matrix(pivotted[,-1])
  rownames(m) <- pivotted[,1][[1]]
  return(m)
}
