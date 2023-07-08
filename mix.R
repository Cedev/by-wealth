source('utils.R')

setClass(
  "Mix",
  contains="UnivarMixingDistribution",
  slots=c(quantile_bins="function")
)

mix <- function(distributions, weights) {
  umd <- justFuckingCall(distr::UnivarMixingDistribution, distributions, mixCoeff = w, withSimplify = FALSE)
  
  obj <- as(umd, 'Mix')
  
  # UnivarMixingDistribution doesn't include a probability density function
  obj@d <- function(x) obj@mixCoeff * sapply(obj@mixDistr, function(d) d@d(x))
  
  obj@quantile_bins <- function(bins=1024) {
    qs <- obj@q(seq(1, bins-1)/bins)
    
    ps <- t(sapply(obj@mixDistr, function(d) d@p(qs)))
    
    unweighted <- cbind(ps, fill(obj@mixCoeff, 1)) - cbind(fill(obj@mixCoeff, 0), ps)
    
    rownames(unweighted) <- names(obj@mixCoeff)
    return ((obj@mixCoeff * bins) * unweighted)
  }
  
  return(obj)
}