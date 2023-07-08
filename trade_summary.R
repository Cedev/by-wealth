source('libs.R')
source('make.R')

baci_trade <- make.parquet(
  'computed/baci_trade.parquet',
  c('data/CEPII/BACI_HS17_Y2021_V202301.csv', this.path::this.path()),
  function(dependencies) read.csv(dependencies[[1]])
)

baci_trade_summary <- make.csv(
  'computed/trade_summary.csv',
  c('data/CEPII/BACI_HS17_Y2021_V202301.csv', this.path::this.path()),
  function(dependencies) {
    baci_trade <- read.csv(
      dependencies[[1]],
      colClasses = c('integer', 'integer', 'integer', 'character', 'numeric', 'numeric')
    ) |>
      mutate(hs2=substr(k,1,2)) |>
      group_by(t,i,j,hs2) |>
      arrange(v) |>
      summarize(v=sum(v), q=sum(q)) |>
      ungroup()
  })
