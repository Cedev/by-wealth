source('trade_summary.R')

baci_codes <- read.csv("data/CEPII/country_codes_V202301.csv")

# values are in thousands of dollars
baci_unit = 1000

trade_summary <- baci_trade_summary |>
  inner_join(baci_codes, join_by(i == country_code)) |>
  inner_join(baci_codes, join_by(j == country_code)) |>
  mutate(
    value = v*baci_unit,
    quantity = q,
    exporter = iso_3digit_alpha.x,
    importer = iso_3digit_alpha.y
  ) |>
  select(
    exporter,
    importer,
    hs2,
    value,
    quantity
  )

exports_by_hs2 <- trade_summary |>
  group_by(exporter, hs2) |>
  arrange(value) |>
  summarize(value=sum(value), quantity=sum(quantity)) |>
  select(iso_3digit_alpha=exporter, hs2, value, quantity)

imports_by_hs2 <- trade_summary |>
  group_by(importer, hs2) |>
  arrange(value) |>
  summarize(value=sum(value), quantity=sum(quantity)) |>
  select(iso_3digit_alpha=importer, hs2, value, quantity)

exports <- exports_by_hs2 |>
  group_by(iso_3digit_alpha) |>
  arrange(value) |>
  summarize(value=sum(value), quantity=sum(quantity)) |>
  select(iso_3digit_alpha, value, quantity)

imports <- imports_by_hs2 |>
  group_by(iso_3digit_alpha) |>
  arrange(value) |>
  summarize(value=sum(value), quantity=sum(quantity)) |>
  select(iso_3digit_alpha, value, quantity)
