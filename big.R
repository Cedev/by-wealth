source('mix.R')
source('population.R')

big <- read.csv(text = 'big_category
CHN
IND
USA
IDN
PAK
BRA
NGA
BGD
RUS
MEX
JPN
ETH') |>
  select(iso_3digit_alpha = big_category, big_category=big_category )
  

eu <- melt(
  c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
    "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
    "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")
) |>
  mutate(big_category = 'EU') |>
  select(iso_3digit_alpha = value, big_category)

world_pop = sum(countries$population)

categorized <- countries |>
  left_join(union(big, eu), join_by(iso_3digit_alpha == iso_3digit_alpha)) |>
  arrange(gdp_per_capita) |>
  mutate(
    gdp = gdp_per_capita * population,
    big_category = coalesce(big_category, 'World'),
    p = (cumsum(as.numeric(population)) - population/2)/world_pop
  )

category_order <- categorized |>
  mutate(
    big_name = ifelse(big_category == iso_3digit_alpha, name, big_category)
  ) |>
  group_by(big_category, big_name) |>
  summarise(
    gdp=sum(gdp),
    population=sum(population)
  ) |>
  mutate(
    gdp_per_capita = gdp/population,
    ) |>
  arrange(-gdp_per_capita)

categorized$big_category <- factor(categorized$big_category, levels=category_order$big_category )

m <- log(categorized$gdp_per_capita)
s <- 2*erf.inv(categorized$gini)
w <- categorized$population/sum(categorized$population)

world <- mix(mapply(Norm, m, s), w)

library(scales)

ggplot(
  data.frame(x=world@p(m), m, categorized$gdp_per_capita),
  aes(x=x, y=categorized.gdp_per_capita)
) + 
  geom_point() +
  scale_y_log10(labels = dollar) +
  labs(title = "Nation GDP per capita by GDP/capita quantile", y="GDP per capita", x="GDP/capita quantile")


ggplot(
  data.frame(x=categorized$p, m, categorized$gdp_per_capita),
  aes(x=x, y=categorized.gdp_per_capita)
) + 
  geom_point() +
  scale_y_log10(labels = dollar) +
  labs(title = "Nation GDP per capita in GDP/capita order", y="GDP per capita", x="Population quantile by national GDP/capita")

gdp_by_gdp <- data.frame(log_y=m, log_x=t(scale1(world@d(m))) %*% m)
gdp_by_gdp$x <- exp(gdp_by_gdp$log_x)
gdp_by_gdp$y <- exp(gdp_by_gdp$log_y)

gdp_by_gdp_lim <- range(c(gdp_by_gdp$x, gdp_by_gdp$y))


ggplot(
  gdp_by_gdp,
  aes(x=x, y=y)
) + 
  geom_point() +
  scale_y_log10(labels = dollar) +
  scale_x_log10(labels = dollar) +
  geom_abline(slope = 1) +
  geom_text( aes(x=gdp_by_gdp_lim[1], y=gdp_by_gdp_lim[2], hjust=0, vjust=1), label = r2_label, parse = TRUE, show.legend = FALSE) +
  coord_fixed(xlim = gdp_by_gdp_lim, ylim = gdp_by_gdp_lim) +
  labs(title = "Nation GDP per capita vs smoothed GDP/capita", y="GDP per capita", x="smoothed GDP/capita")


resolution <- 1024

ds <- world@quantile_bins(resolution)
df <- melt(t(ds)) |>
  mutate(
    p = (Var1-0.5)/resolution,
    big_category = categorized[Var2,'big_category'])

ggplot(df, aes(x=p, y=value, fill=big_category)) + 
  geom_bar(stat="identity", width=1/resolution) +
  scale_fill_manual("legend",
    values = rev(c("#b98d32", "#115740", "#1B7339", "#FF9933", "#815f4b", "#ff8080", "#76a442", "#CCCCCC", "#ab8179", "#dcacb1", "#EE1C25", "#BC002D", "#001489", "#d9d2d6")),
    labels = category_order$big_name
  ) +
  labs(title = "World population by national GDP/capita", y = "portion of population", x = "GDP/capita quantile")

                    