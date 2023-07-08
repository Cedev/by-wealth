source('libs.R')

un_population <- read.csv('data/UN/population.csv')
un_gdp_capita <- read.csv('data/UN/gdp_capita.csv')
un_codes <- read.csv('data/UN/country_codes.csv')
baci_codes <- read.csv("data/CEPII/country_codes_V202301.csv")
worldbank_gini <- read.csv('data/World Bank/API_SI.POV.GINI_DS2_en_csv_v2_5607101.csv')
cia_gini <- read.csv('data/CIA/gini_index.csv')
cia_codes <- read.csv('data/CIA/country_codes.csv')

baci_names <- baci_codes |>
  distinct(iso_3digit_alpha, .keep_all=TRUE)

un_codes <- un_population |>
  left_join(un_codes, join_by(Country.Area == Country.Area)) |>
  left_join(baci_codes, join_by(Country.Area == country_name_full), suffix=c("", ".full")) |>
  left_join(baci_codes, join_by(Country.Area == country_name_abbreviation), suffix=c("", ".abbr")) |>
  mutate(iso_3digit_alpha = coalesce(iso_3digit_alpha, iso_3digit_alpha.full, iso_3digit_alpha.abbr)) |>
  select(Country.Area, iso_3digit_alpha)

gini_cia <- cia_gini |> 
  left_join(cia_codes, join_by(name == name)) |>
  left_join(baci_codes, join_by(name == country_name_full), suffix=c("", ".full")) |>
  left_join(baci_codes, join_by(name == country_name_abbreviation), suffix=c("", ".abbr")) |>
  mutate(
    iso_3digit_alpha = coalesce(iso_3digit_alpha, iso_3digit_alpha.full, iso_3digit_alpha.abbr),
    gini = value/100,
    source='CIA') |>
  separate(date_of_information, into = 'year', extra = 'drop') |>
  select(name, iso_3digit_alpha, source, year, gini) |>
  arrange(iso_3digit_alpha)

gini_wb <- worldbank_gini |>
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
    names_prefix = "X",
    values_to = "Indicator"
  ) |>
  filter(!is.na(Indicator)) |>
  group_by(Country.Code) |>
  arrange(year == 2021, year) |>
  filter(row_number()==n()) |>
  mutate(
    gini=Indicator/100,
    source='WorldBank') |>
  select(name=Country.Name, iso_3digit_alpha=Country.Code, source, year, gini)

gini <- gini_wb |>
  union(gini_cia) |>
  group_by(iso_3digit_alpha) |>
  arrange(year == 2021, year, source == 'WorldBank') |>
  filter(row_number()==n())

all_countries <- un_population |>
  select(Country.Area, Population) |>
  left_join(un_codes, join_by(Country.Area == Country.Area)) |>
  select(Country.Area, Population, iso_3digit_alpha) |>
  left_join(un_gdp_capita, join_by(Country.Area == Country.Area)) |>
  full_join(gini, join_by(iso_3digit_alpha == iso_3digit_alpha)) |>
  full_join(baci_names, join_by(iso_3digit_alpha == iso_3digit_alpha)) |>
  mutate(name=coalesce(country_name_abbreviation, Country.Area, name)) |>
  select(iso_3digit_alpha, name, population=Population, gdp_per_capita=GDP..Per.Capita.GDP...US.Dollars, gini_year = year, gini, gini_source = source)

countries <- all_countries |>
  filter(!is.na(gini)) |>
  filter(!is.na(gdp_per_capita))
