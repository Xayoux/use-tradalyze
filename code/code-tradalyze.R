# Goal : make a competitive analysis on beer and wine for 2015 to 2020 years

# Setup the analysis --------------------------------------------------------
# Librabries to be used
# tidyverse
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# here
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

# devtools
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
library(devtools)

# arrow
if (!requireNamespace("arrow", quietly = TRUE)) {
  install.packages("arrow")
}
library(arrow)


# Download the tradalyze package from the right branch
devtools::install_github("Xayoux/tradalyze", ref = "refonte_totale")

# Use the tradalyze package
library(tradalyze)

# Create sub-folders
dir.create(here("data", "BACI"), recursive = TRUE)
dir.create(here("data", "Gravity"), recursive = TRUE)
dir.create(here("output"), recursive = TRUE)


# Download data -------------------------------------------------------------
## BACI data ----------------------------------------------------------------
# Download BACI data for the last revision and version
dl_baci(
  version = NULL,
  revision = "HS92",
  dl_folder = here("data", "BACI"),
  download = FALSE,
  unzip = FALSE,
  to_parquet = FALSE,
  rm_zip = FALSE,
  rm_csv = FALSE
)

## Gravity data -------------------------------------------------------------
# Download Gravity latest gravity data available
dl_gravity(
  dl_folder = here("data", "Gravity"),
  dl_zip = FALSE
)


# Define the products codes to be used ---------------------------------------
# beer and wine. Convert these code into the HS92 nomenclature
df_products <-
  extract_product(
    codes_vector = c("2203", "2204"),
    path_output = here("output", "products-description.xlsx"),
    revision_origin = "HS6",
    revision_destination = "HS0",
    return_output = TRUE
  )

# Prepare BACI data ----------------------------------------------------------
# Clean outliers
df_baci <- clean_uv_outliers(
  baci = here("data", "BACI", "BACI_HS92_V202401b-parquet"), # Parquet folder
  years = 2015:2020, # Keep only wanted years
  codes = df_products$code_HS0, # Keep only wanted products
  method = "sd", # Remove observation greater than x standard deviations,
  alpha_H = 3, # x = 3
  alpha_L = 3,
  outliers.rm = TRUE,
  rm_temp_var = TRUE,
  return_output = TRUE,
  return_arrow = TRUE,
  path_output = NULL
)  |>
  # Compute fontagne_1997 flow classification
  # Don't need to indicate baci or years or code because of the pipe (|>)
  flow_classification(
    method = "fontagne_1997",
    alpha_H = 1.15, # Treshold
    alpha_L = 1.15,
    var_weighting = "q", # Weight by quantities
    return_output = TRUE,
    return_arrow = TRUE,
    path_output = NULL
  ) |>
  # Keep only high end flows
  filter(fontagne_1997 == "H") |>
  # Create a sector variable indicating if the product is a beer or wine product
  mutate(
    sector = substr(k, 1, 4), # Extract the 4 first digits
    sector =
      case_when(
        sector == "2203" ~ "Beer",
        sector == "2204" ~ "Wine"
      )
  ) |>
  # Add chelem geographic classification
  add_chelem_classification(
    return_output = TRUE,
    return_arrow = FALSE,
    path_output = NULL
  ) |>
  # Isolate FRA, ITA and CHN as regions and reduce the number of regions
  mutate(
    exporter_name_region =
      case_when(
        exporter == "FRA" ~ "France",
        exporter == "ITA" ~ "Italy",
        exporter == "CHN" ~ "China",
        exporter_name_region %in% c("Others in Europe", "European Union") ~ "Europe",
        exporter_name_region %in% c("South Asia and Pacific", "South-East Asia",
                                    "North-East Asia") ~ "Asia",
        exporter_name_region %in% c("North Africa", "Sub-Sahara Africa") ~ "Africa",
        exporter_name_region %in% c("South America, central America and Caribbean",
                                    "North America") ~ "America",        
        .default = "RoW"
      ),
    importer_name_region =
      case_when(
        importer == "FRA" ~ "France",
        importer == "ITA" ~ "Italy",
        importer == "CHN" ~ "China",
        importer_name_region %in% c("Others in Europe", "European Union") ~ "Europe",
        importer_name_region %in% c("South Asia and Pacific", "South-East Asia",
                                    "North-East Asia") ~ "Asia",
        importer_name_region %in% c("North Africa", "Sub-Sahara Africa") ~ "Africa",
        importer_name_region %in% c("South America, central America and Caribbean",
                                    "North America") ~ "America",        
        .default = "RoW"
      )
  ) |>
  # Remove iso code for regions
  select(!c(exporter_iso_region, importer_iso_region)) |>
  print()

# Save baci data
df_baci |>
  group_by(t) |>
  write_dataset(here("output", "processed-data", "BACI-processed"))


# Exporter's market shares ---------------------------------------------------
# Market share by time, regions and sectors of v and q
df_market_share <-
  market_share(
    baci = df_baci,
    var_aggregate = c("t", "sector", "exporter_name_region"),
    var_share = c("v", "q"),
    return_output = TRUE,
    return_arrow = FALSE
  ) |>
  print()

# Graph for v
graph_market_share(
  baci = df_market_share,
  x = "t",
  y = "market_share_v",
  graph_type = "area",
  var_fill_color = "exporter_name_region",
  palette_color = "Paired",
  percent = TRUE,
  x_title = "Years",
  y_title = "market share of v",
  title = "Evolution of the market share",
  type_theme = "bw",
  var_facet = "sector",
  print = TRUE,
  return_output = FALSE
)


# Adressed demand -----------------------------------------------------------
# Compute the adressed demand
df_adressed_demand <-
  adressed_demand(
    baci = df_baci,
    year_ref = 2015,
    var_exporter = "exporter_name_region",
    var_k = "sector",
    var_importer = "importer",
    base_100 = TRUE,
    compare = TRUE,
    exporter_ref = "France",
    return_output = TRUE,
    return_arrow = FALSE,
    path_output = NULL
  ) |>
  print()

# Graph
graph_lines_comparison(
  baci = df_adressed_demand,
  x = "t",
  y = "DA_100_diff",
  var_color = "exporter_name_region",
  palette_color = "Paired",
  x_title = "years",
  y_title = "DA ration with France",
  title = "Comparison of the evolution of the DA with France",
  type_theme = "bw",
  var_facet = "sector",
  print = FALSE,
  return_output = TRUE
)+
  # Line at 1
  geom_hline(yintercept = 1, linetype = "dashed")


# Evolutions of unit values for regions and sectors --------------------------
# Aggregate unit values by regions and sectors
aggregate_compare(
  baci = df_baci,
  method = "weighted_median",
  var = "uv",
  var_aggregation = c("t", "sector", "exporter_name_region"),
  var_temporal = "t",
  var_weight = "q",
  fixed_weight = FALSE, # Floating weights
  year_ref_fixed_weight = NULL, # Floating weights
  base_100 = TRUE, # Compute the base 100
  year_ref_base_100 = 2015,
  compare = TRUE, # Compare base 100 with a reference country,
  var_exporter = "exporter_name_region",
  exporter_ref = "France",
  return_output = TRUE,
  return_arrow = FALSE
) |>
  print() |>
  # Graph
  graph_bar_comp_year(
    x = "exporter_name_region",
    y = "uv",
    double_bar = FALSE,
    var_t = "t",
    year_1 = 2020,
    year_2 = 2015,
    color_1 = "black",
    color_2 = "black",
    var_fill = "exporter_name_region",
    palette_fill = "Paired",
    var_fill_shape = "exporter_name_region",
    x_title = "Exporter",
    y_title = "Weighted median of unit values",
    title = "Evolution of the unit values",
    caption = "Data from BACI",
    type_theme = "bw",
    var_facet = "sector",
    print = FALSE,
    return_output = TRUE
  ) +
  # Remove legend of the graph
  theme(
    legend.position = "none"
  )


# Quality -------------------------------------------------------------------
## Create the dataframe for the khandelwal equation -------------------------
df_khandelwal_eq <-
  create_quality_df(
    baci =  here("data", "BACI", "BACI_HS92_V202401b-parquet"),
    gravity = here("data", "Gravity", "Gravity_csv_V202211", "Gravity-parquet"),
    years = 2015:2020,
    codes = df_products$code_HS0,
    gravity_variables =
      c(
        "year", "iso3_o", "iso3_d", "dist", "contig", "distw_harmonic",
        "comlang_off", "comlang_ethno", "comcol", "col45", "col_dep_ever",
        "gdp_o", "gdp_d"
      ),
    revision_codes = "HS0",
    return_output = TRUE,
    return_arrow = FALSE,
    path_output = NULL
  ) |>
  print()

## Perform the khandelwal equation ------------------------------------------
df_quality <-
  khandelwal_quality_eq(
    data_reg = df_khandelwal_eq,
    y_var = "demand",
    x_var = "gdp_o + contig + dist + comlang_off + col_dep_ever",
    fe_var = "k^importer^t",
    print_reg_output = TRUE,
    return_output = TRUE
  )

print(df_quality$lm)

print(df_quality$data_reg)

df_quality$data_reg |>
  group_by(t) |>
  write_dataset(here("output", "processed-data", "quality-data"))

## Aggregate and display the quality ---------------------------------------
df_quality$data_reg |>
  # Aggregate only for high end flows
  filter(fontagne_1997 == "H") |>
  aggregate_compare(
    method = "weighted_median",
    var = "quality",
    var_aggregation = c("t", "sector", "exporter_name_region"),
    var_temporal = "t",
    var_weight = "q",
    fixed_weight = FALSE, # Floating weights
    year_ref_fixed_weight = NULL, # Floating weights
    base_100 = TRUE, # Compute the base 100
    year_ref_base_100 = 2015,
    compare = TRUE, # Compare base 100 with a reference country,
    var_exporter = "exporter_name_region",
    exporter_ref = "France",
    return_output = TRUE,
    return_arrow = FALSE
  ) |>
  print() |>
  # Graph
  graph_bar_comp_year(
    x = "exporter_name_region",
    y = "quality",
    double_bar = FALSE,
    var_t = "t",
    year_1 = 2020,
    year_2 = 2015,
    color_1 = "black",
    color_2 = "black",
    var_fill = "exporter_name_region",
    palette_fill = "Paired",
    var_fill_shape = "exporter_name_region",
    x_title = "Exporter",
    y_title = "Weighted median of quality",
    title = "Evolution of the quality",
    caption = "Data from BACI",
    type_theme = "bw",
    var_facet = "sector",
    print = FALSE,
    return_output = TRUE
  ) +
  # Remove legend of the graph
  theme(
    legend.position = "none"
  )









