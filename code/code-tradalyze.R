# Goal : make a competitive analysis on beer and wine for 2015 to 2020 years

# Setup the analysis --------------------------------------------------------
## Install packages if needed -----------------------------------------------
# Needed to use all the tidyverse syntaxe
if (!rlang::is_installed("tidyverse")){
  install.packages("tidyverse")
}

# Needed to set path easily
if (!rlang::is_installed("here")){
  install.packages("here")
}

# Needed to download packages from github
if (!rlang::is_installed("devtools")){
  install.packages("devtools")
}

# Needed to manipulate data with parquet format and not in memory
if (!rlang::is_installed("arrow")){
  install.packages("arrow")
}

# Needed to manipulate label axis with ggplot2 for some graphs
if (!rlang::is_installed("scales")){
  install.packages("scales")
}

# Needed to clean dataframe name easily
if (!rlang::is_installed("janitor")){
  install.packages("janitor")
}

# Needed to create LATEX table
if (!rlang::is_installed("xtable")){
  install.packages("xtable")
}

# Install the dev version of concordance to have access of the latest
# Revision of the HS classification
# Check if package 'concordance' is not installed with a minimum version of 2.1.0
if (!rlang::is_installed("concordance", version = "2.1.0")){
  # Check if a latest version of concordance is installed
  if (rlang::is_installed("concordance")){
    # If its the case remove it
    remove.packages("concordance")
  }
  # Install the package from github
  devtools::install_github("insongkim/concordance")
}

# Install the package tradalyze (need to take 'refonte_totale' branch for the moment)
# Make sure to have download this pecific branch of the package
# `dependencies = TRUE` in order to install all the packages used by tradalyze even those in Suggest
devtools::install_github("Xayoux/tradalyze", dependencies = TRUE)


## Load all the libraries needed --------------------------------------------
library(tidyverse)
library(here)
library(arrow)
library(concordance)
library(tradalyze)
library(scales)
library(janitor)
library(xtable)



## Create sub-folders -------------------------------------------------------
# Subfolder for BACI data
dir.create(here("data", "BACI"), recursive = TRUE)
# Subfolder for Gravity data
dir.create(here("data", "Gravity"), recursive = TRUE)
# Subfolder for the output
dir.create(here("output"), recursive = TRUE)
# subfolder for output graphs
dir.create(here("output", "graphs"), recursive = TRUE)
# subfolder for output latex table
dir.create(here("output", "tables"), recursive = TRUE)

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
# If not working the first time just relaunch the command it work... IDK why...
dl_gravity(
  dl_folder = here("data", "Gravity"),
  dl_zip = TRUE # bug: Seems that FALSE doesn't work if data are not already dl... 
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
df_baci <-
  clean_uv_outliers(
    baci = here("data", "BACI", "BACI_HS92_V202501-parquet"), # Parquet folder
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


# World Trade evolution -----------------------------------------------------
# Graph to show the evolution of the world trade
g_world_trade <-
  df_baci |>
  # Compute the sum of exports for each year and product
  summarize(
    .by = c(t, sector),
    v = sum(v, na.rm = TRUE)
  ) |>
  collect() |>
  # Create the ggplot2 graph
  ggplot(aes(x = t, y = v)) +
  geom_line() +
  labs(
    x = "Années",
    y = "Valeur commerciale en milliers de dollars courants"
  ) +
  scale_x_continuous(breaks = seq(2015,2020, 1)) +
  scale_y_continuous(labels = label_dollar())+
  facet_wrap(~sector, scales = "free") +
  theme_bw() +
  ggplot2::theme(
    # Option des gridlines : les enlever
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    # Option du texte de l'axe des X
    axis.text.x =
      ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 18,
        color = "black"
      ),
    axis.title.x =
      ggplot2::element_text(
        size = 22,
        vjust = -0.5
      ),
    # Option du texte de l'axe des Y
    axis.text.y =
      ggplot2::element_text(
        size = 18,
        color = "black"
      ),
    axis.title.y =
      ggplot2::element_text(
        size = 22
      ),
    # Options des facettes
    strip.background =
      ggplot2::element_rect(
        colour = "black",
        fill = "#D9D9D9"
      ),
    strip.text =
      ggplot2::element_text(
        size = 18,
        color = "black"
      )
  )

g_world_trade

ggsave(here("output", "graphs", "commerce-mondial-HG.png"),
       g_world_trade, width = 15, height = 8)


# Taux de couverture de la France --------------------------------------------
# Calculer le total des exportations de chaque région
df_total_export <-
  df_baci |>
  summarize(
    .by = c(t, sector, exporter_name_region),
    total_export = sum(v, na.rm = TRUE)
  ) |>
  collect()


# Calculer le total des importations de chaque région
df_total_import <-
  df_baci |>
  collect() |>
  summarize(
    .by = c(t, sector, importer_name_region),
    total_import = sum(v, na.rm = TRUE)
  ) 


# Calculer le taux de couverture du haut de gamme
df_tx_couverture <-
  df_total_export |>
  left_join(
    df_total_import,
    join_by(t, sector, exporter_name_region == importer_name_region)
  )  |>
  mutate(
    tx_couverture = total_export / total_import
  )  |>
  arrange(desc(t), sector, exporter_name_region)

# Enregistrer les données 
write_csv(df_tx_couverture, here("output", "processed-data", "taux-couverture-HG.csv"))


g_tx_couverture <-
  df_tx_couverture |>
  filter(exporter_name_region == "France") |>
  graph_lines_comparison(
    x = "t",
    y = "tx_couverture",
    var_color = "sector",
    palette_color = "Paired",
    x_title = "Années",
    y_title = "Taux de couverture",
    title = "Evolution du taux de couverture français",
    print = FALSE
  )+
  geom_hline(
    yintercept = 1, color = "black"
  )

g_tx_couverture

ggsave(here("output","graphs", "tx-couverture-HG-france.png"),
       g_tx_couverture, width = 15, height = 8)


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
  return_output = FALSE,
  path_output = here("output", "graphs", "01-graph-market-share.png")
)


# Nb market reach by countries ---------------------------------------------
# Df with the number of product by sectors
# Needed to compute the total number of possible market (nb_produt * nb_countries)
df_nb_k_sector <-
  df_baci |>
  filter(t == 2020, exporter == "FRA") |>
  select(sector, k) |>
  distinct() |>
  summarize(
    .by = c(sector),
    nb_k = n()
  ) |>
  collect()

# Total number of countries
nb_countries <-
  df_baci  |>
  filter(t == 2020) |>
  collect() |>
  pull(exporter) |>
  unique() |>
  length()

# Nb of market reach by countries
df_nb_market <-
  df_baci |>
  summarize(
    .by = c(t, exporter, sector),
    nb_market = n()
  ) |>
  collect() |>
  arrange(desc(t), sector, desc(nb_market)) |>
  # compute the %
  left_join(
    df_nb_k_sector,
    join_by(sector)
  ) |>
  # Remove 1 countries to avoid the exporting country in the computation
  mutate(
    share_nb_market = nb_market / (nb_k * (nb_countries - 1)) * 100
  )

write_csv(df_nb_market, here("output","processed-data", "nb-market.csv"))


# Nb moyen de produits envoyés par pays dans chaque secteur
df_nb_mean_k <-
  df_baci |>
  # Compter le nombre de produits envoys dans chaque pays par secteur
  summarize(
    .by = c(t, sector, exporter, importer),
    nb_produits = n()
  )  |>
  collect() |>
  # Moyenne du nombre de produits envoyés
  summarize(
    .by = c(t, sector, exporter),
    mean_k = mean(nb_produits, na.rm = TRUE)
  ) |>
  # Trier
  arrange(desc(t), sector, desc(mean_k)) |>
  # Garder uniquement 2010 et 2022
  filter(t %in% c(2015, 2020)) |>
  # Mettre les années en colonne : meilleure présentation
  pivot_wider(
    names_from = t,
    values_from = mean_k
  ) |>
  clean_names()


# Bar graph
g_nb_market <-
  df_nb_market |>
  filter(exporter %in% c("FRA", "ITA", "CHN"))  |>
  graph_bar_comp_year(
    x = "exporter",
    y = "share_nb_market",
    double_bar = FALSE,
    var_t = "t",
    year_1 = 2020,
    year_2 = 2015,
    color_1 = "black",
    color_2 = "black",
    var_fill = "exporter",
    palette_fill = "Paired",
    var_fill_shape = "exporter",
    x_title = "Exporter",
    y_title = "Nombre de marchés où le pays est présent (%)",
    title = "Evolution du nombre de marché entre 2015 et 2020",
    caption = "Data from BACI",
    type_theme = "bw",
    var_facet = "sector",
    print = FALSE,
    return_output = TRUE
  ) +
  scale_x_discrete(labels = c("FRA" = "France", "ITA" = "Italie", "CHN" = "Chine")) +
  scale_y_continuous(labels = label_percent(scale = 1))+
  theme(legend.position = "none")

g_nb_market

ggsave(
  here("output", "graphs", "nb_market.png"),
  g_nb_market,
  width = 15,
  height = 8
)


# Table du nombre de produits moyen exportés par pays par secteur
table <-
  df_nb_mean_k |>
  # Garder que les pays d'intérets de l'étude : FRA et concurrents
  filter(exporter %in% c("FRA", "ITA", "CHN")) |>
  # Organiser la table
  relocate(sector, exporter, x2015) |>
  # Renommer les pays avec leur nom complet
  mutate(
    exporter =
      case_when(
        exporter == "FRA" ~ "France",
        exporter == "ITA" ~ "Italie",
        exporter == "CHN" ~ "Chine"
      )
  ) |>
  # Trier pour aficher les secteurs ensemble
  arrange(sector, desc(x2020)) |>
  # Garder uniquement la valeur au milieu pour le nom du secteur
  # Meilleure présentation
  mutate(
    .by = sector,
    num = row_number(),
    sector =
      case_when(
        num != 2 ~ "",
        .default = sector
      )
  ) |>
  select(-num) %>%
  # passer en table latex
  {
    nb_lignes <- nrow(.) # Sert pour mettre la dernière hline
    xtable(.) %>%
      print.xtable(
        type             = "latex",
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents    = TRUE,
        hline.after      = seq(3, nb_lignes - 1, 3)
      )
  }

# Supprimer les derniers \\
writeLines(
  substr(table, 1, nchar(table)-7), 
  here("output", "tables", "table-nb-mean-product-export.tex")
)


# Adressed demand -----------------------------------------------------------
# Compute the adressed demand : compared to France ; with Base 100 in 2015
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
g_adressed_demand <-
  graph_lines_comparison(
  baci = df_adressed_demand,
  x = "t",
  y = "DA_100_diff",
  var_color = "exporter_name_region",
  palette_color = "Paired",
  x_title = "years",
  y_title = "DA ratio with France",
  title = "Comparison of the evolution of the DA with France",
  type_theme = "bw",
  var_facet = "sector",
  print = FALSE,
  return_output = TRUE
)+
  # Line at 1
  geom_hline(yintercept = 1, linetype = "dashed")

g_adressed_demand

ggsave(
  here(
    "output",
    "graphs",
    "02-graph-adressed-demand.png"
  ),
  g_adressed_demand,
  width = 15,
  height = 8
)


# Evolutions of unit values for regions and sectors --------------------------
# Aggregate unit values by regions and sectors
g_unit_value <-
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

g_unit_value

ggsave(
  here(
    "output",
    "graphs",
    "03-graph-unit-value.png"
  ),
  g_unit_value,
  width = 15,
  height = 8
)


# Quality -------------------------------------------------------------------
## Create the dataframe for the khandelwal equation -------------------------
df_khandelwal_eq <-
  create_quality_df(
    baci =  here("data", "BACI", "BACI_HS92_V202501-parquet"),
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
# Use all data to perform the regression (no matter their classification)
df_quality <-
  khandelwal_quality_eq(
    data_reg = df_khandelwal_eq,
    y_var = "demand",
    x_var = "gdp_o + contig + dist + comlang_off + col_dep_ever",
    fe_var = "k^importer^t",
    print_reg_output = TRUE,
    return_output = TRUE
  )

# Print the result of regression
print(df_quality$lm)

# Print the data used for the regression and the output 
print(df_quality$data_reg)

# Save the data output
df_quality$data_reg |>
  group_by(t) |>
  write_dataset(here("output", "processed-data", "quality-data"))

## Aggregate and display the quality ---------------------------------------
g_quality <-
  df_quality$data_reg |>
  # Reperform the flow classification (not optimal can do better I guess)
  # Need to redo this because regression had been performed with raw data
  flow_classification(
    method = "fontagne_1997",
    alpha_H = 1.15, # Treshold
    alpha_L = 1.15,
    var_weighting = "q", # Weight by quantities
    return_output = TRUE,
    return_arrow = TRUE,
    path_output = NULL
  ) |>
  # Aggregate only for high end flows
  filter(fontagne_1997 == "H") |>
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
  # Graph of quality evolution
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

g_quality

ggsave(
  here(
    "output",
    "graphs",
    "04-graph-quality.png"
  ),
  g_quality,
  width = 15,
  height = 8
)






