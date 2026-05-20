library(readr)
library(dplyr)
library(janitor)

eds1 <- read_csv("data/eds/eds_depth_rugosity.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  rename_with(~ gsub("^bathymetry_|_all_units|_nc$", "", .x)) %>%
  select(
    longitude, 
    latitude, 
    date, 
    matches("crm|etopo|hmrg")
  ) %>%
  distinct()

# Check the results
names(eds1)
head(eds1)

eds2 <- read_csv("data/eds/eds_sedac_gfw_otp.csv", show_col_types = FALSE) %>%
  select(LONGITUDE, LATITUDE, DATE_, (ncol(.)-30):ncol(.)) %>% 
  clean_names() %>%
  rename_with(~ gsub("tiff|tif|nc|gpw_v4_|hi_otp_all_|_v[0-9]{8}", "", .x)) %>%
  rename_with(~ gsub("_+", "_", .x)) %>%
  rename_with(~ gsub("_$", "", .x)) %>%
  select(
    longitude, 
    latitude, 
    date = date, # Corrected: maps the cleaned 'date_' to 'date'
    contains("port"), 
    contains("shore"), 
    contains("population"),
    contains("fishing"),
    contains("invasive"), 
    contains("nearshore"),
    contains("osds")
  ) %>%
  distinct()

names(eds2)
