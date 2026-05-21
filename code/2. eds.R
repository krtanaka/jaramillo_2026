library(readr)
library(dplyr)
library(janitor)

rm(list = ls())

eds1 <- read_csv("data/eds/eds_depth_rugosity.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  rename_with(~ gsub("^bathymetry_|_all_units|_nc$", "", .x)) %>%
  select(
    longitude, 
    latitude, 
    contains("rugosity")
  ) %>% 
  select(where(~ !all(is.na(.x))))

vis_miss(eds1)

eds2 <- read_csv("data/eds/eds_sedac_gfw_otp.csv", show_col_types = FALSE) %>%
  select(LONGITUDE, LATITUDE, (133:163)) %>% 
  clean_names() %>%
  # Global name trimming
  rename_with(~ gsub("tiff|tif|nc|gpw_v4_|hi_otp_all_|_v[0-9]{8}", "", .x)) %>%
  rename_with(~ gsub("_+", "_", .x)) %>%
  rename_with(~ gsub("_$", "", .x)) %>%
  # SPECIFIC CLEANUP FOR POPULATION DENSITY & TYPOS
  rename_with(~ gsub("^population_density_rev11_", "pop_den_", .x)) %>%
  rename_with(~ gsub("_30_sec_[0-9]{4}$", "", .x)) %>% # Drops the redundant trailing years
  rename_with(~ gsub("distae", "distance", .x)) %>%     # Fixes 'distae' typo
  select(
    longitude, 
    latitude, 
    contains("port"), 
    contains("shore"), 
    contains("pop_den"), # Updated keyword
    contains("fishing"),
    contains("invasive"), 
    contains("nearshore"),
    contains("osds")
  ) %>%
  distinct()

vis_miss(eds2)

eds3 <- read_csv("data/eds/eds_time.csv", show_col_types = FALSE) %>% 
  # Step 1: Grab the core identifier and 3-month mean columns
  select(LONGITUDE, LATITUDE, DATE_, new_MIN_DEPTH_M, new_MAX_DEPTH_M, 
         ISLAND,
         CCAH, CCAR, PESP,
         intersect(starts_with("mean"), ends_with("03mo"))) %>% 
  
  # Step 2: Drop the annual and monthly range columns from that selection
  select(!contains("monthly_range") & !contains("annual_range")) %>% 
  
  # Step 3: Run regex replacements matching the EXACT mixed-case of your raw data
  rename_with(function(x) {
    x %>%
      # 1. Clean up structural clutter and typos first
      str_replace_all("durnal", "diurnal") %>%
      str_replace_all("^mean_biweekly_range_", "bwr_") %>% 
      str_replace_all("^mean_", "") %>%                    
      str_replace_all("_03mo$", "") %>%                    
      # 2. Swap long environmental phrases with clean oceanographic acronyms
      str_replace_all("Bleaching_Alert_Area_7daymax", "baa_7dmax") %>%
      str_replace_all("Bleaching_Alert_Area", "baa") %>%
      str_replace_all("Bleaching_Hotspot", "hotspot") %>%
      str_replace_all("Chlorophyll_A", "chla") %>%
      str_replace_all("Degree_Heating_Weeks", "dhw") %>%
      str_replace_all("Sea_Surface_Temperature", "sst") %>%
      str_replace_all("Precipitation", "precip") %>%
      str_replace_all("Wave_Height", "wvht") %>%
      str_replace_all("Wave_Period", "wvpd") %>%
      str_replace_all("Wind_Speed", "wind") %>%
      # 3. CRITICAL FIX: Convert long tracking tags to short satellite tags instead of deleting them!
      str_replace_all("_CRW_daily", "") %>%
      str_replace_all("_ESA_OC_CCI_v6.0", "_cci") %>%
      str_replace_all("_ESA_OC_CCI", "_cci") %>%
      str_replace_all("_NPP_VIIRS", "_viirs") %>%
      str_replace_all("_NOAA_VIIRS", "_viirs") %>%
      str_replace_all("_NASA_VIIRS", "_viirs") %>%
      str_replace_all("_Aqua_MODIS", "_modis") %>%
      str_replace_all("_CHIRPS_daily", "") %>%
      str_replace_all("_ASCAT_daily", "") %>%
      str_replace_all("_WW3_Global_Hourly", "_global") %>%
      str_replace_all("_WW3_HI_Hourly", "_hi")
  }) %>% 
  # Step 4: Final formatting clean up to drop everything to lowercase snake_case
  clean_names() %>% 
  distinct()

# 1. Round coordinates uniformly across all sets to guarantee matching
eds1 <- eds1 %>% mutate(longitude = round(longitude, 3), latitude = round(latitude, 3))
eds2 <- eds2 %>% mutate(longitude = round(longitude, 3), latitude = round(latitude, 3))
eds3 <- eds3 %>% mutate(longitude = round(longitude, 3), latitude = round(latitude, 3))

# 2. Sequential Left Join: Base everything on the spatio-temporal frame (eds3)
eds <- eds3 %>%
  # Bring in depth and rugosity layers
  left_join(eds1, by = c("longitude", "latitude")) %>%
  # Bring in socioeconomic, human impacts, and fishing footprints
  left_join(eds2, by = c("longitude", "latitude"))

save(eds, file = "data/mhi_eds.rdata")
