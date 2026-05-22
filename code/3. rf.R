library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext) 
library(pdp)
library(gridExtra) 
library(ggpubr)

set.seed(123)

rm(list = ls())

select = dplyr::select

load("data/mhi_eds.rdata"); df = eds; rm(eds)

df = df %>%
  # 1. Create the single depth column by averaging min and max
  mutate(depth = (new_min_depth_m + new_max_depth_m) / 2) %>%
  
  # 2. Keep pop_den_2020, but drop all other redundant population metrics
  # Using select(-...) drops the specific columns while keeping everything else intact
  select(
    -new_min_depth_m, -new_max_depth_m,   # Drop old individual depth columns
    -pop_den_1_deg, -pop_den_15_min,       # Drop coarse satellite resolutions
    -pop_den_2pt5_min, -pop_den_30_min,
    -pop_den_2000, -pop_den_2005,          # Drop outdated historical years
    -pop_den_2010, -pop_den_2015
  )

df = df %>%
  mutate(
    # Merge hierarchically: Highest resolution -> Medium -> Coarse
    rugosity = coalesce(
      hmrg_mhi_50m_rugosity, 
      crm_vol10_3s_rugosity, 
      etopo_2022_v1_15s_rugosity
    )
  ) %>%
  # Drop the original redundant individual layers to keep the dataset clean
  select(-hmrg_mhi_50m_rugosity, -crm_vol10_3s_rugosity, -etopo_2022_v1_15s_rugosity)

df = df %>% 
  select(
    # Target
    ccah, ccar, pesp,
    
    longitude, latitude, island, date, 
    
    # Structural/Physical
    depth, rugosity,
    
    # Oceanography & Climate Stress
    par_viirs_monthly, kd_par_viirs_monthly, maximum_dhw, chla_viirs_monthly,
    
    # Land-Based Pollution
    osds_nitrogen, nearshore_sediment,
    
    # Human Impact & Fishing
    fishing_rec_shore, pop_den_2020,
    
    # Biological Competition
    invasive_algae
  ) 

df %>% 
  ggplot(aes(longitude, latitude, fill = ccah)) + 
  geom_point(shape = 21, size = 5, alpha = 0.8) + 
  scale_fill_viridis_c(trans = "sqrt") + 
  facet_wrap(~island, scales = "free")

visdat::vis_miss(df)

hist(df$ccah)

df <- df %>%
  select(where(~ mean(is.na(.)) <= 0.10)) %>% 
  na.omit()

# 1. Prepare data: Keep ccah as a continuous variable and drop non-predictors
clean_df <- df %>% 
  select(
    -longitude, -latitude, -date, -island, # Drop metadata/coordinates
    -ccar, -pesp                          # Drop other response variables
    # everything()                           # Keep ccah and all environmental predictors
  ) %>% 
  na.omit() # Random Forest requires complete cases

# 2. Define X (features) and Y (target as continuous numeric)
x <- clean_df %>% select(-ccah)
y <- clean_df$ccah # Numeric vector for continuous regression

# A. Recursive Feature Elimination (RFE) for Regression
# Caret uses 'RMSE' as the default metric when y is numeric
ctrl_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
profile  <- rfe(x, y, sizes = c(1:10, 15, 20), rfeControl = ctrl_rfe, metric = "RMSE")
best_vars <- predictors(profile)

cat("Top selected predictors for continuous cover:\n")
print(best_vars)

# B. Final Model Tuning (Optimizing mtry based on RMSE)
fit_control <- trainControl(method = "cv", number = 5) # Removed classProbs
tune_grid   <- expand.grid(.mtry = seq(2, length(best_vars), by = 1))

tuned_rf <- train(x[, best_vars], y,
                  method = "rf",
                  metric = "RMSE",
                  trControl = fit_control,
                  tuneGrid = tune_grid,
                  ntree = 1000,
                  importance = TRUE)

# Show model performance summary (RMSE and R-squared)
print(tuned_rf)

plot(varImp(tuned_rf))

# Install packages if you don't have them yet
# install.packages(c("pdp", "patchwork"))

library(pdp)
library(ggplot2)
library(patchwork)
library(dplyr)

# 1. Dynamically pull the top 4 most important variables from your model
imp_matrix <- varImp(tuned_rf)$importance
top_vars   <- rownames(imp_matrix)[order(imp_matrix$Overall, decreasing = TRUE)][1:4]

# 2. Initialize a list to hold the individual plots
response_plots <- list()

# 3. Loop through the top variables and generate partial dependence curves
for (var in top_vars) {
  
  # Calculate the partial dependence grid
  # 'train = x[, best_vars]' provides the exact background data context needed
  pd_data <- partial(tuned_rf, pred.var = var, train = x[, best_vars])
  
  # Build a clean, publication-ready response curve
  p <- ggplot(pd_data, aes(x = .data[[var]], y = yhat)) +
    # Smooth local regression line to filter random forest step-jumps
    geom_smooth(method = "loess", formula = y ~ x, color = "darkcyan", 
                linewidth = 1.2, se = FALSE, span = 0.5) +
    # Faint raw-step line behind it to see the actual raw tree behavior
    geom_line(color = "darkcyan", alpha = 0.3, linewidth = 0.6) +
    
    theme_minimal() +
    labs(
      title = paste("Response Curve:", var),
      x = paste(var, "(gradient)"),
      y = "Predicted CCAH Cover (%)"
    ) +
    theme(
      plot.title = element_text(size = 11, face = "bold", color = "#222222"),
      axis.title = element_text(size = 9, color = "#444444"),
      panel.grid.minor = element_blank()
    )
  
  # Save plot to list
  response_plots[[var]] <- p
}

# 4. Stitch the plots together into a 2x2 grid using patchwork
final_response_grid <- wrap_plots(response_plots, ncol = 4) +
  plot_annotation(
    title = "Partial Dependence (Response Curves)",
    # subtitle = "Showing marginal effects on continuous CCAH percent cover",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, face = "italic", hjust = 0.5)
    )
  )

# Display the final plot grid
print(final_response_grid)

# 3. Predict continuous % cover back onto your dataframe
df_mapped <- df %>% 
  na.omit() 

# This saves the actual predicted % cover value for each site
df_mapped$pred_cover <- predict(tuned_rf, df_mapped[, best_vars])

map_df %>%
  ggplot(aes(x = ccah, y = pred_cover)) +
  geom_abline(intercept = 0, slope = 1, color = "grey60", linetype = "dashed", linewidth = 0.8) +
  geom_point(shape = 21, fill = "#185d94", color = "white", size = 5, alpha = 0.6) +
  geom_smooth(method = "lm", color = "#00f5d4", se = TRUE, fill = "#00f5d4", 
              alpha = 0.15, linewidth = 1) +
  coord_equal() + 
  lims(x = c(0, 45), y = c(0, 45)) +
  labs(
    title = "Model Performance: Predicted vs. Observed",
    x = "Observed Crustose Coralline Algae (ccah)",
    y = "Predicted Cover (pred_cover)"
  ) 

# 4. Prepare finalized dataframe for mapping/plotting
map_df <- df_mapped %>%
  mutate(
    island_name = island,
    actual_cover = ccah
  ) %>%
  mutate(island_name = factor(island_name, 
                              levels = c("Hawaii", "Maui", "Lanai", "Molokai", "Oahu", "Kauai", "Niihau")))

ggmap::register_google("AIzaSyDpirvA5gB7bmbEbwB1Pk__6jiV4SXAEcY")

units_list <- unique(map_df$island_name) 

all_unit_maps <- list()

current_zoom <- 10

for (unit in units_list) {
  
  # unit = units_list[2]
  
  unit_data <- map_df %>% filter(island_name == unit)
  
  unit_center <- c(lon = mean(unit_data$longitude, na.rm = TRUE), 
                   lat = mean(unit_data$latitude, na.rm = TRUE))
  
  basemap <- ggmap::get_googlemap(center = unit_center, 
                                  zoom = current_zoom, 
                                  maptype = "satellite", 
                                  color = "bw")
  
  lon_buffer <- diff(range(unit_data$longitude, na.rm = TRUE)) * 0.1
  lat_buffer <- diff(range(unit_data$latitude, na.rm = TRUE)) * 0.1
  
  lon_range <- c(min(unit_data$longitude) - lon_buffer, max(unit_data$longitude) + lon_buffer)
  lat_range <- c(min(unit_data$latitude) - lat_buffer, max(unit_data$latitude) + lat_buffer)
  
  p1 <- ggmap(basemap) +
    geom_point(data = unit_data, aes(x = longitude, y = latitude, fill = pred_cover),
               shape = 21, size = 5, alpha = 0.8, stroke = 0.1, color = "gray80") +
    scale_fill_viridis_c(name = "Pred\nCover", option = "viridis", trans = "sqrt") +
    # coord_cartesian(xlim = lon_range, ylim = lat_range) +
    # coord_cartesian() +
    coord_fixed() +
    labs(
      # title = unit,
      x = "Longitude (°W)",
      y = "Latitude (°N)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      legend.position = c(0.13, 0.98),
      legend.justification = c("right", "top"),
      legend.background = element_rect(fill = alpha("white", 0.6), color = "gray80"),
      legend.margin = ggplot2::margin(6, 6, 6, 6),
      plot.subtitle = element_text(margin = ggplot2::margin(b = 10))
    )
  
  p2 <- ggmap(basemap) +
    geom_point(data = unit_data, aes(x = longitude, y = latitude, fill = ccah),
               shape = 21, size = 5, alpha = 0.8, stroke = 0.1, color = "gray80") +
    scale_fill_viridis_c(name = "Obs\nCover", option = "viridis", trans = "sqrt") +
    # coord_cartesian(xlim = lon_range, ylim = lat_range) +
    # coord_cartesian() +
    coord_fixed() +
    labs(
      # title = unit,
      x = "Longitude (°W)",
      y = "Latitude (°N)"
    ) +
    theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      legend.position = c(0.13, 0.98),
      legend.justification = c("right", "top"),
      legend.background = element_rect(fill = alpha("white", 0.6), color = "gray80"),
      legend.margin = ggplot2::margin(6, 6, 6, 6),
      plot.subtitle = element_text(margin = ggplot2::margin(b = 10))
    )
  
  p2 + p1
  
  clean_unit <- trimws(unit)
  clean_filename <- gsub("[^[:alnum:]]", "_", clean_unit) # Removes slashes/spaces
  
  out_dir <- "C:/Users/Kisei.Tanaka/jaramillo_2026/"
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  file_path <- file.path(out_dir, paste0("Map_", clean_filename, "_", current_zoom, ".png"))
  
  ggsave(file_path, plot = p, width = 16, height = 9, dpi = 300)
  
  file_path_final <- file.path(out_dir,  paste0("Map_", clean_filename, "_", current_zoom, "_Survey_Comparison.png"))
  
  ggsave(file_path_final, 
         plot = p_combined, 
         width = 16, 
         height = 9, 
         dpi = 300)
  
  message(paste("Combined plot saved to:", file_path_final))
  
}

eds <- readr::read_rds("data/eds_grid.rds")
eds <- readr::read_rds("/Users/Kisei.Tanaka/Desktop/eds_grid.rds")
eds = eds %>% as.data.frame()
colnames(eds)[5] = "depth"
eds$depth <- eds$depth * -1

eds <- eds %>%
  mutate(unit = case_when(
    unit == "French_Frigate" ~ "French Frigate Shoals",
    unit == "Gardner"        ~ "Gardner Pinnacles",
    unit == "Kure"           ~ "Kure Atoll",
    unit == "Laysan"         ~ "Laysan Island",
    unit == "Lisianski"      ~ "Lisianski Island",
    unit == "Maro"           ~ "Maro Reef",
    unit == "Midway"         ~ "Midway Atoll",
    unit == "Necker"         ~ "Mokumanamana", 
    unit == "Nihoa"          ~ "Nihoa",
    unit == "Pearl_&_Hermes" ~ "Pearl and Hermes Atoll", 
    TRUE ~ unit 
  ))

table(eds$unit, useNA = "always")

# eds = eds %>% filter(date_r == unique(eds$date_r)[1])

rf_vars <- predictors(tuned_rf)

eds <- eds %>%
  drop_na(all_of(rf_vars))
# eds <- eds %>% na.omit()

eds$pred_prob <- predict(tuned_rf, eds[, best_vars], type = "prob")$X1
eds$pred_class <- predict(tuned_rf, eds[, best_vars], type = "raw")

eds %>% 
  filter(depth > 5) %>%
  filter(depth < 30) %>%
  group_by(lon, lat, unit) %>% 
  summarise(pred_prob = mean(pred_prob)) %>% 
  ggplot(aes(x = lon, y = lat, z = pred_prob)) +
  stat_summary_hex(fun = mean, bins = 20) +
  scale_fill_viridis_c(option = "plasma", "Predicted Probability of Occurrence") +
  facet_wrap(~unit, scales = "free")

eds %>%
  filter(depth > 5) %>%
  filter(depth < 30) %>%
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>%
  group_by(lon, lat, unit) %>%
  summarise(pred_prob = mean(pred_prob, na.rm = T)) %>%
  ggplot(aes(x = lon, y = lat, fill = pred_prob)) +
  geom_raster(interpolate = F) +
  scale_fill_viridis_c(option = "magma", 
                       # limits = c(0, 1),
                       # breaks = c(0, 1),s
                       name = "Predicted Probability of Occurrence") + 
  facet_wrap(~unit, scales = "free") +
  coord_quickmap() +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#1a1a1a", color = NA),
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.grid = element_blank(),
    text = element_text(color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 12),
    plot.title = element_text(size = 20, face = "italic"),
    legend.position = c(0.85, 0.15),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.title = element_text(size = 9, face = "bold"),
    legend.key.width = unit(1, "cm")) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

eds %>%
  filter(depth > 5) %>%
  filter(depth < 30) %>%
  mutate(lon = round(lon, 2),
         lat = round(lat, 2)) %>%
  group_by(lon, lat, unit) %>%
  summarise(pred_prob = mean(pred_prob, na.rm = T)) %>% 
  ggplot(aes(x = lon, y = lat, fill = pred_prob, color = pred_prob)) +
  geom_point(size = 2, alpha = 0.8, shape = 22) + 
  scale_fill_viridis_c("Probability", option = "turbo") +
  scale_color_viridis_c("Probability", option = "turbo") +
  facet_wrap(~unit, scales = "free") +
  coord_quickmap() +
  labs(title = "Predicted Probability of Occurrence",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

units_list <- unique(eds$unit)

current_zoom <- 13

for (current_unit in units_list) {
  
  # current_unit = units_list[1]
  
  unit_data <- eds %>% 
    filter(depth > 5) %>%
    filter(depth < 30) %>%
    mutate(lon = round(lon, 2),
           lat = round(lat, 2)) %>%
    group_by(lon, lat, unit) %>%
    summarise(pred_prob = mean(pred_prob, na.rm = T)) %>% 
    filter(unit == current_unit)
  
  lon_center <- mean(unit_data$lon, na.rm = TRUE)
  lat_center <- mean(unit_data$lat, na.rm = TRUE)
  
  basemap <- get_googlemap(
    center = c(lon = lon_center, lat = lat_center),
    zoom = current_zoom,
    maptype = "satellite",
    color = "bw")
  
  p <- ggmap(basemap, extent = "panel") +
    
    # geom_tile(data = unit_data, aes(x = lon, y = lat), 
    #           fill = "white", alpha = 0.1, width = 0.012, height = 0.012) +
    # geom_tile(data = unit_data, aes(x = lon, y = lat, fill = pred_prob), alpha = 0.8) +
    
    geom_tile(data = unit_data, 
              aes(x = lon, y = lat, fill = pred_prob), 
              alpha = 0.8, 
              color = "gray80",    # Thin white border
              linewidth = 0.1) + 
    
    scale_fill_viridis_c(
      # option = "magma", 
      name = "Prob", # Shortened name to fit inside the panel better
      # limits = c(0, 1),
      # breaks = c(0, 0.5, 1)
    ) +
    labs(
      title = paste("Predicted Probability of Occurrence:", gsub("_", " ", current_unit)),
      x = "Longitude", y = "Latitude"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      legend.position = c(0.02, 0.98), # Relative coordinates (x, y)
      legend.justification = c("left", "top"), # Anchors the legend at its top-left corner
      legend.background = element_rect(fill = alpha("white", 0.5), color = "gray80"),
      legend.margin = ggplot2::margin(6, 6, 6, 6),
      legend.title = element_text(size = 20, face = "bold"),
      legend.text = element_text(size = 12)
    ) +
    scale_x_continuous(guide = guide_axis(n.dodge = 2))
  
  file_name <- paste0("output/Map_Gridded_", current_unit, "_", current_zoom, ".png")
  
  ggsave(
    filename = file_name, 
    plot = p, 
    width = 10, 
    height = 8, 
    dpi = 400
  )
  
  message(paste("Successfully saved map for:", current_unit))
  
  colnames(eds)[3] = "date"
  
  unit_data <- eds %>% 
    filter(depth > 5, depth < 30) %>%
    mutate(lon = round(lon, 2),
           lat = round(lat, 2)) %>%
    group_by(lon, lat, unit, date) %>%
    summarise(pred_prob = mean(pred_prob, na.rm = T), .groups = "drop") %>% 
    filter(unit == current_unit)
  
  # 1. Identify the key date milestones
  first_date <- min(unit_data$date, na.rm = TRUE)
  last_date  <- max(unit_data$date, na.rm = TRUE)
  
  # Find the date that has the highest average probability for this island
  best_date <- unit_data %>%
    group_by(date) %>%
    summarise(avg_p = mean(pred_prob, na.rm = TRUE)) %>%
    slice_max(avg_p, n = 1, with_ties = FALSE) %>%
    pull(date)
  
  # 2. Extract data for specific date facets
  # We use unique() to avoid duplicates if best_date is also first or last
  selected_dates <- unique(c(first_date, best_date, last_date))
  
  endpoints <- unit_data %>%
    mutate(date = as.Date(date)) %>%
    filter(date %in% as.Date(selected_dates)) %>%
    mutate(facet_label = case_when(
      date == as.Date(first_date) ~ paste(date, "(First)"),
      date == as.Date(best_date)  ~ paste(date, "(Highest Prob)"),
      date == as.Date(last_date)  ~ paste(date, "(Last)"),
      TRUE ~ as.character(date)
    ))
  
  # 3. Create the aggregate facet
  all_dates <- unit_data %>%
    mutate(facet_label = "All Dates Combined")
  
  # 4. Combine and set factor levels for correct plotting order
  combined_data <- bind_rows(all_dates, endpoints) %>%
    mutate(facet_label = factor(facet_label, 
                                levels = c("All Dates Combined", 
                                           paste(as.Date(first_date), "(First)"), 
                                           paste(as.Date(best_date), "(Highest Prob)"),
                                           paste(as.Date(last_date), "(Last)"))))
  
  combined_data = combined_data %>% 
    filter(facet_label != "All Dates Combined") %>% 
    group_by(lon, lat, date, facet_label) %>% 
    summarise(pred_prob = mean(pred_prob))
  
  # 5. Plotting (Note: Removed coord_fixed(1) to avoid ggmap conflicts)
  p_combined <- ggmap(basemap) +
    geom_tile(data = combined_data, # geom_tile is usually safer for gridded rasters
              aes(x = lon, y = lat, fill = pred_prob),
              alpha = 0.8) +
    scale_fill_viridis_c(name = "Prob", option = "viridis") +
    facet_wrap(~facet_label, ncol = 3) + # Changed to 2 columns for a cleaner 2x2 grid
    labs(
      title = paste("Spatial Habitat Suitability:", current_unit),
      subtitle = "Gridded Random Forest Predictions",
      x = "Longitude", 
      y = "Latitude",
      caption = "All Dates Combined shows temporal mean. Specific dates show 'First', 'Last', and 'Highest Probability' events."
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "gray20", color = NA),
      strip.text = element_text(face = "bold", color = "white"),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )
  
  file_name <- paste0("output/Map_Gridded_", current_unit, "_", current_zoom, "_comparison.png")
  
  ggsave(file_name, 
         plot = p_combined, 
         width = 16, 
         height = 9, 
         dpi = 300)
  
  message(paste("Combined plot saved to:", file_name))
}