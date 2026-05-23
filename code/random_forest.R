library(caret)
library(randomForest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(pdp)
library(patchwork)
library(ggmap)
library(visdat)

set.seed(123)
rm(list = ls())

# Load prepared EDS dataset
load("data/mhi_eds.rdata")
df <- eds
rm(eds)

# Prepare predictors and response variables
df <- df %>%
  mutate(
    depth = (new_min_depth_m + new_max_depth_m) / 2,
    rugosity = coalesce(
      hmrg_mhi_50m_rugosity,
      crm_vol10_3s_rugosity,
      etopo_2022_v1_15s_rugosity
    )
  ) %>%
  select(
    -new_min_depth_m, -new_max_depth_m,
    -hmrg_mhi_50m_rugosity, -crm_vol10_3s_rugosity, -etopo_2022_v1_15s_rugosity,
    -pop_den_1_deg, -pop_den_15_min, -pop_den_2pt5_min, -pop_den_30_min,
    -pop_den_2000, -pop_den_2005, -pop_den_2010, -pop_den_2015
  ) %>%
  select(
    ccah, ccar, pesp,
    longitude, latitude, island, date,
    depth, rugosity,
    par_viirs_monthly, kd_par_viirs_monthly, maximum_dhw, chla_viirs_monthly,
    osds_nitrogen, nearshore_sediment,
    fishing_rec_shore, pop_den_2020,
    invasive_algae
  )

# Quick data checks
df %>%
  ggplot(aes(longitude, latitude, size = ccah, fill = ccah)) +
  geom_point(alpha = 0.6, shape = 21) +
  scale_fill_viridis_c() +
  facet_wrap(~ island, scales = "free")

visdat::vis_miss(df)
hist(df$ccah)

# Remove variables with >10% missing data and retain complete cases
df <- df %>%
  select(where(~ mean(is.na(.)) <= 0.10)) %>%
  na.omit()

# Prepare RF regression dataset
model_df <- df %>%
  select(
    -longitude, -latitude, -date, -island,
    -ccar, -pesp
  ) %>%
  na.omit()

x <- model_df %>% select(-ccah)
y <- model_df$ccah

# Recursive feature elimination
ctrl_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

profile <- rfe(
  x = x,
  y = y,
  sizes = c(1:10, 15, 20),
  rfeControl = ctrl_rfe,
  metric = "RMSE"
)

best_vars <- predictors(profile)

cat("Top selected predictors for continuous CCAH cover:\n")
print(best_vars)

# Tune final random forest model
fit_control <- trainControl(method = "cv", number = 5)

tune_grid <- expand.grid(
  .mtry = seq(2, length(best_vars), by = 1)
)

tuned_rf <- train(
  x = x[, best_vars],
  y = y,
  method = "rf",
  metric = "RMSE",
  trControl = fit_control,
  tuneGrid = tune_grid,
  ntree = 1000,
  importance = TRUE
)

print(tuned_rf)
plot(varImp(tuned_rf))

# Partial dependence plots for top predictors
imp_matrix <- varImp(tuned_rf)$importance

top_vars <- rownames(imp_matrix)[
  order(imp_matrix$Overall, decreasing = TRUE)
][1:4]

response_plots <- lapply(top_vars, function(var) {
  
  pd_data <- partial(
    object = tuned_rf,
    pred.var = var,
    train = x[, best_vars]
  )
  
  ggplot(pd_data, aes(x = .data[[var]], y = yhat)) +
    geom_line(alpha = 0.3, linewidth = 0.6) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      linewidth = 1.2,
      se = FALSE,
      span = 0.5
    ) +
    theme_classic() +
    labs(
      title = paste(var),
      x = var,
      y = "Predicted CCAH Cover (%)"
    ) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
})

final_response_grid <- wrap_plots(response_plots, ncol = 4) +
  plot_annotation(
    title = "Partial Dependence Response Curves",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
  )

print(final_response_grid)

# Predict CCAH cover for mapped observations
map_df <- df %>%
  na.omit() %>%
  mutate(
    pred_cover = predict(tuned_rf, .[, best_vars]),
    actual_cover = ccah,
    island_name = factor(
      island,
      levels = c("Hawaii", "Maui", "Lanai", "Molokai", "Oahu", "Kauai", "Niihau")
    )
  )

# Observed vs predicted model check
map_df %>%
  ggplot(aes(x = actual_cover, y = pred_cover)) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "grey60",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_point(shape = 21, fill = "#185d94", color = "gray70", size = 5, alpha = 0.6) +
  geom_smooth(method = "lm", color = "#00f5d4", se = TRUE, fill = "#00f5d4",
              alpha = 0.5, linewidth = 1) +
  coord_equal() +
  theme_classic() +
  lims(x = c(0, 45), y = c(0, 45)) +
  labs(
    title = "Model Performance: Predicted vs. Observed",
    x = "Observed CCAH Cover (%)",
    y = "Predicted CCAH Cover (%)"
  )

# Mapping output
ggmap::register_google("")

out_dir <- "output/"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

units_list <- unique(na.omit(map_df$island_name))
current_zoom <- 9

for (unit in units_list) {
  
  unit_data <- map_df %>%
    filter(island_name == unit)
  
  unit_center <- c(
    lon = mean(unit_data$longitude, na.rm = TRUE),
    lat = mean(unit_data$latitude, na.rm = TRUE)
  )
  
  basemap <- ggmap::get_googlemap(
    center = unit_center,
    zoom = current_zoom,
    maptype = "satellite",
    color = "bw"
  )
  
  # Universal scale across observed and predicted cover
  cover_range <- range(c(unit_data$actual_cover, unit_data$pred_cover), na.rm = TRUE)
  
  cover_breaks <- scales::pretty_breaks(n = 6)(cover_range)
  cover_breaks <- cover_breaks[cover_breaks >= cover_range[1] & cover_breaks <= cover_range[2]]
  
  cover_cols  <- viridisLite::viridis(length(cover_breaks))
  cover_sizes <- seq(2, 10, length.out = length(cover_breaks))
  
  # Predicted cover map
  p_pred <- ggmap(basemap) +
    geom_point(data = unit_data,
               aes(longitude, latitude, size = pred_cover, fill = pred_cover),
               shape = 21, alpha = 0.8, stroke = 0.3, color = "gray80") +
    scale_fill_viridis_c(limits = cover_range, trans = "sqrt", guide = "none") +
    scale_size_continuous(name = "Predicted Cover (%)",
                          limits = cover_range, range = c(1.5, 6), breaks = cover_breaks) +
    guides(size = guide_legend(title.position = "top", title.hjust = 0.5,
                               nrow = 1, byrow = TRUE,
                               override.aes = list(fill = cover_cols, alpha = 0.8,
                                                   color = "gray80", size = cover_sizes))) +
    labs(x = "Longitude (°W)", y = "Latitude (°N)") +
    theme_minimal(base_size = 15) +
    theme(legend.position = c(0.985, 0.985),
          legend.justification = c("right", "top"),
          legend.direction = "vertical",
          legend.background = element_rect(fill = ggplot2::alpha("white", 0.6),
                                           color = "gray85", linewidth = 0.2),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.height = unit(0.32, "cm"),
          legend.key.width = unit(0.45, "cm"),
          legend.spacing.y = unit(0.08, "cm"),
          legend.margin = ggplot2::margin(1, 2, 1, 2),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 7),
          panel.grid = element_blank())
  
  # Observed cover map
  p_obs <- ggmap(basemap) +
    geom_point(data = unit_data,
               aes(longitude, latitude, size = actual_cover, fill = actual_cover),
               shape = 21, alpha = 0.8, stroke = 0.3, color = "gray80") +
    scale_fill_viridis_c(limits = cover_range, trans = "sqrt", guide = "none") +
    scale_size_continuous(name = "Observed Cover (%)",
                          limits = cover_range, range = c(1.5, 6), breaks = cover_breaks) +
    guides(size = guide_legend(title.position = "top", title.hjust = 0.5,
                               nrow = 1, byrow = TRUE,
                               override.aes = list(fill = cover_cols, alpha = 0.8,
                                                   color = "gray80", size = cover_sizes))) +
    labs(x = "Longitude (°W)", y = "Latitude (°N)") +
    theme_minimal(base_size = 15) +
    theme(legend.position = c(0.985, 0.985),
          legend.justification = c("right", "top"),
          legend.direction = "vertical",
          legend.background = element_rect(fill = ggplot2::alpha("white", 0.6),
                                           color = "gray85", linewidth = 0.2),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.height = unit(0.32, "cm"),
          legend.key.width = unit(0.45, "cm"),
          legend.spacing.y = unit(0.08, "cm"),
          legend.margin = ggplot2::margin(1, 2, 1, 2),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 7),
          panel.grid = element_blank())
  
  # Combine and save
  p_combined <- p_obs + p_pred +
    plot_annotation(title = paste("Observed vs. Predicted CCAH Cover:", unit))
  
  clean_unit <- gsub("[^[:alnum:]]", "_", trimws(unit))
  
  file_path <- file.path(
    out_dir,
    paste0("Map_", clean_unit, "_", current_zoom, "_Survey_Comparison.png")
  )
  
  ggsave(filename = file_path, plot = p_combined, width = 16, height = 9, dpi = 300)
  
  message("Combined plot saved to: ", file_path)
}
