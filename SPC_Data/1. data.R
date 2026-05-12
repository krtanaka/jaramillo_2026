library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())

cca_codes <- read.csv("All_Photoquad_codes.csv") %>% filter(TIER_1 == "CCA") %>% pull(CODE) 
pac_codes <- read.csv("All_Photoquad_codes.csv") %>% filter(grepl("Peyssonnelia", T3_DESC, ignore.case = TRUE)) %>% pull(CODE)

site_effort <- read_rds("MHI_2010-24_BenthicCover.rds") %>%
  select(SITE, LATITUDE, LONGITUDE, ISLAND) %>% 
  distinct()

benthic_cover <- read_csv("MHI_BenthicCover_SiteLevel_Clean.csv") %>% 
  select(LONGITUDE_LOV, LATITUDE_LOV, DATE_, AVG_SITE_DEPTH_M, REGION, ISLAND, SITE, METHOD, REEF_ZONE, cca_codes, pac_codes)

library(tidyr)
library(ggplot2)
library(dplyr)
library(patchwork) 

create_species_map <- function(target_column, title_text) {
  ggplot(benthic_cover, aes(x = LONGITUDE_LOV, y = LATITUDE_LOV)) +
    geom_point(color = "grey10", size = 0.5, alpha = 0.4) +
    geom_point(aes(size = .data[[target_column]], color = .data[[target_column]]), alpha = 0.7) +
    scale_size_continuous(name = "Cover (%)", range = c(1, 10)) +
    scale_color_viridis_c(name = "Cover (%)", option = "mako", direction = -1) +
    guides(color = guide_legend(), size = guide_legend()) +
    theme_bw() +
    labs(title = title_text, x = "Longitude", y = "Latitude") +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )
}

map_ccah <- create_species_map("CCAH", "CCAH Cover")
map_ccar <- create_species_map("CCAR", "CCAR Cover")
map_pesp <- create_species_map("PESP", "PESP (PAC) Cover")

(map_ccah | map_ccar) / (map_pesp + plot_spacer())

mu <- benthic_cover %>%
  select(CCAH, CCAR, PESP) %>%
  pivot_longer(everything(), names_to = "Species", values_to = "Cover") %>%
  filter(Cover > 0) %>%
  group_by(Species) %>%
  summarise(grp.median = median(Cover))

benthic_cover %>%
  select(CCAH, CCAR, PESP) %>%
  pivot_longer(everything(), names_to = "Species", values_to = "Cover") %>%
  filter(Cover > 0) %>% # FOCUS ON PRESENCE
  ggplot(aes(x = Cover)) +
  geom_histogram(aes(y = after_stat(density), fill = after_stat(x)), 
                 color = "grey20",
                 bins = 30, alpha = 0.8, show.legend = FALSE) +
  geom_vline(data = mu, aes(xintercept = grp.median), 
             linetype = "dashed", color = "red", alpha = 0.8, size = 2) +
  facet_wrap(~Species, scales = "free", ncol = 1) + 
  scale_fill_viridis_c(direction = -1) +
  scale_y_continuous(trans = "pseudo_log") + 
  labs(
    title = "Conditional Abundance Distribution (Presence Only)",
    subtitle = "Red dashed line indicates median % cover per species",
    x = "% Cover (at occupied sites)", 
    y = "Density (Log Scale)"
  ) +
  theme(
    strip.text = element_text(size = 10, face = "bold.italic"), 
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank()
  )


