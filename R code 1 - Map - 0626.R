library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(scales)

# -------------------------
# Figure 1
# -------------------------
options(tigris_use_cache = TRUE)
la_state <- states(cb = TRUE, year = 2023) |>
  filter(STUSPS == "LA")

la_parish <- counties(state = "LA", cb = TRUE, year = 2023)
la_state  <- st_transform(la_state, 3857)
la_parish <- st_transform(la_parish, 3857)
site <- data.frame(lon = -90.66147,
                   lat =  29.25401) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

site3857 <- st_transform(site, 3857)

p1a <- ggplot() +
  geom_sf(data = la_state,  fill = "white",
          color = "black", linewidth = 0.6) +
  geom_sf(data = la_parish,
          fill = NA,
          color = alpha("grey70", 0.6),
          linewidth = 0.3) +
  geom_sf(data = site3857,
          shape = 21, size = 7, stroke = 0.6,
          fill = "red", colour = "white") +
  coord_sf() +
  labs(title = "Louisiana (USA) with Parish Borders") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", linewidth = 0.8, fill = NA),
        axis.text   = element_text(size = 14))
p1a

ggsave("map.pdf",
       plot   = p1a,
       width  = 10,
       height = 8,
       units  = "in")