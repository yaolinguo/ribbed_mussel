library(glmmTMB)
library(cowplot)
library(nortest)
library(sf)
library(knitr)
library(tidyverse)
library(brms)
library(ape)
library(coda)
library(patchwork)
library(modelr)
library(gridExtra)
library(pBrackets)
library(RColorBrewer)
library(performance)
library(phytools)
library(kableExtra)
library(tidybayes)
library(formattable)
library(grid)
library(ggplot2)
library(bayesplot)
library(metafor)
library(ggbeeswarm)
library(pander)
library(raster)
library(maps)
library(mapdata)
library(rworldmap)
library(readxl)
library(utils)
library(reshape2)
library(ggpubr)
library(stringr)
library(ggstance)
library(ggridges)
library(sp)
library(geodata)
library(multcompView)
library(multcomp)
library(rotl)
library(psych)
library(meta)
library(devtools)
library(processx)
library(gghalves)
library(ggtree)
library(leaflet)
library(ggspatial)
library(clipr)
library(gstat)
library(base)
library(car)
library(dplyr)
library(broom)
library(extrafont)
library(tidyselect)
library(lme4)
library(emmeans)
library(ggcorrplot)
library(MASS)
library(ggsignif)
library(agricolae)
library(scales)
library(ggExtra)
library(piecewiseSEM)
library(nlme)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(tigris)

# Figure 1a
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
