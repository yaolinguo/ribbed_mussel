library(Hmisc)
library(readxl)
library(ggplot2)
library(dplyr)
library(purrr)
library(broom)
library(tibble)
library(cowplot)
library(scales)
library(flextable)
library(officer)

# -------------------------
# Figure S1 - sea level rise
# -------------------------
# setwd("Final version/GitHub")
data <- read_excel("Raw data - Final data - 0623.xlsx")
data <- as.data.frame(data)

model1  <- lm(Below_biomass        ~ Elevation, data = data)
summary(model1)
model2  <- lm(Above_biomass        ~ Elevation, data = data)
summary(model2)
model3  <- lm(Plant_biomass        ~ Elevation, data = data)
summary(model3)
model4  <- lm(lnSRR                ~ Elevation, data = data)
summary(model4)
model5  <- lm(Live_stems           ~ Elevation, data = data)
summary(model5)
model6  <- lm(Live_leaves          ~ Elevation, data = data)
summary(model6)
model7  <- lm(Height_average       ~ Elevation, data = data)
summary(model7)
model8  <- lm(Flowers              ~ Elevation, data = data)
summary(model8)
model9  <- lm(Below_C              ~ Elevation, data = data)
summary(model9)
model10 <- lm(Below_N              ~ Elevation, data = data)
summary(model10)
model11 <- lm(Below_CN             ~ Elevation, data = data)
summary(model11)
model12 <- lm(Above_C              ~ Elevation, data = data)
summary(model12)
model13 <- lm(Above_N              ~ Elevation, data = data)
summary(model13)
model14 <- lm(Above_CN             ~ Elevation, data = data)
summary(model14)
model15 <- lm(Soil_water_content   ~ Elevation, data = data)
summary(model15)
model16 <- lm(Soil_organic_matter  ~ Elevation, data = data)
summary(model16)

p_label <- function(model, term) {
  p <- summary(model)$coefficients[term, "Pr(>|t|)"]
  if (p < 0.001) "italic(p) < 0.001" else paste0("italic(p) == ", sprintf("%.3f", p))
}

models <- mget(paste0("model", 1:16), ifnotfound = list(NULL))
res_all <- imap_dfr(models, ~ {
  fit <- .x
  mod_name <- .y
  td <- tidy(fit) %>% filter(term == "Elevation")
  gl <- glance(fit)
  tibble(
    Model = mod_name,
    beta = round(td$estimate, 3),
    SE   = round(td$std.error, 3),
    t    = round(td$statistic, 3),
    p    = round(td$p.value, 3),
    R2   = round(gl$r.squared, 3),
    df   = fit$df.residual
  )
})
print(res_all)

# Below_biomass_see_level_rise
data1 <- subset(data, !is.na(Below_biomass))
model <- lm(Below_biomass ~ Elevation, data = data1)
summary(model)

p_Below_biomass_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Below_biomass)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Below biomass (g/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Below_biomass_see_level_rise)

# Above_biomass_see_level_rise
data1 <- subset(data, !is.na(Above_biomass))
model <- lm(Above_biomass ~ Elevation, data = data1)
summary(model)

p_Above_biomass_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Above_biomass)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Above biomass (g/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Above_biomass_see_level_rise)

# Plant_biomass_see_level_rise
data1 <- subset(data, !is.na(Plant_biomass))
model <- lm(Plant_biomass ~ Elevation, data = data1)
summary(model)

p_Plant_biomass_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Plant_biomass)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Plant biomass (g/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Plant_biomass_see_level_rise)

# lnSRR_see_level_rise
data1 <- subset(data, !is.na(lnSRR))
model <- lm(lnSRR ~ Elevation, data = data1)
summary(model)

p_LnSRR_see_level_rise <- ggplot(data1, aes(x = Elevation, y = lnSRR)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("ln(stem:root ratio)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_LnSRR_see_level_rise)

# Live_stems_see_level_rise
data1 <- subset(data, !is.na(Live_stems))
model <- lm(Live_stems ~ Elevation, data = data1)
summary(model)

p_Live_stems_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Live_stems)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Live leaves (n/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Live_stems_see_level_rise)

# Live_leaves_see_level_rise
data1 <- subset(data, !is.na(Live_leaves))
model <- lm(Live_leaves ~ Elevation, data = data1)
summary(model)

p_Live_leaves_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Live_leaves)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Live leaves (g/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Live_leaves_see_level_rise)

# Height_average_see_level_rise
data1 <- subset(data, !is.na(Height_average))
model <- lm(Height_average ~ Elevation, data = data1)
summary(model)

p_Height_average_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Height_average)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Height average (g/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Height_average_see_level_rise)

# Flowers_see_level_rise
data1 <- subset(data, !is.na(Flowers))
model <- lm(Flowers ~ Elevation, data = data1)
summary(model)

p_Flowers_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Flowers)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Flowers (n)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Flowers_see_level_rise)

# Below_C_see_level_rise
data1 <- subset(data, !is.na(Below_C))
model <- lm(Below_C ~ Elevation, data = data1)
summary(model)

p_Below_C_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Below_C)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Below C (mg/g)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Below_C_see_level_rise)

# Below_N_see_level_rise
data1 <- subset(data, !is.na(Below_N))
model <- lm(Below_N ~ Elevation, data = data1)
summary(model)

p_Below_N_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Below_N)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Below N (g/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1)+
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Below_N_see_level_rise)

# Below_CN_see_level_rise
data1 <- subset(data, !is.na(Below_CN))
model <- lm(Below_CN ~ Elevation, data = data1)
summary(model)

p_Below_CN_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Below_CN)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Below C:N") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Below_CN_see_level_rise)


# Above_C_see_level_rise
data1 <- subset(data, !is.na(Above_C))
model <- lm(Above_C ~ Elevation, data = data1)
summary(model)

p_Above_C_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Above_C)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Above C (g/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Above_C_see_level_rise)

# Above_N_see_level_rise
data1 <- subset(data, !is.na(Above_N))
model <- lm(Above_N ~ Elevation, data = data1)
summary(model)

p_Above_N_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Above_N)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Above N (g/m²)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Above_N_see_level_rise)

# Below_CN_see_level_rise
data1 <- subset(data, !is.na(Above_CN))
model <- lm(Above_CN ~ Elevation, data = data1)
summary(model)

p_Above_CN_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Above_CN)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Above C:N") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Above_CN_see_level_rise)

# Soil_Soil_water_content_see_level_rise
data1 <- subset(data, !is.na(Soil_water_content))
model <- lm(Soil_water_content ~ Elevation, data = data1)
summary(model)

p_Soil_water_content_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Soil_water_content)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Soil water content (%)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Soil_water_content_see_level_rise)

# Soil_organic_matter_see_level_rise
data1 <- subset(data, !is.na(Soil_organic_matter))
model <- lm(Soil_organic_matter ~ Elevation, data = data1)
summary(model)

p_Soil_organic_matter_see_level_rise <- ggplot(data1, aes(x = Elevation, y = Soil_organic_matter)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Soil organic matter (%)") +
  xlab("Inundation level (cm)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Elevation"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Soil_organic_matter_see_level_rise)

combined <- list(p_Below_biomass_see_level_rise,
                 p_Above_biomass_see_level_rise,
                 p_Plant_biomass_see_level_rise,
                 p_LnSRR_see_level_rise,
                 p_Live_stems_see_level_rise,
                 p_Live_leaves_see_level_rise,
                 p_Height_average_see_level_rise,
                 p_Flowers_see_level_rise,
                 p_Below_C_see_level_rise,
                 p_Below_N_see_level_rise,
                 p_Below_CN_see_level_rise,
                 p_Above_C_see_level_rise,
                 p_Above_N_see_level_rise,
                 p_Above_CN_see_level_rise,
                 p_Soil_water_content_see_level_rise,
                 p_Soil_organic_matter_see_level_rise)
combined <- plot_grid(plotlist = lapply(combined, function(p) p + theme(legend.position = "none")),
                      ncol = 4,
                      align = "hv",
                      labels = letters[1:length(combined)],
                      label_size = 16,
                      label_x = 0.025,
                      label_y = 1.02,
                      hjust = 0,
                      vjust = 1)
print(combined)                 
ggsave("Figure S1 - 0701.pdf", combined, height = 240, width = 300, units = "mm")

# -------------------------
# Figure S2
# -------------------------
data <- read_excel("Raw data - Final data - 0623.xlsx")
data <- as.data.frame(data)

model1  <- lm(Below_biomass        ~ Mussel_density, data = data)
summary(model1)
model2  <- lm(Above_biomass        ~ Mussel_density, data = data)
summary(model2)
model3  <- lm(Plant_biomass        ~ Mussel_density, data = data)
summary(model3)
model4  <- lm(lnSRR                ~ Mussel_density, data = data)
summary(model4)
model5  <- lm(Live_stems           ~ Mussel_density, data = data)
summary(model5)
model6  <- lm(Live_leaves          ~ Mussel_density, data = data)
summary(model6)
model7  <- lm(Height_average       ~ Mussel_density, data = data)
summary(model7)
model8  <- lm(Flowers              ~ Mussel_density, data = data)
summary(model8)
model9  <- lm(Below_C              ~ Mussel_density, data = data)
summary(model9)
model10 <- lm(Below_N              ~ Mussel_density, data = data)
summary(model10)
model11 <- lm(Below_CN             ~ Mussel_density, data = data)
summary(model11)
model12 <- lm(Above_C              ~ Mussel_density, data = data)
summary(model12)
model13 <- lm(Above_N              ~ Mussel_density, data = data)
summary(model13)
model14 <- lm(Above_CN             ~ Mussel_density, data = data)
summary(model14)
model15 <- lm(Soil_water_content   ~ Mussel_density, data = data)
summary(model15)
model16 <- lm(Soil_organic_matter  ~ Mussel_density, data = data)
summary(model16)

models <- mget(paste0("model", 1:16), ifnotfound = list(NULL))
res_all <- imap_dfr(models, ~ {
  fit <- .x
  mod_name <- .y
  td <- tidy(fit) %>% filter(term == "Mussel_density")
  gl <- glance(fit)
  tibble(
    Model = mod_name,
    beta = round(td$estimate, 3),
    SE   = round(td$std.error, 3),
    t    = round(td$statistic, 3),
    p    = round(td$p.value, 3),
    R2   = round(gl$r.squared, 3),
    df   = fit$df.residual
  )
})
print(res_all)

# Below_biomass_mussel_density
data1 <- subset(data, !is.na(Below_biomass))
model <- lm(Below_biomass ~ Mussel_density, data = data1)
summary(model)

p_Below_biomass_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Below_biomass)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Below biomass (g/m²)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Below_biomass_mussel_density)

# Above_biomass_mussel_density
data1 <- subset(data, !is.na(Above_biomass))
model <- lm(Above_biomass ~ Mussel_density, data = data1)
summary(model)

p_Above_biomass_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Above_biomass)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Above biomass (g/m²)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Above_biomass_mussel_density)

# Plant_biomass_mussel_density
data1 <- subset(data, !is.na(Plant_biomass))
model <- lm(Plant_biomass ~ Mussel_density, data = data1)
summary(model)

p_Plant_biomass_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Plant_biomass)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Plant biomass (g/m²)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Plant_biomass_mussel_density)

# lnSRR_mussel_density
data1 <- subset(data, !is.na(lnSRR))
model <- lm(lnSRR ~ Mussel_density, data = data1)
summary(model)

p_lnSRR_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = lnSRR)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("ln(stem:root ratio)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_lnSRR_mussel_density)

# Live_stems_mussel_density
data1 <- subset(data, !is.na(Live_stems))
model <- lm(Live_stems ~ Mussel_density, data = data1)
summary(model)

p_Live_stems_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Live_stems)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Live stems (n/m²)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Live_stems_mussel_density)

# Live_leaves_mussel_density
data1 <- subset(data, !is.na(Live_leaves))
model <- lm(Live_leaves ~ Mussel_density, data = data1)
summary(model)

p_Live_leaves_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Live_leaves)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Live leaves (n/m²)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Live_leaves_mussel_density)

# Height_average_mussel_density
data1 <- subset(data, !is.na(Height_average))
model <- lm(Height_average ~ Mussel_density, data = data1)
summary(model)

p_Height_average_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Height_average)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Height average (cm)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Height_average_mussel_density)

# Flowers_mussel_density
data1 <- subset(data, !is.na(Flowers))
model <- lm(Flowers ~ Mussel_density, data = data1)
summary(model)

p_Flowers_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Flowers)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Flowers (n)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Flowers_mussel_density)

# Below_C_mussel_density
data1 <- subset(data, !is.na(Below_C))
model <- lm(Below_C ~ Mussel_density, data = data1)
summary(model)

p_Below_C_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Below_C)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Below C (mg/g)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Below_C_mussel_density)

# Below_N_mussel_density
data1 <- subset(data, !is.na(Below_N))
model <- lm(Below_N ~ Mussel_density, data = data1)
summary(model)

p_Below_N_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Below_N)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Below N (mg/g)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Below_N_mussel_density)

# Below_CN_mussel_density
data1 <- subset(data, !is.na(Below_CN))
model <- lm(Below_CN ~ Mussel_density, data = data1)
summary(model)

p_Below_CN_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Below_CN)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Below C:N (mg/g)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Below_CN_mussel_density)

# Above_C_mussel_density
data1 <- subset(data, !is.na(Above_C))
model <- lm(Above_C ~ Mussel_density, data = data1)
summary(model)

p_Above_C_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Above_C)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Above C (mg/g)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Above_C_mussel_density)

# Above_N_mussel_density
data1 <- subset(data, !is.na(Above_N))
model <- lm(Above_N ~ Mussel_density, data = data1)
summary(model)

p_Above_N_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Above_N)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Above N (mg/g)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Above_N_mussel_density)

# Above_CN_mussel_density
data1 <- subset(data, !is.na(Above_CN))
model <- lm(Above_CN ~ Mussel_density, data = data1)
summary(model)

p_Above_CN_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Above_CN)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Above C:N (mg/g)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Above_CN_mussel_density)

# Soil_water_content_mussel_density
data1 <- subset(data, !is.na(Soil_water_content))
model <- lm(Soil_water_content ~ Mussel_density, data = data1)
summary(model)

p_Soil_water_content_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Soil_water_content)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Soil water content (%)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Soil_water_content_mussel_density)

# Soil_organic_matter_mussel_density
data1 <- subset(data, !is.na(Soil_organic_matter))
model <- lm(Soil_organic_matter ~ Mussel_density, data = data1)
summary(model)

p_Soil_organic_matter_mussel_density <- ggplot(data1, aes(x = Mussel_density, y = Soil_organic_matter)) +
  geom_point(size = 3.5, shape = 16, stroke = 0.25, alpha = 0.4, colour = "steelblue") +
  geom_smooth(data = subset(data1),
              colour  = "steelblue4",
              fill    = "steelblue1", 
              method  = "lm",
              formula = y ~ x,
              se      = TRUE,
              alpha   = 0.25,
              linetype = "solid") +
  theme_minimal() +
  theme(panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line          = element_line(color = "black", linewidth = 0.75),
        axis.text.x        = element_text(size  = 14, hjust = 0.5),
        axis.title.x       = element_text(size  = 14),
        axis.line.x        = element_line(color = "black", linewidth = 0.5),
        axis.line.y        = element_line(color = "black", linewidth = 0.5),
        axis.ticks         = element_line(color = "black", linewidth = 0.5),
        axis.title         = element_text(size  = 14),
        axis.text          = element_text(size  = 14),
        legend.position = "none") +
  ylab("Soil organic matter (%)") +
  xlab("Mussel density (n)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",  width = 0, color = "steelblue", linewidth = 1,  alpha = 1) +
  stat_summary(fun = mean, geom = "point", size = 4, color = "steelblue", shape = 18, alpha = 1) +
  annotate("text", x = -Inf, y = Inf,
           label = p_label(model, "Mussel_density"),
           hjust = -0.2, vjust = 1.4, color = "steelblue", size   = 4, parse = TRUE)
print(p_Soil_organic_matter_mussel_density)

combined <- list(p_Below_biomass_mussel_density,
                 p_Above_biomass_mussel_density,
                 p_Plant_biomass_mussel_density,
                 p_lnSRR_mussel_density,
                 p_Live_stems_mussel_density,
                 p_Live_leaves_mussel_density,
                 p_Height_average_mussel_density,
                 p_Flowers_mussel_density,
                 p_Below_C_mussel_density,
                 p_Below_N_mussel_density,
                 p_Below_CN_mussel_density, 
                 p_Above_C_mussel_density,
                 p_Above_N_mussel_density, 
                 p_Above_CN_mussel_density, 
                 p_Soil_water_content_mussel_density,
                 p_Soil_organic_matter_mussel_density)
combined <- plot_grid(plotlist = lapply(combined, function(p) p + theme(legend.position = "none")),
                      ncol = 4,
                      align = "hv",
                      labels = letters[1:length(combined)],
                      label_size = 16,
                      label_x = 0.025,
                      label_y = 1.02,
                      hjust = 0,
                      vjust = 1)
print(combined)                 
ggsave("Figure S2 - 0630.pdf", combined, height = 240, width = 300, units = "mm")

# -------------------------
# Table S6 & S7 output
# -------------------------
make_pooled_table <- function(dat, predictor, group_label, p_header,
                              tbl_no, out_file, partner_word) {
  
  resp_vars <- c("Below_biomass","Above_biomass","Plant_biomass","lnSRR",
                 "Live_stems","Live_leaves","Height_average","Flowers",
                 "Below_C","Below_N","Below_CN","Above_C","Above_N","Above_CN",
                 "Soil_water_content","Soil_organic_matter")
  resp_labs <- c("Belowground biomass","Aboveground biomass","Total plant biomass",
                 "ln(stem:root ratio)","Live stems","Live leaves","Stem height","Flowers",
                 "Belowground carbon","Belowground nitrogen","Belowground C:N",
                 "Aboveground carbon","Aboveground nitrogen","Aboveground C:N",
                 "Soil water content","Soil organic matter")
  n_plant <- 14
  
  fmt3 <- function(x){ s <- formatC(x, format = "f", digits = 3); s[s == "-0.000"] <- "0.000"; s }
  pfmt <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))
  
  stat <- vector("list", length(resp_vars))
  sig  <- logical(length(resp_vars))
  for (i in seq_along(resp_vars)) {
    m  <- lm(reformulate(predictor, response = resp_vars[i]), data = dat)
    co <- summary(m)$coefficients[predictor, ]
    stat[[i]] <- c(fmt3(co["Estimate"]), fmt3(co["Std. Error"]), fmt3(co["t value"]),
                   pfmt(co["Pr(>|t|)"]), fmt3(summary(m)$r.squared),
                   as.character(m$df.residual))
    sig[i] <- co["Pr(>|t|)"] < 0.05
  }
  
  cols  <- c("resp","b","se","t","p","r2","df")
  blank <- function(lab){ r <- as.data.frame(matrix("", 1, 7), stringsAsFactors = FALSE)
  names(r) <- cols; r$resp <- lab; r }
  drow  <- function(lab, s){ r <- blank(lab)
  r$b<-s[1]; r$se<-s[2]; r$t<-s[3]; r$p<-s[4]; r$r2<-s[5]; r$df<-s[6]; r }
  
  body <- blank("Plant traits")
  for (i in 1:n_plant)                     body <- rbind(body, drow(resp_labs[i], stat[[i]]))
  body <- rbind(body, blank("Soil traits"))
  for (i in (n_plant+1):length(resp_vars)) body <- rbind(body, drow(resp_labs[i], stat[[i]]))
  
  sec_rows <- c(1, n_plant + 2)
  var_brow <- c(1:n_plant + 1, (n_plant+1):length(resp_vars) + 2)
  sig_brow <- var_brow[which(sig)]
  
  big   <- fp_border(color = "black", width = 1.5)
  small <- fp_border(color = "black", width = 0.75)
  
  ft <- flextable(body, col_keys = cols)
  ft <- set_header_labels(ft, resp = "Response variables", b = "\u03b2", se = "SE",
                          t = "t", p = p_header, r2 = "R2", df = "df")
  ft <- add_header_row(ft, top = TRUE, values = c("Response variables", group_label),
                       colwidths = c(1, 6))
  ft <- merge_v(ft, j = "resp", part = "header")
  ft <- flextable::compose(ft, i = 2, j = "b",  part = "header", value = as_paragraph(as_i("\u03b2")))
  ft <- flextable::compose(ft, i = 2, j = "t",  part = "header", value = as_paragraph(as_i("t")))
  ft <- flextable::compose(ft, i = 2, j = "p",  part = "header", value = as_paragraph(as_i(p_header)))
  ft <- flextable::compose(ft, i = 2, j = "r2", part = "header", value = as_paragraph(as_i("R"), as_sup("2")))
  ft <- flextable::compose(ft, i = 2, j = "df", part = "header", value = as_paragraph(as_i("df")))
  ft <- bold(ft, part = "header")
  
  ft <- bold(ft,   i = sec_rows, part = "body")
  ft <- italic(ft, i = sec_rows, part = "body")
  if (length(sig_brow)) ft <- bold(ft, i = sig_brow, part = "body")
  
  ft <- border_remove(ft)
  ft <- hline_top(ft, part = "header", border = big)
  ft <- hline_bottom(ft, part = "header", border = small)
  ft <- hline_bottom(ft, part = "body", border = big)
  
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = 10.5, part = "all")
  ft <- align(ft, align = "left", part = "all")
  ft <- valign(ft, valign = "center", part = "all")
  ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
  ft <- width(ft, j = cols, width = c(2961, 1270, 1129, 1127, 1268, 1127, 870) / 1440)
  ft <- set_table_properties(ft, layout = "fixed")
  
  pr    <- fp_text(font.family = "Times New Roman", font.size = 10.5)
  pr_b  <- update(pr, bold = TRUE)
  pr_bi <- update(pr, bold = TRUE, italic = TRUE)
  pr_i  <- update(pr, italic = TRUE)
  pr_is <- update(pr, italic = TRUE, vertical.align = "superscript")
  
  cap <- fpar(
    ftext(paste0("Table ", tbl_no, " Summary of pooled simple linear regressions between ",
                 partner_word, " and response variables of "), pr_b),
    ftext("Spartina alterniflora", pr_bi),
    ftext(" and associated soil traits.", pr_b),
    ftext(" Abbreviations: ", pr), ftext("\u03b2", pr_i),
    ftext(" = slope estimator; SE = standard error; ", pr), ftext("df", pr_i),
    ftext(" = degrees of freedom; ", pr), ftext("R", pr_i), ftext("2", pr_is),
    ftext(" = goodness of fit of the model.", pr))
  
  doc <- read_docx()
  doc <- body_add_fpar(doc, cap)
  doc <- body_add_par(doc, "")
  doc <- body_add_flextable(doc, ft)
  print(doc, target = out_file)
  cat("Wrote", normalizePath(out_file), "\n")
}

make_pooled_table(data, "Elevation",      "Relative elevation treatment", "p",
                  "S6", "Table S6.docx", "relative elevation")

make_pooled_table(data, "Mussel_density", "Mussel density",               "p",
                  "S7", "Table S7.docx", "mussel density")