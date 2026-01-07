# Biweekly data cleaning - height_average
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
data <- read_excel("Raw data_Biweekly.xlsx")
data <- as.data.frame(data)

data_Corrected_height_average <- data %>%
  mutate(Corrected_height = as.numeric(Corrected_height)) %>%
  group_by(Days_deployed, Organ, Level, Treat) %>%
  mutate(Corrected_height_average = mean(Corrected_height, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(Date, Days_deployed, Organ, Level, Treat, Corrected_height_average) %>%
  mutate(Treat = str_trim(as.character(Treat))) %>%
  mutate(Treat = factor(Treat, levels = c("B", "P", "Y", "OR")))
data_Corrected_height_average

# Figure 2
# Figure - Height_BL_5
data_Corrected_height_average_filter <- data_Corrected_height_average %>% filter(Level == "BL")
data_Corrected_height_average_filter <- data_Corrected_height_average_filter %>% mutate(Treat = factor(Treat)) %>% filter(!is.na(Corrected_height_average))

model1 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "B"))
summary(model1)
model2 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "P"))
summary(model2)
model3 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "Y"))
summary(model3)
model4 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "OR"))
summary(model4)

# Figure - Height_B_0
data_Corrected_height_average_filter <- data_Corrected_height_average %>% filter(Level == "B")
data_Corrected_height_average_filter <- data_Corrected_height_average_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Corrected_height_average))

model5 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "B"))
summary(model5)
model6 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "P"))
summary(model6)
model7 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "Y"))
summary(model7)
model8 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "OR"))
summary(model8)

# Figure - Height_P_m5
data_Corrected_height_average_filter <- data_Corrected_height_average %>% filter(Level == "P")
data_Corrected_height_average_filter <- data_Corrected_height_average_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Corrected_height_average))

model9 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "B"))
summary(model9)
model10 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "P"))
summary(model10)
model11 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "Y"))
summary(model11)
model12 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "OR"))
summary(model12)

# Figure - Height_Y_m10
data_Corrected_height_average_filter <- data_Corrected_height_average %>% filter(Level == "Y")
data_Corrected_height_average_filter <- data_Corrected_height_average_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Corrected_height_average))

model13 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "B"))
summary(model13)
model14 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "P"))
summary(model14)
model15 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "Y"))
summary(model15)
model16 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "OR"))
summary(model16)

# Figure - Height_OR_m15
data_Corrected_height_average_filter <- data_Corrected_height_average %>% filter(Level == "OR")
data_Corrected_height_average_filter <- data_Corrected_height_average_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Corrected_height_average))

model17 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "B"))
summary(model17)
model18 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "P"))
summary(model18)
model19 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "Y"))
summary(model19)
model20 <- lm(Corrected_height_average ~ Days_deployed, data = subset(data_Corrected_height_average_filter, Treat == "OR"))
summary(model20)

# Biweekly data cleaning - Live leaves
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
data <- read_excel("Raw data_Biweekly.xlsx")
data <- as.data.frame(data)

data_Live_leaves_sum <- data %>%
  mutate(Live_leaves = as.numeric(Live_leaves)) %>%
  group_by(Days_deployed, Organ, Level, Treat) %>%
  mutate(Live_leaves_sum = sum(as.numeric(Live_leaves), na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(Date, Days_deployed, Organ, Level, Treat, Live_leaves_sum) %>%
  mutate(Treat = str_trim(as.character(Treat))) %>%
  mutate(Treat = factor(Treat, levels = c("B", "P", "Y", "OR")))
data_Live_leaves_sum

# Figure - Live_leaves_BL_5
data_Live_leaves_sum_filter <- data_Live_leaves_sum %>% filter(Level == "BL")
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_leaves_sum))
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Live_leaves_sum = Live_leaves_sum * 10000 / 78.539816)

model21 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "B"))
summary(model21)
model22 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "P"))
summary(model22)
model23 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "Y"))
summary(model23)
model24 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "OR"))
summary(model24)

# Figure - Live_leaves_B_0
data_Live_leaves_sum_filter <- data_Live_leaves_sum %>% filter(Level == "B")
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_leaves_sum))
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Live_leaves_sum = Live_leaves_sum * 10000 / 78.539816)
    
model25 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "B"))
summary(model25)
model26 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "P"))
summary(model26)
model27 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "Y"))
summary(model27)
model28 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "OR"))
summary(model28)

# Figure - Live_leaves_P_m5
data_Live_leaves_sum_filter <- data_Live_leaves_sum %>% filter(Level == "P")
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_leaves_sum))
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Live_leaves_sum = Live_leaves_sum * 10000 / 78.539816)

model29 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "B"))
summary(model29)
model30 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "P"))
summary(model30)
model31 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "Y"))
summary(model31)
model32 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "OR"))
summary(model32)

# Figure - Live_leaves_Y_m10
data_Live_leaves_sum_filter <- data_Live_leaves_sum %>% filter(Level == "Y")
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_leaves_sum))
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Live_leaves_sum = Live_leaves_sum * 10000 / 78.539816)

model33 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "B"))
summary(model33)
model34 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "P"))
summary(model34)
model35 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "Y"))
summary(model35)
model36 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "OR"))
summary(model36)

# Figure - Live_leaves_OR_m15
data_Live_leaves_sum_filter <- data_Live_leaves_sum %>% filter(Level == "OR")
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_leaves_sum))
data_Live_leaves_sum_filter <- data_Live_leaves_sum_filter %>% mutate(Live_leaves_sum = Live_leaves_sum * 10000 / 78.539816)

model37 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "B"))
summary(model37)
model38 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "P"))
summary(model38)
model39 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "Y"))
summary(model39)
model40 <- lm(Live_leaves_sum ~ Days_deployed, data = subset(data_Live_leaves_sum_filter, Treat == "OR"))
summary(model40)

# Biweekly data cleaning - Live_stems
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
data <- read_excel("Raw data_Biweekly.xlsx")
data <- as.data.frame(data)

data_Live_stems <- data %>%
  mutate(L_D = as.factor(L_D)) %>%
  group_by(Days_deployed, Organ, Level, Treat) %>%
  mutate(Live_stems = sum(L_D == "L", na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(Date, Days_deployed, Organ, Level, Treat, Live_stems) %>%
  mutate(Treat = str_trim(as.character(Treat))) %>%
  mutate(Treat = factor(Treat, levels = c("B", "P", "Y", "OR")))
data_Live_stems

# Figure - Live_stems_BL_5
data_Live_stems_filter <- data_Live_stems %>% filter(Level == "BL")
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_stems))
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Live_stems = Live_stems * 10000 / 78.539816)

model41 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "B"))
summary(model41)
model42 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "P"))
summary(model42)
model43 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "Y"))
summary(model43)
model44 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "OR"))
summary(model44)

# Figure - Live_stems_B_0
data_Live_stems_filter <- data_Live_stems %>% filter(Level == "B")
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_stems))
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Live_stems = Live_stems * 10000 / 78.539816)

model45 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "B"))
summary(model45)
model46 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "P"))
summary(model46)
model47 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "Y"))
summary(model47)
model48 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "OR"))
summary(model48)

# Figure - Live_stems_P_m5
data_Live_stems_filter <- data_Live_stems %>% filter(Level == "P")
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_stems))
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Live_stems = Live_stems * 10000 / 78.539816)

model49 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "B"))
summary(model49)
model50 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "P"))
summary(model50)
model51 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "Y"))
summary(model51)
model52 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "OR"))
summary(model52)

# Figure - Live_stems_Y_m10
data_Live_stems_filter <- data_Live_stems %>% filter(Level == "Y")
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_stems))
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Live_stems = Live_stems * 10000 / 78.539816)

model53 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "B"))
summary(model53)
model54 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "P"))
summary(model54)
model55 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "Y"))
summary(model55)
model56 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "OR"))
summary(model56)

# Figure - Live_stems_OR_m15
data_Live_stems_filter <- data_Live_stems %>% filter(Level == "OR")
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Live_stems))
data_Live_stems_filter <- data_Live_stems_filter %>% mutate(Live_stems = Live_stems * 10000 / 78.539816)

model57 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "B"))
summary(model57)
model58 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "P"))
summary(model58)
model59 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "Y"))
summary(model59)
model60 <- lm(Live_stems ~ Days_deployed, data = subset(data_Live_stems_filter, Treat == "OR"))
summary(model60)

# Biweekly data cleaning - Mortality_rate
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
data <- read_excel("Raw data_Biweekly.xlsx")
data <- as.data.frame(data)

data_Mortality_rate <- data %>%
  mutate(L_D = as.factor(L_D)) %>%
  group_by(Days_deployed, Organ, Level, Treat) %>%
  mutate(n_dead         = sum(L_D == "D", na.rm = TRUE),
         n_total        = n(),
         Mortality_rate = if_else(n_total > 0, n_dead / n_total, NA_real_)) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(Date, Days_deployed, Organ, Level, Treat, Mortality_rate) %>%
  mutate(Treat = str_trim(as.character(Treat))) %>%
  mutate(Treat = factor(Treat, levels = c("B", "P", "Y", "OR")))
data_Mortality_rate


# Figure - Mortality_rate_BL_5
data_Mortality_rate_filter <- data_Mortality_rate %>% filter(Level == "BL")
data_Mortality_rate_filter <- data_Mortality_rate_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Mortality_rate))

model61 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "B"))
summary(model61)
model62 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "P"))
summary(model62)
model63 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "Y"))
summary(model63)
model64 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "OR"))
summary(model64)

# Figure - Mortality_rate_B_0
data_Mortality_rate_filter <- data_Mortality_rate %>% filter(Level == "B")
data_Mortality_rate_filter <- data_Mortality_rate_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Mortality_rate))

model65 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "B"))
summary(model65)
model66 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "P"))
summary(model66)
model67 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "Y"))
summary(model67)
model68 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "OR"))
summary(model68)

# Figure - Mortality_rate_P_m5
data_Mortality_rate_filter <- data_Mortality_rate %>% filter(Level == "P")
data_Mortality_rate_filter <- data_Mortality_rate_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Mortality_rate))

model69 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "B"))
summary(model69)
model70 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "P"))
summary(model70)
model71 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "Y"))
summary(model71)
model72 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "OR"))
summary(model72)

# Figure - Mortality_rate_Y_m10
data_Mortality_rate_filter <- data_Mortality_rate %>% filter(Level == "Y")
data_Mortality_rate_filter <- data_Mortality_rate_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Mortality_rate))

model73 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "B"))
summary(model73)
model74 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "P"))
summary(model74)
model75 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "Y"))
summary(model75)
model76 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "OR"))
summary(model76)

# Figure - Mortality_rate_OR_m15
data_Mortality_rate_filter <- data_Mortality_rate %>% filter(Level == "OR")
data_Mortality_rate_filter <- data_Mortality_rate_filter %>% mutate(Treat = factor(Treat)) %>% 
  filter(!is.na(Mortality_rate))

model77 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "B"))
summary(model77)
model78 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "P"))
summary(model78)
model79 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "Y"))
summary(model79)
model80 <- lm(Mortality_rate ~ Days_deployed, data = subset(data_Mortality_rate_filter, Treat == "OR"))
summary(model80)

library(dplyr)
library(purrr)
library(broom)

models <- mget(paste0("model", 1:80), ifnotfound = list(NULL))
res_all <- imap_dfr(models, ~ {
  fit <- .x
  mod_name <- .y
  td <- tidy(fit) %>% filter(term == "Days_deployed")
  gl <- glance(fit)
  tibble(Model = mod_name,
         beta = round(td$estimate, 3),
         SE   = round(td$std.error, 3),
         t    = round(td$statistic, 3),
         p    = round(td$p.value, 3),
         R2   = round(gl$r.squared, 3),
         df   = fit$df.residual)
    })
print(res_all, n = 80)

setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
treat_cols <- c("B"  = "#55B7E6",
                "P"  = "#193E8F",
                "Y"  = "#E53528",
                "OR" = "#F09739")
style_ci <- function(p,
                     pt_size = 2.2,
                     pt_alpha = 0.20,
                     line_width = 1.7,
                     ribbon_alpha = 0.08
) {
  xvar <- rlang::as_name(p$mapping$x)
  yvar <- rlang::as_name(p$mapping$y)
  ribbon <- ggplot2::stat_smooth(
    data = p$data,
    mapping = ggplot2::aes_string(x = xvar, y = yvar, fill = "Treat", group = "Treat"),
    method = "lm",
    se = TRUE,
    geom = "ribbon",
    alpha = ribbon_alpha,
    colour = NA,
    linewidth = 0,
    inherit.aes = FALSE,
    show.legend = FALSE
  )
  p$layers <- append(list(ribbon), p$layers, after = 0)
  for (i in seq_along(p$layers)) {
    if (inherits(p$layers[[i]]$geom, "GeomPoint")) {
      p$layers[[i]]$aes_params$size  <- pt_size
      p$layers[[i]]$aes_params$alpha <- pt_alpha
    }
    if (inherits(p$layers[[i]]$geom, "GeomSmooth")) {
      p$layers[[i]]$aes_params$linewidth <- line_width
      p$layers[[i]]$aes_params$size      <- line_width
      if (!is.null(p$layers[[i]]$stat_params$se)) p$layers[[i]]$stat_params$se <- FALSE
    }
  }
  p + scale_fill_manual(values = treat_cols, guide = "none")
}

# -------------------------
# MAIN Figure (3 columns): Stem height + Live stems + Mortality rate
# -------------------------

all_plots_main <- list(p_Height_BL_5   + labs(y = "Stem height"),  p_Live_stems_BL_5,   p_Mortality_rate_BL_5,
                       p_Height_B_0    + labs(y = "Stem height"),  p_Live_stems_B_0,    p_Mortality_rate_B_0,
                       p_Height_P_m5   + labs(y = "Stem height"),  p_Live_stems_P_m5,   p_Mortality_rate_P_m5,
                       p_Height_Y_m10  + labs(y = "Stem height"),  p_Live_stems_Y_m10,  p_Mortality_rate_Y_m10,
                       p_Height_OR_m15 + labs(y = "Stem height"),  p_Live_stems_OR_m15, p_Mortality_rate_OR_m15)

combined_main <- cowplot::plot_grid(plotlist = lapply(all_plots_main, function(pp) style_ci(pp) + theme(legend.position = "none")),
                                    ncol = 3,
                                    align = "hv",
                                    labels = letters[1:length(all_plots_main)],
                                    label_size = 16,
                                    label_x = 0.025,
                                    label_y = 1.02,
                                    hjust = 0,
                                    vjust = 1)

print(combined_main)
ggsave("Figure 2.pdf", combined_main, width = 220, height = 300, units = "mm")

# -------------------------
# APPENDIX Figure (Live leaves only)
# -------------------------

all_plots_appendix <- list(p_Live_leaves_BL_5,
                           p_Live_leaves_B_0,
                           p_Live_leaves_P_m5,
                           p_Live_leaves_Y_m10,
                           p_Live_leaves_OR_m15)

combined_appendix <- cowplot::plot_grid(plotlist = lapply(all_plots_appendix, function(pp) style_ci(pp) + theme(legend.position = "none")),
                                        ncol = 1,
                                        align = "v",
                                        labels = letters[1:length(all_plots_appendix)],
                                        label_size = 16,
                                        label_x = 0.025,
                                        label_y = 1.02,
                                        hjust = 0,
                                        vjust = 1)

print(combined_appendix)
ggsave("Figure S - 1218.pdf", combined_appendix, width = 150, height = 300, units = "mm")
