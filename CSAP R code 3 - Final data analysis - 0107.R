# Table 1
# all lmms
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
data <- read_excel("Raw data_Final.xlsx")
data <- as.data.frame(data)

model <- lmer(Below_biomass  ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Above_biomass  ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Plant_biomass  ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(lnSRR          ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Live_stems     ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Live_leaves    ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Height_average ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Flowers        ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Below_C        ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Below_nitrogen ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Below_CN       ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Above_carbon   ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Above_nitrogen ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Above_CN       ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Water_content  ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)
model <- lmer(Organic_matter ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model)

# simple linear
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
data <- read_excel("Raw data_Final.xlsx")
data1 <- as.data.frame(data)

model1 <- lm(Below_biomass ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model1)
model2 <- lm(Below_biomass ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model2)
model3 <- lm(Below_biomass ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model3)
model4 <- lm(Below_biomass ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model4)
model5 <- lm(Below_biomass ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model5)

model6 <- lm(Above_biomass ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model6)
model7 <- lm(Above_biomass ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model7)
model8 <- lm(Above_biomass ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model8)
model9 <- lm(Above_biomass ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model9)
model10 <- lm(Above_biomass ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model10)

model11 <- lm(Plant_biomass ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model11)
model12 <- lm(Plant_biomass ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model12)
model13 <- lm(Plant_biomass ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model13)
model14 <- lm(Plant_biomass ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model14)
model15 <- lm(Plant_biomass ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model15)

model16 <- lm(lnSRR ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model16)
model17 <- lm(lnSRR ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model17)
model18 <- lm(lnSRR ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model18)
model19 <- lm(lnSRR ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model19)
model20 <- lm(lnSRR ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model20)

model21 <- lm(Live_stems ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model21)
model22 <- lm(Live_stems ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model22)
model23 <- lm(Live_stems ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model23)
model24 <- lm(Live_stems ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model24)
model25 <- lm(Live_stems ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model25)

model26 <- lm(Live_leaves ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model26)
model27 <- lm(Live_leaves ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model27)
model28 <- lm(Live_leaves ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model28)
model29 <- lm(Live_leaves ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model29)
model30 <- lm(Live_leaves ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model30)

model31 <- lm(Height_average ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model31)
model32 <- lm(Height_average ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model32)
model33 <- lm(Height_average ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model33)
model34 <- lm(Height_average ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model34)
model35 <- lm(Height_average ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model35)

model36 <- lm(Flowers ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model36)
model37 <- lm(Flowers ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model37)
model38 <- lm(Flowers ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model38)
model39 <- lm(Flowers ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model39)
model40 <- lm(Flowers ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model40)

model41 <- lm(Below_N ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model41)
model42 <- lm(Below_N ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model42)
model43 <- lm(Below_N ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model43)
model44 <- lm(Below_N ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model44)
model45 <- lm(Below_N ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model45)

model46 <- lm(Below_C ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model46)
model47 <- lm(Below_C ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model47)
model48 <- lm(Below_C ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model48)
model49 <- lm(Below_C ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model49)
model50 <- lm(Below_C ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model50)

model51 <- lm(Below_CN ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model51)
model52 <- lm(Below_CN ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model52)
model53 <- lm(Below_CN ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model53)
model54 <- lm(Below_CN ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model54)
model55 <- lm(Below_CN ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model55)

model56 <- lm(Above_N ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model56)
model57 <- lm(Above_N ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model57)
model58 <- lm(Above_N ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model58)
model59 <- lm(Above_N ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model59)
model60 <- lm(Above_N ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model60)

model61 <- lm(Above_C ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model61)
model62 <- lm(Above_C ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model62)
model63 <- lm(Above_C ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model63)
model64 <- lm(Above_C ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model64)
model65 <- lm(Above_C ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model65)

model66 <- lm(Above_CN ~ Mussel_density, data = subset(data1, Elevation == "5"))
summary(model66)
model67 <- lm(Above_CN ~ Mussel_density, data = subset(data1, Elevation == "0"))
summary(model67)
model68 <- lm(Above_CN ~ Mussel_density, data = subset(data1, Elevation == "-5"))
summary(model68)
model69 <- lm(Above_CN ~ Mussel_density, data = subset(data1, Elevation == "-10"))
summary(model69)
model70 <- lm(Above_CN ~ Mussel_density, data = subset(data1, Elevation == "-15"))
summary(model70)

models <- mget(paste0("model", 1:70), ifnotfound = list(NULL))
res_all <- imap_dfr(models, ~ {
  fit <- .x
  mod_name <- .y
  td <- tidy(fit) %>% filter(term == "Mussels_density")
  gl <- glance(fit)
  tibble(Model = mod_name,
         beta = round(td$estimate, 3),
         SE   = round(td$std.error, 3),
         t    = round(td$statistic, 3),
         p    = round(td$p.value, 3),
         R2   = round(gl$r.squared, 3),
         df   = fit$df.residual)
})
print(res_all)



# Print table
# read data
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
dat <- read_excel("Raw data_Final.xlsx") |> as.data.frame()

# rename columns: carbon/nitrogen -> C/N
dat <- dat %>%
  rename_with(~ str_replace_all(.x, c("(?i)carbon" = "C", "(?i)nitrogen" = "N")))

# basic types
elev_levels <- c("5","0","-5","-10","-15")
dat <- dat %>%
  mutate(Elevation      = factor(trimws(as.character(Elevation)), levels = elev_levels),
         Mussel_density = as.numeric(Mussel_density),
         Level          = as.factor(Level),
         Treat          = as.factor(Treat),
         Organ          = as.factor(Organ))

# x-axis labels
x_labels_show <- c("0","254","509","762","1018")
x_breaks_raw <- sort(unique(dat$Mussel_density[!is.na(dat$Mussel_density)]))
if (length(x_breaks_raw) > length(x_labels_show)) {
  x_breaks_raw <- x_breaks_raw[seq_len(length(x_labels_show))]
} else if (length(x_breaks_raw) < length(x_labels_show)) {
  x_breaks_raw <- seq(min(dat$Mussel_density, na.rm = TRUE),
                      max(dat$Mussel_density, na.rm = TRUE),
                      length.out = length(x_labels_show))
}

# Table 1 (optional): LMMs  Level*Treat + (1|Organ)
resp_table1 <- c("Below_biomass","Above_biomass","Plant_biomass","lnSRR",
                 "Live_stems","Live_leaves","Height_average","Flowers",
                 "Below_C","Below_N","Below_CN","Above_C","Above_N","Above_CN",
                 "Water_content","Organic_matter")
resp_table1 <- resp_table1[resp_table1 %in% names(dat)]

lmm_results <- purrr::map(resp_table1, function(resp){
  f <- as.formula(paste0(resp, " ~ Level*Treat + (1|Organ)"))
  fit <- lmer(f, data = dat, na.action = na.omit)
  out <- car::Anova(fit)
  out_df <- as.data.frame(out)
  out_df$Response <- resp
  out_df$Effect <- rownames(out_df)
  rownames(out_df) <- NULL
  out_df[, c("Response","Effect", setdiff(names(out_df), c("Response","Effect")))]
}) |> bind_rows()
print(lmm_results)
#write.csv(lmm_results, "Table1_LMM_Anova.csv", row.names = FALSE)

# LM summary: y ~ Mussel_density within each Elevation (all vars)
vars_all_candidates <- c("Below_biomass","Above_biomass","Plant_biomass","lnSRR",
                         "Live_stems","Live_leaves","Height_average","Flowers",
                         "Below_C","Below_N","Below_CN","Above_C","Above_N","Above_CN",
                         "Water_content","Organic_matter")
vars_all <- vars_all_candidates[vars_all_candidates %in% names(dat)]

lm_by_elev <- function(df, var){
  df <- df %>% filter(!is.na(.data[[var]]), !is.na(Mussel_density))
  if (nrow(df) < 3 || length(unique(df$Mussel_density)) < 2) {
    return(tibble(beta=NA_real_, SE=NA_real_, t=NA_real_, p=NA_real_, R2=NA_real_, df=NA_real_))
  }
  fit <- lm(df[[var]] ~ Mussel_density, data = df)
  td  <- broom::tidy(fit) %>% filter(term == "Mussel_density")
  gl  <- broom::glance(fit)
  tibble(beta = td$estimate,
         SE   = td$std.error,
         t    = td$statistic,
         p    = td$p.value,
         R2   = gl$r.squared,
         df   = fit$df.residual)
}

res_all <- purrr::map_dfr(vars_all, function(v){
  dat %>%
    group_by(Elevation) %>%
    group_modify(~ lm_by_elev(.x, v)) %>%
    ungroup() %>%
    mutate(Variable = v, .before = 1)
})
print(res_all)
#write.csv(res_all, "LM_byElevation_summary.csv", row.names = FALSE)


# Figure 3
base_theme <- theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.75),
        axis.line        = element_line(color = "black", linewidth = 0.5),
        axis.ticks       = element_line(color = "black", linewidth = 0.5),
        axis.text        = element_text(size = 12),
        axis.title       = element_text(size = 12),
        plot.title       = element_text(size = 12, hjust = 0.5),
        legend.position  = "none",
        plot.margin      = margin(3, 3, 3, 3))

fmt_p_label <- function(p){
  if (is.na(p)) return(NULL)
  if (p < 0.001) return("italic(p) < 0.001")
  paste0("italic(p) == ", sprintf("%.3f", p))
}

ylab_map <- c(Height_average = "Stem height (cm)",
              Live_stems     = "Live stems (n/m²)",
              Plant_biomass  = "Plant biomass (g/m²)",
              Below_biomass  = "Below biomass (g/m²)",
              Above_biomass  = "Above biomass (g/m²)",
              Live_leaves    = "Live leaves (n/m²)",
              Flowers        = "Flowers (n)",
              lnSRR          = "lnSRR",
              Below_C        = "Below C",
              Below_N        = "Below N",
              Below_CN       = "Below C:N",
              Above_C        = "Above C",
              Above_N        = "Above N",
              Above_CN       = "Above C:N",
              Water_content  = "Water content",
              Organic_matter = "Organic matter")
  
ylim_fixed <- list(Height_average = c(0, 80),
                   Live_stems     = c(0, 4000),
                   Plant_biomass  = c(0, 15000),
                   Live_leaves    = c(0, 20000),
                   Flowers        = c(0, 20),
                   Below_biomass  = c(0, 8000),
                   Above_biomass  = c(0, 10000))

get_ylim <- function(var){
  if (var %in% names(ylim_fixed)) return(ylim_fixed[[var]])
  v <- dat[[var]]
  v <- v[is.finite(v)]
  if (length(v) == 0) return(NULL)
  mn <- min(v, na.rm = TRUE)
  mx <- max(v, na.rm = TRUE)
  if (mx == mn) { mx <- mx + 1; mn <- mn - 1 }
  pad <- 0.10 * (mx - mn)
  lo <- if (mn >= 0) 0 else (mn - pad)
  hi <- mx + pad
  c(lo, hi)
}

make_panel <- function(var, elev,
                       pt_size = 2.6,
                       pt_alpha = 0.22,
                       line_width = 1.5,
                       ribbon_alpha = 0.12){
  
  df <- dat %>%
    filter(Elevation == elev) %>%
    filter(!is.na(.data[[var]]), !is.na(Mussel_density))
  
  # slope p-value
  pval <- NA_real_
  if (nrow(df) >= 3 && length(unique(df$Mussel_density)) >= 2) {
    fit <- lm(df[[var]] ~ Mussel_density, data = df)
    pval <- summary(fit)$coefficients["Mussel_density","Pr(>|t|)"]
  }
  p_lab <- fmt_p_label(pval)
  
  ylab <- ifelse(var %in% names(ylab_map), ylab_map[[var]], var)
  ylim <- get_ylim(var)
  
  g <- ggplot(df, aes(x = Mussel_density, y = .data[[var]])) +
    geom_point(color = "steelblue", size = pt_size, alpha = pt_alpha) +
    # 95% CI ribbon (NO EDGE)
    stat_smooth(
      method = "lm", formula = y ~ x, se = TRUE, geom = "ribbon",
      fill = "steelblue", alpha = ribbon_alpha, color = NA
    ) +
    # fitted line
    geom_smooth(
      method = "lm", formula = y ~ x, se = FALSE,
      color = "steelblue", linewidth = line_width
    ) +
    labs(
      x = "Mussel density (n)",
      y = ylab,
      title = paste0("Elevation ", as.character(elev))
    ) +
  scale_x_continuous(
    breaks = x_breaks_raw,
    labels = x_labels_show
  ) +
    base_theme
  
  if (!is.null(ylim)) g <- g + coord_cartesian(ylim = ylim)
  if (!is.null(p_lab)) {
    g <- g + annotate(
      "text", x = Inf, y = Inf, label = p_lab,
      parse = TRUE, hjust = 1.10, vjust = 1.15,
      size = 3.6, color = "black"
    )
  }
  
  g
}

panel_labels <- function(n){
  alpha <- letters
  out <- character(n)
  for (i in seq_len(n)) {
    x <- i
    s <- ""
    while (x > 0) {
      r <- (x - 1) %% 26 + 1
      s <- paste0(alpha[r], s)
      x <- (x - 1) %/% 26
    }
    out[i] <- s
  }
  out
}

build_grid_by_vars <- function(vars_vec,
                               outfile,
                               width_mm,
                               height_mm,
                               add_labels = FALSE){
  plots <- list()
  for (elev in elev_levels) {
    for (v in vars_vec) {
      plots[[length(plots) + 1]] <- make_panel(v, elev)
    }
  }
  
  labs <- if (add_labels) panel_labels(length(plots)) else NULL
  comb <- cowplot::plot_grid(
    plotlist   = plots,
    ncol       = length(vars_vec),
    align      = "hv",
    axis       = "tblr",
    labels     = labs,
    label_size = 14,
    label_x    = 0.02,
    label_y    = 0.98,
    hjust      = 0,
    vjust      = 1,
    label_fontface = "bold"
  )
  
  print(comb)
  ggsave(outfile, comb, width = width_mm, height = height_mm, units = "mm")
  invisible(comb)
}

W_S3_MM <- 200
H_ALL_MM <- 300
PANEL_W_MM <- W_S3_MM / 3

# Figure 3
vars_S3 <- c("Height_average", "Live_stems", "Plant_biomass")
if (!all(vars_S3 %in% names(dat))) {
  stop("Figure S3 need：Height_average, Live_stems, Plant_biomass")
}

build_grid_by_vars(vars_vec   = vars_S3,
                   outfile    = "Figure 3.pdf",
                   width_mm   = W_S3_MM,
                   height_mm  = H_ALL_MM,
                   add_labels = TRUE)
  
# Figure S3 & S4
vars_appendix <- setdiff(vars_all, vars_S3)
if (length(vars_appendix) > 0) {
  W_APP_MM <- PANEL_W_MM * length(vars_appendix)
  build_grid_by_vars(vars_vec   = vars_appendix,
                     outfile    = "Figure S3 - Appendix ALL panels - 1219.pdf",
                     width_mm   = W_APP_MM,
                     height_mm  = H_ALL_MM,
                     add_labels = TRUE)
} else {
  message("No appendix variables found (vars_appendix is empty).")
}

