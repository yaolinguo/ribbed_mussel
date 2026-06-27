# -------------------------
# Biweekly analysis
# -------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
library(cowplot)
library(rlang)
library(officer)
library(flextable)

# setwd("Final version/GitHub")
raw <- read_excel("Raw data - Biweekly data - 0623.xlsx") |> as.data.frame()
AREA <- 10000 / 78.539816

unit <- raw %>%
  mutate(Corrected_height = as.numeric(Corrected_height),
         Live_leaves      = as.numeric(Live_leaves),
         L_D              = trimws(as.character(L_D))) %>%
  group_by(Days_deployed, Organ, Elevation, Mussel_density) %>%
  summarise(
    Stem_height    = mean(Corrected_height, na.rm = TRUE),
    Live_leaves    = sum(Live_leaves, na.rm = TRUE) * AREA,   # -> n m^-2
    Live_stems     = sum(L_D == "L",  na.rm = TRUE) * AREA,   # -> n m^-2
    Mortality_rate = sum(L_D == "D",  na.rm = TRUE) / n(),
    .groups = "drop"
  ) %>%
  mutate(Dens = factor(Mussel_density, levels = c(0, 2, 4, 8)))

# -------------------------
# simple linear regression
# -------------------------
fit_slope <- function(d) {
  d <- d[!is.na(d$value), ]
  if (nrow(d) < 3 || dplyr::n_distinct(d$Days_deployed) < 2)
    return(tibble(beta = NA_real_, SE = NA_real_, t = NA_real_,
                  p = NA_real_, R2 = NA_real_, df = NA_integer_))
  m  <- lm(value ~ Days_deployed, data = d)
  co <- summary(m)$coefficients["Days_deployed", ]
  tibble(beta = co["Estimate"], SE = co["Std. Error"], t = co["t value"],
         p = co["Pr(>|t|)"], R2 = summary(m)$r.squared, df = m$df.residual)
}

resp_map <- c(Stem_height = "Stem height", Live_leaves = "Live leaves",
              Live_stems  = "Live stems",  Mortality_rate = "Mortality rate")

res <- imap_dfr(resp_map, function(label, var) {
  unit %>%
    transmute(Elevation, Mussel_density, Days_deployed, value = .data[[var]]) %>%
    group_by(Elevation, Mussel_density) %>%
    group_modify(~ fit_slope(.x)) %>%
    ungroup() %>%
    mutate(Response = label, .before = 1)
})

res <- res %>%
  mutate(Response = factor(Response, levels = unname(resp_map))) %>%
  arrange(Response, desc(Elevation), Mussel_density)

print(res, n = 80)

# -------------------------
# Table S4
# -------------------------
fmt3 <- function(x) { s <- formatC(x, format = "f", digits = 3); s[s == "-0.000"] <- "0.000"; s }
pfmt <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))
elev_fmt <- function(e) ifelse(e > 0, paste0("+", e),
                               ifelse(e < 0, paste0("\u2013", abs(e)), "0"))

tab <- data.frame(
  resp = ifelse(duplicated(res$Response), "", as.character(res$Response)),
  elev = elev_fmt(res$Elevation),
  dens = as.character(res$Mussel_density),
  beta = fmt3(res$beta),
  SE   = fmt3(res$SE),
  t    = fmt3(res$t),
  P    = pfmt(res$p),
  R2   = fmt3(res$R2),
  df   = as.character(res$df),
  stringsAsFactors = FALSE
)

sig <- which(res$p < 0.05)
big   <- fp_border(width = 1.5,  color = "black")
small <- fp_border(width = 0.75, color = "black")

ft <- flextable(tab)
ft <- set_header_labels(ft, resp = "", elev = "", dens = "",
                        beta = "\u03b2", SE = "SE", t = "t", P = "P", R2 = "R2", df = "df")
ft <- add_header_row(ft, top = TRUE,
                     values = c("Response variables", "Relative elevation treatment",
                                "Mussel density treatment", "Deployment duration (days)"),
                     colwidths = c(1, 1, 1, 6))
ft <- merge_v(ft, part = "header", j = 1:3)
ft <- flextable::compose(ft, part = "header", i = 2, j = "beta", value = as_paragraph(as_i("\u03b2")))
ft <- flextable::compose(ft, part = "header", i = 2, j = "t",    value = as_paragraph(as_i("t")))
ft <- flextable::compose(ft, part = "header", i = 2, j = "P",    value = as_paragraph(as_i("P")))
ft <- flextable::compose(ft, part = "header", i = 2, j = "R2",   value = as_paragraph(as_i("R"), as_sup("2")))
ft <- flextable::compose(ft, part = "header", i = 2, j = "df",   value = as_paragraph(as_i("df")))
ft <- bold(ft, part = "header")
ft <- bold(ft, i = sig, part = "body")
ft <- border_remove(ft)
ft <- hline_top(ft, part = "header", border = big)
ft <- hline(ft, i = 1, j = 4:9, part = "header", border = small)
ft <- hline_bottom(ft, part = "header", border = small)
ft <- hline_bottom(ft, part = "body", border = big)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 10, part = "all")
ft <- align(ft, align = "left", part = "all")
ft <- valign(ft, valign = "center", part = "header")
ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
ft <- width(ft, j = 1:9, width = c(1.084, 1.279, 1.085, 0.592, 0.592, 0.590, 0.685, 0.496, 0.369))
ft <- set_table_properties(ft, layout = "fixed")

tt  <- fp_text(font.family = "Times New Roman", font.size = 11, bold = TRUE)
tti <- fp_text(font.family = "Times New Roman", font.size = 11, bold = TRUE, italic = TRUE)
nn  <- fp_text(font.family = "Times New Roman", font.size = 11)
nni <- fp_text(font.family = "Times New Roman", font.size = 11, italic = TRUE)
sup <- fp_text(font.family = "Times New Roman", font.size = 11, italic = TRUE, vertical.align = "superscript")

cap <- fpar(
  ftext("Table S4 Summary of simple linear regressions evaluating the relationships between deployment duration and plant growth and mortality traits of ", tt),
  ftext("Spartina alterniflora", tti),
  ftext(" across relative elevation treatments and mussel density treatments.", tt),
  ftext(" Abbreviations: ", nn), ftext("\u03b2", nni),
  ftext(" = slope estimator; SE = standard error; ", nn), ftext("df", nni),
  ftext(" = degrees of freedom; ", nn), ftext("R", nni), ftext("2", sup),
  ftext(" = goodness of fit of the model. Significant regression slopes are indicated by bold ", nn),
  ftext("p", nni), ftext("-values.", nn)
)

doc <- read_docx()
doc <- body_add_fpar(doc, cap)
doc <- body_add_par(doc, "")
doc <- body_add_flextable(doc, ft)
print(doc, target = "Table S4.docx")

# -------------------------
# Figure 2
# -------------------------
dens_cols <- c("0" = "#E53528", "2" = "#F09739", "4" = "#193E8F", "8" = "#55B7E6")
elev_order <- c(5, 0, -5, -10, -15)
elev_lab   <- function(e) ifelse(e > 0, paste0(e, " cm"),
                                 ifelse(e < 0, paste0("\u2013", abs(e), " cm"), "0 cm"))

traits <- list(
  list(var = "Stem_height",    lab = "Stem height (cm)",
       ylim = c(0, 120),  brks = c(0, 40, 80, 120),      res = "Stem height"),
  list(var = "Live_stems",     lab = expression("Live stems (n/m"^2*")"),
       ylim = c(0, 6000), brks = c(0, 2000, 4000, 6000), res = "Live stems"),
  list(var = "Mortality_rate", lab = "Stem mortality rate (%)",
       ylim = c(0, 1),    brks = c(0, .25, .5, .75, 1),   res = "Mortality rate")
)

stat_tab <- res %>%
  transmute(Response = as.character(Response), Elevation,
            Dens = factor(Mussel_density, levels = c(0, 2, 4, 8)),
            p, sig = !is.na(p) & p < 0.05)

make_panel <- function(tr, e, show_x, show_strip) {
  d  <- unit %>%
    transmute(Days_deployed, Elevation, Dens, value = .data[[tr$var]]) %>%
    filter(Elevation == e, !is.na(value))
  st <- stat_tab %>% filter(Response == tr$res, Elevation == e) %>% arrange(Dens)
  d_sig <- d %>% filter(Dens %in% st$Dens[st$sig])
  span <- tr$ylim[2] - tr$ylim[1]
  lab  <- st %>% mutate(
    txt  = ifelse(is.na(p), "n/a",
                  ifelse(p < 0.001, "< 0.001", paste0("= ", formatC(p, format = "f", digits = 3)))),
    expr = sprintf('italic(P)[%s]~"%s"', as.character(Dens), txt),
    x    = 3,
    y    = tr$ylim[2] - span * 0.075 * (as.integer(Dens) - 1)
  )
  g <- ggplot(d, aes(Days_deployed, value, colour = Dens)) +
    geom_point(size = 1.3, alpha = 0.30)
  if (nrow(d_sig) > 0)
    g <- g + geom_smooth(data = d_sig, aes(fill = Dens),
                         method = "lm", formula = y ~ x, se = TRUE,
                         linewidth = 0.9, alpha = 0.15)
  g <- g +
    geom_text(data = lab, aes(x = x, y = y, label = expr, colour = Dens),
              parse = TRUE, hjust = 0, vjust = 1, size = 3.5, show.legend = FALSE) +
    scale_colour_manual(values = dens_cols, drop = FALSE) +
    scale_fill_manual(values = dens_cols, drop = FALSE) +
    scale_x_continuous(breaks = c(0, 50, 100)) +
    scale_y_continuous(breaks = tr$brks) +
    coord_cartesian(xlim = c(0, 140), ylim = tr$ylim, clip = "off") +
    labs(x = if (show_x) "Days deployed" else NULL, y = tr$lab) +
    theme_classic(base_size = 11) +
    theme(legend.position = "none",
          panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
          axis.title.x = if (show_x) element_text() else element_blank(),
          plot.margin  = margin(4, if (show_strip) 22 else 4, 4, 4))
  if (show_strip)
    g <- g + annotate("text", x = 140 * 1.14, y = mean(tr$ylim),
                      label = elev_lab(e), angle = -90, fontface = 2, size = 4)
  g
}

panels <- list()
for (e in elev_order)
  for (j in seq_along(traits))
    panels[[length(panels) + 1]] <- make_panel(traits[[j]], e,
                                               show_x = (e == -15),
                                               show_strip = (j == 3))

grid <- cowplot::plot_grid(plotlist = panels, ncol = 3, align = "hv", axis = "tblr",
                           labels = letters[1:15], label_size = 15,
                           label_fontface = "bold", label_x = 0.01, label_y = 1.0,
                           hjust = 0, vjust = 1)

leg <- cowplot::get_legend(
  ggplot(unit, aes(Days_deployed, Stem_height, colour = Dens)) +
    geom_point() +
    scale_colour_manual("Density", values = dens_cols, drop = FALSE) +
    theme(legend.position = "bottom",
          legend.text  = element_text(size = 12),
          legend.title = element_text(face = "bold", size = 12))
)

combined_main <- cowplot::plot_grid(grid, leg, ncol = 1, rel_heights = c(1, 0.045))
print(combined_main)
ggsave("Figure 2.pdf", combined_main, width = 200, height = 280, units = "mm")