library(readxl)
library(lme4)
library(car)
library(broom)
library(purrr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(cowplot)
library(rlang)
library(flextable)
library(officer)

# -------------------------
# Table S5 - output
# -------------------------
# setwd("Final version/GitHub")
data <- read_excel("Raw data - Final data - 0623.xlsx")
data <- as.data.frame(data)

model1  <- lmer(Below_biomass       ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model1)
model2  <- lmer(Above_biomass       ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model2)
model3  <- lmer(Plant_biomass       ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model3)
model4  <- lmer(lnSRR               ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model4)
model5  <- lmer(Live_stems          ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model5)
model6  <- lmer(Live_leaves         ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model6)
model7  <- lmer(Height_average      ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model7)
model8  <- lmer(Flowers             ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model8)
model9  <- lmer(Below_C             ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model9)
model10 <- lmer(Below_N             ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model10)
model11 <- lmer(Below_CN            ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model11)
model12 <- lmer(Above_C             ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model12)
model13 <- lmer(Above_N             ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model13)
model14 <- lmer(Above_CN            ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model14)
model15 <- lmer(Soil_water_content  ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model15)
model16 <- lmer(Soil_organic_matter ~ Level*Treat + (1 | Organ), data = data, na.action = na.omit)
Anova(model16)

tbl_no   <- "S5"
out_file <- paste0("Table ", tbl_no, ".docx")
data <- as.data.frame(read_excel("Raw data - Final data - 0623.xlsx"))

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
fmt_chi <- function(x) sprintf("%.3f", x)
fmt_p   <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
terms   <- c("Level", "Treat", "Level:Treat")
stat <- vector("list", length(resp_vars))
sig  <- matrix(FALSE, length(resp_vars), 3, dimnames = list(NULL, c("E","D","I")))

for (i in seq_along(resp_vars)) {
  f <- as.formula(paste0(resp_vars[i], " ~ Level*Treat + (1 | Organ)"))
  m <- lmer(f, data = data, na.action = na.omit)
  a <- as.data.frame(car::Anova(m))
  chi <- a[terms, "Chisq"]; df <- a[terms, "Df"]; p <- a[terms, "Pr(>Chisq)"]
  stat[[i]] <- c(fmt_chi(chi[1]), as.character(df[1]), fmt_p(p[1]),
                 fmt_chi(chi[2]), as.character(df[2]), fmt_p(p[2]),
                 fmt_chi(chi[3]), as.character(df[3]), fmt_p(p[3]))
  sig[i, ] <- p < 0.05
}

cols <- c("resp","Echi","Edf","Ep","sp1","Dchi","Ddf","Dp","sp2","Ichi","Idf","Ip")
blank_row <- function(lab) { r <- as.data.frame(matrix("", 1, 12), stringsAsFactors = FALSE)
names(r) <- cols; r$resp <- lab; r }
data_row  <- function(lab, s) { r <- blank_row(lab)
r$Echi<-s[1]; r$Edf<-s[2]; r$Ep<-s[3]
r$Dchi<-s[4]; r$Ddf<-s[5]; r$Dp<-s[6]
r$Ichi<-s[7]; r$Idf<-s[8]; r$Ip<-s[9]; r }
body <- blank_row("Plant traits")
for (i in 1:n_plant)                       body <- rbind(body, data_row(resp_labs[i], stat[[i]]))
body <- rbind(body, blank_row("Soil traits"))
for (i in (n_plant+1):length(resp_vars))   body <- rbind(body, data_row(resp_labs[i], stat[[i]]))
sec_rows <- c(1, n_plant + 2)
var_brow <- c(1:n_plant + 1, (n_plant+1):length(resp_vars) + 2)

ft <- flextable(body, col_keys = cols)
ft <- set_header_labels(ft,
                        resp = "Response variables",
                        Echi = "\u03C7\u00B2", Edf = "df", Ep = "p", sp1 = "",
                        Dchi = "\u03C7\u00B2", Ddf = "df", Dp = "p", sp2 = "",
                        Ichi = "\u03C7\u00B2", Idf = "df", Ip = "p")
ft <- add_header_row(ft, top = TRUE,
                     values   = c("Response variables", "Relative elevation treatment (E)", "",
                                  "Mussel density treatment (D)", "", "E \u00D7 D interaction"),
                     colwidths = c(1, 3, 1, 3, 1, 3))
ft <- merge_v(ft, j = "resp", part = "header")
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 10.5, part = "all")
ft <- valign(ft, valign = "center", part = "all")

num_cols <- c("Echi","Edf","Ep","Dchi","Ddf","Dp","Ichi","Idf","Ip")

ft <- align(ft, j = num_cols, align = "center", part = "all")
ft <- align(ft, j = "resp",   align = "left",   part = "all")
ft <- align(ft, i = 1, align = "center", part = "header")
ft <- bold(ft, part = "header")
ft <- italic(ft, j = c("Ep","Dp","Ip"), part = "header")
ft <- bold(ft,   i = sec_rows, part = "body")
ft <- italic(ft, i = sec_rows, part = "body")

grp_cols <- list(E = c("Echi","Edf","Ep"),
                 D = c("Dchi","Ddf","Dp"),
                 I = c("Ichi","Idf","Ip"))
for (i in seq_along(resp_vars)) {
  br <- var_brow[i]
  for (g in 1:3) if (sig[i, g]) ft <- bold(ft, i = br, j = grp_cols[[g]], part = "body")
}

big   <- fp_border(color = "black", width = 1.5)
small <- fp_border(color = "black", width = 0.75)

ft <- border_remove(ft)
ft <- hline_top(ft, border = big,   part = "header")
ft <- hline(ft, i = 1, j = c("Echi","Edf","Ep"),  border = small, part = "header")
ft <- hline(ft, i = 1, j = c("Dchi","Ddf","Dp"),  border = small, part = "header")
ft <- hline(ft, i = 1, j = c("Ichi","Idf","Ip"),  border = small, part = "header")
ft <- hline_bottom(ft, border = small, part = "header")
ft <- hline_bottom(ft, border = big,   part = "body")

w <- c(2410,851,425,992,284,850,425,993,283,851,567,708) / 1440
for (k in seq_along(cols)) ft <- width(ft, j = cols[k], width = w[k])
ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
pr   <- fp_text(font.family = "Times New Roman", font.size = 10.5)
pr_b <- update(pr, bold = TRUE)
pr_i <- update(pr, italic = TRUE)

cap <- fpar(
  ftext(paste0("Table ", tbl_no, " "), pr_b),
  ftext(paste0("Effects of relative elevation treatments (E), mussel density treatments (D) ",
               "and their interactions on response variables including plant traits of "), pr),
  ftext("Spartina alterniflora", pr_i),
  ftext(paste0(" and soil traits. df, degrees of freedom; \u03C7\u00B2, chi\u2013squared test ",
               "statistic. The significant effects are shown in bold."), pr))

doc <- read_docx()
doc <- body_add_fpar(doc, cap)
doc <- body_add_par(doc, "")
doc <- body_add_flextable(doc, ft)
print(doc, target = out_file)
cat("Wrote", normalizePath(out_file), "\n")

# -------------------------
# Table S8 - output
# -------------------------
setwd("Final version/GitHub")
data <- read_excel("Raw data - Final data - 0623.xlsx")
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

d <- as.data.frame(read_excel("Raw data - Final data - 0623.xlsx"))

vars <- c(
  Below_biomass  = "Belowground biomass",
  Above_biomass  = "Aboveground biomass",
  Plant_biomass  = "Total plant biomass",
  lnSRR          = "ln(stem:root ratio)",
  Live_stems     = "Live stems",
  Live_leaves    = "Live leaves",
  Height_average = "Stem height",
  Flowers        = "Flowers",
  Below_N        = "Belowground nitrogen",
  Below_C        = "Belowground carbon",
  Below_CN       = "Belowground C:N",
  Above_N        = "Aboveground nitrogen",
  Above_C        = "Aboveground carbon",
  Above_CN       = "Aboveground C:N"
)

elevs    <- c(5, 0, -5, -10, -15)
elev_lab <- c("+5", "0", "\u20135", "\u201310", "\u201315")
rows <- list()
for (v in names(vars)) {
  for (j in seq_along(elevs)) {
    sub <- d[d$Elevation == elevs[j], ]
    fit <- lm(reformulate("Mussel_density", v), data = sub)
    co  <- summary(fit)$coefficients["Mussel_density", ]
    rows[[length(rows) + 1]] <- data.frame(
      var = vars[[v]], first = (j == 1), elev = elev_lab[j],
      beta = co["Estimate"], SE = co["Std. Error"], t = co["t value"],
      p = co["Pr(>|t|)"], R2 = summary(fit)$r.squared, df = fit$df.residual,
      stringsAsFactors = FALSE
    )
  }
}
res <- do.call(rbind, rows)

fmt3 <- function(x) { s <- formatC(x, format = "f", digits = 3); s[s == "-0.000"] <- "0.000"; s }
pfmt <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))

tab <- data.frame(
  resp = ifelse(res$first, res$var, ""),
  elev = res$elev,
  beta = fmt3(res$beta), SE = fmt3(res$SE), t = fmt3(res$t),
  p    = pfmt(res$p),    R2 = fmt3(res$R2), df = as.character(res$df),
  stringsAsFactors = FALSE
)

sig <- which(res$p < 0.05)
big   <- fp_border(width = 1.5,  color = "black")
small <- fp_border(width = 0.75, color = "black")

ft <- flextable(tab)
ft <- set_header_labels(ft, resp = "", elev = "",
                        beta = "\u03b2", SE = "SE", t = "t", p = "p", R2 = "R2", df = "df")
ft <- add_header_row(ft, top = TRUE,
                     values = c("Response variables", "Relative elevation treatment", "Mussel density"),
                     colwidths = c(1, 1, 6))
ft <- merge_v(ft, part = "header", j = 1:2)
ft <- compose(ft, part = "header", i = 2, j = "beta", value = as_paragraph(as_i("\u03b2")))
ft <- compose(ft, part = "header", i = 2, j = "t",    value = as_paragraph(as_i("t")))
ft <- compose(ft, part = "header", i = 2, j = "p",    value = as_paragraph(as_i("p")))
ft <- compose(ft, part = "header", i = 2, j = "R2",   value = as_paragraph(as_i("R"), as_sup("2")))
ft <- compose(ft, part = "header", i = 2, j = "df",   value = as_paragraph(as_i("df")))
ft <- bold(ft, part = "header")
ft <- bold(ft, i = sig, part = "body")
ft <- border_remove(ft)
ft <- hline_top(ft, part = "header", border = big)
ft <- hline(ft, i = 1, j = 3:8, part = "header", border = small)
ft <- hline_bottom(ft, part = "header", border = small)
ft <- hline_bottom(ft, part = "body", border = big)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 9, part = "all")
ft <- align(ft, align = "left", part = "all")
ft <- valign(ft, valign = "center", part = "header")
ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
ft <- width(ft, j = 1:8, width = c(1.75, 1.45, 0.62, 0.55, 0.52, 0.60, 0.50, 0.40))
ft <- set_table_properties(ft, layout = "fixed")

tt  <- fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE)
tti <- fp_text(font.family = "Times New Roman", font.size = 10, bold = TRUE, italic = TRUE)
nn  <- fp_text(font.family = "Times New Roman", font.size = 10)
nni <- fp_text(font.family = "Times New Roman", font.size = 10, italic = TRUE)
sup <- fp_text(font.family = "Times New Roman", font.size = 10, italic = TRUE, vertical.align = "superscript")

cap <- fpar(
  ftext("Table S8 Summary of within-elevation simple linear regressions describing associations between mussel density and response variables of ", tt),
  ftext("Spartina alterniflora", tti),
  ftext(" across relative elevation treatments.", tt),
  ftext(" Abbreviations: ", nn), ftext("\u03b2", nni),
  ftext(" = slope estimator; SE = standard error; ", nn), ftext("df", nni),
  ftext(" = degrees of freedom; ", nn), ftext("R", nni), ftext("2", sup),
  ftext(" = goodness of fit of the model.", nn)
)

doc <- read_docx()
doc <- body_add_fpar(doc, cap)
doc <- body_add_par(doc, "")
doc <- body_add_flextable(doc, ft)
print(doc, target = "Table S8.docx")

# -------------------------
# Figure 3 & S3 & S4
# -------------------------
dat <- as.data.frame(read_excel("Raw data - Final data - 0623.xlsx"))
dat$Elevation      <- as.numeric(dat$Elevation)
dat$Mussel_density <- as.numeric(dat$Mussel_density)

elev_levels   <- c(5, 0, -5, -10, -15)
x_breaks_raw  <- c(0, 2, 4, 8)
x_labels_show <- c("0", "254", "509", "1018")
vars_all <- c("Height_average","Live_stems","Plant_biomass",
              "Below_biomass","Above_biomass","Live_leaves","Flowers","lnSRR",
              "Below_C","Below_N","Below_CN","Above_C","Above_N","Above_CN")

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
              Soil_water_content  = "Water content",
              Soil_organic_matter = "Organic matter")

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
    stat_smooth(
      method = "lm", formula = y ~ x, se = TRUE, geom = "ribbon",
      fill = "steelblue", alpha = ribbon_alpha, color = NA
    ) +
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

vars_S3 <- c("Height_average", "Live_stems", "Plant_biomass")
if (!all(vars_S3 %in% names(dat))) {
  stop("Figure S3 need：Height_average, Live_stems, Plant_biomass")
}

build_grid_by_vars(vars_vec   = vars_S3,
                   outfile    = "Figure 3.pdf",
                   width_mm   = W_S3_MM,
                   height_mm  = H_ALL_MM,
                   add_labels = TRUE)

vars_appendix <- setdiff(vars_all, vars_S3)
if (length(vars_appendix) > 0) {
  W_APP_MM <- PANEL_W_MM * length(vars_appendix)
  build_grid_by_vars(vars_vec   = vars_appendix,
                     outfile    = "Figure S3 & S4 - Appendix ALL panels.pdf",
                     width_mm   = W_APP_MM,
                     height_mm  = H_ALL_MM,
                     add_labels = TRUE)
} else {
  message("No appendix variables found (vars_appendix is empty).")
}

# -------------------------
# Figure S6
# -------------------------
dat <- read_excel("Raw data - Final data - 0623.xlsx") %>%
  mutate(
    Elevation = as.numeric(Elevation),
    Mussel_biomass = as.numeric(Mussel_biomass),
    Treat = factor(Treat, levels = c("B", "P", "OR", "Y"))
  )

dat_mus <- dat %>%
  filter(Treat %in% c("B", "P", "OR")) %>%
  droplevels()

treat_cols <- c(
  "B"  = "#55B7E6",
  "P"  = "#193E8F",
  "Y"  = "#E53528",
  "OR" = "#F09739"
)

treat_labs <- c(
  "B"  = "8 mussels",
  "P"  = "4 mussels",
  "OR" = "2 mussels"
)

fmt_p <- function(p) {
  if (is.na(p)) {
    "p = NA"
  } else if (p < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", sprintf("%.3f", p))
  }
}

make_p_labels <- function(df, yvar, treat_order, x_pos = -13.8,
                          start_frac = 0.05, step_frac = 0.10) {
  
  yvals <- df[[yvar]]
  y_max <- max(yvals, na.rm = TRUE)
  y_min <- min(yvals, na.rm = TRUE)
  y_span <- y_max - y_min
  if (y_span == 0) y_span <- 1
  
  out <- lapply(seq_along(treat_order), function(i) {
    tr <- treat_order[i]
    dsub <- df %>% filter(Treat == tr)
    
    fit <- try(lm(reformulate("Elevation", response = yvar), data = dsub), silent = TRUE)
    
    pval <- NA_real_
    if (!inherits(fit, "try-error")) {
      ctab <- summary(fit)$coefficients
      if ("Elevation" %in% rownames(ctab)) {
        pval <- ctab["Elevation", "Pr(>|t|)"]
      }
    }
    
    data.frame(
      Treat = tr,
      label = paste0(treat_labs[tr], ": ", fmt_p(pval)),
      x = x_pos,
      y = y_max - start_frac * y_span - (i - 1) * step_frac * y_span
    )
  })
  
  bind_rows(out)
}

p_lab_1 <- make_p_labels(dat_mus, "Mussel_biomass", c("B", "P", "OR"))

style_ci <- function(p,
                     pt_size = 2.2,
                     pt_alpha = 0.20,
                     line_width = 1.7,
                     ribbon_alpha = 0.08) {
  
  xvar <- rlang::as_name(p$mapping$x)
  yvar <- rlang::as_name(p$mapping$y)
  
  ribbon <- ggplot2::stat_smooth(
    data = p$data,
    mapping = ggplot2::aes_string(
      x = xvar, y = yvar, fill = "Treat", group = "Treat"
    ),
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
      if (!is.null(p$layers[[i]]$stat_params$se)) {
        p$layers[[i]]$stat_params$se <- FALSE
      }
    }
  }
  
  p + scale_fill_manual(values = treat_cols, guide = "none")
}

p1 <- ggplot(
  dat_mus,
  aes(x = Elevation, y = Mussel_biomass, colour = Treat)
) +
  geom_point(position = position_jitter(width = 0.35, height = 0)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(
    data = p_lab_1,
    aes(x = x, y = y, label = label, colour = Treat),
    inherit.aes = FALSE,
    hjust = 1,
    size = 3.8,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = treat_cols,
    breaks = c("B", "P", "OR"),
    labels = c("8", "4", "2"),
    name = "Initial mussel density"
  ) +
  scale_x_reverse(breaks = c(5, 0, -5, -10, -15)) +
  labs(
    x = "Relative elevation treatment (cm)",
    y = expression("Realized final mussel biomass (g " * m^{-2} * ")")
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(colour = "black", linewidth = 0.6)
  )

p1f <- style_ci(p1)
print(p1f)
ggsave(
  "Figure S6.pdf",
  p1f,
  width = 95,
  height = 90,
  units = "mm"
)