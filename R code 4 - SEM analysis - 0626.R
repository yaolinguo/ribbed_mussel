library(readxl)
library(piecewiseSEM)
library(flextable)
library(officer)
library(corrplot)

# -------------------------
# Relationship between plant functional trait and soil traits
# -------------------------
# setwd("Final version/GitHub")
data <- read_excel("Raw data - Final data - 0623.xlsx")
data <- as.data.frame(data)

colnames(data) <- gsub("\\.", "_", colnames(data))
cont_vars <- c("Elevation", 
               "Inundation_percentage", 
               "Mussel_density",
               "Mussel_biomass",	
               "Below_biomass",
               "Above_biomass",
               "Plant_biomass",	
               "lnSRR",
               "Live_stems",
               "Live_leaves",
               "Height_average",	
               "Flowers",
               "Below_C",
               "Below_N",
               "Below_CN",	
               "Above_C",
               "Above_N",
               "Above_CN",
               "Soil_water_content",
               "Soil_organic_matter")

setdiff(cont_vars, colnames(data))
data_std <- data
data_std[cont_vars] <- scale(data[cont_vars])

vars <- c("Elevation", 
          "Inundation_percentage", 
          "Mussel_density",
          "Mussel_biomass",	
          "Below_biomass",
          "Above_biomass",
          "Plant_biomass",	
          "lnSRR",
          "Live_stems",
          "Live_leaves",
          "Height_average",	
          "Flowers",
          "Below_C",
          "Below_N",
          "Below_CN",	
          "Above_C",
          "Above_N",
          "Above_CN",
          "Soil_water_content",
          "Soil_organic_matter")
M <- cor(data_std[, vars], use = "pairwise.complete.obs")
which(abs(M) > 0.8 & abs(M) < 1, arr.ind = TRUE)
M <- M[vars, vars]

pdf("correlation_matrix.pdf", width = 8, height = 8)

corrplot(M, 
         method = "circle",
         order = "original",
         tl.cex = 0.8,
         tl.col = "black")
dev.off()

# -------------------------
# SEM
# -------------------------
data <- as.data.frame(data)
colnames(data) <- gsub("\\.", "_", colnames(data))
cont_vars <- c("Elevation", "Inundation_percentage",
               "Mussel_density", "Mussel_biomass",
               "Below_biomass","Above_biomass", "Plant_biomass",
               "Below_CN", "Above_CN",
               "Soil_water_content", "Soil_organic_matter")

setdiff(cont_vars, colnames(data))
data_std <- data
data_std[cont_vars] <- scale(data[cont_vars])
colnames(data_std)

data_nomiss <- na.omit(data_std)

m1 <- lm(Soil_water_content   ~ Inundation_percentage,
         data = data_nomiss)
m2 <- lm(Soil_organic_matter  ~ Inundation_percentage + Mussel_biomass + Soil_water_content,
         data = data_nomiss)
m3 <- lm(Above_biomass        ~ Soil_water_content + Soil_organic_matter + Mussel_biomass + Inundation_percentage + Below_biomass,
         data = data_nomiss)
m4 <- lm(Below_biomass        ~ Soil_water_content + Soil_organic_matter + Mussel_biomass + Inundation_percentage,
         data = data_nomiss)
m5 <- lm(Above_CN             ~ Above_biomass + Soil_water_content + Soil_organic_matter + Mussel_biomass + Inundation_percentage + Below_CN,
         data = data_nomiss)
m6 <- lm(Below_CN             ~ Below_biomass + Soil_water_content + Soil_organic_matter + Mussel_biomass + Inundation_percentage,
         data = data_nomiss)

pw_mod_updated <- psem(m1, m2, m3, m4, m5, m6)
summary(pw_mod_updated)

# -------------------------
# Table S9 - output (Word)
# -------------------------
co <- as.data.frame(coefs(pw_mod_updated))

lab <- c(Soil_water_content    = "Soil water content",
         Soil_organic_matter   = "Soil organic matter",
         Above_biomass         = "Aboveground biomass",
         Below_biomass         = "Belowground biomass",
         Above_CN              = "Aboveground C:N",
         Below_CN              = "Belowground C:N",
         Inundation_percentage = "Inundation frequency",
         Mussel_biomass        = "Mussel biomass")

relab <- function(x){ x <- as.character(x); out <- lab[x]; out[is.na(out)] <- x[is.na(out)]; unname(out) }
fmt3 <- function(x){ s <- formatC(x, format = "f", digits = 3); s[s == "-0.000"] <- "0.000"; s }
pfmt <- function(p) ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3))

S9tab <- data.frame(
  resp = relab(co$Response),
  pred = relab(co$Predictor),
  est  = fmt3(co$Estimate),
  se   = fmt3(co$Std.Error),
  df   = as.character(co$DF),
  t    = fmt3(co$Crit.Value),
  p    = pfmt(co$P.Value),
  stringsAsFactors = FALSE
)

sig_rows <- which(co$P.Value < 0.05)
big   <- fp_border(color = "black", width = 1.5)
small <- fp_border(color = "black", width = 0.75)

ft <- flextable(S9tab)
ft <- set_header_labels(ft, resp = "Response variables", pred = "Predictor",
                        est = "Estimate", se = "SE", df = "df", t = "t", p = "p")
ft <- bold(ft, part = "header")
ft <- italic(ft, j = c("df","t","p"), part = "header")
ft <- bold(ft, i = sig_rows, part = "body")

ft <- border_remove(ft)
ft <- hline_top(ft, part = "header", border = big)
ft <- hline_bottom(ft, part = "header", border = small)
ft <- hline_bottom(ft, part = "body", border = big)

ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 11, part = "all")
ft <- align(ft, align = "left", part = "all")
ft <- valign(ft, valign = "center", part = "all")
ft <- padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
ft <- width(ft, j = 1:7, width = c(2552, 2410, 1134, 992, 709, 992, 993) / 1440)
ft <- set_table_properties(ft, layout = "fixed")

pr   <- fp_text(font.family = "Times New Roman", font.size = 11)
pr_b <- update(pr, bold = TRUE)
pr_i <- update(pr, italic = TRUE)

cap <- fpar(
  ftext(paste0("Table S9 Standardized path coefficients from the piecewise structural ",
               "equation model (SEM) evaluating associations among inundation frequency, ",
               "mussel biomass, soil traits, plant biomass, and plant C:N ratios."), pr_b),
  ftext(" The table reports path coefficients (Estimate), standard errors (SE), degrees of freedom (", pr),
  ftext("df", pr_i), ftext("), t-values (", pr), ftext("t", pr_i),
  ftext("), and ", pr), ftext("P", pr_i), ftext("-values (", pr), ftext("P", pr_i),
  ftext(paste0("). All continuous variables were z-standardized prior to modeling; thus, ",
               "all reported estimates are standardized coefficients."), pr))

doc <- read_docx()
doc <- body_add_fpar(doc, cap)
doc <- body_add_par(doc, "")
doc <- body_add_flextable(doc, ft)
print(doc, target = "Table S9.docx")
cat("Wrote", normalizePath("Table S9.docx"), "\n")