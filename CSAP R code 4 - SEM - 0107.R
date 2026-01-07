# Relationship between plant functional trait and soil traits
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
data <- read_excel("Raw data_Final.xlsx")
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

### SEM ###
setwd("/Users/yaolin/Desktop/Postdoc in LSU/Project - Ribbed Mussels Ecology/Submission - 0601")
data <- read_excel("Raw data_Final.xlsx")
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
