#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Data Exploration
# Script 4 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

## Load packages
require(lattice) #v0.20-41
require(tidyverse) #v1.3.0
require(lme4) #v1.1-26
require(piecewiseSEM) #v2.1.2
require(MuMIn) #v1.43.1
require(corrplot) #v0.84

## clear workspace ##
rm(list=ls())

## set working directory
datWD <- c("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper") #set path for the location of the environment image of script 2 output
setwd(datWD)
#load data from previous 
load("./scripts/script2_output.RData")

####Data exploration steps for survival data####
#Data exploration steps
#trim out the variables we aren't using from the datset
poly_temp <- CO_poly_all
#variables to test
MyVar <- c("species","quad","year_t","area_t","survives_tplus1","TLP",
           "SPEI_uniform","LDMC_g_g","Tribe", "SLA_adj_cm2_g", "SRL_m_g", "RDMC_g_g","RTD_g_cm3", "AvgDiam_mm", "neighbor_area_5",
           "neighbor_area_15", "neighbor_area_10","neighbor_area_20")
poly_temp <- poly_temp[,MyVar]

## Data exploration for graminoids
### Look for outliers
par(mfrow=c(3,4))
hist(poly_temp$LDMC_g_g)
hist(poly_temp$TLP)
hist(poly_temp$survives)
hist(poly_temp$SPEI_uniform)
hist(poly_temp$SLA_adj_cm2_g)
hist(poly_temp$RDMC_g_g)
hist(poly_temp$RTD_g_cm3)
hist(poly_temp$AvgDiam_mm)
hist(poly_temp$SRL_m_g)
boxplot(poly_temp$area,
varwidth = TRUE,
main = "Boxplot of area",
ylab = "LDMC_g_g", col=c("gray40", "gray60", "gray80"), data = poly_temp, pch=16)	

###	Extra 0s
#From the above histograms, it looks like the only variable with a lot of zeros is survival, which makes sense (it is all 0sand 1s, so we expect lots of 0s).  

###	Check for collinearity
#for TLP models
par(mfrow = c(1,1))
MyVar2 <- c("area_t","survives_tplus1","TLP","SPEI_uniform")
corrplot(cor(poly_temp[,MyVar2], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#There is some corrrelation between survival and area, but not enough to worry about (0.32)

#for LDMC_g_g models
MyVar3 <- c("area_t","survives_tplus1","LDMC_g_g","SPEI_uniform")
corrplot(cor(poly_temp[,MyVar3], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#There is some corrrelation between survival and area, but not enough to worry about (0.3)

#for SLA models
MyVar4 <- c("area_t","survives_tplus1","SLA_adj_cm2_g","SPEI_uniform")
corrplot(cor(poly_temp[,MyVar4], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#There is some corrrelation between survival and area, but not enough to worry about (0.3)

#for SRL models
MyVar5 <- c("area_t","survives_tplus1","SRL_m_g","SPEI_uniform")
corrplot(cor(poly_temp[,MyVar5], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#There is some corrrelation between survival and area, but not enough to worry about (0.3)

#for RDMC models
MyVar6<- c("area_t","survives_tplus1","RDMC_g_g","SPEI_uniform")
corrplot(cor(poly_temp[,MyVar6], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#There is some corrrelation between survival and area, but not enough to worry about (0.3)

#for RTD models
MyVar7 <- c("area_t","survives_tplus1","RTD_g_cm3","SPEI_uniform")
corrplot(cor(poly_temp[,MyVar7], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#There is some corrrelation between survival and area, but not enough to worry about (0.3)

#for RDiam models
MyVar8 <- c("area_t","survives_tplus1","AvgDiam_mm","SPEI_uniform")
corrplot(cor(poly_temp[,MyVar8], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#There is some corrrelation between survival and area, but not enough to worry about (0.3)


### examing variance inflation factors (VIF) for fixed effects
#for TLP model
corvif(poly_temp[,MyVar2])
#for LDMC_g_g model
corvif(poly_temp[,MyVar3])
#for SLA model
corvif(poly_temp[,MyVar4])
#for SRL model
corvif(poly_temp[,MyVar5])
#for RDMC_g_g model
corvif(poly_temp[,MyVar6])
#for RTD model
corvif(poly_temp[,MyVar7])
#for RDiam model
corvif(poly_temp[,MyVar8])

# All values are below 3, which is good


### Data exploration steps for forbs
#trim the dataset to those variables you actually want 
temp_point <- CO_point_all
#variables to test
MyVar <- c("species","quad","year","stems","survives","TLP",
           "SPEI_uniform","LDMC_g_g","Tribe", "SLA_adj_cm2_g", "SRL_m_g", "RDMC_g_g","RTD_g_cm3", "AvgDiam_mm",
           "neighbors_15", "neighbors_10","neighbors_20")
temp_point <- temp_point[,MyVar]

### Data outliers
par(mfrow=c(3,3))
hist(temp_point$LDMC_g_g)
hist(temp_point$TLP)
hist(temp_point$survives)
hist(temp_point$SPEI_uniform)
hist(temp_point$SLA_adj_cm2_g)
hist(temp_point$RDMC_g_g)
hist(temp_point$RTD_g_cm3)
hist(temp_point$AvgDiam_mm)
hist(temp_point$SRL_m_g)
#There don't seem to be any really crazy outliers. The values for LDMC_g_g and LOP are definitely skewed, and I think that is because this dataset is largely dominated by one species. But that is one reason why I will include a random intercept for species

###	Extra 0s (looking at previous histograms)
#It looks like the only variable with a lot of zeros is survival, which makes sense 
#(it is all 0sand 1s, so we expect lots of 0s).  Obvious hint that I'll want to use a binomial distribution

##	Collinearity
par(mfrow=c(1,1))
#for LOP
MyVar9 <-c("survives","TLP","SPEI_uniform")  
corrplot(cor(temp_point[,MyVar9], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#No correlation between covariates for LOP model

#for LDMC_g_g
MyVar10 <-c("survives","LDMC_g_g","SPEI_uniform")  
corrplot(cor(temp_point[,MyVar10], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#No significant correlation between covariates

#for SLA
MyVar15 <-c("survives","SLA_adj_cm2_g","SPEI_uniform")  
corrplot(cor(temp_point[,MyVar15], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#No significant correlation between covariates

#for RDMC
MyVar11 <-c("survives","RDMC_g_g","SPEI_uniform")  
corrplot(cor(temp_point[,MyVar11], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#No significant correlation between covariates

#for SRL
MyVar12 <-c("survives","SRL_m_g","SPEI_uniform")  
corrplot(cor(temp_point[,MyVar12], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#No significant correlation between covariates

#for RTD
MyVar13 <-c("survives","RTD_g_cm3","SPEI_uniform")  
corrplot(cor(temp_point[,MyVar13], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#No significant correlation between covariates

#for RDiam
MyVar14 <-c("survives","AvgDiam_mm","SPEI_uniform")  
corrplot(cor(temp_point[,MyVar14], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
#No significant correlation between covariates

### Variance Inflation Factors (VIF) for fixed effects
#For LDMC model
corvif(temp_point[,MyVar10])
#for TLP model
corvif(temp_point[,MyVar9])
#For RDMC model
corvif(temp_point[,MyVar11])
#For SRL model
corvif(temp_point[,MyVar12])
#For RTD model
corvif(temp_point[,MyVar13])
#For AvgDiam model
corvif(temp_point[,MyVar14])
#For SLA model
corvif(temp_point[,MyVar15])

#All of the VIFs are below 3 
  

#### Data exploration steps for growth data ####
## calculate growth for each polygon ####
CO_poly_all$logDiffArea <- (log(CO_poly_all$area_tplus1)) - (log(CO_poly_all$area_t))

## subset dataset for growth dataset
poly_temp_growth <- CO_poly_all
#variables to test
MyVar <- c("species","quad","year_t","area_t","logDiffArea","TLP",
           "SPEI_uniform","LDMC_g_g","Tribe", "SLA_adj_cm2_g", "SRL_m_g", "RDMC_g_g","RTD_g_cm3", "neighbor_area_5", "AvgDiam_mm", 
           "neighbor_area_15", "neighbor_area_10","neighbor_area_20")
poly_temp_growth <- poly_temp_growth[,MyVar]

## Data exploration for graminoids
### Data outliers

par(mfrow=c(3,4))
hist(poly_temp_growth$LDMC_g_g)
hist(poly_temp_growth$TLP)
hist(poly_temp_growth$logDiffArea)
hist(poly_temp_growth$SPEI_uniform)
hist(poly_temp_growth$SLA_adj_cm2_g)
hist(poly_temp_growth$RDMC_g_g)
hist(poly_temp_growth$SRL_m_g)
hist(poly_temp_growth$RTD_g_cm3)
hist(poly_temp_growth$AvgDiam_mm)
boxplot(poly_temp_growth$area,
        varwidth = TRUE,
        main = "Boxplot of area",
        ylab = "area", col=c("gray40", "gray60", "gray80"), data = poly_temp_growth, pch=16)	

###	Collinearity
par(mfrow=c(1,1))
#for TLP models
MyVar2 <- c("area_t","logDiffArea","TLP","SPEI_uniform")
corrplot(cor(poly_temp_growth[,MyVar2], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
# no substantial correlations between covariates

#for LDMC models
MyVar3 <- c("area_t","logDiffArea","LDMC_g_g","SPEI_uniform")
corrplot(cor(poly_temp_growth[,MyVar3], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
# no substantial correlations between covariates

#for SLA models
MyVar4 <- c("area_t","logDiffArea","SLA_adj_cm2_g","SPEI_uniform")
corrplot(cor(poly_temp_growth[,MyVar4], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
# no substantial correlations between covariates

#for RDMC models
MyVar5 <- c("area_t","logDiffArea","RDMC_g_g","SPEI_uniform")
corrplot(cor(poly_temp_growth[,MyVar5], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
# no substantial correlations between covariates

#for SRL models
MyVar6 <- c("area_t","logDiffArea","SRL_m_g","SPEI_uniform")
corrplot(cor(poly_temp_growth[,MyVar6], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
# no substantial correlations between covariates

#for RTD models
MyVar7 <- c("area_t","logDiffArea","RTD_g_cm3","SPEI_uniform")
corrplot(cor(poly_temp_growth[,MyVar7], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
# no substantial correlations between covariates

#for RDiam models
MyVar8 <- c("area_t","logDiffArea","AvgDiam_mm","SPEI_uniform")
corrplot(cor(poly_temp_growth[,MyVar8], use = "na.or.complete", method = "pearson"), method = "number", type = "lower")
# no substantial correlations between covariates

### Variance Inflation Factors (VIF) for fixed effects

#for TLP model
temp <- poly_temp_growth[,MyVar2] %>% 
  dplyr::filter(!is.na(TLP))
corvif(temp)
#for LDMC_g_g model
temp <- poly_temp_growth[,MyVar3] %>% 
  dplyr::filter(!is.na("LDMC_g_g"))
corvif(temp)
#for SLA model
temp <- poly_temp_growth[,MyVar4] %>% 
  dplyr::filter(!is.na(SLA_adj_cm2_g))
corvif(temp)
#for RDMC_g_g model
temp <- poly_temp_growth[,MyVar5] %>% 
  dplyr::filter(!is.na("RDMC_g_g"))
corvif(temp)
#for SRL model
temp <- poly_temp_growth[,MyVar6] %>% 
  dplyr::filter(!is.na("SRL_m_g"))
corvif(temp)
#for RTD model
temp <- poly_temp_growth[,MyVar7] %>% 
  dplyr::filter(!is.na("RTD_g_cm3"))
corvif(temp)
#for RDiam
temp <- poly_temp_growth[,MyVar8] %>% 
  dplyr::filter(!is.na("AvgDiam_mm"))
corvif(temp)

# All values are below 3, which is good

#### subset and scale data sets ####
#subset the point dataset to include only 500 of Sphaeralcea coccinea indivduals
#divide out the sphaeralcea data
CO_point_SPH <- CO_point_all[CO_point_all$species == "Sphaeralcea coccinea",]
#subset randomly for 360 observations (randomly select 15 observations per quadrat) (3 iterations)
CO_point_SPHfew_1 <- CO_point_SPH %>% 
  group_by(quad) %>% 
  sample_n(size = 15, replace = FALSE) %>% 
  as.data.frame()
CO_point_SPHfew_2 <- CO_point_SPH %>% 
  group_by(quad) %>% 
  sample_n(size = 15, replace = FALSE)%>% 
  as.data.frame()
CO_point_SPHfew_3 <- CO_point_SPH %>% 
  group_by(quad) %>% 
  sample_n(size = 15, replace = FALSE)%>% 
  as.data.frame()

#rest of points 
CO_point_rest <- CO_point_all[CO_point_all$species != "Sphaeralcea coccinea",]

#join subsetted points for sphaeralcea with rest of dataset 
CO_point_all_1 <- rbind(CO_point_SPHfew_1, CO_point_rest)
CO_point_all_2 <- rbind(CO_point_SPHfew_2, CO_point_rest)
CO_point_all_3 <- rbind(CO_point_SPHfew_3, CO_point_rest)
CO_point_all <- rbind(rbind(CO_point_SPHfew_3, CO_point_rest))

#### scaling and log-transforming variables####
#scale variables in polygon dataset
CO_poly_all$TLP_s <- as.numeric(scale(CO_poly_all$TLP))
CO_poly_all$LDMC_s <- as.numeric(scale(CO_poly_all$LDMC_g_g))
CO_poly_all$SPEI_s <- as.numeric(scale(CO_poly_all$SPEI_uniform))
CO_poly_all$neighbors_5_s <- as.numeric(scale(CO_poly_all$neighbor_area_5))
CO_poly_all$neighbors_10_s <- as.numeric(scale(CO_poly_all$neighbor_area_10))
CO_poly_all$neighbors_15_s <- as.numeric(scale(CO_poly_all$neighbor_area_15))
CO_poly_all$neighbors_20_s <- as.numeric(scale(CO_poly_all$neighbor_area_20))
CO_poly_all$SLA_s <- as.numeric(scale(CO_poly_all$SLA_adj_cm2_g))
CO_poly_all$RDMC_s <- as.numeric(scale(CO_poly_all$RDMC_g_g))
CO_poly_all$RTD_s <- as.numeric(scale(CO_poly_all$RTD_g_cm3))
CO_poly_all$RDiam_s <- as.numeric(scale(CO_poly_all$AvgDiam_mm))
CO_poly_all$SRL_s <- as.numeric(scale(CO_poly_all$SRL_best_m_g))
CO_poly_all$SPEI_uniform_s <- as.numeric(scale(CO_poly_all$SPEI_uniform))
CO_poly_all$precip_s <- as.numeric(scale(CO_poly_all$Ann.Sum.Precip))
#log transform area
CO_poly_all$area_s <- as.numeric((log(CO_poly_all$area_t)))
CO_poly_all$area_tplus1_s <- as.numeric(log(CO_poly_all$area_tplus1))


## scale variables in point dataset
CO_point_all$TLP_s <- as.numeric(scale(CO_point_all$TLP))
CO_point_all$LDMC_s <- as.numeric(scale(CO_point_all$LDMC_g_g))
CO_point_all$SPEI_s <- as.numeric(scale(CO_point_all$SPEI_uniform))
CO_point_all$SLA_s <- as.numeric(scale(CO_point_all$SLA_adj))
CO_point_all$neighbors_5_s <- as.numeric(scale(CO_point_all$neighbors_5))
CO_point_all$neighbors_10_s <- as.numeric(scale(CO_point_all$neighbors_10))
CO_point_all$neighbors_15_s <- as.numeric(scale(CO_point_all$neighbors_15))
CO_point_all$neighbors_20_s <- as.numeric(scale(CO_point_all$neighbors_20))
CO_point_all$RDMC_s <- as.numeric(scale(CO_point_all$RDMC_g_g))
CO_point_all$RTD_s <- as.numeric(scale(CO_point_all$RTD_g_cm3))
CO_point_all$RDiam_s <- as.numeric(scale(CO_point_all$AvgDiam_mm))
CO_point_all$SRL_s <- as.numeric(scale(CO_point_all$SRL_best_m_g))
CO_point_all$SPEI_uniform_s <- as.numeric(scale(CO_point_all$SPEI_uniform))
CO_point_all$precip_s <- as.numeric(scale(CO_point_all$Ann.Sum.Precip))

#### save workspace image for next script ####
rm(list = ls()[!(ls() %in% c('CO_point_all','CO_poly_all'))])

## for next script, need CO_point_all and CO_poly_all data.frames
#save as an .RData file 
path = "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper/scripts" #location where you'll put the environment data file
setwd(path)
save.image('script3_output.RData')
