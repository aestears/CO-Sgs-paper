#////////////////////////////////////
## set working directory
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis")
## Load files from "prep" file
# source("./SCRIPTS/2_SPEI_calculations_CO.R")
## Load packages
require(lattice)
require(tidyr)
require(dplyr)
require(lme4)
library(piecewiseSEM)
library(MuMIn)
require(visreg)
require(plotly)
source("~/Dropbox/Grad School/Courses/Fall 2018 Courses/Quantitative Analyses for Field Data/QuantitativeFieldData_2018/source/HighstatLibV8.r")

##load intermediate data files (demographic data with nearest neighbor and SPEI)
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files")
CO_point_all <- read.csv("./Intermediate Analysis Files/Point_Demo_SPEI_2_24_20.csv",stringsAsFactors = FALSE)
CO_poly_all <- read.csv("./Intermediate Analysis Files/Poly_Demo_SPEI_2_24_20.csv",stringsAsFactors =  FALSE)

#scale variables in polygon dataset
CO_poly_all$TLP_s <- as.numeric(scale(CO_poly_all$TLP))
CO_poly_all$LDMC_s <- as.numeric(scale(CO_poly_all$LDMC_g_g))
CO_poly_all$SPEI_s <- as.numeric(scale(CO_poly_all$SPEI_unique))
CO_poly_all$neighbors_10_s <- as.numeric(scale(CO_poly_all$neighbor_area_10))
CO_poly_all$neighbors_15_s <- as.numeric(scale(CO_poly_all$neighbor_area_15))
CO_poly_all$neighbors_20_s <- as.numeric(scale(CO_poly_all$neighbor_area_20))
CO_poly_all$SLA_s <- as.numeric(scale(CO_poly_all$SLA_adj_cm2_g))
CO_poly_all$Height_s <- as.numeric(scale(CO_poly_all$PlantHeight_mm))
CO_poly_all$RDMC_s <- as.numeric(scale(CO_poly_all$RDMC_g_g))
CO_poly_all$RTD_s <- as.numeric(scale(CO_poly_all$RTD_g_cm3))
#log transform area
CO_poly_all$area_s <- as.numeric((log(CO_poly_all$area_t)))
CO_poly_all$delta_area <- as.numeric((log(CO_poly_all$area_t)))
CO_poly_all$area_tplus1_s <- as.numeric(log(CO_poly_all$area_tplus1))
#scale variables in point dataset
CO_point_all$TLP_s <- as.numeric(scale(CO_point_all$TLP))
CO_point_all$LDMC_s <- as.numeric(scale(CO_point_all$LDMC_g_g))
CO_point_all$SPEI_s <- as.numeric(scale(CO_point_all$SPEI_unique))
CO_point_all$SLA_s <- as.numeric(scale(CO_point_all$SLA_adj))
CO_point_all$neighbors_10_s <- as.numeric(scale(CO_point_all$neighbors_10))
CO_point_all$neighbors_15_s <- as.numeric(scale(CO_point_all$neighbors_15))
CO_point_all$neighbors_20_s <- as.numeric(scale(CO_point_all$neighbors_20))
CO_point_all$Height_s <- as.numeric(scale(CO_point_all$PlantHeight_mm))
CO_point_all$RDMC_s <- as.numeric(scale(CO_point_all$RDMC_g_g))
CO_point_all$RTD_s <- as.numeric(scale(CO_point_all$RTD_g_cm3))


#//////////////////////////////////////
####Data exploration steps for survival data####
##Create a variable for phylogeny in each group (polygons and points)
#Data exploration steps
#trim out the variables we aren't using from the datset
poly_temp <- CO_poly_all
#variables to test
MyVar <- c("species","quad","year_t","area_t","survives_tplus1","TLP",
           "SPEI_unique","SPEI_unique_prev","LDMC_g_g","Tribe", "SLA_adj_cm2_g", "SRL_m_g", "RDMC_g_g","RTD_g_cm3",
           "neighbor_area_15", "neighbor_area_10","neighbor_area_20")
poly_temp <- poly_temp[,MyVar]
#remove values for which we do not have trait data
#poly_temp <- drop_na(poly_temp)

## Data exploration for graminoids
### Data outliers

par(mfrow=c(2,3))
hist(poly_temp$LDMC_g_g)
hist(poly_temp$TLP)
hist(poly_temp$survives)
hist(poly_temp$SPEI_unique)
boxplot(poly_temp$area,
varwidth = TRUE,
main = "Boxplot of area",
ylab = "LDMC_g_g", col=c("gray40", "gray60", "gray80"), data = poly_temp, pch=16)	

###	Extra 0s
#From the above histograms, it looks like the only variable with a lot of zeros is survival, which makes sense (it is all 0sand 1s, so we expect lots of 0s).  Obvious hint that I'll want to use a binomial distribution

###	Collinearity

#for TLP models
MyVar2 <- c("area_t","survives_tplus1","TLP","SPEI_unique")
pairs(poly_temp[,MyVar2], lower.panel = panel.cor)

#There is some corrrelation between survival and area, but not enough to worry about (0.3)

#for LDMC_g_g models
MyVar3 <- c("area_t","survives_tplus1","LDMC_g_g","SPEI_unique")
pairs(poly_temp[,MyVar3], lower.panel = panel.cor)

#There is some corrrelation between survival and area, but not enough to worry about (0.3)

### VARIANCE INFLATION FACTORS (VIF) for fixed effects

#for LOP model
corvif(poly_temp[,MyVar2])


#for LDMC_g_g model
corvif(poly_temp[,MyVar3])

# All values are below 3, which is good


###	Interactions 
#I want to include an interaction term between SPEI and trait (SPEI*TLP and SPEI*LDMC_g_g)
#Plot SPEI and LDMC_g_g against survival at high and low values of SPEI to see if there seems to be an interaction that makes sense to include (bottom 25% of SPEI vs. top 25% of SPEI)

 #It does look like there are different patterns of survival (interacting patterns) for species with different LOP at high vs. low SPEI

#For LDMC_g_g
par(mfrow=c(1,2))
plot(jitter(survives_tplus1)~jitter(LDMC_g_g),
data=poly_temp[poly_temp$SPEI_unique<quantile(poly_temp$SPEI_unique)[2],],
main="Bottom 25% of SPEI_values")
#abline(glm(survives_tplus1~LDMC_g_g, data=poly_temp[poly_temp$SPEI_unique<quantile(poly_temp$SPEI_unique)[2],], family=binomial(link="logit")))

plot(jitter(survives_tplus1)~jitter(LDMC_g_g),
data=poly_temp[poly_temp$SPEI_unique>quantile(poly_temp$SPEI_unique)[3],],
main="Top 25% of SPEI values")
#abline(glm(survives_tplus1~LDMC_g_g, data=poly_temp[poly_temp$SPEI_unique>quantile(poly_temp$SPEI_unique)[3],], family=binomial(link="logit")))

#There is not nearly as much of an interaction here with LDMC_g_g, so I'm not too sure if it makes sense to include one here?  I think it makes biological sense, but 
#it is hard to visually see any trends here.  But I definitely think there is a high enough sample size to include an interaction. 

###Standardize, scale, or leave covariates alone
#I want to scale the variables of SPEI, LOP, LDMC_g_g, and area so that they are on similar scales
poly_temp$TLP_s <- as.numeric(scale(poly_temp$TLP))
poly_temp$LDMC_s <- as.numeric(scale(poly_temp$LDMC_g_g))
poly_temp$SPEI_s <- as.numeric(scale(poly_temp$SPEI_unique))

#log transform area
poly_temp$area_s <- as.numeric(scale(log(poly_temp$area_t)))

par(mfrow=c(2,3))
hist(poly_temp$TLP_s)
hist(poly_temp$LDMC_s)
hist(poly_temp$SPEI_s)
hist(poly_temp$area_s)


###	Violation of homogeneity (before analysis)
#We already know that the response variable is heterogeneous (somewhat) with respect to year and quadrat, so I will include random effects for quadrat and year

### Data exploration steps for forbs
#trim the dataset to those variables you actually want 
temp_point <- CO_point_all
#variables to test
MyVar <- c("species","quad","year","stems","survives","TLP",
           "SPEI_unique","SPEI_unique_prev","LDMC_g_g","Tribe", "SLA_adj_cm2_g", "SRL_m_g", "RDMC_g_g","RTD_g_cm3",
           "neighbors_15", "neighbors_10","neighbors_20")
temp_point <- temp_point[,MyVar]
#remove values for which we do not have trait data
temp_point <- temp_point %>% 
  drop_na()

### Data outliers
par(mfrow=c(2,3))
hist(temp_point$LDMC_g_g)
hist(temp_point$TLP)
hist(temp_point$survives)
hist(temp_point$SPEI_unique)
boxplot(temp_point$stems,
        varwidth = TRUE,
        main = "Boxplot of stems",
        ylab = "LDMC_g_g", col=c("gray40", "gray60", "gray80"), data = temp_point, pch=16)
#There don't seem to be any really crazy outliers. The values for LDMC_g_g and LOP are definitely skewed, and I think that is because this dataset is largely dominated by one species. But that is one reason why I will include a random intercept for species

###	Extra 0s (looking at previous histograms)
#It looks like the only variable with a lot of zeros is survival, which makes sense 
#(it is all 0sand 1s, so we expect lots of 0s).  Obvious hint that I'll want to use a binomial distribution

##	Collinearity
#for LOP
MyVar4 <-c("survives","TLP","SPEI_unique")  
pairs(temp_point[,MyVar4], lower.panel = panel.cor)
#No correlation between covariates for LOP model

#for LDMC_g_g
MyVar5 <-c("survives","LDMC_g_g","SPEI_unique")  
pairs(temp_point[,MyVar5], lower.panel = panel.cor)
#No significant correlation between covariates

### Variance Inflation Factors (VIF) for fixed effects

#For LDMC_g_g model
#remove categorical variables (species and quad)
corvif(temp_point[,MyVar5])
#for LOP model
corvif(temp_point[,MyVar4])
#All of the VIFs look small enough! 
  
 ###Interactions 
#  I want to include an interaction term between SPEI and trait (SPEI*LOP and SPEI*LDMC_g_g)
#Plot SPEI and LDMC_g_g against survival at high and low values of SPEI to see if there seems to be an interaction that makes sense to include (bottom 25% of SPEI vs. top 25% of SPEI)

#For LOP
par(mfrow=c(1,2))
plot(jitter(survives)~jitter(TLP),
     data=temp_point[temp_point$SPEI_unique<quantile(temp_point$SPEI_unique)[2],],
     main="Bottom 25% of SPEI values")
abline(glm(survives~TLP,
           data=temp_point[temp_point$SPEI_unique<quantile(temp_point$SPEI_unique)[2],],
           family=binomial(link="logit")))
plot(jitter(survives)~jitter(TLP),
     data=temp_point[temp_point$SPEI_unique>quantile(temp_point$SPEI_unique)[3],],
     main="Top 25% of SPEI values")
abline(glm(survives~TLP,
           data=temp_point[temp_point$SPEI_unique>quantile(temp_point$SPEI_unique)[3],],
           family=binomial(link="logit")))

#It is still difficult to see patterns, especially since the data isn't uniform accross the LOP values. 

#For LDMC_g_g
par(mfrow=c(1,2))
plot(jitter(survives)~jitter(LDMC_g_g),
data=temp_point[temp_point$SPEI_unique<quantile(temp_point$SPEI_unique)[2],],
main="Bottom 25% of SPEI values")
abline(glm(survives~LDMC_g_g,
data=temp_point[temp_point$SPEI_unique<quantile(temp_point$SPEI_unique)[2],],
family=binomial(link="logit")))
plot(jitter(survives)~jitter(LDMC_g_g),
data=temp_point[temp_point$SPEI_unique>quantile(temp_point$SPEI_unique)[3],],
main="Top 25% of SPEI values")
abline(glm(survives~LDMC_g_g,
data=temp_point[temp_point$SPEI_unique>quantile(temp_point$SPEI_unique)[3],],
family=binomial(link="logit")))

#Again, it is hard to tell because the LDMC_g_g values are not uniformly distributed, but it looks like there are different relationships. 
###Standardize, scale, or leave covariates alone
#I want to scale the variables of SPEI, LOP, LDMC_g_g, and area so that they are on similar scales
temp_point$TLP_s <- as.numeric(scale(temp_point$TLP))
temp_point$LDMC_g_g_s <- as.numeric(scale(temp_point$LDMC_g_g))
temp_point$SPEI_s <- as.numeric(scale(temp_point$SPEI_unique))
temp_point$SLA_s <- as.numeric(scale(temp_point$SLA_adj))
#log transform stems

par(mfrow=c(2,3))
hist(temp_point$TLP_s)
hist(temp_point$LDMC_g_g_s)
hist(temp_point$SPEI_s)


#### Data exploration steps for growth data ####
poly_temp_growth <- CO_poly_all
#variables to test
MyVar <- c("species","quad","year_t","area_t","logDiffArea","TLP",
           "SPEI_unique","SPEI_unique_prev","LDMC_g_g","Tribe", "SLA_adj_cm2_g", "SRL_m_g", "RDMC_g_g","RTD_g_cm3", "neighbor_area_5",
           "neighbor_area_15", "neighbor_area_10","neighbor_area_20")
poly_temp_growth <- poly_temp_growth[,MyVar]

# remove values for which we don't have growth data
poly_temp_growth <- poly_temp_growth %>% 
  filter(!is.na(poly_temp_growth$logDiffArea))

which(is.na(poly_temp_growth))
## Data exploration for graminoids
### Data outliers

par(mfrow=c(2,3))
hist(poly_temp_growth$LDMC_g_g)
hist(poly_temp_growth$TLP)
hist(poly_temp_growth$logDiffArea)
hist(poly_temp_growth$SPEI_unique)
boxplot(poly_temp_growth$area,
        varwidth = TRUE,
        main = "Boxplot of area",
        ylab = "area", col=c("gray40", "gray60", "gray80"), data = poly_temp_growth, pch=16)	

###	Extra 0s
#From the above histograms, it looks like the only variable with a lot of zeros is survival, which makes sense (it is all 0sand 1s, so we expect lots of 0s).  Obvious hint that I'll want to use a binomial distribution

###	Collinearity

#for TLP models
MyVar2 <- c("area_t","logDiffArea","TLP","SPEI_unique")
pairs(poly_temp_growth[,MyVar2], lower.panel = panel.cor)

#There is some corrrelation between survival and area, but not enough to worry about (0.3)

#for LDMC_g_g models
MyVar3 <- c("area_t","logDiffArea","LDMC_g_g","SPEI_unique")
pairs(poly_temp_growth[,MyVar3], lower.panel = panel.cor)

#There is some corrrelation between survival and area, but not enough to worry about (0.3)

### VARIANCE INFLATION FACTORS (VIF) for fixed effects

#for LOP model
temp <- poly_temp_growth[,MyVar2] %>% 
  dplyr::filter(!is.na(TLP))
corvif(temp)


#for LDMC_g_g model
temp <- poly_temp_growth[,MyVar3] %>% 
  dplyr::filter(!is.na(LDMC_g_g))
corvif(temp)

# All values are below 3, which is good


###	Interactions 
#I want to include an interaction term between SPEI and trait (SPEI*TLP and SPEI*LDMC_g_g)
#Plot SPEI and LDMC_g_g against growth at high and low values of SPEI to see if there seems to be an interaction that makes sense to include (bottom 25% of SPEI vs. top 25% of SPEI)

#It does look like there are different patterns of growth (interacting patterns) for species with different LOP at high vs. low SPEI

#For LDMC_g_g
par(mfrow=c(1,2))
plot(jitter(logDiffArea)~jitter(LDMC_g_g),
     data=poly_temp_growth[poly_temp_growth$SPEI_unique<quantile(poly_temp_growth$SPEI_unique)[2],],
     main="Bottom 25% of SPEI_values")
#abline(glm(logDiffArea~LDMC_g_g, data=poly_temp_growth[poly_temp_growth$SPEI_unique<quantile(poly_temp_growth$SPEI_unique)[2],]))

plot(jitter(logDiffArea)~jitter(LDMC_g_g),
     data=poly_temp_growth[poly_temp_growth$SPEI_unique>quantile(poly_temp_growth$SPEI_unique)[3],],
     main="Top 25% of SPEI values")
#abline(glm(logDiffArea~LDMC_g_g,data=poly_temp_growth[poly_temp_growth$SPEI_unique>quantile(poly_temp_growth$SPEI_unique)[3],]))

#There is not nearly as much of an interaction here with LDMC_g_g, so I'm not too sure if it makes sense to include one here?  I think it makes biological sense, but 
#it is hard to visually see any trends here.  But I definitely think there is a high enough sample size to include an interaction. 
 

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



#scale variables in polygon dataset
CO_poly_all$TLP_s <- as.numeric(scale(CO_poly_all$TLP))
CO_poly_all$LDMC_s <- as.numeric(scale(CO_poly_all$LDMC_g_g))
CO_poly_all$SPEI_s <- as.numeric(scale(CO_poly_all$SPEI_unique))
CO_poly_all$neighbors_5_s <- as.numeric(scale(CO_poly_all$neighbor_area_5))
CO_poly_all$neighbors_10_s <- as.numeric(scale(CO_poly_all$neighbor_area_10))
CO_poly_all$neighbors_15_s <- as.numeric(scale(CO_poly_all$neighbor_area_15))
CO_poly_all$neighbors_20_s <- as.numeric(scale(CO_poly_all$neighbor_area_20))
CO_poly_all$SLA_s <- as.numeric(scale(CO_poly_all$SLA_adj_cm2_g))
CO_poly_all$Height_s <- as.numeric(scale(CO_poly_all$PlantHeight_mm))
CO_poly_all$RDMC_s <- as.numeric(scale(CO_poly_all$RDMC_g_g))
CO_poly_all$RTD_s <- as.numeric(scale(CO_poly_all$RTD_g_cm3))
CO_poly_all$year <- as.factor(CO_poly_all$year_t)
CO_poly_all$species <- as.factor(CO_poly_all$species)
CO_poly_all$quad <- as.factor(CO_poly_all$quad)
CO_poly_all$SRL_s <- as.numeric(scale(CO_poly_all$SRL_best_m_g))
CO_poly_all$RDiam_s <- as.numeric(scale(CO_poly_all$AvgDiam_mm))
CO_poly_all$survives_tplus1 <- as.factor(CO_poly_all$survives_tplus1)
CO_poly_all$nearEdge <- as.factor(CO_poly_all$nearEdge_t)
#log transform area
CO_poly_all$area_s <- as.numeric(scale(log(CO_poly_all$area_t)))

#scale variables in point dataset
#iteration 1
CO_point_all$TLP_s <- as.numeric(scale(CO_point_all$TLP))
CO_point_all$LDMC_s <- as.numeric(scale(CO_point_all$LDMC_g_g))
CO_point_all$SPEI_s <- as.numeric(scale(CO_point_all$SPEI_unique))
CO_point_all$SLA_s <- as.numeric(scale(CO_point_all$SLA_adj))
CO_point_all$neighbors_10_s <- as.numeric(scale(CO_point_all$neighbors_10))
CO_point_all$neighbors_15_s <- as.numeric(scale(CO_point_all$neighbors_15))
CO_point_all$neighbors_20_s <- as.numeric(scale(CO_point_all$neighbors_20))
CO_point_all$Height_s <- as.numeric(scale(CO_point_all$PlantHeight_mm))
CO_point_all$RDMC_s <- as.numeric(scale(CO_point_all$RDMC_g_g))
CO_point_all$RTD_s <- as.numeric(scale(CO_point_all$RTD_g_cm3))
CO_point_all$year <- as.factor(CO_point_all$year)
CO_point_all$species <- as.factor(CO_point_all$species)
CO_point_all$quad <- as.factor(CO_point_all$quad)
CO_point_all$SRL_s <- as.numeric(scale(CO_point_all$SRL_best_m_g))
CO_point_all$RDiam_s <- as.numeric(scale(CO_point_all$AvgDiam_mm))
CO_point_all$nearEdge <- as.factor(CO_point_all$nearEdge)


