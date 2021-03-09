#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Analysis 
# Script 5 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

##### Load Packages #####
require(tidyverse) #v1.3.0
require(fields) #v11.6
require(lme4) #v1.1-26
require(MuMIn) #v1.43.17
require(lmerTest) #v3.1-3
require(stargazer) #v5.2.2
require(glmmTMB) #v1.0.2.1
require(sjPlot) #v2.8.7
require(effects) #v4.2-0
require(ggeffects) #v1.1.3.1

## clear workspace ##
rm(list=ls())

##### Load Data Files #####
## set working directory
datWD <- c("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper") #set path for the location of the environment image of script 3 output
setwd(datWD)
#load data from previous 
load("./scripts/script3_output.RData")

#### ensure that the structure of the variables is correct ####
CO_poly_all <- CO_poly_all %>% 
  dplyr::select(species, quad, year_t, area_t, survives_tplus1, nearEdge_t, area_tplus1, neighbors_5_s, neighbors_10_s, neighbors_15_s, neighbors_20_s, area_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform_s, SPEI_uniform, SPEI_unique, precip_s, logDiffArea, area_tplus1_s) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year_t), nearEdge_t = as.factor(nearEdge_t))

CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform_s, SPEI_uniform, SPEI_unique, precip_s) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year), survives_tplus1 = as.integer(survives), nearEdge_t = as.integer(nearEdge)) 
  
#### testing viability of different neighborhood distance radii####
#simple model using TLP for graminoids 
mNeigh5 <- glmer(survives_tplus1 ~ area_s + neighbors_5_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh10 <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh15 <- glmer(survives_tplus1 ~ area_s + neighbors_15_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh20 <- glmer(survives_tplus1 ~ area_s + neighbors_20_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#compare the coefficients for the different radii
NeCoeff <- data.frame(Radius = c(5,10,15,20), Coeff = c(
  coeffs(mNeigh5)[which(names(coeffs(mNeigh5))=="neighbors_5_s")],
  coeffs(mNeigh10)[which(names(coeffs(mNeigh10))=="neighbors_10_s")],
  coeffs(mNeigh15)[which(names(coeffs(mNeigh15))=="neighbors_15_s")],
  coeffs(mNeigh20)[which(names(coeffs(mNeigh20))=="neighbors_20_s")]
  ))
#plot fixed effect coefficients for each neighborhood radius model
ggplot(data = NeCoeff) +
  geom_point(aes(Radius, Coeff)) +
  xlab("Neighborhood Radius From Focal Indivdiual (cm)") +
  ylab("Model Coefficient") +
  theme_classic()
#the coefficient for a radius of 10cm was the 'largest' (most negative), so use 10_cm

#####Graminoid Survival Models #####
### rename polygon dataset
CO_grams <- CO_poly_all

### TLP graminoid model ###
#global model
CO_poly_TLP <- CO_grams %>% 
  filter(!is.na(TLP_s))

m1_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m1_grams)
rsquared_m1_grams <- piecewiseSEM::rsquared(m1_grams)

#don't have to calculate overdispersion--not possible for bernoulli binomial variables
# https://stats.stackexchange.com/questions/206007/can-there-be-overdispersion-in-a-logistic-regression-model-where-each-observatio

#calculate R2 
rsquaredm1 <- piecewiseSEM::rsquared(m1_grams)


### LDMC graminoid model ###
CO_poly_LDMC <- CO_grams %>% 
  filter(!is.na(LDMC_s))

m2_grams <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  area_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(m2_grams)

#calculate the R2 
rsquaredm2 <- piecewiseSEM::rsquared(m2_grams)

### SLA graminoid model ###
CO_poly_SLA <- CO_grams %>% 
  filter(is.na(SLA_s)==FALSE)

m5 <- glmer(survives_tplus1 ~ area_s + SPEI_s * SLA_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m5)

#calculate the R2 
rsquaredm5 <- piecewiseSEM::rsquared(m5)

### RDMC graminoid model ###
CO_poly_RDMC <-
  CO_grams %>% 
  filter(is.na(RDMC_s)==FALSE)

m9 <- glmer(survives_tplus1 ~ area_s + SPEI_s * RDMC_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m9)

#calculate the R2 
rsquaredm9 <- piecewiseSEM::rsquared(m9)

### RTD graminoid model ###
CO_poly_RTD <-
  CO_grams %>% 
  filter(is.na(RTD_s)==FALSE)

m10 <- glmer(survives_tplus1 ~ area_s + SPEI_s * RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
# removed random slope for area, since the model didn't converge
summary(m10)
#calculate the R2 
rsquaredm10 <- piecewiseSEM::rsquared(m10)


### SRL graminoid model ###
CO_poly_SRL <-
  CO_grams %>% 
  filter(is.na(SRL_s)==FALSE)

m13 <- glmer(survives_tplus1 ~ area_s + SPEI_s * SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
#removed random slope for area, since the model doesn't converge with it

#calculate the R2 
rsquaredm13 <- piecewiseSEM::rsquared(m13)

### RDiam graminoid model ###
CO_poly_Diam <-
  CO_grams %>% 
  filter(is.na(RDiam_s)==FALSE)

m14 <- glmer(survives_tplus1 ~ area_s + SPEI_s * RDiam_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) , data = CO_poly_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#calculate the R2 
rsquaredm14 <- piecewiseSEM::rsquared(m14)

#### Point Survival Modelcs ####
### TLP point model ###
options(na.action = na.omit)
CO_point_TLP <- CO_point_all %>% 
  filter(!is.na(TLP_s))
m3 <- glmer(survives_tplus1 ~ SPEI_s*TLP_s + neighbors_10_s + nearEdge_t 
              + (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
 summary(m3)

#/////////////////////////////////

### LDMC point model ###
#for LDMC
CO_point_LDMC <- CO_point_all %>% 
  filter(!is.na(LDMC_s))

m4 <- glmer(survives_tplus1 ~ SPEI_s*LDMC_s + neighbors_10_s + nearEdge_t + 
               (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(m4)


##### Test forb survival models for non-water related traits####

#SLA for forbs
CO_point_SLA <- CO_point_all %>% 
  filter(is.na(SLA_s)==FALSE)
m6 <- glmer(survives_tplus1 ~ SPEI_s*SLA_s + neighbors_10_s + nearEdge_t  + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m6)

#### test forb survival root traits ####

#rdmc for forbs
CO_point_RDMC <-
  CO_point_all %>% 
  filter(is.na(RDMC_s)==FALSE)

m11 <- glmer(survives_tplus1 ~ SPEI_s*RDMC_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m11)

#rtd for forbs
CO_point_RTD <-
  CO_point_all %>% 
  filter(is.na(RTD_s)==FALSE)

m12 <- glmer(survives_tplus1 ~ SPEI_s*RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m12)

#SRL for forbs
CO_point_SRL <-
  CO_point_all %>% 
  filter(is.na(SRL_s)==FALSE)

m15 <- glmer(survives_tplus1 ~ SPEI_s*SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#root diameter for forbs
CO_point_Diam <-
  CO_point_all %>% 
  filter(is.na(RDiam_s)==FALSE)

m16 <- glmer(survives_tplus1 ~ SPEI_s*RDiam_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))



rsquared5 <- piecewiseSEM::rsquared(m5)

rsquared9 <- piecewiseSEM::rsquared(m9)

rsquared10 <- piecewiseSEM::rsquared(m10)

rsquared13 <- piecewiseSEM::rsquared(m13)

rsquared14 <- piecewiseSEM::rsquared(m14)

rsquared11 <- piecewiseSEM::rsquared(m11)

rsquared12 <- piecewiseSEM::rsquared(m12)

rsquared6 <- piecewiseSEM::rsquared(m6)

rsquared15 <- piecewiseSEM::rsquared(m15)

rsquared16 <- piecewiseSEM::rsquared(m16)

#### fit survival models w/out traits ####
m1_NO <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) ,
                  data = (CO_poly_TLP), family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

m2_NO <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) ,
               data = CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

m3_NO <- glmer(survives_tplus1 ~ SPEI_s  + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_TLP, family = binomial(link = logit),control=glmerControl(optimizer="bobyqa"))

m4_NO <- glmer(survives_tplus1 ~  neighbors_10_s +  SPEI_s + nearEdge_t +
                 (1|species) + (1|quad) + (1|year_t), data=CO_point_LDMC, 
               family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

m5_NO <- glmer(survives_tplus1 ~ area_s + SPEI_s  + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m5_NO)

m6_NO <-  glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m6_NO)

m9_NO <- glmer(survives_tplus1 ~ area_s + SPEI_s  + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m9_NO)

m10_NO <- glmer(survives_tplus1 ~ area_s + SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m10_NO)

m13_NO <- glmer(survives_tplus1 ~ area_s + SPEI_s  + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

m14_NO <- glmer(survives_tplus1 ~ area_s + SPEI_s  + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) , data = CO_poly_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))


m11_NO <- glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m11_NO)


m12_NO <- glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m12)


m15_NO <- glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))


m16_NO <- glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#### compare survival models w/ traits to those without traits ####
#For polygons
diff(AIC(m1_grams,m1_NO)$AIC) #TLP
diff(AIC(m2_grams, m2_NO)$AIC) #LDMC
diff(AIC(m5, m5_NO)$AIC) #SLA
diff(AIC(m9, m9_NO)$AIC) #RDMC
diff(AIC(m10, m10_NO)$AIC) #RTD
diff(AIC(m13, m13_NO)$AIC) #SRL
diff(AIC(m14, m14_NO)$AIC) #RDiam

#for forbs
diff(AIC(m3, m3_NO)$AIC) #TLP
diff(AIC(m4, m4_NO)$AIC) #LDMC
diff(AIC(m6, m6_NO)$AIC) #SLA
diff(AIC(m11, m11_NO)$AIC) #RDMC
diff(AIC(m12, m12_NO)$AIC) #RTD
diff(AIC(m15, m15_NO)$AIC) #SRL
diff(AIC(m16, m16_NO)$AIC) #RDiam


#### models w/ growth as response variable #### 
# (only for graminoids, no size metric for forbs)
# using size in year t+1 as response variable, w/ a random slope for size in year t

## Graminoid Models ##
#subset the datasets to only include observations that have growth data (for those plants that survived)
CO_poly_growth <- CO_grams %>% 
  filter(!is.na(CO_grams$logDiffArea))

## TLP model
#global model
options(na.action = "na.omit")

CO_grow_TLP <-
  CO_poly_growth %>% 
  filter(is.na(TLP_s)==FALSE)

mGrowTLP<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

#LDMC model
CO_grow_LDMC<-
  CO_poly_growth %>% 
  filter(is.na(LDMC_s)==FALSE)
mGrowLDMC <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC , control=lmerControl(optimizer="bobyqa"))

CO_grow_SLA <-
  CO_poly_growth %>% 
  filter(is.na(SLA_s)==FALSE)
mGrowSLA <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s * SLA_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lmerControl(optimizer="bobyqa"))

CO_grow_RDMC <-
  CO_poly_growth %>% 
  filter(is.na(RDMC_s)==FALSE)
mGrowRDMC <- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s * RDMC_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC , control=lmerControl(optimizer="bobyqa"))

CO_grow_SRL <-
  CO_poly_growth %>% 
  filter(is.na(SRL_s)==FALSE)
mGrowSRL <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s * SRL_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

CO_grow_RTD<-
  CO_poly_growth %>% 
  filter(is.na(RTD_s)==FALSE)
mGrowRTD  <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s * RTD_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

CO_grow_RDiam<-
  CO_poly_growth %>% 
  filter(is.na(RDiam_s)==FALSE)
mGrowRDiam  <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s * RDiam_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))


#### testing different growth response variables ####
## log-transformed area in year t: area_s
## log-transformed area in year t+1: area_tplus1_s

#still using logDiffArea as response, but including a random slope for area_t
mGrow_size <- lme4::lmer(logDiffArea ~ area_s +neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

## using area_t+1 as the response variable
mSize_TLP <- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

mSize_LDMC <- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))
## models w/out traits 
mSize_TLP_NO <- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

mSize_LDMC_NO <- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

## see if including traits improves the models
diff(AIC(mSize_TLP, mSize_TLP_NO)$AIC) #TLP
diff(AIC(mSize_LDMC, mSize_LDMC_NO)$AIC) #LDMC
# including the traits makes the models worse :-( :-( :-( )))


#### make growth models without traits for comparison ####
#fit models

mGrowTLP_NO <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s  + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowLDMC_NO <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s  + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSLA_NO <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowRDMC_NO <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s  + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSRL_NO <- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

mGrowRTD_NO  <- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

mGrowRDiam_NO  <- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))

#compare AIC of models with and without traits-10

#### table of model results ####
#for graminoidsurvival  models
require(stargazer)
stargazer(m1_grams, m2_grams, m5, m9, m10, m13, m14)
sjPlot::tab_model(m1_grams, m2_grams, m5, m9, m10, m13, m14, show.se = TRUE)

#for forb survival models
stargazer(m3, m4, m6, m11, m12, m15, m16)
sjPlot::tab_model(m3, m4, m6, m11, m12, m15, m16, show.se = TRUE)
diff(AIC(m3, m3_NO)$AIC) #TLP
diff(AIC(m4, m4_NO)$AIC) #LDMC
diff(AIC(m6, m6_NO)$AIC) #SLA
diff(AIC(m11, m11_NO)$AIC) #RDMC
diff(AIC(m12, m12_NO)$AIC) #RTD
diff(AIC(m15, m15_NO)$AIC) #SRL
diff(AIC(m16, m16_NO)$AIC) #RDiam

#for graminoid growth models
stargazer(mGrowTLP, mGrowLDMC, mGrowSLA, mGrowRDMC, mGrowRTD, mGrowSRL, mGrowRDiam)
sjPlot::tab_model(mGrowTLP, mGrowLDMC, mGrowSLA, mGrowRDMC, mGrowRTD, mGrowSRL, mGrowRDiam)

#### Get Correlation TLP and LDMC for polygons (graminoids) ####
# "traits_Not_dups" is the data.frame with the species-level trait averages
# make sure that it is subset for the species we used in our analysis
polySpp <- as.character(unique(CO_grams$species))
pointSpp <- as.character(unique(CO_point_all$species))

#get trait data
datWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files" #set working directory for location of flowering time data file
setwd(datWD)
CO_traits <- read.csv("./CO_mean_traits.csv", stringsAsFactors = FALSE)

#l
#get trait correlations for all species used in the models
traits_modSpp <- CO_traits[CO_traits$species %in% c(polySpp, pointSpp),]

mComp <- lm(data = traits_modSpp, TLP ~ LDMC_g_g)

plot(x = traits_modSpp$LDMC_g_g, 
     y = traits_modSpp$TLP)
abline(reg = mComp)

#traits of interest
traits <- c("TLP", "AvgDiam_mm", "RTD_g_cm3", "RDMC_g_g", "SRL_best_m_g", "LDMC_g_g", "SLA_adj_cm2_g")
pairs(traits_modSpp[,traits], lower.panel = panel.cor)
allSpp_corMatrix <- cor(traits_modSpp[,traits], use="complete.obs")
stargazer(allSpp_corMatrix)

#correlation matrix for traits in polygon analysis
polySpp_corMatrix <- cor(CO_traits[CO_traits$species %in% polySpp,traits], use="complete.obs")

#correlation matrix for traits in point analysis
pointSpp_corMatrix <- cor(CO_traits[CO_traits$species %in% pointSpp,traits], use="complete.obs")


#### test models w/ uniform SPEI ####
#models w/ species level SPEI
summary(m1_grams)
summary(m2_grams)
summary(mGrowTLP)
summary(mGrowLDMC)

m1_SPEI <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_uniform_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

m2_SPEI <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_uniform_s * LDMC_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mGrowTLP_SPEI <- lme4::lmer(logDiffArea ~ neighbors_10_s + SPEI_uniform_s * TLP_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

mGrowLDMC_SPEI <- lme4::lmer(logDiffArea ~ neighbors_10_s + SPEI_uniform_s * LDMC_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC , control=lmerControl(optimizer="bobyqa"))

#compare models with different SPEI

plot(predictorEffect(mod = m1_grams, predictor = "TLP_s", response = TRUE))
plot(predictorEffect(mod = m1_SPEI, predictor = "TLP_s", response = TRUE))
visreg::visreg2d(m1_grams, xvar = "TLP_s", yvar =  "SPEI_s", scale = "response", plot.type = "persp")
visreg::visreg2d(m1_SPEI, xvar = "TLP_s", yvar =  "SPEI_uniform_s", scale = "response", plot.type = "persp")

visreg::visreg2d(m2_grams, xvar = "LDMC_s", yvar =  "SPEI_s", scale = "response", plot.type = "persp")
visreg::visreg2d(m2_SPEI, xvar = "LDMC_s", yvar =  "SPEI_uniform_s", scale = "response", plot.type = "persp")

visreg::visreg2d(mGrowTLP, xvar = "TLP_s", yvar =  "SPEI_s", scale = "response", plot.type = "persp")
visreg::visreg2d(mGrow_size, xvar = "TLP_s", yvar =  "SPEI_s", scale = "response", plot.type = "persp")

#### compare models using SPEI to models using precip ####
m1_precip <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + precip_s* TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

m2_precip <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + precip_s * LDMC_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mGrowTLP_precip <- lme4::lmer(logDiffArea ~ neighbors_10_s + precip_s * TLP_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

mGrowLDMC_precip <- lme4::lmer(logDiffArea ~ neighbors_10_s + precip_s * LDMC_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC , control=lmerControl(optimizer="bobyqa"))

#### figures comparing precip predictor variables ####
## Figures for comparing unique to uniform SPEI (TLP and LDMC only)
#model objects (unique SPEI): m1_grams, m2_grams, mGrow_TLP, mGrow_LDMC
#model objects (uniform SPEI): m1_SPEI, m2_SPEI, mGrowTLP_SPEI, mGrowLDMC_SPEI 

## Make figure of unique SPEI of TLP and LDMC survival
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI<- mean(CO_poly_TLP$SPEI_s)
sdSPEI <- sd(CO_poly_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP
TLP_vals <- seq(min(CO_poly_TLP$TLP_s, na.rm = TRUE), max(CO_poly_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_surv_dat <- ggpredict(m1_grams, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC
LDMC_vals <- seq(min(CO_poly_LDMC$LDMC_s, na.rm = TRUE), max(CO_poly_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_surv_dat <- ggpredict(m2_grams, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
UniqueSurvDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_surv_dat$x, Surv = TLP_surv_dat$predicted, CI_low = TLP_surv_dat$conf.low, CI_high = TLP_surv_dat$conf.high, SPEI = TLP_surv_dat$group)

UniqueSurvDat <- rbind(UniqueSurvDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_surv_dat$x, Surv = LDMC_surv_dat$predicted, CI_low = LDMC_surv_dat$conf.low, CI_high = LDMC_surv_dat$conf.high, SPEI = LDMC_surv_dat$group))

#make data for rug plot
RugDat_surv <-  data.frame(rug = CO_poly_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_surv <- rbind(RugDat_surv, data.frame(rug = CO_poly_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
UniqueSPEI_Surv <- ggplot(data = UniqueSurvDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = Surv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_surv) +
  labs(title = "Unique SPEI Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Graminoid Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))

# Make figure of unique SPEI of TLP and LDMC growth
meanSPEI<- mean(CO_grow_TLP$SPEI_s)
sdSPEI <- sd(CO_grow_TLP$SPEI_s)

#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP
TLP_vals <- seq(min(CO_grow_TLP$TLP_s, na.rm = TRUE), max(CO_grow_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_grow_dat <- ggpredict(mGrowTLP, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC
LDMC_vals <- seq(min(CO_poly_LDMC$LDMC_s, na.rm = TRUE), max(CO_poly_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_grow_dat <- ggpredict(mGrowLDMC, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
UniqueGrowDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_grow_dat$x, grow = TLP_grow_dat$predicted, CI_low = TLP_grow_dat$conf.low, CI_high = TLP_grow_dat$conf.high, SPEI = TLP_grow_dat$group)

UniqueGrowDat <- rbind(UniqueGrowDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_grow_dat$x, grow = LDMC_grow_dat$predicted, CI_low = LDMC_grow_dat$conf.low, CI_high = LDMC_grow_dat$conf.high, SPEI = LDMC_grow_dat$group))

#make data for rug plot
RugDat_grow <-  data.frame(rug = CO_grow_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_grow <- rbind(RugDat_grow, data.frame(rug = CO_poly_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
UniqueSPEI_Grow <- ggplot(data = UniqueGrowDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = grow, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_grow) +
  labs(title = "Unique SPEI Graminoid Growth") +
  xlab(NULL) +
  ylab(expression("Graminoid Growth: log" ~ bgroup("(",frac(size[year_t+1],size[year_t]),")"))) +
  #scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"))+
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))

##Figures showing response of graminoid survival and growth using species-level SPEI

plot_grid(UniqueSPEI_Surv, UniqueSPEI_Grow, ncol = 2, align = "h", axis = "tb", rel_widths = c(.9,1))

##Figures for uniform SPEI
#model objects (unique SPEI): m1_grams, m2_grams, mGrow_TLP, mGrow_LDMC
#model objects (uniform SPEI): m1_SPEI, m2_SPEI, mGrowTLP_SPEI, mGrowLDMC_SPEI 

## Make figure of unique SPEI of TLP and LDMC survival
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI<- mean(CO_poly_TLP$SPEI_s)
sdSPEI <- sd(CO_poly_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP
TLP_vals <- seq(min(CO_poly_TLP$TLP_s, na.rm = TRUE), max(CO_poly_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_surv_dat <- ggpredict(m1_SPEI, terms = c("TLP_s[TLP_vals]", "SPEI_uniform_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC
LDMC_vals <- seq(min(CO_poly_LDMC$LDMC_s, na.rm = TRUE), max(CO_poly_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_surv_dat <- ggpredict(m2_SPEI, terms = c("LDMC_s[LDMC_vals]", "SPEI_uniform_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
UniformSurvDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_surv_dat$x, Surv = TLP_surv_dat$predicted, CI_low = TLP_surv_dat$conf.low, CI_high = TLP_surv_dat$conf.high, SPEI = TLP_surv_dat$group)

UniformSurvDat <- rbind(UniformSurvDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_surv_dat$x, Surv = LDMC_surv_dat$predicted, CI_low = LDMC_surv_dat$conf.low, CI_high = LDMC_surv_dat$conf.high, SPEI = LDMC_surv_dat$group))

#make data for rug plot
RugDat_surv <-  data.frame(rug = CO_poly_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_surv <- rbind(RugDat_surv, data.frame(rug = CO_poly_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
UniformSPEI_Surv <- ggplot(data = UniformSurvDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = Surv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_surv) +
  labs(title = "Uniform SPEI Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Graminoid Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"))  +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))

## Make figure of unique SPEI of TLP and LDMC growth
meanSPEI<- mean(CO_grow_TLP$SPEI_s)
sdSPEI <- sd(CO_grow_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP
TLP_vals <- seq(min(CO_grow_TLP$TLP_s, na.rm = TRUE), max(CO_grow_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_grow_dat <- ggpredict(mGrowTLP_SPEI, terms = c("TLP_s[TLP_vals]", "SPEI_uniform_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC
LDMC_vals <- seq(min(CO_poly_LDMC$LDMC_s, na.rm = TRUE), max(CO_poly_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_grow_dat <- ggpredict(mGrowLDMC_SPEI, terms = c("LDMC_s[LDMC_vals]", "SPEI_uniform_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
UniformGrowDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_grow_dat$x, grow = TLP_grow_dat$predicted, CI_low = TLP_grow_dat$conf.low, CI_high = TLP_grow_dat$conf.high, SPEI = TLP_grow_dat$group)

UniformGrowDat <- rbind(UniformGrowDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_grow_dat$x, grow = LDMC_grow_dat$predicted, CI_low = LDMC_grow_dat$conf.low, CI_high = LDMC_grow_dat$conf.high, SPEI = LDMC_grow_dat$group))

#make data for rug plot
RugDat_grow <-  data.frame(rug = CO_grow_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_grow <- rbind(RugDat_grow, data.frame(rug = CO_poly_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
UniformSPEI_Grow <- ggplot(data = UniformGrowDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = grow, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_grow) +
  labs(title = "Uniform SPEI Graminoid Growth") +
  xlab(NULL) +
  ylab(expression("Graminoid Growth: log" ~ bgroup("(",frac(size[year_t+1],size[year_t]),")"))) +
  #scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))


##Figures showing response of graminoid survival and growth using uniform SPEI

plot_grid(UniformSPEI_Surv, UniformSPEI_Grow, ncol = 2, align = "h", axis = "tb", rel_widths = c(.9,1))


## Figures for models using precip instead of SPEI
## Make figure of unique SPEI of TLP and LDMC survival
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI<- mean(CO_poly_TLP$SPEI_s)
sdSPEI <- sd(CO_poly_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP
TLP_vals <- seq(min(CO_poly_TLP$TLP_s, na.rm = TRUE), max(CO_poly_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_surv_dat <- ggpredict(m1_precip, terms = c("TLP_s[TLP_vals]", "precip_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC
LDMC_vals <- seq(min(CO_poly_LDMC$LDMC_s, na.rm = TRUE), max(CO_poly_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_surv_dat <- ggpredict(m2_precip, terms = c("LDMC_s[LDMC_vals]", "precip_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
PrecipSurvDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_surv_dat$x, Surv = TLP_surv_dat$predicted, CI_low = TLP_surv_dat$conf.low, CI_high = TLP_surv_dat$conf.high, SPEI = TLP_surv_dat$group)

PrecipSurvDat <- rbind(PrecipSurvDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_surv_dat$x, Surv = LDMC_surv_dat$predicted, CI_low = LDMC_surv_dat$conf.low, CI_high = LDMC_surv_dat$conf.high, SPEI = LDMC_surv_dat$group))

#make data for rug plot
RugDat_surv <-  data.frame(rug = CO_poly_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_surv <- rbind(RugDat_surv, data.frame(rug = CO_poly_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
Precip_Surv <- ggplot(data = PrecipSurvDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = Surv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_surv) +
  labs(title = "Precip Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Graminoid Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))

## Make figure of unique SPEI of TLP and LDMC growth
meanSPEI<- mean(CO_grow_TLP$SPEI_s)
sdSPEI <- sd(CO_grow_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP
TLP_vals <- seq(min(CO_grow_TLP$TLP_s, na.rm = TRUE), max(CO_grow_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_grow_dat <- ggpredict(mGrowTLP_precip, terms = c("TLP_s[TLP_vals]", "precip_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC
LDMC_vals <- seq(min(CO_poly_LDMC$LDMC_s, na.rm = TRUE), max(CO_poly_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_grow_dat <- ggpredict(mGrowLDMC_precip, terms = c("LDMC_s[LDMC_vals]", "precip_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
PrecipGrowDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_grow_dat$x, grow = TLP_grow_dat$predicted, CI_low = TLP_grow_dat$conf.low, CI_high = TLP_grow_dat$conf.high, SPEI = TLP_grow_dat$group)

PrecipGrowDat <- rbind(PrecipGrowDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_grow_dat$x, grow = LDMC_grow_dat$predicted, CI_low = LDMC_grow_dat$conf.low, CI_high = LDMC_grow_dat$conf.high, SPEI = LDMC_grow_dat$group))

#make data for rug plot
RugDat_grow <-  data.frame(rug = CO_grow_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_grow <- rbind(RugDat_grow, data.frame(rug = CO_poly_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
Precip_Grow <- ggplot(data = PrecipGrowDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = grow, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_grow) +
  labs(title = "Precip Graminoid Growth") +
  xlab(NULL) +
  ylab(expression("Graminoid Growth: log" ~ bgroup("(",frac(size[year_t+1],size[year_t]),")"))) +
  #scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))

##Figures showing response of graminoid survival and growth using uniform SPEI
plot_grid(Precip_Surv, Precip_Grow, ncol = 2, align = "h", axis = "tb", rel_widths = c(.9,1))

##table of model results
stargazer(m1_grams, m2_grams, mGrowTLP, mGrowLDMC, type = "html")

#### Make figure comparing survival to growth for TLP and LDMC of graminoids using area_tplus1 as response variable ####
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI<- mean(CO_poly_TLP$SPEI_s)
sdSPEI <- sd(CO_poly_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP
TLP_vals <- seq(min(CO_poly_TLP$TLP_s, na.rm = TRUE), max(CO_poly_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_surv_dat <- ggpredict(m1_grams, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC
LDMC_vals <- seq(min(CO_poly_LDMC$LDMC_s, na.rm = TRUE), max(CO_poly_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_surv_dat <- ggpredict(m2_grams, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
UniqueSurvDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_surv_dat$x, Surv = TLP_surv_dat$predicted, CI_low = TLP_surv_dat$conf.low, CI_high = TLP_surv_dat$conf.high, SPEI = TLP_surv_dat$group)

UniqueSurvDat <- rbind(UniqueSurvDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_surv_dat$x, Surv = LDMC_surv_dat$predicted, CI_low = LDMC_surv_dat$conf.low, CI_high = LDMC_surv_dat$conf.high, SPEI = LDMC_surv_dat$group))

#make data for rug plot
RugDat_surv <-  data.frame(rug = CO_poly_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_surv <- rbind(RugDat_surv, data.frame(rug = CO_poly_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
UniqueSPEI_Surv <- ggplot(data = UniqueSurvDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = Surv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_surv) +
  labs(title = "Unique SPEI Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Graminoid Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))

## make figures for growth data
meanSPEI<- mean(CO_grow_TLP$SPEI_s)
sdSPEI <- sd(CO_grow_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)
#for TLP
TLP_vals_g <- seq(min(CO_grow_TLP$TLP_s, na.rm = TRUE), max(CO_grow_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_grow_dat <- ggpredict(mSize_TLP, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC
LDMC_vals <- seq(min(CO_grow_LDMC$LDMC_s, na.rm = TRUE), max(CO_grow_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_grow_dat <- ggpredict(mSize_LDMC, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
UniqueGrowDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_grow_dat$x, grow = TLP_grow_dat$predicted, CI_low = TLP_grow_dat$conf.low, CI_high = TLP_grow_dat$conf.high, SPEI = TLP_grow_dat$group)

UniqueGrowDat <- rbind(UniqueGrowDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_grow_dat$x, grow = LDMC_grow_dat$predicted, CI_low = LDMC_grow_dat$conf.low, CI_high = LDMC_grow_dat$conf.high, SPEI = LDMC_grow_dat$group))

#make data for rug plot
RugDat_grow <-  data.frame(rug = CO_grow_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_grow <- rbind(RugDat_surv, data.frame(rug = CO_grow_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
UniqueSPEI_Grow <- ggplot(data = UniqueGrowDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = grow, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_grow) +
  labs(title = "Graminoid Size") +
  xlab(NULL) +
  ylab("Size in year t+1") +
  #scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))

##Figures showing response of graminoid survival and growth using uniform SPEI
plot_grid(UniqueSPEI_Surv, UniqueSPEI_Grow, ncol = 2, align = "h", axis = "tb", rel_widths = c(.9,1))


#### compare SPEI between species (using species-specific values) ####
#SPEI_unique is SPEI value unique for each species
#SPEI_s is the scaled version of this variable

#SPEI_uniform is SPEI value for all species
#SPEI_uniform_s is the scaled version of this variable

## compare raw values for unique SPEI accross species
temp <- CO_grams[,c("species", "year_t", "SPEI_unique", "SPEI_uniform")]
gramsSPEI_unique <- aggregate(temp[,c("SPEI_unique", "SPEI_uniform")], by = list(temp$species, temp$year_t), FUN = mean) %>% 
  rename(species = Group.1, year_t = Group.2) %>% 
  arrange(species) %>% 
  filter(is.na(SPEI_unique) == FALSE) %>% 
  mutate(year_num = as.numeric(as.character(year_t)))

## get photosynthetic pathway data
CO_traits_temp <- CO_traits[,c("species", "C3_C4")]
## merge w/ SPEI data
gramsSPEI_unique <- left_join(gramsSPEI_unique, CO_traits_temp)
#add additional pathway data
gramsSPEI_unique[gramsSPEI_unique$species=="Schedonnardus paniculatus", "C3_C4"] <- "C4"
gramsSPEI_unique[gramsSPEI_unique$species=="Muhlenbergia torreyi", "C3_C4"] <- "C4"
gramsSPEI_unique[gramsSPEI_unique$species=="Carex spp.", "C3_C4"] <- "C3"
gramsSPEI_unique[gramsSPEI_unique$species=="Carex filifolia", "C3_C4"] <- "C3"

gramsSPEI_unique <- gramsSPEI_unique %>% 
  mutate(C3_C4 = as.factor(C3_C4))

## plot the unique SPEI values (grams only)
# plotted by species
ggplot(data = gramsSPEI_unique) +
  geom_line(aes(x = year_num, y = SPEI_unique, col = C3_C4, lty = species)) +
  geom_line(aes(x = year_num, y = SPEI_uniform)) +
  theme_classic()
# plotted by photosynthetic type (grams only)
ggplot(data = gramsSPEI_unique) +
  geom_boxplot(aes(x = year_t, y = SPEI_unique, col = C3_C4)) +
geom_point(aes(x = year_t, y = SPEI_uniform), col= "black", shape = 8) +
  theme_classic()

## plot the uniform SPEI values (grams only)
ggplot(data = gramsSPEI_unique) +
  geom_line(aes(x = year_num, y = SPEI_uniform)) +
  theme_classic()

## is the variation between years greater than the variation between species in one year?
testM <- lm(SPEI_unique ~ year_t + species, data = gramsSPEI_unique)
anova(testM)
#year is a more important predictor than species (is significant, while species isn't)

## Do this, but with forbs 
temp <- CO_point_all[,c("species", "year_t", "SPEI_unique", "SPEI_uniform")]
forbsSPEI <- aggregate(temp[,c("SPEI_unique", "SPEI_uniform")], by = list(temp$species, temp$year), FUN = mean) %>% 
  rename(species = Group.1, year_t = Group.2) %>% 
  arrange(species) %>% 
  filter(is.na(SPEI_unique) == FALSE) %>% 
  mutate(year_num = as.numeric(as.character(year_t)))

## merge photosynthetic pathway with SPEI data
forbsSPEI <- left_join(forbsSPEI, CO_traits_temp)
#add additional pathway data
forbsSPEI[, "C3_C4"] <- "C3" #assuming all forbs are C3
forbsSPEI <- forbsSPEI %>% 
  mutate(C3_C4 = as.factor(C3_C4))

## plot the unique SPEI values (forbs only)
# plotted by species
ggplot(data = forbsSPEI) +
  geom_line(aes(x = year_num, y = SPEI_unique, col = C3_C4, lty = species)) +
  #geom_line(aes(x = year_num, y = SPEI_uniform)) +
  theme_classic()
# plotted by year
ggplot(data = forbsSPEI) +
  geom_boxplot(aes(x = year_t, y = SPEI_unique)) +
  geom_point(aes(x = year_t, y = SPEI_uniform), col= "red", shape = 8) +
  theme_classic()

## plot the uniform SPEI values (forbs only)
ggplot(data = forbsSPEI) +
  geom_line(aes(x = year_num, y = SPEI_uniform)) +
  theme_classic()

## is the variation between years greater than the variation between species in one year?
testM_forb <- lm(SPEI_unique ~ year_t + species, data = forbsSPEI)
anova(testM_forb)
