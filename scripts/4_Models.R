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
require(Ryacas) #v1.1.3.1

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
  dplyr::select(species, quad, year_t, area_t, survives_tplus1, nearEdge_t, area_tplus1, neighbors_5_s, neighbors_10_s, neighbors_15_s, neighbors_20_s, area_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform_s, SPEI_uniform, SPEI_unique) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year_t), nearEdge_t = as.factor(nearEdge_t))

CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform_s, SPEI_uniform, SPEI_unique) %>% 
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

mslist <- dredge(m10)
mslist
# here you can see all of the models compared and they are ranked in order
# here we see we have multiple models <2 delta AIC from the top model 
topmod<-get.models(mslist, subset=cumsum(weight) <= .95)


out.put<-model.sel(m1,m2,m3,m4,m5)
out.put #models 4 and 5 are the best by a lot. 



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
#(only for graminoids, no size metric for forbs)

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

mGrowTLP<- lme4::lmer(logDiffArea ~ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

CO_grow_LDMC<-
  CO_poly_growth %>% 
  filter(is.na(LDMC_s)==FALSE)
mGrowLDMC <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC , control=lmerControl(optimizer="bobyqa"))

CO_grow_SLA <-
  CO_poly_growth %>% 
  filter(is.na(SLA_s)==FALSE)
mGrowSLA <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s * SLA_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lmerControl(optimizer="bobyqa"))

CO_grow_RDMC <-
  CO_poly_growth %>% 
  filter(is.na(RDMC_s)==FALSE)
mGrowRDMC <- lme4::lmer(logDiffArea ~ neighbors_10_s + SPEI_s * RDMC_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC , control=lmerControl(optimizer="bobyqa"))

CO_grow_SRL <-
  CO_poly_growth %>% 
  filter(is.na(SRL_s)==FALSE)
mGrowSRL <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s * SRL_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

CO_grow_RTD<-
  CO_poly_growth %>% 
  filter(is.na(RTD_s)==FALSE)
mGrowRTD  <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s * RTD_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

CO_grow_RDiam<-
  CO_poly_growth %>% 
  filter(is.na(RDiam_s)==FALSE)
mGrowRDiam  <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s * RDiam_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))

#### make growth models without traits for comparison ####
#fit models

mGrowTLP_NO <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s  + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_TLP, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowLDMC_NO <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s  + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSLA_NO <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowRDMC_NO <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s  + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSRL_NO <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

mGrowRTD_NO  <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

mGrowRDiam_NO  <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))

#compare AIC of models with and without traits
diff(AIC(mGrowTLP, mGrowTLP_NO)$AIC) #TLP
diff(AIC(mGrowLDMC, mGrowLDMC_NO)$AIC) #LDMC
diff(AIC(mGrowSLA, mGrowSLA_NO)$AIC) #SLA
diff(AIC(mGrowRDMC, mGrowRDMC_NO)$AIC) #RDMC
diff(AIC(mGrowRTD, mGrowRTD_NO)$AIC) #RTD
diff(AIC(mGrowSRL, mGrowSRL_NO)$AIC) #SRL
diff(AIC(mGrowRDiam, mGrowRDiam_NO)$AIC) #RDiam

#calculate R2 values
piecewiseSEM::rsquared(mGrowTLP)
piecewiseSEM::rsquared(mGrowTLP_ratio)
piecewiseSEM::rsquared(mGrowTLP_NO)
piecewiseSEM::rsquared(mGrowLDMC)
piecewiseSEM::rsquared(mGrowLDMC_NO)
piecewiseSEM::rsquared(mGrowSLA)
piecewiseSEM::rsquared(mGrowSLA_NO)
# 
#### table of model results ####
#for graminoid models
require(stargazer)
stargazer(m1_grams, m2_grams, m5, m9, m10, m13, m14)
sjPlot::tab_model(m1_grams, m2_grams, m5, m9, m10, m13, m14, show.se = TRUE)

#for forb models
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
visreg::visreg2d(mGrowTLP_SPEI, xvar = "TLP_s", yvar =  "SPEI_uniform_s", scale = "response", plot.type = "persp")


#### try a GAMM model for TLP survival ####
require(gamm4)
m1<-gamm(re~s(winterpc1,bs="cr")+logfrz,gamma=1.4,random=list(fem=~1), family=poisson, method="REML",data=end)
glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_uniform_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

m1GAMM <- gamm(survives_tplus1 ~ )