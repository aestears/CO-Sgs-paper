#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Analysis 
# Script 5 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

##### Load Packages #####
library(tidyverse) #v1.3.0
library(lme4) #v1.1-27.1
library(MuMIn) #v1.43.17
library(lmerTest) #v3.1-3
library(stargazer) #v5.2.2
library(sjPlot) #v2.8.7
library(effects) #v4.2-0
library(ggeffects) #v1.1.3.1
library(afex) #v0.28.1

##### Load Data Files #####
## set working directory
# datWD <-  set path for the location of the environment image of script 3 output
setwd(datWD)
#load data from previous 
load("./scripts/script3_output.RData")

CO_poly_old <- CO_poly_all

CO_point_old <- CO_point_all


#### ensure that the structure of the variables is correct ####
CO_poly_all <- CO_poly_all %>% 
  dplyr::select(species, quad, year_t, area_t, survives_tplus1, nearEdge_t, area_tplus1, neighbors_5_s, neighbors_10_s, neighbors_15_s, neighbors_20_s, size_t_log, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform, precip_s,  size_tplus1_log) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year_t), nearEdge_t = as.factor(nearEdge_t))

CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform, precip_s) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year), survives_tplus1 = as.integer(survives), nearEdge_t = as.integer(nearEdge)) 

#### testing viability of different neighborhood distance radii####
#simple model using TLP for graminoids 

mNeigh5 <- glmer(survives_tplus1 ~ size_t_log + neighbors_5_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh10 <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh15 <- glmer(survives_tplus1 ~ size_t_log + neighbors_15_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh20 <- glmer(survives_tplus1 ~ size_t_log + neighbors_20_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

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

AIC(mNeigh5, mNeigh10, mNeigh15, mNeigh20)

#### Model validation functions ####
### for glmer models (survival)(Can't make REML = FALSE) 
glmerInfo <- function(mod) {
  #'mod' is the fitted lmer model
  # output is stored as a list
  list("summary" = summary(mod, cor = FALSE), #see the model summary
       "fixedEff" = drop1(mod,test="Chisq"), #see the effect of the fixed effects (as estimated by single-term deletion of fixed-effects) 
       "randEff" = rand(mod), #see the effect of the random effects (as estimated by single-term deletion of random effects)
       "confInts" = confint(mod), #computing profile confidence intervals for these models
       "rSquared" = r.squaredGLMM(mod))
}

### for lmer models (growth) (make REML = FALSE )
lmerInfo <- function(mod) {
  #'mod' is the fitted lmer model
  # output is stored as a list
  list("summary" = summary(as_lmerModLmerTest(update(mod, REML = FALSE)), corr = FALSE), #see the model summary)
       "fixedEff" = drop1(as_lmerModLmerTest(update(mod, REML = FALSE)),test="Chisq"), #see the effect of the fixed effects (as estimated by single-term deletion of fixed-effects) 
       "randEff" = rand(as_lmerModLmerTest(update(mod, REML = FALSE))), #see the effect of the random effects (as estimated by single-term deletion of random effects)
        "confInts" = confint(mod), #computing profile confidence intervals for these models
        "rSquared" = r.squaredGLMM(mod))
}

### to compare models w and w/out traits
modCompare <- function(m1, m2){
  # m1 is the model w/out traits
  # m2 is the model with traits
  list("AICtable" = print(aictab(list("TraitMod" = m2, "NoTraitMod" = m1))), #get an AIC model comparison table
  "LRT" = pchisq(as.numeric(-2 * logLik(m1) + 2 * logLik(m2)), df=1, lower.tail=F), #perform a likelihood ratio test using the loglik() function
  "deltaAIC" =  diff(AIC(m2, m1)$AIC)) #calculate the differences in AIC
}

#####Graminoid Survival Models #####
### rename polygon dataset
CO_grams <- CO_poly_all

### TLP graminoid model ###
#global model
CO_poly_TLP <- CO_grams %>% 
  filter(!is.na(TLP_s))

mSurvTLP_grams <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvTLP_grams)

# compare model with SPEI models to temp and precip
mSurvTLP_grams_temp <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + ann_temp_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mSurvTLP_grams_precip <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + precip_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

### LDMC graminoid model ###
CO_poly_LDMC <- CO_grams %>% 
  filter(!is.na(LDMC_s))

mSurvLDMC_grams <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  size_t_log + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvLDMC_grams)

### SLA graminoid model ###
CO_poly_SLA <- CO_grams %>% 
  filter(is.na(SLA_s)==FALSE)

mSurvSLA_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * SLA_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvSLA_grams)

### RDMC graminoid model ###
CO_poly_RDMC <-
  CO_grams %>% 
  filter(is.na(RDMC_s)==FALSE)

mSurvRDMC_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RDMC_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_grams)

### RTD graminoid model ###
CO_poly_RTD <-
  CO_grams %>% 
  filter(is.na(RTD_s)==FALSE)

mSurvRTD_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

### SRL graminoid model ###
CO_poly_SRL <-
  CO_grams %>% 
  filter(is.na(SRL_s)==FALSE)

mSurvSRL_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
#removed random slope for area, since the model doesn't converge with it

### RDiam graminoid model ###
CO_poly_Diam <-
  CO_grams %>% 
  filter(is.na(RDiam_s)==FALSE)

mSurvRDiam_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RDiam_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) , data = CO_poly_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#### Point Survival Models ####
### TLP point model ###
options(na.action = na.omit)
CO_point_TLP <- CO_point_all %>% 
  filter(!is.na(TLP_s))

mSurvTLP_forbs <- glmer(survives_tplus1 ~ SPEI_s*TLP_s + neighbors_10_s + nearEdge_t 
              + (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

 summary(mSurvTLP_forbs)

### LDMC point model ###

#for LDMC
CO_point_LDMC <- CO_point_all %>% 
  filter(!is.na(LDMC_s))

mSurvLDMC_forbs <- glmer(survives_tplus1 ~ SPEI_s*LDMC_s + neighbors_10_s + nearEdge_t + 
               (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvLDMC_forbs)

#SLA for forbs
CO_point_SLA <- CO_point_all %>% 
  filter(is.na(SLA_s)==FALSE)

mSurvSLA_forbs <- glmer(survives_tplus1 ~ SPEI_s*SLA_s + neighbors_10_s + nearEdge_t  + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvSLA_forbs)

#### test forb survival root traits ####

#rdmc for forbs
CO_point_RDMC <-
  CO_point_all %>% 
  filter(is.na(RDMC_s)==FALSE)

mSurvRDMC_forbs <- glmer(survives_tplus1 ~ SPEI_s*RDMC_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_forbs)

#rtd for forbs
CO_point_RTD <-
  CO_point_all %>% 
  filter(is.na(RTD_s)==FALSE)

mSurvRTD_forbs <- glmer(survives_tplus1 ~ SPEI_s*RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRTD_forbs)

#SRL for forbs
CO_point_SRL <-
  CO_point_all %>% 
  filter(is.na(SRL_s)==FALSE)

mSurvSRL_forbs <- glmer(survives_tplus1 ~ SPEI_s*SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#root diameter for forbs
CO_point_Diam <-
  CO_point_all %>% 
  filter(is.na(RDiam_s)==FALSE)

mSurvRDiam_forbs <- glmer(survives_tplus1 ~ SPEI_s*RDiam_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#### fit survival models w/out traits ####
mSurvTLP_grams_NO <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) ,
                  data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))


mSurvLDMC_grams_NO <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) ,
               data = CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mSurvRTD_grams_NO <- glmer(survives_tplus1 ~ size_t_log + SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRTD_grams_NO)

mSurvSRL_grams_NO <- glmer(survives_tplus1 ~ size_t_log + SPEI_s  + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mSurvRDiam_grams_NO <- glmer(survives_tplus1 ~ size_t_log + SPEI_s  + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) , data = CO_poly_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))


mSurvSLA_grams_NO <- glmer(survives_tplus1 ~ size_t_log + SPEI_s  + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvSLA_grams_NO)


mSurvRDMC_grams_NO <- glmer(survives_tplus1 ~ size_t_log + SPEI_s  + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_grams_NO)


mSurvTLP_forbs_NO <- glmer(survives_tplus1 ~ SPEI_s  + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_TLP, family = binomial(link = logit),control=glmerControl(optimizer="bobyqa"))

mSurvLDMC_forbs_NO <- glmer(survives_tplus1 ~  neighbors_10_s +  SPEI_s + nearEdge_t +
                 (1|species) + (1|quad) + (1|year_t), data=CO_point_LDMC, 
               family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mSurvSLA_forbs_NO <-  glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvSLA_forbs_NO)


mSurvRDMC_forbs_NO <- glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_forbs_NO)


mSurvRTD_forbs_NO <- glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRTD_forbs)


mSurvSRL_forbs_NO <- glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))


mSurvRDiam_forbs_NO <- glmer(survives_tplus1 ~ SPEI_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))


#### models w/ growth as response variable #### 
# (only for graminoids, no size metric for forbs)
# using size in year t+1 as response variable, w/ a random slope for size in year t

## Graminoid Models ##
#subset the datasets to only include observations that have growth data (for those plants that survived)
CO_poly_growth <- CO_grams %>% 
  filter(!is.na(CO_grams$size_tplus1_log))

## TLP model
#global model
options(na.action = "na.omit")

CO_grow_TLP <-
  CO_poly_growth %>% 
  filter(is.na(TLP_s)==FALSE)

mGrowTLP<- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

summary(as_lmerModLmerTest(mGrowTLP))

#LDMC model
CO_grow_LDMC<-
  CO_poly_growth %>% 
  filter(is.na(LDMC_s)==FALSE)

mGrowLDMC <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC , control=lmerControl(optimizer="bobyqa"))

summary(as_lmerModLmerTest(mGrowLDMC))

#SLA model
CO_grow_SLA <-
  CO_poly_growth %>% 
  filter(is.na(SLA_s)==FALSE)

mGrowSLA <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * SLA_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lmerControl(optimizer="bobyqa"))

summary(as_lmerModLmerTest(mGrowSLA))

#RDMC model
CO_grow_RDMC <-
  CO_poly_growth %>% 
  filter(is.na(RDMC_s)==FALSE)

mGrowRDMC <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s * RDMC_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC , control=lmerControl(optimizer="bobyqa"))

summary(as_lmerModLmerTest(mGrowRDMC))

#SRL model
CO_grow_SRL <-
  CO_poly_growth %>% 
  filter(is.na(SRL_s)==FALSE)

mGrowSRL <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * SRL_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

summary(as_lmerModLmerTest(mGrowSRL))

#RTD model
CO_grow_RTD<-
  CO_poly_growth %>% 
  filter(is.na(RTD_s)==FALSE)

mGrowRTD  <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * RTD_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

summary(as_lmerModLmerTest(mGrowRTD))

#RDiam model
CO_grow_RDiam<-
  CO_poly_growth %>% 
  filter(is.na(RDiam_s)==FALSE)

mGrowRDiam  <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * RDiam_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))

summary(as_lmerModLmerTest(mGrowRDiam))


#### make growth models without traits for comparison ####
#fit models
mGrowTLP_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowLDMC_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSLA_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowRDMC_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSRL_NO <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

mGrowRTD_NO  <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

mGrowRDiam_NO  <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))

#### Get Correlation TLP and LDMC for polygons (graminoids) ####
# "traits_Not_dups" is the data.frame with the species-level trait averages
# make sure that it is subset for the species we used in our analysis
polySpp <- as.character(unique(CO_grams$species))
pointSpp <- as.character(unique(CO_point_all$species))

#get trait data
# datWD <-  #set working directory for location of flowering time data file
setwd(datWD)
CO_traits <- read.csv("./CO_mean_traits.csv", stringsAsFactors = FALSE)

#get trait correlations for all species used in the models
traits_modSpp <- CO_traits[CO_traits$species %in% c(polySpp, pointSpp),]

#traits of interest
traits <- c("TLP", "AvgDiam_mm", "RTD_g_cm3", "RDMC_g_g", "SRL_best_m_g", "LDMC_g_g", "SLA_adj_cm2_g")

#calculate a correlation matrix
allSpp_corMatrix <- cor(traits_modSpp[,names(traits_modSpp) %in% traits], use="complete.obs")
stargazer(allSpp_corMatrix)

#correlation matrix for traits in polygon analysis
polySpp_corMatrix <- cor(CO_traits[CO_traits$species %in% polySpp,names(traits_modSpp) %in% traits], use="complete.obs")

#correlation matrix for traits in point analysis
pointSpp_corMatrix <- cor(CO_traits[CO_traits$species %in% pointSpp,names(traits_modSpp) %in% traits], use="complete.obs")


#### tables of model results ####
#for graminoid survival  models
library(stargazer)
stargazer(mSurvTLP_grams, mSurvLDMC_grams, mSurvSLA_grams, mSurvRDMC_grams, mSurvRTD_grams, mSurvSRL_grams, mSurvRDiam_grams, 
style = "all2", column.labels = c("TLP", "LDMC", "SLA", "RDMC","RTD", "SRL", "RDiam"), dep.var.labels = c("P(Survival)"), digits = 2, model.numbers = FALSE, report = c("vc*"), 
#omit = c("TLP_s", "LDMC_s", "SLA_s", "RDMC_s", "RTD_s", "SRL_s", "RDiam_s"), 
          type = "text",
add.lines =  list(
            Trait = c("Trait", "-0.13**", "0.26**", "-0.07 ", "-0.02", "0.36***", "0.14", "-0.04") ,
            Blank = c("", "", "", "", "", "", "", ""),
            TraitBySPEI = c("Trait:SPEI", "0.16***", "-0.26***", "-0.08***", "-0.21***", "0.01", "-0.05**", "-0.15***"),
            Blank = c("", "", "", "", "", "", "", ""),
            deltaAIC <- c("Delta AIC", "55.79", "123.18", "10.64", "87.60", "5.06", "2.99", "46.13")
          ),
          omit.table.layout = c("-"),
          omit.stat = c("bic", "ll")
          )
#get random effect coefficients
sjPlot::tab_model(mSurvTLP_grams, mSurvLDMC_grams, mSurvSLA_grams, mSurvRDMC_grams, mSurvRTD_grams, mSurvSRL_grams, mSurvRDiam_grams, show.se = TRUE)

diff(AIC(mSurvTLP_grams, mSurvTLP_grams_NO)$AIC) #TLP
anova(mSurvTLP_grams, mSurvTLP_grams_NO, test="Chisq")
diff(AIC(mSurvLDMC_grams, mSurvLDMC_grams_NO)$AIC) #LDMC
anova(mSurvLDMC_grams, mSurvLDMC_grams_NO, test="Chisq")
diff(AIC(mSurvSLA_grams, mSurvSLA_grams_NO)$AIC) #SLA
anova(mSurvSLA_grams, mSurvSLA_grams_NO, test = "Chisq")
diff(AIC(mSurvRDMC_grams, mSurvRDMC_grams_NO)$AIC) #RDMC
anova(mSurvRDMC_grams, mSurvRDMC_grams_NO, test = "Chisq")
diff(AIC(mSurvRTD_grams, mSurvRTD_grams_NO)$AIC) #RTD
anova(mSurvRTD_grams, mSurvRTD_grams_NO, test = "Chisq")
diff(AIC(mSurvSRL_grams, mSurvSRL_grams_NO)$AIC) #SRL
anova(mSurvSRL_grams, mSurvSRL_grams_NO, test = "Chisq")
diff(AIC(mSurvRDiam_grams, mSurvRDiam_grams_NO)$AIC) #RDiam
anova(mSurvRDiam_grams, mSurvRDiam_grams_NO, test = "Chisq")

#for forb survival models
stargazer(mSurvTLP_forbs, mSurvLDMC_forbs, mSurvSLA_forbs, mSurvRDMC_forbs, mSurvRTD_forbs, mSurvSRL_forbs, mSurvRDiam_forbs,  
          style = "all2", column.labels = c("TLP", "LDMC", "SLA", "RDMC","RTD", "SRL", "RDiam"), dep.var.labels = c("P(Survival)"), digits = 2, model.numbers = FALSE, report = c("vc*"), omit = c("TLP_s", "LDMC_s", "SLA_s", "RDMC_s", "RTD_s", "SRL_s", "RDiam_s"), 
          #type = "text",
          add.lines =  list(
            Trait = c("Trait", "-0.06", "-0.19", "0.22", "-0.39", "0.14", "0.07" , "0.08" ) ,
            Blank = c("", "", "", "", "", "", "", ""),
            TraitBySPEI = c("Trait:SPEI", "0.12", "-0.38***", "0.57**", "-0.31**", "-0.27**", "0.20", "0.02"),
            Blank = c("", "", "", "", "", "", "", ""),
            deltaAIC <- c("Delta AIC", "-3.22", "4.63", "1.39", "1.05", "-0.90", "-2.87", "-3.91")
          ), omit.table.layout = c("-"),
          omit.stat = c("bic", "ll"))

#get random effect coefficients
sjPlot::tab_model(mSurvTLP_forbs, mSurvLDMC_forbs, mSurvSLA_forbs, mSurvRDMC_forbs, mSurvRTD_forbs, mSurvSRL_forbs, mSurvRDiam_forbs, show.se = TRUE)
diff(AIC(mSurvTLP_forbs, mSurvTLP_forbs_NO)$AIC) #TLP
anova(mSurvTLP_forbs, mSurvTLP_forbs_NO, test = "Chisq")
diff(AIC(mSurvLDMC_forbs, mSurvLDMC_forbs_NO)$AIC) #LDMC
anova(mSurvLDMC_forbs, mSurvLDMC_forbs_NO, test = "Chisq")
diff(AIC(mSurvSLA_forbs, mSurvSLA_forbs_NO)$AIC) #SLA
anova(mSurvSLA_forbs, mSurvSLA_forbs_NO, test = "Chisq")
diff(AIC(mSurvRDMC_forbs, mSurvRDMC_forbs_NO)$AIC) #RDMC
anova(mSurvRDMC_forbs, mSurvRDMC_forbs_NO, test = "Chisq")
diff(AIC(mSurvRTD_forbs, mSurvRTD_forbs_NO)$AIC) #RTD
anova(mSurvRTD_forbs, mSurvRTD_forbs_NO, test = "Chisq")
diff(AIC(mSurvSRL_forbs, mSurvSRL_forbs_NO)$AIC) #SRL
anova(mSurvSRL_forbs, mSurvSRL_forbs_NO, test = "Chisq")
diff(AIC(mSurvRDiam_forbs, mSurvRDiam_forbs_NO)$AIC) #RDiam
anova(mSurvRDiam_forbs, mSurvRDiam_forbs_NO, test = "Chisq")

#for graminoid growth models
diff(AIC(mGrowTLP, mGrowTLP_NO)$AIC) #TLP
anova(mGrowTLP, mGrowTLP_NO, test = "Chisq")
diff(AIC(mGrowLDMC, mGrowLDMC_NO)$AIC) #LDMC
anova(mGrowLDMC, mGrowLDMC_NO, test = "Chisq")
diff(AIC(mGrowSLA, mGrowSLA_NO)$AIC) #SLA
anova(mGrowSLA, mGrowSLA_NO, test = "Chisq")
diff(AIC(mGrowRDMC, mGrowRDMC_NO)$AIC) #RDMC
anova(mGrowRDMC, mGrowRDMC_NO, test = "Chisq")
diff(AIC(mGrowRTD, mGrowRTD_NO)$AIC) #RTD
anova(mGrowRTD, mGrowRTD_NO, test = "Chisq")
diff(AIC(mGrowSRL, mGrowSRL_NO)$AIC) #SRL
anova(mGrowSRL, mGrowSRL_NO, test = "Chisq")
diff(AIC(mGrowRDiam, mGrowRDiam_NO)$AIC) #RDiam

## get p-values using LRT
mGrowTLP_pvals <- mixed(mGrowTLP, data = CO_grow_TLP, method = "LRT")
mGrowLDMC_pvals <- mixed(mGrowLDMC, data = CO_grow_LDMC, method = "LRT")
mGrowSLA_pvals <- mixed(mGrowSLA, data = CO_grow_SLA, method = "LRT")
mGrowRDMC_pvals <- mixed(mGrowRDMC, data = CO_grow_RDMC, method = "LRT")
mGrowRTD_pvals <- mixed(mGrowRTD, data = CO_grow_RTD, method = "LRT")
mGrowSRL_pvals <- mixed(mGrowSRL, data = CO_grow_SRL, method = "LRT")
mGrowRDiam_pvals <- mixed(mGrowRDiam, data = CO_grow_RDiam, method = "LRT")

stargazer(mGrowTLP, mGrowLDMC, mGrowSLA, mGrowRDMC, mGrowRTD, mGrowSRL, mGrowRDiam, style = "all2", column.labels = c("TLP", "LDMC", "SLA", "RDMC","RTD", "SRL", "RDiam"), dep.var.labels = c("P(Survival)"), digits = 2, model.numbers = FALSE, report = c("vc*"), omit = c("TLP_s", "LDMC_s", "SLA_s", "RDMC_s", "RTD_s", "SRL_s", "RDiam_s"), 
          #type = "text",
          add.lines =  list(
            Trait = c("Trait", "-0.17**", "0.13", "0.05", "0.05", "-0.20**", "-0.09", "0.02") ,
            Blank = c("", "", "", "", "", "", "", ""),
            TraitBySPEI = c("Trait:SPEI", "0.01" , "-0.02", "-0.004", "-0.01", "0.01", "0.003", "-0.02"),
            Blank = c("", "", "", "", "", "", "", ""),
            deltaAIC <- c("Delta AIC", "-11.02", "-10.40", "-13.16", "-12.25", "-9.55", "-12.66", "-12.39")
          ), omit.table.layout = c("-"),
          omit.stat = c("bic", "ll"))
sjPlot::tab_model(mGrowTLP, mGrowLDMC, mGrowSLA, mGrowRDMC, mGrowRTD, mGrowSRL, mGrowRDiam)

#### save output to use in figure script ####
# path = "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper/scripts" #location where you'll put the environment data file
setwd(path)

save.image('script4_output.RData')