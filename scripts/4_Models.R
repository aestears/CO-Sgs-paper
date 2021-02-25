#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Analysis 
# Script 5 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

##### Load Packages #####
<<<<<<< HEAD
library(tidyverse) #v1.3.0
library(lme4) #v1.1-26
library(MuMIn) #v1.43.17
library(lmerTest) #v3.1-3
library(stargazer) #v5.2.2
library(sjPlot) #v2.8.7
library(effects) #v4.2-0
library(ggeffects) #v1.1.3.1
=======
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
>>>>>>> a4403b8... update modeling script

## clear workspace ##
rm(list=ls())

##### Load Data Files #####
## set working directory
<<<<<<< HEAD
<<<<<<< HEAD
# datWD <-  set path for the location of the environment image of script 3 output
=======
datWD <- setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper") #set path for the location of the environment image of script 3 output
>>>>>>> a4403b8... update modeling script
=======
datWD <- c("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper") #set path for the location of the environment image of script 3 output
>>>>>>> f170c74... checked models with uniform SPEI
setwd(datWD)
#load data from previous 
load("./scripts/script3_output.RData")

<<<<<<< HEAD
CO_poly_old <- CO_poly_all

CO_point_old <- CO_point_all

#### ensure that the structure of the variables is correct ####
CO_poly_all <- CO_poly_all %>% 
  dplyr::select(species, quad, year_t, area_t, survives_tplus1, nearEdge_t, area_tplus1, neighbors_5_s, neighbors_10_s, neighbors_15_s, neighbors_20_s, size_t_log, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform, precip_s,  size_tplus1_log) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year_t), nearEdge_t = as.factor(nearEdge_t))

CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform, precip_s) %>% 
=======
#### ensure that the structure of the variables is correct ####
CO_poly_all <- CO_poly_all %>% 
  dplyr::select(species, quad, year_t, area_t, survives_tplus1, nearEdge_t, area_tplus1, neighbors_5_s, neighbors_10_s, neighbors_15_s, neighbors_20_s, area_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform_s, SPEI_uniform, SPEI_unique) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year_t), nearEdge_t = as.factor(nearEdge_t))

<<<<<<< HEAD
CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s) %>% 
>>>>>>> a4403b8... update modeling script
=======
CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform_s, SPEI_uniform, SPEI_unique) %>% 
>>>>>>> f170c74... checked models with uniform SPEI
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year), survives_tplus1 = as.integer(survives), nearEdge_t = as.integer(nearEdge)) 
  
#### testing viability of different neighborhood distance radii####
#simple model using TLP for graminoids 
<<<<<<< HEAD
mNeigh5 <- glmer(survives_tplus1 ~ size_t_log + neighbors_5_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh10 <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh15 <- glmer(survives_tplus1 ~ size_t_log + neighbors_15_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh20 <- glmer(survives_tplus1 ~ size_t_log + neighbors_20_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
=======
mNeigh5 <- glmer(survives_tplus1 ~ area_s + neighbors_5_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh10 <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh15 <- glmer(survives_tplus1 ~ area_s + neighbors_15_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh20 <- glmer(survives_tplus1 ~ area_s + neighbors_20_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> a4403b8... update modeling script

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
<<<<<<< HEAD
AIC(mNeigh5, mNeigh10, mNeigh15, mNeigh20)

#### Model validation functions ####
### for glmer models (survival)(Can't make REML = FALSE) 
glmerInfo <- function(mod) {
  #'mod' is the fitted lmer model
  # output is stored as a list
  list("summary" = summary(mod, cor = FALSE), #see the model summary)
       "anova" = anova(mod, method = "Chisq"),
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
=======
>>>>>>> a4403b8... update modeling script

#####Graminoid Survival Models #####
### rename polygon dataset
CO_grams <- CO_poly_all

### TLP graminoid model ###
#global model
CO_poly_TLP <- CO_grams %>% 
  filter(!is.na(TLP_s))

<<<<<<< HEAD
mSurvTLP_grams <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvTLP_grams)
=======
m1_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m1_grams)
rsquared_m1_grams <- piecewiseSEM::rsquared(m1_grams)

<<<<<<< HEAD
#plot residuals to assess model fit
plot_model(m1_grams, type = "diag") #get qqplots for random effects and fixed effects
>>>>>>> a4403b8... update modeling script

#don't have to calculate overdispersion--not possible for bernoulli binomial variables
# https://stats.stackexchange.com/questions/206007/can-there-be-overdispersion-in-a-logistic-regression-model-where-each-observatio

<<<<<<< HEAD
=======
##tried to include Tribe as a random effect, but the model wouldn't converge--I think there is too much variation in Tribe (7 Tribes, 9 species) --> remove tribe as a covariate

#The r-squared is good enough to continue with this as a global model 

#test different random effect structures
m1A_grams <- glmer(survives_tplus1 ~ area_s +  neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grams, family = binomial(link = logit))
m1B_grams <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad),  data = CO_grams, family = binomial(link = logit))
m1C_grams <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|year_t), data = CO_grams, family = binomial(link = logit))
m1D_grams <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) , data = CO_grams, family = binomial(link = logit))
m1E_grams <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|year_t), data = CO_grams, family = binomial(link = logit))
m1F_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) ,  data = CO_grams, family = binomial(link = logit))
m1G_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (area_s|species), data = CO_grams, family = binomial(link = logit))
m1H_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (1|quad) , data = CO_grams, family = binomial(link = logit))
m1I_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (1|year_t), data = CO_grams, family = binomial(link = logit))
m1Q_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|year_t) + (1|quad),  data = CO_grams, family = binomial(link = logit))
m1U_grams <- glm(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t , 
             data = CO_grams, family = binomial(link = logit))
#compare the random effect structures using AIC
m1_AIC_grams <- MuMIn::AICc(m1_grams, m1A_grams, m1B_grams, m1C_grams, m1D_grams, m1E_grams, m1F_grams, m1G_grams, m1H_grams, m1I_grams, m1Q_grams, m1U_grams)
m1_AIC_grams[m1_AIC_grams$AICc==min(m1_AIC_grams$AICc),]

#m1Q (same as m1) has the lowest AIC, so use the random effects structure without a random intercept for Tribe

=======
#don't have to calculate overdispersion--not possible for bernoulli binomial variables
# https://stats.stackexchange.com/questions/206007/can-there-be-overdispersion-in-a-logistic-regression-model-where-each-observatio

>>>>>>> f170c74... checked models with uniform SPEI
#calculate R2 
rsquaredm1 <- piecewiseSEM::rsquared(m1_grams)


>>>>>>> a4403b8... update modeling script
### LDMC graminoid model ###
CO_poly_LDMC <- CO_grams %>% 
  filter(!is.na(LDMC_s))

<<<<<<< HEAD
mSurvLDMC_grams <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  size_t_log + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvLDMC_grams)
=======
m2_grams <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  area_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(m2_grams)

#calculate the R2 
rsquaredm2 <- piecewiseSEM::rsquared(m2_grams)
>>>>>>> a4403b8... update modeling script

### SLA graminoid model ###
CO_poly_SLA <- CO_grams %>% 
  filter(is.na(SLA_s)==FALSE)

<<<<<<< HEAD
mSurvSLA_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * SLA_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvSLA_grams)
=======
m5 <- glmer(survives_tplus1 ~ area_s + SPEI_s * SLA_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m5)

#calculate the R2 
rsquaredm5 <- piecewiseSEM::rsquared(m5)
>>>>>>> a4403b8... update modeling script

### RDMC graminoid model ###
CO_poly_RDMC <-
  CO_grams %>% 
  filter(is.na(RDMC_s)==FALSE)

<<<<<<< HEAD
mSurvRDMC_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RDMC_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_grams)
=======
m9 <- glmer(survives_tplus1 ~ area_s + SPEI_s * RDMC_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m9)

#calculate the R2 
rsquaredm9 <- piecewiseSEM::rsquared(m9)
>>>>>>> a4403b8... update modeling script

### RTD graminoid model ###
CO_poly_RTD <-
  CO_grams %>% 
  filter(is.na(RTD_s)==FALSE)

<<<<<<< HEAD
mSurvRTD_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
# removed random slope for area, since the model didn't converge
summary(mSurvRTD_grams)
=======
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


>>>>>>> a4403b8... update modeling script

### SRL graminoid model ###
CO_poly_SRL <-
  CO_grams %>% 
  filter(is.na(SRL_s)==FALSE)

<<<<<<< HEAD
mSurvSRL_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
#removed random slope for area, since the model doesn't converge with it

=======
m13 <- glmer(survives_tplus1 ~ area_s + SPEI_s * SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
#removed random slope for area, since the model doesn't converge with it

#calculate the R2 
rsquaredm13 <- piecewiseSEM::rsquared(m13)

>>>>>>> a4403b8... update modeling script
### RDiam graminoid model ###
CO_poly_Diam <-
  CO_grams %>% 
  filter(is.na(RDiam_s)==FALSE)

<<<<<<< HEAD
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

=======
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
>>>>>>> a4403b8... update modeling script
#for LDMC
CO_point_LDMC <- CO_point_all %>% 
  filter(!is.na(LDMC_s))

<<<<<<< HEAD
mSurvLDMC_forbs <- glmer(survives_tplus1 ~ SPEI_s*LDMC_s + neighbors_10_s + nearEdge_t + 
               (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvLDMC_forbs)
=======
m4 <- glmer(survives_tplus1 ~ SPEI_s*LDMC_s + neighbors_10_s + nearEdge_t + 
               (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(m4)


##### Test forb survival models for non-water related traits####
>>>>>>> a4403b8... update modeling script

#SLA for forbs
CO_point_SLA <- CO_point_all %>% 
  filter(is.na(SLA_s)==FALSE)
<<<<<<< HEAD

mSurvSLA_forbs <- glmer(survives_tplus1 ~ SPEI_s*SLA_s + neighbors_10_s + nearEdge_t  + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvSLA_forbs)
=======
m6 <- glmer(survives_tplus1 ~ SPEI_s*SLA_s + neighbors_10_s + nearEdge_t  + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m6)
>>>>>>> a4403b8... update modeling script

#### test forb survival root traits ####

#rdmc for forbs
CO_point_RDMC <-
  CO_point_all %>% 
  filter(is.na(RDMC_s)==FALSE)

<<<<<<< HEAD
mSurvRDMC_forbs <- glmer(survives_tplus1 ~ SPEI_s*RDMC_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_forbs)
=======
m11 <- glmer(survives_tplus1 ~ SPEI_s*RDMC_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m11)
>>>>>>> a4403b8... update modeling script

#rtd for forbs
CO_point_RTD <-
  CO_point_all %>% 
  filter(is.na(RTD_s)==FALSE)

<<<<<<< HEAD
mSurvRTD_forbs <- glmer(survives_tplus1 ~ SPEI_s*RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRTD_forbs)
=======
m12 <- glmer(survives_tplus1 ~ SPEI_s*RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m12)
>>>>>>> a4403b8... update modeling script

#SRL for forbs
CO_point_SRL <-
  CO_point_all %>% 
  filter(is.na(SRL_s)==FALSE)

<<<<<<< HEAD
mSurvSRL_forbs <- glmer(survives_tplus1 ~ SPEI_s*SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
=======
m15 <- glmer(survives_tplus1 ~ SPEI_s*SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> a4403b8... update modeling script

#root diameter for forbs
CO_point_Diam <-
  CO_point_all %>% 
  filter(is.na(RDiam_s)==FALSE)

<<<<<<< HEAD
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
=======
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
>>>>>>> a4403b8... update modeling script

## TLP model
#global model
options(na.action = "na.omit")

CO_grow_TLP <-
  CO_poly_growth %>% 
  filter(is.na(TLP_s)==FALSE)

<<<<<<< HEAD
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

#### testing different growth response variables ####
## log-transformed area in year t: size_t_log
## log-transformed area in year t+1: size_tplus1_log

#still using logDiffArea as response, but including a random slope and fixed effect for size_t
mGrow_GROWTH <- lme4::lmer(logDiffArea ~ size_t_log +neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

#compare the results of the above model to the model using size_t+1 as the response variable
=======
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
>>>>>>> a4403b8... update modeling script

#### make growth models without traits for comparison ####
#fit models

<<<<<<< HEAD
mGrowTLP_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowLDMC_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSLA_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowRDMC_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSRL_NO <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

mGrowRTD_NO  <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

mGrowRDiam_NO  <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))

#compare AIC of models with and without traits

=======
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
>>>>>>> a4403b8... update modeling script

#### Get Correlation TLP and LDMC for polygons (graminoids) ####
# "traits_Not_dups" is the data.frame with the species-level trait averages
# make sure that it is subset for the species we used in our analysis
polySpp <- as.character(unique(CO_grams$species))
pointSpp <- as.character(unique(CO_point_all$species))

<<<<<<< HEAD
#get trait data
# datWD <-  #set working directory for location of flowering time data file
setwd(datWD)
CO_traits <- read.csv("./CO_mean_traits.csv", stringsAsFactors = FALSE)

#l
=======
>>>>>>> a4403b8... update modeling script
#get trait correlations for all species used in the models
traits_modSpp <- CO_traits[CO_traits$species %in% c(polySpp, pointSpp),]

mComp <- lm(data = traits_modSpp, TLP ~ LDMC_g_g)

plot(x = traits_modSpp$LDMC_g_g, 
     y = traits_modSpp$TLP)
abline(reg = mComp)

#traits of interest
<<<<<<< HEAD
traits <- c("TLP", "AvgDiam_mm", "RTD_g_cmSurvTLP_forbs", "RDMC_g_g", "SRL_best_m_g", "LDMC_g_g", "SLA_adj_cm2_g")

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
anova(mGrowRDiam, mGrowRDiam_NO, test = "Chisq")

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

  #### save output to use in figure script ####vc s/CO-Sgs-paper/scripts" #location where you'll put the environment data file
setwd(path)
save.image('script4_output.RData')
=======
traits <- c("TLP", "AvgDiam_mm", "RTD_g_cm3", "RDMC_g_g", "SRL_best_m_g", "LDMC_g_g", "SLA_adj_cm2_g")
pairs(traits_modSpp[,traits], lower.panel = panel.cor)
allSpp_corMatrix <- cor(traits_modSpp[,traits], use="complete.obs")
stargazer(allSpp_corMatrix)

#correlation matrix for traits in polygon analysis
polySpp_corMatrix <- cor(CO_traits[CO_traits$species %in% polySpp,traits], use="complete.obs")

#correlation matrix for traits in point analysis
pointSpp_corMatrix <- cor(CO_traits[CO_traits$species %in% pointSpp,traits], use="complete.obs")


<<<<<<< HEAD
>>>>>>> a4403b8... update modeling script
=======
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
>>>>>>> f170c74... checked models with uniform SPEI
