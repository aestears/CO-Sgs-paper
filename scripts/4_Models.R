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
#require(fields) #v11.6
require(lme4) #v1.1-26
require(MuMIn) #v1.43.17
require(lmerTest) #v3.1-3
require(stargazer) #v5.2.2
#require(glmmTMB) #v1.0.2.1
require(sjPlot) #v2.8.7
require(effects) #v4.2-0
<<<<<<< HEAD
require(Ryacas) #v1.1.3.1
>>>>>>> a4403b8... update modeling script
=======
require(ggeffects) #v1.1.3.1
>>>>>>> d1625fe... try different precip models

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
<<<<<<< HEAD
=======
>>>>>>> 7e5a451... adding h-line to growth plots
CO_poly_old <- CO_poly_all

CO_point_old <- CO_point_all

<<<<<<< HEAD
#### ensure that the structure of the variables is correct ####
CO_poly_all <- CO_poly_all %>% 
  dplyr::select(species, quad, year_t, area_t, survives_tplus1, nearEdge_t, area_tplus1, neighbors_5_s, neighbors_10_s, neighbors_15_s, neighbors_20_s, size_t_log, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform, precip_s,  size_tplus1_log) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year_t), nearEdge_t = as.factor(nearEdge_t))

CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform, precip_s) %>% 
=======
=======
>>>>>>> 7e5a451... adding h-line to growth plots
#### ensure that the structure of the variables is correct ####
CO_poly_all <- CO_poly_all %>% 
  dplyr::select(species, quad, year_t, area_t, survives_tplus1, nearEdge_t, area_tplus1, neighbors_5_s, neighbors_10_s, neighbors_15_s, neighbors_20_s, size_t_log, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform, precip_s,  size_tplus1_log) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year_t), nearEdge_t = as.factor(nearEdge_t))

<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s) %>% 
>>>>>>> a4403b8... update modeling script
=======
CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform_s, SPEI_uniform, SPEI_unique) %>% 
>>>>>>> f170c74... checked models with uniform SPEI
=======
CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform_s, SPEI_uniform, SPEI_unique, precip_s) %>% 
>>>>>>> fa7c121... updating analysis and figure code
=======
CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s, SPEI_uniform, precip_s) %>% 
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year), survives_tplus1 = as.integer(survives), nearEdge_t = as.integer(nearEdge)) 
  
#### testing viability of different neighborhood distance radii####
#simple model using TLP for graminoids 
<<<<<<< HEAD
<<<<<<< HEAD
mNeigh5 <- glmer(survives_tplus1 ~ size_t_log + neighbors_5_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh10 <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh15 <- glmer(survives_tplus1 ~ size_t_log + neighbors_15_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh20 <- glmer(survives_tplus1 ~ size_t_log + neighbors_20_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
=======
mNeigh5 <- glmer(survives_tplus1 ~ area_s + neighbors_5_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
=======
mNeigh5 <- glmer(survives_tplus1 ~ size_t_log + neighbors_5_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

mNeigh10 <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh15 <- glmer(survives_tplus1 ~ size_t_log + neighbors_15_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

<<<<<<< HEAD
mNeigh20 <- glmer(survives_tplus1 ~ area_s + neighbors_20_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> a4403b8... update modeling script
=======
mNeigh20 <- glmer(survives_tplus1 ~ size_t_log + neighbors_20_s + SPEI_s + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

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
<<<<<<< HEAD
=======
>>>>>>> d8181b2... checking numbers in tables of model results
AIC(mNeigh5, mNeigh10, mNeigh15, mNeigh20)

#### Model validation functions ####
### for glmer models (survival)(Can't make REML = FALSE) 
glmerInfo <- function(mod) {
  #'mod' is the fitted lmer model
  # output is stored as a list
  list("summary" = summary(mod, cor = FALSE), #see the model summary)
<<<<<<< HEAD
<<<<<<< HEAD
       "anova" = anova(mod, method = "Chisq"),
       "rSquared" = r.squaredGLMM(mod))
}



=======
       "fixedEff" = drop1(mod,test="Chisq"), #see the effect of the fixed effects (as estimated by single-term deletion of fixed-effects) 
       "randEff" = rand(mod), #see the effect of the random effects (as estimated by single-term deletion of random effects)
       "confInts" = confint(mod), #computing profile confidence intervals for these models
       "rSquared" = r.squaredGLMM(mod))
}

>>>>>>> d8181b2... checking numbers in tables of model results
=======
       "anova" = anova(mod, method = "Chisq"),
       "rSquared" = r.squaredGLMM(mod))
}



>>>>>>> fc1b797... refined model script
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
<<<<<<< HEAD
=======
>>>>>>> a4403b8... update modeling script
=======
>>>>>>> d8181b2... checking numbers in tables of model results

#####Graminoid Survival Models #####
### rename polygon dataset
CO_grams <- CO_poly_all

### TLP graminoid model ###
#global model
CO_poly_TLP <- CO_grams %>% 
  filter(!is.na(TLP_s))

<<<<<<< HEAD
<<<<<<< HEAD
mSurvTLP_grams <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvTLP_grams)
=======
m1_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m1_grams)
rsquared_m1_grams <- piecewiseSEM::rsquared(m1_grams)
=======
mSurvTLP_grams <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvTLP_grams)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

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

<<<<<<< HEAD
>>>>>>> f170c74... checked models with uniform SPEI
#calculate R2 
rsquaredm1 <- piecewiseSEM::rsquared(m1_grams)


>>>>>>> a4403b8... update modeling script
=======
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections
### LDMC graminoid model ###
CO_poly_LDMC <- CO_grams %>% 
  filter(!is.na(LDMC_s))

<<<<<<< HEAD
<<<<<<< HEAD
mSurvLDMC_grams <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  size_t_log + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvLDMC_grams)
=======
m2_grams <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  area_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(m2_grams)

#calculate the R2 
rsquaredm2 <- piecewiseSEM::rsquared(m2_grams)
>>>>>>> a4403b8... update modeling script
=======
mSurvLDMC_grams <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  size_t_log + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvLDMC_grams)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

### SLA graminoid model ###
CO_poly_SLA <- CO_grams %>% 
  filter(is.na(SLA_s)==FALSE)

<<<<<<< HEAD
<<<<<<< HEAD
mSurvSLA_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * SLA_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvSLA_grams)
=======
m5 <- glmer(survives_tplus1 ~ area_s + SPEI_s * SLA_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m5)

#calculate the R2 
rsquaredm5 <- piecewiseSEM::rsquared(m5)
>>>>>>> a4403b8... update modeling script
=======
mSurvSLA_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * SLA_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvSLA_grams)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

### RDMC graminoid model ###
CO_poly_RDMC <-
  CO_grams %>% 
  filter(is.na(RDMC_s)==FALSE)

<<<<<<< HEAD
<<<<<<< HEAD
mSurvRDMC_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RDMC_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_grams)
=======
m9 <- glmer(survives_tplus1 ~ area_s + SPEI_s * RDMC_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m9)

#calculate the R2 
rsquaredm9 <- piecewiseSEM::rsquared(m9)
>>>>>>> a4403b8... update modeling script
=======
mSurvRDMC_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RDMC_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_grams)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

### RTD graminoid model ###
CO_poly_RTD <-
  CO_grams %>% 
  filter(is.na(RTD_s)==FALSE)

<<<<<<< HEAD
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

<<<<<<< HEAD
mslist <- dredge(m10)
mslist
# here you can see all of the models compared and they are ranked in order
# here we see we have multiple models <2 delta AIC from the top model 
topmod<-get.models(mslist, subset=cumsum(weight) <= .95)


out.put<-model.sel(m1,m2,m3,m4,m5)
out.put #models 4 and 5 are the best by a lot. 


>>>>>>> a4403b8... update modeling script
=======
>>>>>>> fa7c121... updating analysis and figure code
=======
mSurvRTD_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
# removed random slope for area, since the model didn't converge
summary(mSurvRTD_grams)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

### SRL graminoid model ###
CO_poly_SRL <-
  CO_grams %>% 
  filter(is.na(SRL_s)==FALSE)

<<<<<<< HEAD
<<<<<<< HEAD
mSurvSRL_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
#removed random slope for area, since the model doesn't converge with it

=======
m13 <- glmer(survives_tplus1 ~ area_s + SPEI_s * SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
#removed random slope for area, since the model doesn't converge with it

#calculate the R2 
rsquaredm13 <- piecewiseSEM::rsquared(m13)

>>>>>>> a4403b8... update modeling script
=======
mSurvSRL_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
#removed random slope for area, since the model doesn't converge with it

>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections
### RDiam graminoid model ###
CO_poly_Diam <-
  CO_grams %>% 
  filter(is.na(RDiam_s)==FALSE)

<<<<<<< HEAD
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
=======
mSurvRDiam_grams <- glmer(survives_tplus1 ~ size_t_log + SPEI_s * RDiam_s + neighbors_10_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) , data = CO_poly_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

#### Point Survival Models ####
### TLP point model ###
options(na.action = na.omit)
CO_point_TLP <- CO_point_all %>% 
  filter(!is.na(TLP_s))

mSurvTLP_forbs <- glmer(survives_tplus1 ~ SPEI_s*TLP_s + neighbors_10_s + nearEdge_t 
              + (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

 summary(mSurvTLP_forbs)

<<<<<<< HEAD
### LDMC point model ###
>>>>>>> a4403b8... update modeling script
=======
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections
#for LDMC
CO_point_LDMC <- CO_point_all %>% 
  filter(!is.na(LDMC_s))

<<<<<<< HEAD
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
=======
mSurvLDMC_forbs <- glmer(survives_tplus1 ~ SPEI_s*LDMC_s + neighbors_10_s + nearEdge_t + 
               (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvLDMC_forbs)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

#SLA for forbs
CO_point_SLA <- CO_point_all %>% 
  filter(is.na(SLA_s)==FALSE)
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

mSurvSLA_forbs <- glmer(survives_tplus1 ~ SPEI_s*SLA_s + neighbors_10_s + nearEdge_t  + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(mSurvSLA_forbs)
<<<<<<< HEAD
=======
m6 <- glmer(survives_tplus1 ~ SPEI_s*SLA_s + neighbors_10_s + nearEdge_t  + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m6)
>>>>>>> a4403b8... update modeling script
=======
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

#### test forb survival root traits ####

#rdmc for forbs
CO_point_RDMC <-
  CO_point_all %>% 
  filter(is.na(RDMC_s)==FALSE)

<<<<<<< HEAD
<<<<<<< HEAD
mSurvRDMC_forbs <- glmer(survives_tplus1 ~ SPEI_s*RDMC_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_forbs)
=======
m11 <- glmer(survives_tplus1 ~ SPEI_s*RDMC_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m11)
>>>>>>> a4403b8... update modeling script
=======
mSurvRDMC_forbs <- glmer(survives_tplus1 ~ SPEI_s*RDMC_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t),data = CO_point_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRDMC_forbs)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

#rtd for forbs
CO_point_RTD <-
  CO_point_all %>% 
  filter(is.na(RTD_s)==FALSE)

<<<<<<< HEAD
<<<<<<< HEAD
mSurvRTD_forbs <- glmer(survives_tplus1 ~ SPEI_s*RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRTD_forbs)
=======
m12 <- glmer(survives_tplus1 ~ SPEI_s*RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m12)
>>>>>>> a4403b8... update modeling script
=======
mSurvRTD_forbs <- glmer(survives_tplus1 ~ SPEI_s*RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(mSurvRTD_forbs)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

#SRL for forbs
CO_point_SRL <-
  CO_point_all %>% 
  filter(is.na(SRL_s)==FALSE)

<<<<<<< HEAD
<<<<<<< HEAD
mSurvSRL_forbs <- glmer(survives_tplus1 ~ SPEI_s*SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
=======
m15 <- glmer(survives_tplus1 ~ SPEI_s*SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> a4403b8... update modeling script
=======
mSurvSRL_forbs <- glmer(survives_tplus1 ~ SPEI_s*SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

#root diameter for forbs
CO_point_Diam <-
  CO_point_all %>% 
  filter(is.na(RDiam_s)==FALSE)

<<<<<<< HEAD
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
=======
mSurvRDiam_forbs <- glmer(survives_tplus1 ~ SPEI_s*RDiam_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_point_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections


#### fit survival models w/out traits ####
mSurvTLP_grams_NO <- glmer(survives_tplus1 ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t) ,
                  data = (CO_poly_TLP), family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

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
<<<<<<< HEAD
  filter(!is.na(CO_grams$logDiffArea))
>>>>>>> a4403b8... update modeling script
=======
  filter(!is.na(CO_grams$size_tplus1_log))
>>>>>>> d8181b2... checking numbers in tables of model results

## TLP model
#global model
options(na.action = "na.omit")

CO_grow_TLP <-
  CO_poly_growth %>% 
  filter(is.na(TLP_s)==FALSE)

<<<<<<< HEAD
<<<<<<< HEAD
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

<<<<<<< HEAD
#### testing different growth response variables ####
## log-transformed area in year t: size_t_log
## log-transformed area in year t+1: size_tplus1_log

#still using logDiffArea as response, but including a random slope and fixed effect for size_t
mGrow_GROWTH <- lme4::lmer(logDiffArea ~ size_t_log +neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

#compare the results of the above model to the model using size_t+1 as the response variable
=======
mGrowTLP<- lme4::lmer(logDiffArea ~ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))
=======
mGrowTLP<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))
>>>>>>> 1ea0fe1... udpate growth models to have size in t+1 as response variable
=======
mGrowTLP<- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

#LDMC model
CO_grow_LDMC<-
  CO_poly_growth %>% 
  filter(is.na(LDMC_s)==FALSE)

mGrowLDMC <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC , control=lmerControl(optimizer="bobyqa"))

#SLA model
CO_grow_SLA <-
  CO_poly_growth %>% 
  filter(is.na(SLA_s)==FALSE)

mGrowSLA <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * SLA_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lmerControl(optimizer="bobyqa"))

#RDMC model
CO_grow_RDMC <-
  CO_poly_growth %>% 
  filter(is.na(RDMC_s)==FALSE)

mGrowRDMC <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s * RDMC_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC , control=lmerControl(optimizer="bobyqa"))

#SRL model
CO_grow_SRL <-
  CO_poly_growth %>% 
  filter(is.na(SRL_s)==FALSE)

mGrowSRL <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * SRL_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

#RTD model
CO_grow_RTD<-
  CO_poly_growth %>% 
  filter(is.na(RTD_s)==FALSE)

mGrowRTD  <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * RTD_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

#RDiam model
CO_grow_RDiam<-
  CO_poly_growth %>% 
  filter(is.na(RDiam_s)==FALSE)
<<<<<<< HEAD
<<<<<<< HEAD
mGrowRDiam  <- lme4::lmer(logDiffArea ~  neighbors_10_s + SPEI_s * RDiam_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))
>>>>>>> a4403b8... update modeling script
=======
mGrowRDiam  <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s * RDiam_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))
>>>>>>> 1ea0fe1... udpate growth models to have size in t+1 as response variable
=======
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

mGrowRDiam  <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s * RDiam_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))

=======
>>>>>>> 8e68e80... update figures for new growth model version
#### testing different growth response variables ####
## log-transformed area in year t: size_t_log
## log-transformed area in year t+1: size_tplus1_log

#still using logDiffArea as response, but including a random slope and fixed effect for size_t
mGrow_GROWTH <- lme4::lmer(logDiffArea ~ size_t_log +neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

#compare the results of the above model to the model using size_t+1 as the response variable

#### make growth models without traits for comparison ####
#fit models

<<<<<<< HEAD
<<<<<<< HEAD
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
=======
mGrowTLP_NO <- lme4::lmer(area_tplus1_s ~  area_s + neighbors_10_s + SPEI_s  + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_grow_TLP, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)
>>>>>>> 1ea0fe1... udpate growth models to have size in t+1 as response variable
=======
mGrowTLP_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections

mGrowLDMC_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSLA_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SLA, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowRDMC_NO <- lme4::lmer(size_tplus1_log ~  size_t_log + neighbors_10_s + SPEI_s  + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)

mGrowSRL_NO <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_SRL , control=lmerControl(optimizer="bobyqa"))

mGrowRTD_NO  <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RTD , control=lmerControl(optimizer="bobyqa"))

mGrowRDiam_NO  <- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + SPEI_s + nearEdge_t + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam , control=lmerControl(optimizer="bobyqa"))

#compare AIC of models with and without traits


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
traits <- c("TLP", "AvgDiam_mm", "RTD_g_cmSurvTLP_forbs", "RDMC_g_g", "SRL_best_m_g", "LDMC_g_g", "SLA_adj_cm2_g")

#calculate a correlation matrix
allSpp_corMatrix <- cor(traits_modSpp[,names(traits_modSpp) %in% traits], use="complete.obs")
stargazer(allSpp_corMatrix)

#correlation matrix for traits in polygon analysis
polySpp_corMatrix <- cor(CO_traits[CO_traits$species %in% polySpp,names(traits_modSpp) %in% traits], use="complete.obs")

#correlation matrix for traits in point analysis
pointSpp_corMatrix <- cor(CO_traits[CO_traits$species %in% pointSpp,names(traits_modSpp) %in% traits], use="complete.obs")

#### tables of model results ####
#for graminoidsurvival  models
require(stargazer)
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
          ), omit.table.layout = c("-"),
          omit.stat = c("bic", "ll")
          )
#get random effect coefficients
sjPlot::tab_model(mSurvTLP_grams, mSurvLDMC_grams, mSurvSLA_grams, mSurvRDMC_grams, mSurvRTD_grams, mSurvSRL_grams, mSurvRDiam_grams, show.se = TRUE)

diff(AIC(mSurvTLP_grams, mSurvTLP_grams_NO)$AIC) #TLP
anova(mSurvTLP_grams, mSurvTLP_grams_NO, test="Chisq")
diff(AIC(mSurvLDMC_grams, mSurvLDMC_grams_NO)$AIC) #LDMC
anova(mSurvLDMC_grams, mSurvLDMC_grams_NO, test="Chisq")
diff(AIC(mSurvSLA_grams, mSurvSLA_grams_NO)$AIC) #SLA
diff(AIC(mSurvRDMC_grams, mSurvRDMC_grams_NO)$AIC) #RDMC
diff(AIC(mSurvRTD_grams, mSurvRTD_grams_NO)$AIC) #RTD
diff(AIC(mSurvSRL_grams, mSurvSRL_grams_NO)$AIC) #SRL
diff(AIC(mSurvRDiam_grams, mSurvRDiam_grams_NO)$AIC) #RDiam

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
diff(AIC(mSurvLDMC_forbs, mSurvLDMC_forbs_NO)$AIC) #LDMC
diff(AIC(mSurvSLA_forbs, mSurvSLA_forbs_NO)$AIC) #SLA
diff(AIC(mSurvRDMC_forbs, mSurvRDMC_forbs_NO)$AIC) #RDMC
diff(AIC(mSurvRTD_forbs, mSurvRTD_forbs_NO)$AIC) #RTD
diff(AIC(mSurvSRL_forbs, mSurvSRL_forbs_NO)$AIC) #SRL
diff(AIC(mSurvRDiam_forbs, mSurvRDiam_forbs_NO)$AIC) #RDiam

#for graminoid growth models
diff(AIC(mGrowTLP, mGrowTLP_NO)$AIC) #TLP
diff(AIC(mGrowLDMC, mGrowLDMC_NO)$AIC) #LDMC
diff(AIC(mGrowSLA, mGrowSLA_NO)$AIC) #SLA
diff(AIC(mGrowRDMC, mGrowRDMC_NO)$AIC) #RDMC
diff(AIC(mGrowRTD, mGrowRTD_NO)$AIC) #RTD
diff(AIC(mGrowSRL, mGrowSRL_NO)$AIC) #SRL
diff(AIC(mGrowRDiam, mGrowRDiam_NO)$AIC) #RDiam
<<<<<<< HEAD
stargazer(mGrowTLP, mGrowLDMC, mGrowSLA, mGrowRDMC, mGrowRTD, mGrowSRL, mGrowRDiam)
sjPlot::tab_model(mGrowTLP, mGrowLDMC, mGrowSLA, mGrowRDMC, mGrowRTD, mGrowSRL, mGrowRDiam)
>>>>>>> a4403b8... update modeling script

#### Get Correlation TLP and LDMC for polygons (graminoids) ####
# "traits_Not_dups" is the data.frame with the species-level trait averages
# make sure that it is subset for the species we used in our analysis
polySpp <- as.character(unique(CO_grams$species))
pointSpp <- as.character(unique(CO_point_all$species))

<<<<<<< HEAD
<<<<<<< HEAD
#get trait data
# datWD <-  #set working directory for location of flowering time data file
=======
#get trait data
datWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files" #set working directory for location of flowering time data file
>>>>>>> d1625fe... try different precip models
setwd(datWD)
CO_traits <- read.csv("./CO_mean_traits.csv", stringsAsFactors = FALSE)

#l
<<<<<<< HEAD
=======
>>>>>>> a4403b8... update modeling script
=======
>>>>>>> d1625fe... try different precip models
#get trait correlations for all species used in the models
traits_modSpp <- CO_traits[CO_traits$species %in% c(polySpp, pointSpp),]

mComp <- lm(data = traits_modSpp, TLP ~ LDMC_g_g)

plot(x = traits_modSpp$LDMC_g_g, 
     y = traits_modSpp$TLP)
abline(reg = mComp)

#traits of interest
<<<<<<< HEAD
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
=======

stargazer(mGrowTLP, mGrowLDMC, mGrowSLA, mGrowRDMC, mGrowRTD, mGrowSRL, mGrowRDiam, style = "all2", column.labels = c("TLP", "LDMC", "SLA", "RDMC","RTD", "SRL", "RDiam"), dep.var.labels = c("P(Survival)"), digits = 2, model.numbers = FALSE, report = c("vc*"), omit = c("TLP_s", "LDMC_s", "SLA_s", "RDMC_s", "RTD_s", "SRL_s", "RDiam_s"), 
<<<<<<< HEAD
>>>>>>> 677fc22... update effect plots (hopefully for the last time...)
=======
          #type = "text",
>>>>>>> fc1b797... refined model script
          add.lines =  list(
            Trait = c("Trait", "-0.17**", "0.13", "0.05", "0.05", "-0.20**", "-0.09", "0.02") ,
            Blank = c("", "", "", "", "", "", "", ""),
            TraitBySPEI = c("Trait:SPEI", "0.01" , "-0.02", "-0.004", "-0.01", "0.01", "0.003", "-0.02"),
            Blank = c("", "", "", "", "", "", "", ""),
            deltaAIC <- c("Delta AIC", "-11.02", "-10.40", "-13.16", "-12.25", "-9.55", "-12.66", "-12.39")
          ), omit.table.layout = c("-"),
          omit.stat = c("bic", "ll"))
sjPlot::tab_model(mGrowTLP, mGrowLDMC, mGrowSLA, mGrowRDMC, mGrowRTD, mGrowSRL, mGrowRDiam)
<<<<<<< HEAD

  #### save output to use in figure script ####vc s/CO-Sgs-paper/scripts" #location where you'll put the environment data file
setwd(path)
save.image('script4_output.RData')
=======
traits <- c("TLP", "AvgDiam_mm", "RTD_g_cm3", "RDMC_g_g", "SRL_best_m_g", "LDMC_g_g", "SLA_adj_cm2_g")
=======
traits <- c("TLP", "AvgDiam_mm", "RTD_g_cmSurvTLP_forbs", "RDMC_g_g", "SRL_best_m_g", "LDMC_g_g", "SLA_adj_cm2_g")
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections
pairs(traits_modSpp[,traits], lower.panel = panel.cor)
allSpp_corMatrix <- cor(traits_modSpp[,traits], use="complete.obs")
stargazer(allSpp_corMatrix)

#correlation matrix for traits in polygon analysis
polySpp_corMatrix <- cor(CO_traits[CO_traits$species %in% polySpp,traits], use="complete.obs")

#correlation matrix for traits in point analysis
pointSpp_corMatrix <- cor(CO_traits[CO_traits$species %in% pointSpp,traits], use="complete.obs")
=======
>>>>>>> 677fc22... update effect plots (hopefully for the last time...)

<<<<<<< HEAD

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

<<<<<<< HEAD
m1GAMM <- gamm(survives_tplus1 ~ )
>>>>>>> f170c74... checked models with uniform SPEI
=======
## is the variation between years greater than the variation between species in one year?
testM_forb <- lm(SPEI_unique ~ year_t + species, data = forbsSPEI)
anova(testM_forb)
<<<<<<< HEAD
>>>>>>> d1625fe... try different precip models
=======

#### try species-level growth models ####
options(na.action = "na.omit")
 

mGrowTLP_ARILON<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t  + (1|quad) + (1|year_t), data = CO_grow_TLP[CO_grow_TLP$species == "Aristida longiseta",] , control=lmerControl(optimizer="bobyqa"))

mGrowTLP_BOUGRA<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t  + (1|quad) + (1|year_t), data = CO_grow_TLP[CO_grow_TLP$species == "Bouteloua gracilis",] , control=lmerControl(optimizer="bobyqa"))

mGrowTLP_BOUDAC<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t  + (1|quad) + (1|year_t), data = CO_grow_TLP[CO_grow_TLP$species == "Buchloe dactyloides",] , control=lmerControl(optimizer="bobyqa"))

mGrowTLP_SCHPAN<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t  + (1|quad) + (1|year_t), data = CO_grow_TLP[CO_grow_TLP$species == "Schedonnardus paniculatus",] , control=lmerControl(optimizer="bobyqa"))
#fit is Singular (only 17 data points, so makes sense...)

mGrowTLP_ELYELY<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t  + (1|quad) + (1|year_t), data = CO_grow_TLP[CO_grow_TLP$species == "Sitanion hystrix",] , control=lmerControl(optimizer="bobyqa"))

mGrowTLP_SPOCRY<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t  + (1|quad) + (1|year_t), data = CO_grow_TLP[CO_grow_TLP$species == "Sporobolus cryptandrus",] , control=lmerControl(optimizer="bobyqa"))

mGrowTLP_STICOM<- lme4::lmer(area_tplus1_s ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t  + (1|quad) + (1|year_t), data = CO_grow_TLP[CO_grow_TLP$species == "Stipa comata",] , control=lmerControl(optimizer="bobyqa"))

summary(as_lmerModLmerTest(mGrowTLP_ARILON))
summary(as_lmerModLmerTest(mGrowTLP_BOUGRA))
summary(as_lmerModLmerTest(mGrowTLP_BOUDAC))
summary(as_lmerModLmerTest(mGrowTLP_SCHPAN))
summary(as_lmerModLmerTest(mGrowTLP_ELYELY))
summary(as_lmerModLmerTest(mGrowTLP_SPOCRY))
summary(as_lmerModLmerTest(mGrowTLP_STICOM))


<<<<<<< HEAD
=======
>>>>>>> 9c17ea9... Changing model names to reflect traits and response variables used, and cleaning up code to remove unnecessary/exploratory sections
#### save output to use in figure script ####
path = "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper/scripts" #location where you'll put the environment data file
=======
  #### save output to use in figure script ####vc s/CO-Sgs-paper/scripts" #location where you'll put the environment data file
>>>>>>> d8181b2... checking numbers in tables of model results
setwd(path)
save.image('script4_output.RData')
>>>>>>> e46be99... Making / updating R markdown for Dana, Peter, and Daniel
