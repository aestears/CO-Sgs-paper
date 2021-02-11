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
datWD <- setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper") #set path for the location of the environment image of script 3 output
setwd(datWD)
#load data from previous 
load("./scripts/script3_output.RData")

#### ensure that the structure of the variables is correct ####
CO_poly_all <- CO_poly_all %>% 
  dplyr::select(species, quad, year_t, area_t, survives_tplus1, nearEdge_t, area_tplus1, neighbors_5_s, neighbors_10_s, neighbors_15_s, neighbors_20_s, area_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year_t), nearEdge_t = as.factor(nearEdge_t))

CO_point_all <- CO_point_all %>% dplyr::select(species, quad, year, survives, nearEdge,  neighbors_10_s, neighbors_15_s, neighbors_20_s, RTD_s, RDMC_s, SLA_s, SPEI_s, LDMC_s, TLP_s, SRL_s, RDiam_s) %>% 
  mutate(species = as.factor(species), quad = as.factor(quad), year_t = as.factor(year), survives_tplus1 = as.integer(survives), nearEdge_t = as.integer(nearEdge)) 
  
#### testing viability of different neighborhood distance radii####
#simple model using TLP for graminoids 
mNeigh5 <- glmer(survives_tplus1 ~ area_s + neighbors_5_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh10 <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh15 <- glmer(survives_tplus1 ~ area_s + neighbors_15_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

mNeigh20 <- glmer(survives_tplus1 ~ area_s + neighbors_20_s + SPEI_s + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#compare the coefficients for the different radii
NeCoeff <- data.frame(Radius = c(5,10,15,20), Coeff = c(-0.52799, -0.59496, -0.55743, -0.52326))

plot(Coeff ~ Radius, data = NeCoeff, type = "l")
ggplot(data = NeCoeff) +
  geom_point(aes(Radius, Coeff)) +
  xlab("Neighborhood Radius From Focal Indivdiual (cm)") +
  ylab("Model Coefficient") +
  theme_classic()
#the coefficient for a radius of 10cm was the 'largest' (most negative), so use 10_cm


# #####Polygon Survival Models #####
# ### TLP polygon model ###
# #global model
# m1 <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year), data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
# summary(m1)
# rsquared_m1 <- piecewiseSEM::rsquared(m1)
# 
# #plot residuals to assess model fit
# plot_model(m1, type = "diag") #get qqplots for random effects and fixed effects
# 
# #don't have to calculate overdispersion--not possible for bernoulli binomial variables
# # https://stats.stackexchange.com/questions/206007/can-there-be-overdispersion-in-a-logistic-regression-model-where-each-observatio
# 
# #plot residuals vs. fitted data
# ##tried to include Tribe as a random effect, but the model wouldn't converge--I think there is too much variation in Tribe (7 Tribes, 9 species)
# #The r-squared is good enough to continue with this as a global model 
# 
# #test different random effect structures
# m1A <- glmer(survives_tplus1 ~ area_s +  neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad) + (1|year), data = CO_poly_all, family = binomial(link = logit))
# m1B <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad),  data = CO_poly_all, family = binomial(link = logit))
# m1C <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|year), data = CO_poly_all, family = binomial(link = logit))
# m1D <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) , data = CO_poly_all, family = binomial(link = logit))
# m1E <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|year), data = CO_poly_all, family = binomial(link = logit))
# m1F <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) ,  data = CO_poly_all, family = binomial(link = logit))
# m1G <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (area_s|species), data = CO_poly_all, family = binomial(link = logit))
# m1H <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (1|quad) , data = CO_poly_all, family = binomial(link = logit))
# m1I <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (1|year), data = CO_poly_all, family = binomial(link = logit))
# m1Q <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|year) + (1|quad),  data = CO_poly_all, family = binomial(link = logit))
# m1U <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * TLP_s + nearEdge_t , 
#              data = CO_poly_all, family = binomial(link = logit))
# #using AIC to compare models 
# #compare the random effect structures using AIC
# #can't cahnge REML for glmer models, so...  not an issue?
# #want to use marginal AIC (AICc) https://onlinelibrary.wiley.com/doi/full/10.1111/j.1420-9101.2010.02210.x?
# 
# m1_AIC <- MuMIn::AICc(m1, m1A, m1B, m1C, m1D, m1E, m1F, m1G, m1H, m1I, m1Q, m1U)
# m1_AIC[m1_AIC$AICc==min(m1_AIC$AICc),]
# 
# #m1Q (same as m1) has the lowest AIC, so use the random effects structure without a random intercept for Tribe
# #Look at the QQ Plot for m1
# #The model fits pretty well! 
# 
# #for selection of fixed effects, use a global model with REML = FALSE (actually not possible, REML is not defined for GLMER models) http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html 
# 
# options(na.action = "na.fail")
# 
# mslist <- dredge(m1)
# # here you can see all of the models compared and they are ranked in order
# # here we see we have multiple models <2 delta AIC from the top model 
# topmod<-get.models(mslist, subset=cumsum(weight) <= .95)
# topmod <- get.models(mslist, subset = 1) # here you can see all of the models that make up 95% of the model weight
# #avgm1 <- model.avg(topmod)
# #best model statement (survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s + TLP_s +  SPEI_s:TLP_s + (area_s | species) + (1 | quad) + (1 | year) )--same as model 1, but w/out term for nearest edge
# bestMod1 <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s * TLP_s  + (area_s|species) + (1|quad) + (1|year) ,
#                   data = CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
# summary(bestMod1)
# # tells you which models went into this and gives model-averaged coefficients with p-values
# # also gives relative importance
# # use full average, not conditional
# m1conf <- lme4::confint.merMod(bestMod1)
# # gives confidence intervals
# #The confidence intervals suggest that area is the only significant coefficient (95% CI doesn't overlap 0)
# 
# plot_model(bestMod1, type = "diag")
# 
# #/////////////////////////////////
# 
# ### LDMC polygon model ###
# m2 <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  area_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year), data=CO_poly_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
# 
# plot_model(m2, type = "diag")
# 
# #test different random effect structures
# m2A <- glmer(survives_tplus1 ~ area_s +  neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|quad) + (1|year), data = CO_poly_all, family = binomial(link = logit))
# m2B <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|quad),  data = CO_poly_all, family = binomial(link = logit))
# m2C <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|year), data = CO_poly_all, family = binomial(link = logit))
# m2D <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) , data = CO_poly_all, family = binomial(link = logit))
# m2E <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (area_s|species) + (1|year), data = CO_poly_all, family = binomial(link = logit))
# m2F <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (area_s|species) + (1|quad) ,  data = CO_poly_all, family = binomial(link = logit))
# m2G <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (area_s|species), data = CO_poly_all, family = binomial(link = logit))
# m2H <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (1|quad) , data = CO_poly_all, family = binomial(link = logit))
# m2I <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (1|year), data = CO_poly_all, family = binomial(link = logit))
# m2Q <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (area_s|species) + (1|year) + (1|quad),  data = CO_poly_all, family = binomial(link = logit))
# m2U <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t , 
#              data = CO_poly_all, family = binomial(link = logit))
# #using AIC to compare models 
# #compare the random effect structures using AIC
# m2_AIC <- MuMIn::AICc(m2, m2A, m2B, m2C, m2D, m2E, m2F, m2G, m2H, m2I, m2Q, m2U)
# m2_AIC[m2_AIC$AICc==min(m2_AIC$AICc),]
# #the global model is the best 
# 
# #QQ Plot for m2
# qqnorm(resid(m2))
# qqline(resid(m2))
#    
# #dredge for model selection                                                                             
# options(na.action = "na.fail")
# mslist2 <- dredge(m2)
# # here you can see all of the models compared and they are ranked in order
# # here we see we have multiple models <2 delta AIC from the top model 
# topmod2<-get.models(mslist2, subset=delta=="0")
# topmod2 # here you can see all of the models that make up 95% of the model weight
# #don't need to do model averaging, since there is only one model
# #testing both random effect structures and fixed effect structures tells us the global model fixed effect structure is best
# summary(m2)
# # gives confidence intervals
# m2conf <- lme4::confint.merMod(m2)
# rsquaredm2 <- rsquared(m2)
# plot_model(m2, type = "diag")

#/////////////////////////////////
#### test models for polygons w/out C. lanata and E. effusum data ####
# (so it is just graminoids)

### subset polygon dataset
CO_grams <- CO_poly_all[CO_poly_all$species %in% c("Aristida longiseta", "Bouteloua gracilis", "Buchloe dactyloides","Carex eleocharis", "Schedonnardus paniculatus", "Sitanion hystrix", "Sporobolus cryptandrus", "Stipa comata" ) , ]

### TLP polygon model ###
#global model
CO_poly_TLP <- CO_grams %>% 
  filter(!is.na(TLP_s))

m1_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m1_grams)
rsquared_m1_grams <- piecewiseSEM::rsquared(m1_grams)

#plot residuals to assess model fit
plot_model(m1_grams, type = "diag") #get qqplots for random effects and fixed effects

#don't have to calculate overdispersion--not possible for bernoulli binomial variables
# https://stats.stackexchange.com/questions/206007/can-there-be-overdispersion-in-a-logistic-regression-model-where-each-observatio

#plot residuals vs. fitted data
##tried to include Tribe as a random effect, but the model wouldn't converge--I think there is too much variation in Tribe (7 Tribes, 9 species)
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
#using AIC to compare models 
#compare the random effect structures using AIC
#can't cahnge REML for glmer models, so...  not an issue?
#want to use marginal AIC (AICc) https://onlinelibrary.wiley.com/doi/full/10.1111/j.1420-9101.2010.02210.x?

m1_AIC_grams <- MuMIn::AICc(m1_grams, m1A_grams, m1B_grams, m1C_grams, m1D_grams, m1E_grams, m1F_grams, m1G_grams, m1H_grams, m1I_grams, m1Q_grams, m1U_grams)
m1_AIC_grams[m1_AIC_grams$AICc==min(m1_AIC_grams$AICc),]

#m1Q (same as m1) has the lowest AIC, so use the random effects structure without a random intercept for Tribe
#Look at the QQ Plot for m1
#The model fits pretty well! 


#/////////////////////////////////

### LDMC polygon model ###
CO_poly_LDMC <- CO_grams %>% 
  filter(!is.na(LDMC_s))

m2_grams <- glmer(survives_tplus1 ~ SPEI_s * LDMC_s +  area_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

plot_model(m2, type = "diag")

#test different random effect structures
m2A_grams <- glmer(survives_tplus1 ~ area_s +  neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t+ (1|species) + (1|quad) + (1|year_t), data = CO_grams, family = binomial(link = logit))
m2B_grams <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|quad),  data = CO_grams, family = binomial(link = logit))
m2C_grams <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|year_t), data = CO_grams, family = binomial(link = logit))
m2D_grams <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) , data = CO_grams, family = binomial(link = logit))
m2E_grams <- glmer(survives_tplus1 ~ area_s+ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (area_s|species) + (1|year_t), data = CO_grams, family = binomial(link = logit))
m2F_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (area_s|species) + (1|quad) ,  data = CO_grams, family = binomial(link = logit))
m2G_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (area_s|species), data = CO_grams, family = binomial(link = logit))
m2H_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (1|quad) , data = CO_grams, family = binomial(link = logit))
m2I_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (1|year_t), data = CO_grams, family = binomial(link = logit))
m2Q_grams <- glmer(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t + (area_s|species) + (1|year_t) + (1|quad),  data = CO_grams, family = binomial(link = logit))
m2U_grams <- glm(survives_tplus1 ~ area_s + neighbors_10_s+ SPEI_s * LDMC_s + nearEdge_t , 
             data = CO_grams, family = binomial(link = logit))
#using AIC to compare models 
#compare the random effect structures using AIC
m2_AIC_grams <- MuMIn::AICc(m2_grams, m2A_grams, m2B_grams, m2C_grams, m2D_grams, m2E_grams, m2F_grams, m2G_grams, m2H_grams, m2I_grams, m2Q_grams, m2U_grams)
m2_AIC_grams[m2_AIC_grams$AICc==min(m2_AIC_grams$AICc),]
#the global model is the best 

#QQ Plot for m2
qqnorm(resid(m2_grams))
qqline(resid(m2_grams))

# #dredge for model selection                                                                             
# options(na.action = "na.fail")
# mslist2 <- dredge(m2)
# # here you can see all of the models compared and they are ranked in order
# # here we see we have multiple models <2 delta AIC from the top model 
# topmod2<-get.models(mslist2, subset=delta=="0")
# topmod2 # here you can see all of the models that make up 95% of the model weight
# #don't need to do model averaging, since there is only one model
# #testing both random effect structures and fixed effect structures tells us the global model fixed effect structure is best
# summary(m2)
# # gives confidence intervals
# m2conf <- lme4::confint.merMod(m2)
rsquaredm2 <- piecewiseSEM::rsquared(m2_grams)
# plot_model(m2, type = "diag")

#### Models for points ####
##can't use stems, since there is no variation in that variable in the scaled-down dataset!
### TLP point model ###
CO_point_TLP <- CO_point_all %>% 
  filter(!is.na(TLP_s))
m3 <- glmer(survives_tplus1 ~ SPEI_s*TLP_s + neighbors_10_s + nearEdge_t 
              + (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_TLP, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m3)
rsquared(m3)
#the conditional R-squared is crazy good (.949!), but the marginal R-squared is pretty bad

#test different random effect structures
m3A <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad) + (1|year), data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m3B <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad), 
data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m3C <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|year), 
data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m3D <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|species) , 
data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m3E <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|quad) ,data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m3F <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * TLP_s + nearEdge_t + (1|year), data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#compare the random effect structures using AIC
m3_AIC <- MuMIn::AICc(m3, m3A, m3B, m3C, m3D, m3E, m3F)
m3_AIC[m3_AIC$AICc==min(m3_AIC$AICc),]
#the model m3A (without a random intercept for family) has the lowest AIC

#QQ Plot for m3
qqnorm(resid(m3A))
qqline(resid(m3A))
                
#Dredge for model selection                                                                
options(na.action = "na.fail")
mslist3 <- dredge(m3A)
# here you can see all of the models compared and they are ranked in order
# here we see we have multiple models <2 delta AIC from the top model 
topmod3<-get.models(mslist3, subset=weight>.95)
topmod3 # the first model is the one that has the most variation, so we don't need to average the model
#The best model is the one with fixed effects for TLP:SPEI and stems and nearEdge_t (no random effect for Tribe (Family))
m3Final<-glmer(survives_tplus1 ~ SPEI_s * TLP_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year), data = CO_point_all, family = binomial(link = logit),control=glmerControl(optimizer="bobyqa"))

summary(m3Final)

m3conf <- lme4::confint.merMod(m3Final)
rsquared3 <- rsquared(m3Final)
#the conditional r-squared is quite good!
qqnorm(resid(m3Final))                                                                        
qqline(resid(m3Final))

plot_model(m3Final, type = "diag")
# the qqplot is definitely better!  

#/////////////////////////////////

### LDMC point model ###
#for LDMC
CO_point_LDMC <- CO_point_all %>% 
  filter(!is.na(LDMC_s))

m4 <- glmer(survives_tplus1 ~ SPEI_s*LDMC_s + neighbors_10_s + nearEdge_t + 
               (1|species) + (1|quad) + (1|year_t), 
            data=CO_point_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

summary(m4)
plot_model(m4, type = "diag") 

#test different random effect structures                              
m4A <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|quad) + (1|year), data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m4B <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|quad), 
             data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m4C <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|year), 
             data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m4D <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) , 
             data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m4E <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|quad) ,data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
m4F <- glmer(survives_tplus1 ~ neighbors_10_s + SPEI_s * LDMC_s + nearEdge_t + (1|year), data = CO_point_all, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#compare the random effect structures using AIC
m4_AIC <- MuMIn::AICc(m4, m4A, m4B, m4C, m4D, m4E, m4F)
m4_AIC[m4_AIC$AICc==min(m4_AIC$AICc),]
#m4A model is best--the same as the global intercept

#dredge for model selection of fixed efects
options(na.action = "na.fail")
mslist4 <- dredge(m4A)
# here you can see all of the models compared and they are ranked in order
# here we see we have multiple models <2 delta AIC from the top model 
topmod4<-get.models(mslist4, subset=weight>.95)
topmod4 # the first model is the one that has the most variation, so we don't need to average the model (with all fixed effects, but no random effect for Family (Tribe))
m4Final <- glmer(survives_tplus1 ~  neighbors_10_s +  SPEI_s*LDMC_s + nearEdge_t +
                   (1|species) + (1|quad) + (1|year), data=CO_point_all, 
                 family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m4Final)
m4conf <- lme4::confint.merMod(m4Final)
rsquared4 <- rsquared(m4Final)    

plot_model(m4Final, type = "diag")

#### Summary of model results #### 
#polygon TLP
summary(bestMod1)
rsquared1 <- piecewiseSEM::rsquared(bestMod1)
m1conf <- lme4::confint.merMod(bestMod1)

#polygon LDMC
m2
summary(m2)
rsquared2 <- piecewiseSEM::rsquared(m2)
m2conf <- lme4::confint.merMod(m2)

#point TLP
m3   
summary(m3)
rsquared3 <- piecewiseSEM::rsquared(m3)  
m3conf <- lme4::confint.merMod(m3)

#point LDMC
summary(m4Final)
rsquared4 <- piecewiseSEM::rsquared(m4)
m4conf <- lme4::confint.merMod(m4)

#### visualize the survival model results #### 
visreg::visreg2d(fit=m1, xvar="TLP_s", yvar="SPEI_s", 
                 scale="response", plot.type="persp", phi=20, theta=60, 
                 xlab="\n\nTLP (scaled)", 
                 ylab="\n\nSPEI index (scaled)", 
                 zlab="\n\nSurvival Probability", cex.axis=1, cex.lab=1.2, 
                 col=adjustcolor("#019E73",alpha.f=.5))
                                                                 
visreg::visreg2d(fit=m2, xvar="LDMC_s", yvar="SPEI_s", 
                 scale="response", plot.type="persp", phi=20, theta=55, 
                 xlab="\n\nLDMC (scaled)", 
                 ylab="\n\nSPEI index (scaled)", 
                 zlab="\n\nSurvival Probability", cex.axis=1, cex.lab=1.2, 
                 col=adjustcolor("#019E73",alpha.f=.5))

##### Test survival models for non-water related traits####
# SLA for GRAMINOIDS
CO_poly_SLA <- CO_grams %>% 
  filter(is.na(SLA_s)==FALSE)

m5 <- glmer(survives_tplus1 ~ area_s + SPEI_s * SLA_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) , data = CO_poly_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m5)


#SLA for forbs
CO_point_SLA <- CO_point_all %>% 
  filter(is.na(SLA_s)==FALSE)
m6 <- glmer(survives_tplus1 ~ SPEI_s*SLA_s + neighbors_10_s + nearEdge_t  + (1|species) + (1|quad) + (1|year_t), data = CO_point_SLA, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m6)


#### test survival root traits ####
##Subset data for root traits (remove NA's for species that don't have root data)
CO_poly_RDMC <-
  CO_grams %>% 
  filter(is.na(RDMC_s)==FALSE)

#rdmc for graminoids
m9 <- glmer(survives_tplus1 ~ area_s + SPEI_s * RDMC_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t), data = CO_poly_RDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m9)

#rtd for graminoids
CO_poly_RTD <-
  CO_grams %>% 
  filter(is.na(RTD_s)==FALSE)

m10 <- glmer(survives_tplus1 ~ area_s + SPEI_s * RTD_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_RTD, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
summary(m10)

#SRL for graminoids
CO_poly_SRL <-
  CO_grams %>% 
  filter(is.na(SRL_s)==FALSE)

m13 <- glmer(survives_tplus1 ~ area_s + SPEI_s * SRL_s + neighbors_10_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t) , data = CO_poly_SRL, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

#root diameter for graminoids
CO_poly_Diam <-
  CO_grams %>% 
  filter(is.na(RDiam_s)==FALSE)

m14 <- glmer(survives_tplus1 ~ area_s + SPEI_s * RDiam_s + neighbors_10_s + nearEdge_t + (area_s|species) + (1|quad) + (1|year_t) , data = CO_poly_Diam, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
  

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
## Calculate growth variable
# differences in logs of area 
  #add '3' to each value after log transformation, so that there is no 'negative' log(size)
CO_grams$logDiffArea <- (log(CO_grams$area_tplus1)) - (log(CO_grams$area_t))
#strait differences in areas
#CO_grams$diffAreaPlain <- CO_grams$area_tplus1 - CO_grams$area_t
#ratio of size in next year to size in current year (log transformed)
#CO_grams$diffAreaRatio <- log(CO_grams$area_t/CO_grams$area_tplus1)


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

##Plot growth model results using the effects package ##
#check correlation of predictors
pairs(CO_poly_growth[,c( "neighbors_10_s", "SPEI_s", "TLP_s", "nearEdge_t")], lower.panel = panel.cor)

#visualize results
plot(predictorEffect("TLP_s", mGrowTLP, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),confint = list(style = "auto"),
     axes = list(y=list(type="response")))

plot(predictorEffect("LDMC_s", mGrowLDMC, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),confint = list(style = "auto"),
     axes = list(y=list(type="response")))

plot(predictorEffect("SLA_s", mGrowSLA, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),confint = list(style = "auto"),
     axes = list(y=list(type="response")))

plot(predictorEffect("RDMC_s", mGrowRDMC, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),confint = list(style = "auto"),
     axes = list(y=list(type="response")))

plot(predictorEffect("RDiam_s", mGrowRDiam, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),confint = list(style = "auto"),
     axes = list(y=list(type="response")))

plot(predictorEffect("SRL_s", mGrowSRL, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),confint = list(style = "auto"),
     axes = list(y=list(type="response")))

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

# 
# diff(AIC(mGrowTLP_ratio, mGrowTLP_NO)$AIC) #TLP
# diff(AIC(mGrowLDMC_ratio, mGrowLDMC_NO)$AIC) #LDMC
# diff(AIC(mGrowSLA_ratio, mGrowSLA_NO)$AIC) #SLA
# diff(AIC(mGrowRDMC_ratio, mGrowRDMC_NO)$AIC) #RDMC
# diff(AIC(mGrowRTD_ratio, mGrowRTD_NO)$AIC) #RTD
# diff(AIC(mGrowSRL_ratio, mGrowSRL_NO)$AIC) #SRL
# diff(AIC(mGrowRDiam_ratio, mGrowRDiam_NO)$AIC) #RDiam


piecewiseSEM::rsquared(mGrowTLP)
piecewiseSEM::rsquared(mGrowTLP_ratio)
piecewiseSEM::rsquared(mGrowTLP_NO)
piecewiseSEM::rsquared(mGrowLDMC)
piecewiseSEM::rsquared(mGrowLDMC_NO)
piecewiseSEM::rsquared(mGrowSLA)
piecewiseSEM::rsquared(mGrowSLA_NO)
# 
# #### Growth Models 2.0####
# #using size in t+1 as a response variable, and size in t as the predictor variable
# #subset the datasets to only include observations that have growth data (for those plants that survived)
# CO_poly_growth_2 <- CO_grams %>% 
#   filter(!is.na(area_tplus1)) %>% 
#   mutate(area_tplus1_s = log(area_tplus1))
# 
# #log-transform the response variable
# 
# 
# ## TLP model
# #global model
# options(na.action = "na.omit")
# 
# CO_grow_TLP_2 <-
#   CO_poly_growth_2 %>% 
#   filter(is.na(TLP_s)==FALSE)
# 
# mGrowTLP_2<- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + TLP_s + SPEI_s * TLP_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_TLP_2 , control=lmerControl(optimizer="bobyqa"))
# 
# summary(mGrowTLP_2)
# 
# CO_grow_LDMC_2<-
#   CO_poly_growth_2 %>% 
#   filter(is.na(LDMC_s)==FALSE)
# mGrowLDMC_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + LDMC_s + SPEI_s * LDMC_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC_2 , control=lmerControl(optimizer="bobyqa"))
# 
# CO_grow_SLA_2 <-
#   CO_poly_growth_2 %>% 
#   filter(is.na(SLA_s)==FALSE)
# mGrowSLA_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SLA_s + SPEI_s * SLA_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_SLA_2, control=lmerControl(optimizer="bobyqa"))
# 
# CO_grow_RDMC_2 <-
#   CO_poly_growth_2 %>% 
#   filter(is.na(RDMC_s)==FALSE)
# mGrowRDMC_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + RDMC_s + SPEI_s * RDMC_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC_2 , control=lmerControl(optimizer="bobyqa"))
# 
# CO_grow_SRL_2 <-
#   CO_poly_growth_2 %>% 
#   filter(is.na(SRL_s)==FALSE)
# mGrowSRL_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SRL_s + SPEI_s * SRL_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_SRL_2 , control=lmerControl(optimizer="bobyqa"))
# 
# CO_grow_RTD_2<-
#   CO_poly_growth_2 %>% 
#   filter(is.na(RTD_s)==FALSE)
# mGrowRTD_2  <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + RTD_s + SPEI_s * RTD_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RTD_2 , control=lmerControl(optimizer="bobyqa"))
# 
# CO_grow_RDiam_2 <-
#   CO_poly_growth_2 %>% 
#   filter(is.na(RDiam_s)==FALSE)
# mGrowRDiam_2  <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + RDiam_s + SPEI_s * RDiam_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam_2 , control=lmerControl(optimizer="bobyqa"))
# 
# #### make growth models without traits for comparison ####
# #fit models
# 
# mGrowTLP_NO_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_TLP_2, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)
# 
# mGrowLDMC_NO_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_LDMC_2, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)
# 
# mGrowSLA_NO_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_SLA_2, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)
# 
# mGrowRDMC_NO_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SPEI_s  + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDMC_2, control=lme4::lmerControl(optimizer="bobyqa"), na.action = na.omit)
# 
# mGrowSRL_NO_2 <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_SRL_2 , control=lmerControl(optimizer="bobyqa"))
# 
# mGrowRTD_NO_2  <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RTD_2 , control=lmerControl(optimizer="bobyqa"))
# 
# mGrowRDiam_NO_2  <- lme4::lmer(area_tplus1 ~ area_s + neighbors_10_s + SPEI_s + nearEdge_t + (1|species) + (1|quad) + (1|year_t), data = CO_grow_RDiam_2 , control=lmerControl(optimizer="bobyqa"))
# 
# #compare AIC of models with and without traits
# diff(AIC(mGrowTLP_2, mGrowTLP_NO_2)$AIC) #TLP
# diff(AIC(mGrowLDMC_2, mGrowLDMC_NO_2)$AIC) #LDMC
# diff(AIC(mGrowSLA_2, mGrowSLA_NO_2)$AIC) #SLA
# diff(AIC(mGrowRDMC_2, mGrowRDMC_NO_2)$AIC) #RDMC
# diff(AIC(mGrowRTD_2, mGrowRTD_NO_2)$AIC) #RTD
# diff(AIC(mGrowSRL_2, mGrowSRL_NO_2)$AIC) #SRL
# diff(AIC(mGrowRDiam_2, mGrowRDiam_NO_2)$AIC) #RDiam
# 
# #assess significance of the model terms in the TLP model (using a likelihood ratio test)
# drop1(mGrowTLP_2,test="Chisq")
# drop1(mGrowLDMC_2,test="Chisq")
# drop1(mGrowSLA_2,test="Chisq")
# drop1(mGrowRDMC_2,test="Chisq")
# drop1(mGrowSRL_2,test="Chisq")
# drop1(mGrowRDiam_2,test="Chisq")
# drop1(mGrowRTD_2,test="Chisq")
# 
#### make model result figures ####
#TLP
#m1 <- glmer(survives ~ area_s + neighbors_10_s + SPEI_s * TLP_s + (area_s|species) + (1|quad) + (1|year) + (1|nearEdge_t), data = CO_grams, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))
#effect of interaction on survival

#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_G <- mean(CO_grams$SPEI_s)
sdSPEI_G <- sd(CO_grams$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G) #2.10
SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G) #-1.48

#for TLP_s

plot(predictorEffect("TLP_s", m1_grams, xlevels=list(SPEI_s=c(round(SPEI_2_5_G,2), round(SPEI_97_5_G,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "green3")),
     confint = list(style = "auto"),
     axes = list(y=list(type="response", lab = "P(Grass Survival)", ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(TLP_s=list(lab = "scaled(Turgor Loss Point)"))),
     ylim = c(0,1),
     lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0), columns=1, cex=.9, cex.title=1, text = list(lab = c("Dry Year","Wet Year")),title = ""),
                  array=list(row=1, col=1, nrow=2, ncol=2, more=TRUE)), main = NA)

#for LDMC_s
plot(predictorEffect("LDMC_s", m2_grams, xlevels=list(SPEI_s=c(round(SPEI_2_5_G,2), round(SPEI_97_5_G,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "green3")),
     confint = list(style = "auto"),
     axes = list(y=list(type="response", lab = "P(Grass Survival)", ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(LDMC_s=list(lab = "scaled(Leaf Dry Matter Content)"))),
     ylim = c(0,1),
     lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0), columns=1, cex=.9, cex.title=1, text = list(lab = c("Dry Year","Wet Year")),title = ""),
                  array=list(row=1, col=2, nrow=2, ncol=2, more=TRUE)),
     main = NA)


#for forbs
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI <- mean(CO_point_all$SPEI_s)
sdSPEI <- sd(CO_point_all$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

#for TLP_s

plot(predictorEffect("TLP_s", m3Final, xlevels=list(SPEI_s=c(round(SPEI_2_5,2), round(SPEI_97_5,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "green3")),
     confint = list(style = "auto"),
     axes = list(y=list(type="response", lab = "P(Forb Survival)", ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(TLP_s=list(lab = "scaled(Turgor Loss Point)"))),
     ylim = c(0,1),lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0),
                                              columns=1,
                                              cex=.9,
                                              cex.title=1,
                                              text = list(lab = c("Dry Year","Wet Year")),title = ""),array=list(row=2, col=1, nrow=2, ncol=2, more=TRUE)),
     main = NA)

#for LDMC_s
plot(predictorEffect("LDMC_s", m4Final, xlevels=list(SPEI_s=c(round(SPEI_2_5,2), round(SPEI_97_5,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "green3")),
     confint = list(style = "auto"),
     axes = list(y=list(type="response", lab = "P(Forb Survival)", ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(LDMC_s=list(lab = "scaled(Leaf Dry Matter Content)"))),
     ylim = c(0,1),lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0),
                                              columns=1,
                                              cex=.9,
                                              cex.title=1,
                                              text = list(lab = c("Dry Year","Wet Year")),title = ""),array=list(row=2, col=2, nrow=2, ncol=2, more=FALSE)),
     main = NA)



#visreg figure
dev.off()
visreg::visreg2d(fit=m1, xvar="SPEI_s", yvar="TLP_s", 
                 scale="response", plot.type="persp", phi=20, theta=60, 
                 xlab="\n\nSPEI (scaled)", 
                 ylab="\n\nTLP index (scaled)", 
                 zlab="\n\nSurvival Probability", cex.axis=1, cex.lab=1.2, 
                 col=adjustcolor("#019E73",alpha.f=.5))

#still don't entirely trust it...
#manually calculate results and plot them 
#make data.frame for new data
grid.l_1 <- list(seq(min(CO_grams$SPEI_s), max(CO_grams$SPEI_s), length = 100),
                 seq(min(CO_grams$TLP_s), max(CO_grams$TLP_s), length = 100))
TLP.SPEI <- make.surface.grid(grid.l_1)  
#make linear predictions using m1 model results
TLP.SPEI_linear.pred_1 <- as.surface(TLP.SPEI,
                                     -0.25598 +   2.20333*mean(CO_grams$area_s) + -0.53731 * mean(CO_grams$neighbors_10_s) + 0.24638*TLP.SPEI[,1] + -0.05653*TLP.SPEI[,2] + 0.05561 *TLP.SPEI[,1] * TLP.SPEI[,2])
#model coefficients for m1: -0.25266 +  2.25437*newdata_1$area_s + -0.66912 * newdata_1$neighbors_10_s + 0.33210*newdata_1$SPEI_s + -0.06302*newdata_1$TLP_s + 0.07029 *newdata_1$SPEI_s * newdata_1$TLP_s
#plot lienar predictors
persp(TLP.SPEI_linear.pred_1$x, TLP.SPEI_linear.pred_1$y, TLP.SPEI_linear.pred_1$z, phi = 20, theta = 60)
#transform to response (not linear)
TLP.SPEI_prob.pred_1 <- as.surface(TLP.SPEI,
                                   1/(1+exp(-(-0.25598 +   2.20333*mean(CO_grams$area_s) + -0.53731 * mean(CO_grams$neighbors_10_s) + 0.24638*TLP.SPEI[,1] + -0.05653*TLP.SPEI[,2] + 0.05561 *TLP.SPEI[,1] * TLP.SPEI[,2]))))
#plot probabilites
persp(TLP.SPEI_prob.pred_1$x, TLP.SPEI_prob.pred_1$y, TLP.SPEI_prob.pred_1$z, phi = 20, theta = 60,
      ticktype = "detailed",
      main = "Impact of TLP and SPEI on grass survival",
      zlab = "\n\nsurvival",
      ylab = "\n\nTLP_s",
      xlab = "\n\nSPEI_s")

#plot the values at extremes manually
newdata_1 <- data.frame(TLP_s = seq(min(CO_grams$TLP_s), max(CO_grams$TLP_s), length = 100))

#predict values for low SPEI value
newdata_1$linear.preds_low <- -0.25598 + 2.20333*mean(CO_grams$area_s) +  -0.05653 * newdata_1$TLP_s +  0.24638 * min(CO_grams$SPEI_s) + -0.53731 * mean(CO_grams$neighbors_10_s) + 0.05561 * min(CO_grams$SPEI_s) * newdata_1$TLP_s
#predict values for high SPEI value
newdata_1$linear.preds_high <- -0.25598 + 2.20333*mean(CO_grams$area_s) +  -0.05653 * newdata_1$TLP_s +  0.24638 * max(CO_grams$SPEI_s) + -0.53731 * mean(CO_grams$neighbors_10_s) + 0.05561 * max(CO_grams$SPEI_s) * newdata_1$TLP_s

#transform to probabilites
newdata_1$probs_low <- 1/(1+exp(-newdata_1$linear.preds_low))
newdata_1$probs_high <- 1/(1+exp(-newdata_1$linear.preds_high))

plot(probs_high ~ TLP_s, data = newdata_1, type = "l",
     ylim = c(0.1,0.6))
lines(probs_low ~ TLP_s, data = newdata_1)

#LDMC
#effect of SPEI_s on survival
plot(effect("SPEI_s",m2))
plot(predictorEffect("LDMC_s", m2, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),
     confint = list(style = "auto"),
     axes = list(y=list(type="response")))


dev.off()
visreg::visreg2d(fit=m2, xvar="SPEI_s", yvar="LDMC_s", 
                 scale="response", plot.type="persp", phi=20, theta=55, 
                 xlab="\n\nSPEI (scaled)", 
                 ylab="\n\nTLP index (scaled)", 
                 zlab="\n\nSurvival Probability", cex.axis=1, cex.lab=1.2, 
                 col=adjustcolor("#019E73",alpha.f=.5))

#still don't entirely trust it...
#manually calculate results and plot them 
#make data.frame for new data
grid.l_2 <- list(seq(min(CO_grams$SPEI_s), max(CO_grams$SPEI_s), length = 100),
                 seq(min(CO_grams$LDMC_s), max(CO_grams$LDMC_s), length = 100))
LDMC.SPEI <- make.surface.grid(grid.l_2)  
#make linear predictions using m1 model results
LDMC.SPEI_linear.pred_2 <- as.surface(TLP.SPEI,
                                     -0.27309 +   2.18676*mean(CO_grams$area_s) + -0.67083 * mean(CO_grams$neighbors_10_s) + 0.223318*LDMC.SPEI[,1] + 0.08580*LDMC.SPEI[,2] + -0.12379 *LDMC.SPEI[,1] * LDMC.SPEI[,2])

#plot linear predictors
persp(LDMC.SPEI_linear.pred_2$x, LDMC.SPEI_linear.pred_2$y, LDMC.SPEI_linear.pred_2$z, phi = 20, theta = 60)
#transform to response (not linear)
LDMC.SPEI_prob.pred_2 <- as.surface(LDMC.SPEI,
                                   1/(1+exp(-(-0.27309 +   2.18676*mean(CO_grams$area_s) + -0.67083 * mean(CO_grams$neighbors_10_s) + 0.223318*LDMC.SPEI[,1] + 0.08580*LDMC.SPEI[,2] + -0.12379 *LDMC.SPEI[,1] * LDMC.SPEI[,2]))))
#plot probabilites
persp(LDMC.SPEI_prob.pred_2$x, LDMC.SPEI_prob.pred_2$y, LDMC.SPEI_prob.pred_2$z, phi = 20, theta = 60,
      ticktype = "detailed",
      main = "Impact of LDMC and SPEI on grass survival",
      zlab = "\n\nsurvival",
      ylab = "\n\nLDMC_s",
      xlab = "\n\nSPEI_s")

#try to make the effects plot manually
#make new data
newdata_2 <- data.frame(LDMC_s = seq(min(CO_grams$LDMC_s), max(CO_grams$LDMC_s), length = 100))
#predict values for low SPEI value
newdata_2$linear.preds_low <- -0.27309 + 0.08580 * newdata_2$LDMC_s +  0.22331 * min(CO_grams$SPEI_s) + -0.67083 * mean(CO_grams$neighbors_10_s) + -0.12379 * min(CO_grams$SPEI_s) * newdata_2$LDMC_s
#predict values for high SPEI value
newdata_2$linear.preds_high <- -0.27309 + 0.08580 * newdata_2$LDMC_s +  0.22331 * max(CO_grams$SPEI_s) + -0.67083 * mean(CO_grams$neighbors_10_s) + -0.12379 * max(CO_grams$SPEI_s) * newdata_2$LDMC_s
#transform to probabilites
newdata_2$probs_low <- 1/(1+exp(-newdata_2$linear.preds_low))
newdata_2$probs_high <- 1/(1+exp(-newdata_2$linear.preds_high))

plot(probs_high ~ LDMC_s, data = newdata_2, type = "l",
     ylim = c(0.1,0.8))
lines(probs_low ~ LDMC_s, data = newdata_2)

##Forbs (points)
#TLP
#visreg figure
dev.off()
visreg::visreg2d(fit=m3Final, xvar="SPEI_s", yvar="TLP_s", 
                 scale="response", plot.type="persp", phi=20, theta=55, 
                 xlab="\n\nSPEI (scaled)", 
                 ylab="\n\nTLP index (scaled)", 
                 zlab="\n\nSurvival Probability", cex.axis=1, cex.lab=1.2, 
                 col=adjustcolor("#019E73",alpha.f=.5))
#effects plot
hist(fitted(m3Final))
plot(predictorEffect("TLP_s", m3, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),confint = list(style = "auto"),
     axes = list(y=list(type="response")))

#still don't entirely trust it...
#manually calculate results and plot them 
#make data.frame for new data
grid.l_3 <- list(seq(min(CO_point_all$SPEI_s), max(CO_point_all$SPEI_s), length = 100),
                 seq(min(CO_point_all$TLP_s), max(CO_point_all$TLP_s), length = 100))
TLP.SPEI_3 <- make.surface.grid(grid.l_3)  
#make linear predictions using m1 model results
TLP.SPEI_linear.pred_3 <- as.surface(TLP.SPEI_3,
                                     -0.28276 + -0.37654 * mean(CO_point_all$neighbors_10_s) + -0.15083*TLP.SPEI_3[,1] + -0.31563*TLP.SPEI_3[,2] + 0.18950 *TLP.SPEI_3[,1] * TLP.SPEI_3[,2])

#plot linear predictors
persp(TLP.SPEI_linear.pred_3$x, TLP.SPEI_linear.pred_3$y, TLP.SPEI_linear.pred_3$z, phi = 20, theta = 60)
#transform to response (not linear)
TLP.SPEI_prob.pred_3 <- as.surface(TLP.SPEI_3,
                                   1/(1+exp(-(-0.28276 + -0.37654 * mean(CO_point_all$neighbors_10_s) + -0.15083*TLP.SPEI_3[,1] + -0.31563*TLP.SPEI_3[,2] + 0.18950 *TLP.SPEI_3[,1] * TLP.SPEI_3[,2]))))
#plot probabilites
persp(TLP.SPEI_prob.pred_3$x, TLP.SPEI_prob.pred_3$y, TLP.SPEI_prob.pred_3$z, phi = 20, theta = 60,
      ticktype = "detailed",
      main = "Impact of TLP and SPEI on forb survival",
      zlab = "\n\nsurvival",
      ylab = "\n\nTLP_s",
      xlab = "\n\nSPEI_s")

#try to make the effects plot manually
#make new data
newdata_3 <- data.frame(TLP_s = seq(min(CO_point_all$TLP_s, na.rm = TRUE), max(CO_point_all$TLP_s, na.rm = TRUE), length = 100))
#predict values for low SPEI value
newdata_3$linear.preds_low <- -0.28276 + -0.31563 * newdata_3$TLP_s +  -0.15083 * min(CO_point_all$SPEI_s, na.rm = TRUE) + -0.37654 * mean(CO_point_all$neighbors_10_s, na.rm = TRUE) + 0.18950 * min(CO_point_all$SPEI_s, na.rm = TRUE) * newdata_3$TLP_s
#predict values for high SPEI value
newdata_3$linear.preds_high <- -0.28276 + -0.31563 * newdata_3$TLP_s +  -0.15083 * max(CO_point_all$SPEI_s, na.rm = TRUE) + -0.37654 * mean(CO_point_all$neighbors_10_s, na.rm = TRUE) + 0.18950 * max(CO_point_all$SPEI_s, na.rm = TRUE) * newdata_3$TLP_s
#transform to probabilites
newdata_3$probs_low <- 1/(1+exp(-newdata_3$linear.preds_low))
newdata_3$probs_high <- 1/(1+exp(-newdata_3$linear.preds_high))

plot(probs_low ~ TLP_s, data = newdata_3, type = "l", ylim = c(0,1))
lines(probs_high ~ TLP_s, data = newdata_3)


#LDMC
dev.off()
visreg::visreg2d(fit=m4Final, xvar="SPEI_s", yvar="LDMC_s", 
                 scale="response", plot.type="persp", phi=20, theta=55, 
                 xlab="\n\nSPEI (scaled)", 
                 ylab="\n\nLDMC index (scaled)", 
                 zlab="\n\nSurvival Probability", cex.axis=1, cex.lab=1.2, 
                 col=adjustcolor("#019E73",alpha.f=.5))
#effects plot
hist(fitted(m4Final))
plot(predictorEffect("LDMC_s", m4Final, xlevels=list(SPEI_s=c(-2.22,1.72))), lines = list(multiline = TRUE),
     confint = list(style = "auto"),
     axes = list(y=list(type="response")))

#still don't entirely trust it...
#manually calculate results and plot them 
#make data.frame for new data
grid.l_4 <- list(seq(min(CO_point_all$SPEI_s), max(CO_point_all$SPEI_s), length = 100),
                 seq(min(CO_point_all$LDMC_s), max(CO_point_all$LDMC_s), length = 100))
LDMC.SPEI_4 <- make.surface.grid(grid.l_4)  
#make linear predictions using m1 model results
LDMC.SPEI_linear.pred_4 <- as.surface(LDMC.SPEI_4,
                                      -2.09072 + -0.27846 * mean(CO_point_all$neighbors_15_s) + -0.15254*LDMC.SPEI_4[,1] + -0.15247*LDMC.SPEI_4[,2] + -0.18801 *LDMC.SPEI_4[,1] * LDMC.SPEI_4[,2])

#plot linear predictors
persp(LDMC.SPEI_linear.pred_4$x, LDMC.SPEI_linear.pred_4$y, LDMC.SPEI_linear.pred_4$z, phi = 20, theta = 60)
#transform to response (not linear)
LDMC.SPEI_prob.pred_4 <- as.surface(LDMC.SPEI_4,
                                   1/(1+exp(-(-2.09072 + -0.27846 * mean(CO_point_all$neighbors_15_s) + -0.15254*LDMC.SPEI_4[,1] + -0.15247*LDMC.SPEI_4[,2] + -0.18801 *LDMC.SPEI_4[,1] * LDMC.SPEI_4[,2]))))
#plot probabilites
persp(LDMC.SPEI_prob.pred_4$x, LDMC.SPEI_prob.pred_4$y, LDMC.SPEI_prob.pred_4$z, phi = 20, theta = 60,
      ticktype = "detailed",
      main = "Impact of LDMC and SPEI on forb survival",
      zlab = "\n\nsurvival",
      ylab = "\n\nLDMC_s",
      xlab = "\n\nSPEI_s")
##DOESN'T MATCH VISREG2D PLOT...
#try to make the effects plot manually
#make new data
newdata_4 <- data.frame(LDMC_s = seq(min(CO_point_all$LDMC_s), max(CO_point_all$LDMC_s), length = 100))
#predict values for low SPEI value
newdata_4$linear.preds_low <- -2.09072 + -0.15247 * newdata_4$LDMC_s +  -0.15254 * min(CO_point_all$SPEI_s) + -0.27846 * mean(CO_point_all$neighbors_15_s) + -0.18801 * min(CO_point_all$SPEI_s) * newdata_4$LDMC_s
#predict values for high SPEI value
newdata_4$linear.preds_high <- -2.09072 + -0.15247 * newdata_4$LDMC_s +  -0.15254 * max(CO_point_all$SPEI_s) + -0.27846 * mean(CO_point_all$neighbors_15_s) + -0.18801 * max(CO_point_all$SPEI_s) * newdata_4$LDMC_s
#transform to probabilites
newdata_4$probs_low <- 1/(1+exp(-newdata_4$linear.preds_low))
newdata_4$probs_high <- 1/(1+exp(-newdata_4$linear.preds_high))

plot(probs_high ~ LDMC_s, data = newdata_4, type = "l")
lines(probs_low ~ LDMC_s, data = newdata_4)


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


