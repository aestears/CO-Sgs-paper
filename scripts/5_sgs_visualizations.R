
#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Analysis 
# Script 6 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

#### load packages ####
library(effects) # v4.2-0
library(lattice) # v0.20-41
library(latticeExtra) # v0.6-29
library(gridExtra) # v2.3
library(grid) # v4.0.3
library(ggpubr) # v0.4.0
library(lme4) # v1.1-26
library(ggeffects) # v1.0.1
library(tidyverse) # v1.3.0
library(cowplot) # v1.1.1

## Load model output data

#### set wd ####
#set the path to the name of the file containing the 'scripts' folder
# path <-# file containing scripts for analysis
setwd(path)
#get model result data into the environment
#load("./script4_output.RData") #change the file name to the most current version of model runs
load("./script4_output.RData") #change the file name to the most current version of model runs
#### Make a figure of model results for LDMC, RDMC, and TLP for forb and gram survival ####  

## Make figure for graminoid survival
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_G <- mean(CO_grams$SPEI_s, na.rm = TRUE)
sdSPEI_G <- sd(CO_grams$SPEI_s, na.rm = TRUE)

#get 97.5 quantile of the distribution
SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G) 
SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)

#for TLP_s
TLP_vals <- seq(min(CO_grams$TLP_s, na.rm = TRUE), max(CO_grams$TLP_s, na.rm = TRUE), length.out = 20)

TLP_G_dat<- ggpredict(mSurvTLP_grams, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE, typical = "mean")
#the ggpredict() function uses mean values for each of the fixed effects that aren't specified, and population-level values for the random effects

#for LDMC_s
LDMC_vals <- seq(min(CO_grams$LDMC_s, na.rm = TRUE), max(CO_grams$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_G_dat <- ggpredict(mSurvLDMC_grams, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for RDMC_s
RDMC_vals <- seq(min(CO_grams$RDMC_s, na.rm = TRUE), max(CO_grams$RDMC_s, na.rm = TRUE), length.out = 20)
RDMC_G_dat <- ggpredict(mSurvRDMC_grams, terms = c("RDMC_s[RDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

# for RDiam
RDiam_vals <- seq(min(CO_grams$RDiam_s, na.rm = TRUE), max(CO_grams$RDiam_s, na.rm = TRUE), length.out = 20)
RDiam_G_dat <- ggpredict(mSurvRDiam_grams, terms = c("RDiam_s[RDiam_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

# for SLA
SLA_vals <- seq(min(CO_grams$SLA_s, na.rm = TRUE), max(CO_grams$SLA_s, na.rm = TRUE), length.out = 20)
SLA_G_dat <- ggpredict(mSurvSLA_grams, terms = c("SLA_s[SLA_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#make a data.frame to contain all of the values for each trait
GramDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_G_dat$x, GramSurv = TLP_G_dat$predicted, CI_low = TLP_G_dat$conf.low, CI_high = TLP_G_dat$conf.high, SPEI = TLP_G_dat$group, lab = "A")

GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_G_dat$x, GramSurv = LDMC_G_dat$predicted, CI_low = LDMC_G_dat$conf.low, CI_high = LDMC_G_dat$conf.high, SPEI = LDMC_G_dat$group, lab = "B"))

GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Root Dry Matter Content) (g/g)"), x = RDMC_G_dat$x, GramSurv = RDMC_G_dat$predicted, CI_low = RDMC_G_dat$conf.low, CI_high = RDMC_G_dat$conf.high, SPEI = RDMC_G_dat$group, lab = "C"))

#make data for rug plot
RugDat_G <-  data.frame(rug = CO_grams$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_G <- rbind(RugDat_G, data.frame(rug = CO_grams$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))
RugDat_G <- rbind(RugDat_G, data.frame(rug = CO_grams$RDMC_s, trait = "scaled(Root Dry Matter Content) (g/g)"))

RugDat_G <- rbind(RugDat_G, data.frame(rug = CO_grams$RDiam_s, trait = "scaled(Average Root Diameter) (cm)"))
RugDat_G <- rbind(RugDat_G, data.frame(rug = CO_grams$SLA_s, trait = "scaled(Specific Leaf Area) (g/cm2)"))

#text for labels
dat_text <- data.frame(
  label = c("A", "D", "G", "J", "M"),
  sig = c("*,§","*,§","*,§","*,§","*,§"),
  trait = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)","scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)"),
  x    = c(min(TLP_G_dat$x),min(LDMC_G_dat$x),min(RDMC_G_dat$x), min(RDiam_G_dat$x), min(SLA_G_dat$x)),
  y     = c(1,1,1,1,1),
  x1 = c(max(TLP_G_dat$x), max(LDMC_G_dat$x), max(RDMC_G_dat$x), max(RDiam_G_dat$x), max(SLA_G_dat$x)),
  y1 = c(1,1,1,1,1)
)

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
gramSurvFigure <- ggplot(data = GramDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, GramSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_G) +
  labs(title = "Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(.~factor(trait, levels = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)","scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)")), scales = "free_x", strip.position =  "bottom", ncol =1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  geom_text(data= dat_text, mapping = aes(x = x1, y = y1, label = sig), size = 3.5, fontface = "bold")

#### Make a figure for forb survival ####
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_F <- mean(CO_point_all$SPEI_s)
sdSPEI_F <- sd(CO_point_all$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5_F <- qnorm(.975, meanSPEI_F, sdSPEI_F) 
SPEI_2_5_G <- qnorm(.025, meanSPEI_F, sdSPEI_F)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)

#for TLP_s
TLP_vals <- seq(min(CO_point_all$TLP_s, na.rm = TRUE), max(CO_point_all$TLP_s, na.rm = TRUE), length.out = 20)

TLP_F_dat <- ggpredict(mSurvTLP_forbs, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

TLP_F_dat <- ggpredict(mSurvTLP_forbs, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for LDMC_s
LDMC_vals <- seq(min(CO_point_all$LDMC_s, na.rm = TRUE), max(CO_point_all$LDMC_s, na.rm = TRUE), length.out = 20)

LDMC_F_dat <- ggpredict(mSurvLDMC_forbs, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for RDMC_s
RDMC_vals <- seq(min(CO_point_all$RDMC_s, na.rm = TRUE), max(CO_point_all$RDMC_s, na.rm = TRUE), length.out = 20)
RDMC_F_dat <- ggpredict(mSurvRDMC_forbs, terms = c("RDMC_s[RDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for RDiam_s
RDiam_vals <- seq(min(CO_point_all$RDiam_s, na.rm = TRUE), max(CO_point_all$RDiam_s, na.rm = TRUE), length.out = 20)
RDiam_F_dat <- ggpredict(mSurvRDiam_forbs, terms = c("RDiam_s[RDiam_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for SLA
SLA_vals <- seq(min(CO_point_all$SLA_s, na.rm = TRUE), max(CO_point_all$SLA_s, na.rm = TRUE), length.out = 20)
SLA_F_dat <- ggpredict(mSurvSLA_forbs, terms = c("SLA_s[SLA_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#make a data.frame to contain all of the values for each trait
ForbDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_F_dat$x, ForbSurv = TLP_F_dat$predicted, CI_low = TLP_F_dat$conf.low, CI_high = TLP_F_dat$conf.high, SPEI = TLP_F_dat$group, lab = "G")

ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_F_dat$x, ForbSurv = LDMC_F_dat$predicted, CI_low = LDMC_F_dat$conf.low, CI_high = LDMC_F_dat$conf.high, SPEI = LDMC_F_dat$group, lab = "H"))

ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Root Dry Matter Content) (g/g)"), x = RDMC_F_dat$x, ForbSurv = RDMC_F_dat$predicted, CI_low = RDMC_F_dat$conf.low, CI_high = RDMC_F_dat$conf.high, SPEI = RDMC_F_dat$group, lab = "I"))


#make data for rug plot
RugDat_F <-  data.frame(rug = CO_point_all$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_F <- rbind(RugDat_F, data.frame(rug = CO_point_all$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))
RugDat_F <- rbind(RugDat_F, data.frame(rug = CO_point_all$RDMC_s, trait = "scaled(Root Dry Matter Content) (g/g)"))
RugDat_F <- rbind(RugDat_F, data.frame(rug = CO_point_all$RDiam_s, trait = "scaled(Average Root Diameter) (cm)"))
RugDat_F <- rbind(RugDat_F, data.frame(rug = CO_point_all$SLA_s, trait = "scaled(Specific Leaf Area) (g/cm2)"))

#text for labels
dat_text <- data.frame(
  label = c("C", "F", "I", "L", "O"),
  sig = c("*", "*,§","*,§", "", "*,§" ),
  trait = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)", "scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)"),
  x    = c(min(TLP_F_dat$x),min(LDMC_F_dat$x),min(RDMC_F_dat$x), min(RDiam_F_dat$x), min(SLA_F_dat$x)),
  y     = c(1,1,1, 1, 1),
  x1 = c(max(TLP_F_dat$x), max(LDMC_F_dat$x), max(RDMC_F_dat$x), max(RDiam_F_dat$x), max(SLA_F_dat$x)),
  y1 = c(1,1,1,1,1)
)

#text for labels
dat_text <- data.frame(
  label = c("C", "F", "I", "L", "O"),
  sig = c("*", "*,§","*,§", "", "*,§" ),
  trait = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)", "scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)"),
  x    = c(min(TLP_F_dat$x),min(LDMC_F_dat$x),min(RDMC_F_dat$x), min(RDiam_F_dat$x), min(SLA_F_dat$x)),
  y     = c(1,1,1, 1, 1),
  x1 = c(max(TLP_F_dat$x), max(LDMC_F_dat$x), max(RDMC_F_dat$x), max(RDiam_F_dat$x), max(SLA_F_dat$x)),
  y1 = c(1,1,1,1,1)
)

#make a multipanel figure that shows only the graminoid survival probs for 5 traits

forbSurvFigure <- ggplot(data = ForbDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, ForbSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_F) +
  labs(title = "Forb Survival") +
  xlab(NULL) +
  ylab("Probability of Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  geom_text(data= dat_text, mapping = aes(x = x1, y = y1, label = sig), size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~factor(trait, levels = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)","scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)")), scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))


#### Make a figure for graminoid growth ####
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI <- mean(CO_grow_TLP$SPEI_s)
sdSPEI <- sd(CO_grow_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP_s
TLP_vals <- seq(min(CO_grow_TLP$TLP_s, na.rm = TRUE), max(CO_grow_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_grow_dat <- ggpredict(mGrowTLP, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for LDMC_s
LDMC_vals <- seq(min(CO_grow_LDMC$LDMC_s, na.rm = TRUE), max(CO_grow_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_grow_dat <- ggpredict(mGrowLDMC, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for RDMC_s
RDMC_vals <- seq(min(CO_grow_RDMC$RDMC_s, na.rm = TRUE), max(CO_grow_RDMC$RDMC_s, na.rm = TRUE), length.out = 20)
RDMC_grow_dat <- ggpredict(mGrowRDMC, terms = c("RDMC_s[RDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for RDiam_s
RDiam_vals <- seq(min(CO_grow_RDiam$RDiam_s, na.rm = TRUE), max(CO_grow_RDiam$RDiam_s, na.rm = TRUE), length.out = 20)
RDiam_grow_dat <- ggpredict(mGrowRDiam, terms = c("RDiam_s[RDiam_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for SLA_s
SLA_vals <- seq(min(CO_grow_SLA$SLA_s, na.rm = TRUE), max(CO_grow_SLA$SLA_s, na.rm = TRUE), length.out = 20)
SLA_grow_dat <- ggpredict(mGrowSLA, terms = c("SLA_s[SLA_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#make a data.frame to contain all of the values for each trait
GrowthDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_grow_dat$x, Growth = TLP_grow_dat$predicted, CI_low = TLP_grow_dat$conf.low, CI_high = TLP_grow_dat$conf.high, SPEI = TLP_grow_dat$group)
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_grow_dat$x, Growth = LDMC_grow_dat$predicted, CI_low = LDMC_grow_dat$conf.low, CI_high = LDMC_grow_dat$conf.high, SPEI = LDMC_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Root Dry Matter Content) (g/g)" ), x = RDMC_grow_dat$x, Growth = RDMC_grow_dat$predicted, CI_low = RDMC_grow_dat$conf.low, CI_high = RDMC_grow_dat$conf.high, SPEI = RDMC_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Average Root Diameter) (cm)"), x = RDiam_grow_dat$x, Growth = RDiam_grow_dat$predicted, CI_low = RDiam_grow_dat$conf.low, CI_high = RDiam_grow_dat$conf.high, SPEI = RDiam_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Specific Leaf Area) (g/cm2)"), x = SLA_grow_dat$x, Growth = SLA_grow_dat$predicted, CI_low = SLA_grow_dat$conf.low, CI_high = SLA_grow_dat$conf.high, SPEI = SLA_grow_dat$group))

#make a data.frame with data for the rug plot
RugDat <- data.frame(rug = CO_grow_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_RDMC$RDMC_s, trait = "scaled(Root Dry Matter Content) (g/g)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_RDiam$RDiam_s, trait = "scaled(Average Root Diameter) (cm)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_SLA$SLA_s, trait = "scaled(Specific Leaf Area) (g/cm2)"))

#text for labels
dat_text <- data.frame(
  label = c("B", "E", "H", "K","N"),
  sig = c("", "","", "", "" ), 
  trait = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)", "scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)"),
  x    = c(min(TLP_grow_dat$x), min(LDMC_grow_dat$x), min(RDMC_grow_dat$x), min(RDiam_grow_dat$x), min(SLA_grow_dat$x)),
  y     = c(4.4,4.4,4.4,4.4,4.4),
  x1 = c(max(TLP_grow_dat$x), max(LDMC_grow_dat$x), max(RDMC_grow_dat$x), max(RDiam_grow_dat$x), max(SLA_grow_dat$x)),
  y1 = c(4.4,4.4,4.4,4.4,4.4)
)

#make a multipanel figure
GrowthExtraFig <- ggplot(data = GrowthDat) +
  geom_hline(aes(yintercept = 3.54), col = "darkgrey", lty = 2) +# add an hline to show the average size_t, which is 3.545 
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3)+
  geom_line(aes(x=x, Growth, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat) +
  labs(title = "Graminoid Growth") +
  xlab(NULL) +
  ylab(expression(ln(size[italic(t+1)]))) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  geom_text(data = dat_text, mapping = aes(x = x1, y = y1, label = sig), size = 3.5, fontface = "bold") +
  facet_wrap(.~factor(trait, levels = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)","scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)")), scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) 


#### Combine results panels into one figure ####

mainObs <- cowplot::plot_grid( gramSurvFigure, GrowthExtraFig, forbSurvFigure, ncol = 3, align = "h", axis = "tb", rel_widths = c(1,1, 1)) %>% 
  cowplot::ggdraw() + 
  cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .07, y = .059, width = .92, height = .0227) +
  cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .07, y = .006, width = .33, height = .05) + #cover up left legend 
  cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .75, y = .006, width = .33, height = .05) #cover up right legend

mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Specific Leaf Area) (c"*m^2*"/g)"),x = .5, y = 3, size = 9))
mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Specific Leaf Area) (c"*m^2*"/g)"),x = .19, y = 4, size = 9))
mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Specific Leaf Area) (c"*m^2*"/g)"),x = .873, y = 5, size = 9))
mainObs


#### Make a plot of random effects of individual plant size on survival for LDMC model (best model for graminoid survival) (figure 2: panel B)####

#get random effect data
#refit model w/ factors instead of logical values
m2_fac <- glmer(as.factor(survives_tplus1) ~ SPEI_s * LDMC_s +  size_t_log + neighbors_10_s + as.factor(nearEdge_t) + (size_t_log|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

sppAreaPreds_s <- ggpredict(m2_fac, terms = c("size_t_log[all]", "species"), type = "random")
sppAreaPreds_s <- data.frame("x" = sppAreaPreds_s$x, "preds" = sppAreaPreds_s$predicted,"spp" = sppAreaPreds_s$group )

globPreds_a_s <- ggpredict(m2_fac, terms = c("size_t_log[all]"), type = "random")
globPreds_a_s <- data.frame("x" = globPreds_a_s$x, "preds" = globPreds_a_s$predicted, "spp" = as.factor("Global"), "CI_low" = globPreds_a_s$conf.low, "CI_high" = globPreds_a_s$conf.high)

(AreaEffectSurv <- ggplot() +
    geom_line(data = sppAreaPreds_s, aes(x = x, y = preds, col = spp), alpha = .75)+
    geom_line(data = globPreds_a_s, aes(x = x, y = preds), lwd = 1.25) +
    #geom_line(aes(x = globPreds_a_s$x, y = globPreds_a_s$CI_low)) +
    geom_polygon(aes(x = c(globPreds_a_s$x,rev(globPreds_a_s$x)), y = c( globPreds_a_s$CI_low, rev(globPreds_a_s$CI_high))), col = NA, fill = "grey", alpha = .2) +
    theme_classic() +
    ylim(c(0,1))+
    xlab(c(expression(ln(size[italic(t)])))) +
    ylab("Prob.(Graminoid Survival)") +
    scale_color_brewer(palette = "Set2") #+
  #theme(axis.ticks.x.bottom = element_blank(),
  #axis.text.x.bottom = element_blank(),
  #legend.position = "none")
)

#### Make a figure for graminoid growth ####
##make figure for graminoid growth models 
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI <- mean(CO_grow_TLP$SPEI_s)
sdSPEI <- sd(CO_grow_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP_s
TLP_vals <- seq(min(CO_grow_TLP$TLP_s, na.rm = TRUE), max(CO_grow_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_grow_dat <- ggpredict(mGrowTLP, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for LDMC_s
LDMC_vals <- seq(min(CO_grow_LDMC$LDMC_s, na.rm = TRUE), max(CO_grow_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_grow_dat <- ggpredict(mGrowLDMC, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for RDMC_s
RDMC_vals <- seq(min(CO_grow_RDMC$RDMC_s, na.rm = TRUE), max(CO_grow_RDMC$RDMC_s, na.rm = TRUE), length.out = 20)
RDMC_grow_dat <- ggpredict(mGrowRDMC, terms = c("RDMC_s[RDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for RDiam_s
RDiam_vals <- seq(min(CO_grow_RDiam$RDiam_s, na.rm = TRUE), max(CO_grow_RDiam$RDiam_s, na.rm = TRUE), length.out = 20)
RDiam_grow_dat <- ggpredict(mGrowRDiam, terms = c("RDiam_s[RDiam_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for SLA_s
SLA_vals <- seq(min(CO_grow_SLA$SLA_s, na.rm = TRUE), max(CO_grow_SLA$SLA_s, na.rm = TRUE), length.out = 20)
SLA_grow_dat <- ggpredict(mGrowSLA, terms = c("SLA_s[SLA_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#make a data.frame to contain all of the values for each trait
GrowthDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_grow_dat$x, Growth = TLP_grow_dat$predicted, CI_low = TLP_grow_dat$conf.low, CI_high = TLP_grow_dat$conf.high, SPEI = TLP_grow_dat$group)
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_grow_dat$x, Growth = LDMC_grow_dat$predicted, CI_low = LDMC_grow_dat$conf.low, CI_high = LDMC_grow_dat$conf.high, SPEI = LDMC_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Root Dry Matter Content) (g/g)" ), x = RDMC_grow_dat$x, Growth = RDMC_grow_dat$predicted, CI_low = RDMC_grow_dat$conf.low, CI_high = RDMC_grow_dat$conf.high, SPEI = RDMC_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Average Root Diameter) (cm)"), x = RDiam_grow_dat$x, Growth = RDiam_grow_dat$predicted, CI_low = RDiam_grow_dat$conf.low, CI_high = RDiam_grow_dat$conf.high, SPEI = RDiam_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Specific Leaf Area) (g/cm2)"), x = SLA_grow_dat$x, Growth = SLA_grow_dat$predicted, CI_low = SLA_grow_dat$conf.low, CI_high = SLA_grow_dat$conf.high, SPEI = SLA_grow_dat$group))

#make a data.frame with data for the rug plot
RugDat <- data.frame(rug = CO_grow_TLP$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_RDMC$RDMC_s, trait = "scaled(Root Dry Matter Content) (g/g)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_RDiam$RDiam_s, trait = "scaled(Average Root Diameter) (cm)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_SLA$SLA_s, trait = "scaled(Specific Leaf Area) (g/cm2)"))

#text for labels
dat_text <- data.frame(
  label = c("B", "E", "H", "K","N"),
  sig = c("", "","", "", "" ), 
  trait = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)", "scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)"),
  x    = c(min(TLP_grow_dat$x), min(LDMC_grow_dat$x), min(RDMC_grow_dat$x), min(RDiam_grow_dat$x), min(SLA_grow_dat$x)),
  y     = c(4.4,4.4,4.4,4.4,4.4),
  x1 = c(max(TLP_grow_dat$x), max(LDMC_grow_dat$x), max(RDMC_grow_dat$x), max(RDiam_grow_dat$x), max(SLA_grow_dat$x)),
  y1 = c(4.4,4.4,4.4,4.4,4.4)
)

#make a multipanel figure
GrowthExtraFig <- ggplot(data = GrowthDat) +
  geom_hline(aes(yintercept = 3.54), col = "darkgrey", lty = 2) +# add an hline to show the average size_t, which is 3.545 
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3)+
  geom_line(aes(x=x, Growth, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat) +
  labs(title = "Graminoid Growth") +
  xlab(NULL) +
  ylab(expression(ln(size[italic(t+1)]))) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  geom_text(data = dat_text, mapping = aes(x = x1, y = y1, label = sig), size = 3.5, fontface = "bold") +
  facet_wrap(.~factor(trait, levels = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)","scaled(Average Root Diameter) (cm)", "scaled(Specific Leaf Area) (g/cm2)")), scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) 


#### Combine results panels into one figure ####

mainObs <- cowplot::plot_grid( gramSurvFigure, GrowthExtraFig, forbSurvFigure, ncol = 3, align = "h", axis = "tb", rel_widths = c(1,1, 1)) %>% 
  cowplot::ggdraw() + 
  cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .07, y = .059, width = .92, height = .0227) +
  cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .07, y = .006, width = .33, height = .05) + #cover up left legend 
  cowplot::draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .75, y = .006, width = .33, height = .05) #cover up right legend

mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Specific Leaf Area) (c"*m^2*"/g)"),x = .5, y = 3, size = 9))
mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Specific Leaf Area) (c"*m^2*"/g)"),x = .19, y = 4, size = 9))
mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Specific Leaf Area) (c"*m^2*"/g)"),x = .873, y = 5, size = 9))
mainObs

#### Make figure 2 (combination of figures for effect of size and local neighborhood on LDMC*SPEI effect of graminoid survival) ####
### make a palette for all fig. 2 panels 
palette <- data.frame("spp" = factor(
  unique(paste0(str_sub(str_split_fixed(CO_poly_LDMC$species, " ", n = 2)[,1], start = 1L, end = 1L),". ",str_split_fixed(CO_poly_LDMC$species, " ", n = 2)[,2])),
  levels = c("A. longiseta" , "B. gracilis",  "B. dactyloides", "C. eleocharis", "S. paniculatus", "S. hystrix", "S. cryptandrus", "S. comata")), 
  "long_name" = c("Aristida longiseta", "Bouteloua gracilis", "Buchloe dactyloides", "Carex eleocharis", "Schedonnardus paniculatus", "Sitanion hystrix", "Sporobolus cryptandrus", "Stipa comata"  )
)

#### Make a plot of random effects of individual plant size on survival for LDMC model (best model for graminoid survival) (figure 2: panel B)####

#get random effect data
#refit model w/ factors instead of logical values
m2_fac <- glmer(as.factor(survives_tplus1) ~ SPEI_s * LDMC_s +  size_t_log + neighbors_10_s + as.factor(nearEdge_t) + (size_t_log|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa"))

sppAreaPreds_s <- ggpredict(m2_fac, terms = c("size_t_log[all]", "species"), type = "random")
sppAreaPreds_s <- data.frame("x" = sppAreaPreds_s$x, "preds" = sppAreaPreds_s$predicted,"spp" = sppAreaPreds_s$group )
sppAreaPreds_s <- sppAreaPreds_s %>% 
  left_join(palette, by = c("spp" = "long_name")) %>% 
  rename(spp.old = spp, spp = spp.y)


globPreds_a_s <- ggpredict(m2_fac, terms = c("size_t_log[all]"), type = "random")
globPreds_a_s <- data.frame("x" = globPreds_a_s$x, "preds" = globPreds_a_s$predicted, "spp" = as.factor("Global"), "CI_low" = globPreds_a_s$conf.low, "CI_high" = globPreds_a_s$conf.high)

AreaEffectSurv <- ggplot() +
  geom_line(data = sppAreaPreds_s, aes(x = x, y = preds, col = spp), alpha = .8)+
  geom_line(data = globPreds_a_s, aes(x = x, y = preds), lwd = 1.25) +
  #geom_line(aes(x = globPreds_a_s$x, y = globPreds_a_s$CI_low)) +
  geom_polygon(aes(x = c(globPreds_a_s$x,rev(globPreds_a_s$x)), y = c( globPreds_a_s$CI_low, rev(globPreds_a_s$CI_high))), col = NA, fill = "grey", alpha = .2) +
  theme_classic() +
  xlab(c(expression(size[year_t] ))) +
  ylab("P(Graminoid Survival)") +
  scale_color_manual(values = c("grey60", "grey60", "grey60", "grey60", "grey60", "grey60", "grey60", "grey60")) +
  theme(axis.ticks.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        legend.position = "none") 
#send plot to file
pdf("./Manuscript/Figures/AreaEffect_Survival.pdf", width = 4, height = 4)
AreaEffectSurv
dev.off()

#### Make plot of fixed effect of neighborhood density for effect of LDMC*SPEI on graminoid survival (figure 2: panel A) ####

sppNeighPreds_s <- ggpredict(m2_fac, terms = c("neighbors_10_s[all]", "species"), type = "random")
sppNeighPreds_s <- data.frame("x" = sppNeighPreds_s$x, "preds" = sppNeighPreds_s$predicted,"spp" = sppNeighPreds_s$group )
sppNeighPreds_s <- sppNeighPreds_s %>% 
  left_join(palette, by = c("spp" = "long_name")) %>% 
  rename(spp.old = spp, spp = spp.y)

(NeighEffectSurv <- ggplot() +
    geom_line(data = sppNeighPreds_s, aes(x = x, y = preds, col = spp), alpha = .75)+
    geom_line(data = globPreds_n_s, aes(x = x, y = preds), lwd = 1.25) +
    geom_polygon(aes(x = c(globPreds_n_s$x,rev(globPreds_n_s$x)), y = c( globPreds_n_s$CI_low, rev(globPreds_n_s$CI_high))), col = NA, fill = "grey", alpha = .2) +
    theme_classic() +
    ylim(c(0,1))+
    xlab("Conspecific Local \n Neighborhood Competition") +
    ylab("Prob.(Graminoid Survival)") +
    scale_color_manual(values = c("A. longiseta" = "#66C2A5",
                                  "B. gracilis" = "#FC8D62",
                                  "B. dactyloides" = "#8DA0CB",
                                  "C. eleocharis" = "#E78AC3",
                                  "S. paniculatus" = "#A6D854",
                                  "S. hystrix" = "#FFD92F",
                                  "S. cryptandrus" = "#E5C494",
                                  "S. comata" = "#B3B3B3"))) +
    theme(#axis.ticks.x.bottom = element_blank(),
      #axis.text.x.bottom = element_blank(),
      axis.title.x.bottom = element_text(size = 9.5),
      legend.position = "none",
    )


#### Make plot of fixed effect of neighborhood by species on growth for TLP model (figure 2: panel C) ####

mGrowTLP_fac<- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + TLP_s + SPEI_s * TLP_s + as.factor(nearEdge_t) + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

NeighEffectSurv <- ggplot() +
  geom_line(data = sppNeighPreds_s, aes(x = x, y = preds, col = spp), alpha = .8)+
  geom_line(data = globPreds_n_s, aes(x = x, y = preds), lwd = 1.25) +
  geom_polygon(aes(x = c(globPreds_n_s$x,rev(globPreds_n_s$x)), y = c( globPreds_n_s$CI_low, rev(globPreds_n_s$CI_high))), col = NA, fill = "grey", alpha = .2) +
  theme_classic() +
  ylim(c(0,1))+
  xlab("Conspecific Local Neighborhood Competition") +
  ylab("P(Graminoid Survival)") +
  scale_color_manual(values = c("grey60", "grey60", "grey60", "grey60", "grey60", "grey60", "grey60", "grey60")) +
  theme(axis.ticks.x.bottom = element_blank(),
        axis.text.x.bottom = element_blank(),
        legend.position = "none") 
#send plot to file
pdf("./Manuscript/Figures/NeighborhoodEffect_Survival.pdf", width = 4, height = 4)
NeighEffectSurv
dev.off()

#### Make plot of fixed effect of neighborhood by species on growth for TLP model (figure 4: panel A) ####

mGrowTLP_fac<- lme4::lmer(size_tplus1_log ~ size_t_log + neighbors_10_s + TLP_s + SPEI_s * TLP_s + as.factor(nearEdge_t) + (size_t_log|species) + (1|quad) + (1|year_t), data = CO_grow_TLP , control=lmerControl(optimizer="bobyqa"))

sppNeighPreds_g <- ggpredict(mGrowTLP_fac, terms = c("neighbors_10_s[all]", "species"), type = "random")
sppNeighPreds_g <- data.frame("x" = sppNeighPreds_g$x, "preds" = sppNeighPreds_g$predicted,"spp" = sppNeighPreds_g$group )

globPreds_n_g <- ggpredict(mGrowTLP_fac, terms = c("neighbors_10_s[all]"), type = "fixed")
globPreds_n_g <- data.frame("x" = globPreds_n_g$x, "preds" = globPreds_n_g$predicted, "spp" = as.factor("Global"), "CI_low" = globPreds_n_g$conf.low, "CI_high" = globPreds_n_g$conf.high)

(NeighEffectGrowth <- ggplot() +
    geom_line(data = sppNeighPreds_g, aes(x = x, y = preds, col = spp), alpha = .75)+
    geom_line(data = globPreds_n_g, aes(x = x, y = preds), lwd = 1.25) +
    geom_polygon(aes(x = c(globPreds_n_g$x,rev(globPreds_n_g$x)), y = c( globPreds_n_g$CI_low, rev(globPreds_n_g$CI_high))), col = NA, fill = "grey", alpha = .2) +
    theme_classic() +
    xlab(c("Conspecific Local \n Neighborhood Competition")) +
    ylab(expression(ln(size[italic(t+1)]))) +
    ylim(c(-1,8)) + 
    theme(#axis.ticks.x =element_blank(),
      #axis.text.x = element_blank(),
      legend.position = "none",
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(margin = margin(r = 0))) +
    scale_color_manual(values = c("A. longiseta" = "#66C2A5",
                                  "B. gracilis" = "#FC8D62",
                                  "B. dactyloides" = "#8DA0CB",
                                  "C. eleocharis" = "#E78AC3",
                                  "S. paniculatus" = "#A6D854",
                                  "S. hystrix" = "#FFD92F",
                                  "S. cryptandrus" = "#E5C494",
                                  "S. comata" = "#B3B3B3")))


#### Make plot of effect of size_t on size_t+1 using coefficients from TLP model, as well as raw data (figure 4: panel B)####

#model is mGrowTLP
sppSize_Preds_growth <- ggpredict(mGrowTLP_fac, terms = c("size_t_log[all]", "species"), type = "random")
sppSizePreds_growth <- data.frame("x" = sppSize_Preds_growth$x, "preds" = sppSize_Preds_growth$predicted,"spp" = sppSize_Preds_growth$group )

globPreds_growth <- ggpredict(mGrowTLP_fac, terms = c("size_t_log[all]"), type = "random")
globPreds_growth <- data.frame("x" = globPreds_growth$x, "preds" = globPreds_growth$predicted, "spp" = as.factor("Global"), "CI_low" = globPreds_growth$conf.low, "CI_high" = globPreds_growth$conf.high)

(AreaEffectGrowth <- ggplot() +
    geom_abline(aes(slope = 1, intercept = 0), lty = 2, alpha = .8) +
    geom_line(data = sppSizePreds_growth, aes(x = x, y = preds, col = spp), alpha = .75)+
    geom_line(data = globPreds_growth , aes(x = x, y = preds), lwd = 1.25) +
    geom_polygon(aes(x = c(globPreds_growth$x,rev(globPreds_growth$x)), y = c( globPreds_growth$CI_low, rev(globPreds_growth$CI_high))), col = NA, fill = "grey", alpha = .2) +
    ylim(c(-2.3,8.5)) +
    xlim(c(min(CO_grow_TLP$size_t_log)+.2, max(CO_grow_TLP$size_t_log))) +
    theme_classic() +
    xlab(c(expression(ln(size[italic(t)])))) +
    ylab(expression(ln(size[italic(t+1)]))) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("A. longiseta" = "#66C2A5",
                                  "B. gracilis" = "#FC8D62",
                                  "B. dactyloides" = "#8DA0CB",
                                  "C. eleocharis" = "#E78AC3",
                                  "S. paniculatus" = "#A6D854",
                                  "S. hystrix" = "#FFD92F",
                                  "S. cryptandrus" = "#E5C494",
                                  "S. comata" = "#B3B3B3")))


#### Make a figure showing the fitted relationship between size_t and size_t+1 for each graminoid species (figure 4: panel C) ####
<<<<<<< HEAD
>>>>>>> 1d2849d87a02a3a215397b25908f81eff4b56d39
=======
>>>>>>> 1d2849d87a02a3a215397b25908f81eff4b56d39

## use CO_poly_growth dataset
growTemp <- CO_grow_TLP

#make abbreviated species names
growTemp$sppName <- 
  paste0(str_sub(str_split_fixed(CO_grow_TLP$species, " ", n = 2)[,1], start = 1L, end = 1L),". ",str_split_fixed(CO_grow_TLP$species, " ", n = 2)[,2])

(sizet_sizetplus1 <- ggplot(data = growTemp) +
    geom_abline(aes(slope = 1, intercept = 0), lty = 2, alpha = .8)+
    #geom_point(aes(x = size_t_log, y = size_tplus1_log, col = sppName), alpha = .3) +
    geom_smooth(aes(x = size_t_log, y = size_tplus1_log, col = sppName), method = "lm", se = FALSE) +
    theme_classic() +
    ylim(c(-1,8)) +
    xlim(c(min(CO_grow_TLP$size_t_log)+.2, max(CO_grow_TLP$size_t_log))) +
    xlab(expression(ln(size[italic(t)]))) +
    ylab(expression(ln(size[italic(t+1)]))) +
    theme(legend.text = element_text(face = "italic", size = 8), legend.position = "right", legend.key.width = unit(.4, "cm"), legend.margin = margin(t = .1, r = .1, b = 0, l = .1, unit = "pt")) +
    guides(col = guide_legend(title = "Species")) +
    scale_color_manual(values = c("A. longiseta" = "#66C2A5",
                                  "B. gracilis" = "#FC8D62",
                                  "B. dactyloides" = "#8DA0CB",
                                  "S. paniculatus" = "#A6D854",
                                  "S. hystrix" = "#FFD92F",
                                  "S. cryptandrus" = "#E5C494",
                                  "S. comata" = "#B3B3B3")))


### combine panels into one figure
(effectsSurv <- ggarrange(NeighEffectSurv, AreaEffectSurv, 
                          labels = c("A", "B"),
                          ncol = 2, nrow = 1, align ="hv",legend = "right",
                          legend.grob = get_legend(AreaEffectSurv), 
                          common.legend = TRUE))
(effectsGrowth <- ggarrange(NeighEffectGrowth, AreaEffectGrowth,  sizet_sizetplus1,
                            labels = c("C", "D", "E"),
                            widths = c(1,1,1),
                            legend = "none",
                            ncol = 3, nrow = 1, align ="hv"))

ggarrange(effectsSurv, effectsGrowth, nrow = 2, common.legend = TRUE)

#### Make figure 2 (combination of figures for effect of size and local neighborhood on LDMC*SPEI effect of graminoid survival) ####
(effectsSurv <- ggarrange(NeighEffectSurv, AreaEffectSurv, 
                          labels = c("A", "B"),
                          ncol = 2, nrow = 1, align ="hv",legend = "right",
                          legend.grob = get_legend(sizet_sizetplus1)))


#### Make figure 4 (combination of figures for effect of size and neighbors on LDMC*TLP effect on graminoid growth) ####

(effectsGrowth <- ggarrange(NeighEffectGrowth, AreaEffectGrowth,  sizet_sizetplus1,
                            labels = c("A", "B", "C"),
                            widths = c(1,1,1),
                            legend = "right",
                            legend.grob = get_legend(sizet_sizetplus1),
                            ncol = 3, nrow = 1, align ="hv"))

#### Make a figure of model results for forb survival and gram growth and survival for other traits (figure S2)####

#make figure for graminoid survival

#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_G <- mean(CO_grams$SPEI_s, na.rm = TRUE)
sdSPEI_G <- sd(CO_grams$SPEI_s, na.rm = TRUE)
#get 97.5 quantile of the distribution
SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G) 
SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)

#for RTD_s
RTD_vals <- seq(min(CO_grams$RTD_s, na.rm = TRUE), max(CO_grams$RTD_s, na.rm = TRUE), length.out = 20)
RTD_G_dat <- ggpredict(mSurvRTD_grams, terms = c("RTD_s[RTD_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#for SRL_s
SRL_vals <- seq(min(CO_grams$SRL_s, na.rm = TRUE), max(CO_grams$SRL_s, na.rm = TRUE), length.out = 20)
SRL_G_dat <- ggpredict(mSurvSRL_grams, terms = c("SRL_s[SRL_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
GramDat <- data.frame(trait = c("scaled(Root Tissue Density) (g/cm3)"), x = RTD_G_dat$x, GramSurv = RTD_G_dat$predicted, CI_low = RTD_G_dat$conf.low, CI_high = RTD_G_dat$conf.high, SPEI = RTD_G_dat$group, lab = "A")

GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Specific Root Length) (m/g)"), x = SRL_G_dat$x, GramSurv = SRL_G_dat$predicted, CI_low = SRL_G_dat$conf.low, CI_high = SRL_G_dat$conf.high, SPEI = SRL_G_dat$group, lab = "B"))

#make data for rug plot
RugDat_G <-  data.frame(rug = CO_grams$RTD_s, trait = "scaled(Root Tissue Density) (g/cm3)")
RugDat_G <- rbind(RugDat_G, data.frame(rug = CO_grams$SRL_s, trait = "scaled(Specific Root Length) (m/g)"))

#text for labels
dat_text <- data.frame(
  label = c("A", "D"),
  trait = c("scaled(Root Tissue Density) (g/cm3)", "scaled(Specific Root Length) (m/g)"),
  x    = c(min(RTD_G_dat$x),min(SRL_G_dat$x)),
  y     = c(1,1)
)

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
gramSurvFigure <- ggplot(data = GramDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, GramSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_G) +
  labs(title = "Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol =1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")


# make figure for forb survival

#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_F <- mean(CO_point_all$SPEI_s)
sdSPEI_F <- sd(CO_point_all$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5_F <- qnorm(.975, meanSPEI_F, sdSPEI_F) 
SPEI_2_5_G <- qnorm(.025, meanSPEI_F, sdSPEI_F)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)

#for RTD
RTD_vals <- seq(min(CO_point_all$RTD_s, na.rm = TRUE), max(CO_point_all$RTD_s, na.rm = TRUE), length.out = 20)
RTD_F_dat <- ggpredict(mSurvRTD_forbs , terms = c("RTD_s[RTD_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#for SRL
SRL_vals <- seq(min(CO_point_all$SRL_s, na.rm = TRUE), max(CO_point_all$SRL_s, na.rm = TRUE), length.out = 20)
SRL_F_dat <- ggpredict(mSurvSRL_forbs, terms = c("SRL_s[SRL_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
ForbDat <- data.frame(trait = c("scaled(Root Tissue Density) (g/cm3)"), x = RTD_F_dat$x, ForbSurv = RTD_F_dat$predicted, CI_low = RTD_F_dat$conf.low, CI_high = RTD_F_dat$conf.high, SPEI = RTD_F_dat$group, lab = "G")

ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Specific Root Length) (m/g)"), x = SRL_F_dat$x, ForbSurv = SRL_F_dat$predicted, CI_low = SRL_F_dat$conf.low, CI_high = SRL_F_dat$conf.high, SPEI = SRL_F_dat$group, lab = "H"))

#make data for rug plot
RugDat_F <-  data.frame(rug = CO_point_all$RTD_s, trait = "scaled(Root Tissue Density) (g/cm3)")
RugDat_F <- rbind(RugDat_F, data.frame(rug = CO_point_all$SRL_s, trait = "scaled(Specific Root Length) (m/g)"))

#text for labels
dat_text <- data.frame(
  label = c("C", "F"),
  trait = c("scaled(Root Tissue Density) (g/cm3)", "scaled(Specific Root Length) (m/g)"),
  x    = c(min(RTD_F_dat$x),min(SRL_F_dat$x)),
  y     = c(1,1)
)


#make a multipanel figure that shows only the graminoid survival probs for 3 traits
forbSurvFigure <- ggplot(data = ForbDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, ForbSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_F) +
  labs(title = "Forb Survival") +
  xlab(NULL) +
  ylab("Probability of Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))


NeighEffectGrowth <- ggplot() +
  geom_line(data = sppNeighPreds_g, aes(x = x, y = preds, col = spp), alpha = .8)+
  geom_line(data = globPreds_n_g, aes(x = x, y = preds), lwd = 1.25) +
  geom_polygon(aes(x = c(globPreds_n_g$x,rev(globPreds_n_g$x)), y = c( globPreds_n_g$CI_low, rev(globPreds_n_g$CI_high))), col = NA, fill = "grey", alpha = .2) +
  theme_classic() +
  xlab(c(expression("Conspecific Local Neighborhood Competition"))) +
  ylab(expression("log" ~ bgroup("(",frac(size[year_t+1],size[year_t]),")"))) +
  scale_color_manual(values = c("grey60", "grey60", "grey60", "grey60", "grey60", "grey60", "grey60", "grey60")) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") 
#send plot to file
pdf("./Manuscript/Figures/NeighborhoodEffect_Growth.pdf", width = 4, height = 4)
NeighEffectGrowth
dev.off()

## make combined figures for survival and growth

pdf("./Manuscript/Figures/Effects_Growth.pdf", width = 7.5, height = 3.5)
ggarrange(NeighEffectGrowth,
          ncol = 2, nrow = 1)
dev.off()

pdf("./Manuscript/Figures/Effects_Survival.pdf", width = 7.5, height = 3.5)
ggarrange(AreaEffectSurv, NeighEffectSurv,
          labels = c("A", "B"),
          ncol = 2, nrow = 1)
dev.off()

#### plot of all survival model results ####
##polygons
#TLP

#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_G <- mean(CO_grams$SPEI_s, na.rm = TRUE)
sdSPEI_G <- sd(CO_grams$SPEI_s, na.rm = TRUE)
#get 97.5 quantile of the distribution
SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G) 
SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)

#for RTD_s
RTD_vals <- seq(min(CO_grams$RTD_s, na.rm = TRUE), max(CO_grams$RTD_s, na.rm = TRUE), length.out = 20)
RTD_G_dat <- ggpredict(mSurvRTD_grams, terms = c("RTD_s[RTD_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#for SRL_s
SRL_vals <- seq(min(CO_grams$SRL_s, na.rm = TRUE), max(CO_grams$SRL_s, na.rm = TRUE), length.out = 20)
SRL_G_dat <- ggpredict(mSurvSRL_grams, terms = c("SRL_s[SRL_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
GramDat <- data.frame(trait = c("scaled(Root Tissue Density) (g/cm3)"), x = RTD_G_dat$x, GramSurv = RTD_G_dat$predicted, CI_low = RTD_G_dat$conf.low, CI_high = RTD_G_dat$conf.high, SPEI = RTD_G_dat$group, lab = "A")

GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Specific Root Length) (m/g)"), x = SRL_G_dat$x, GramSurv = SRL_G_dat$predicted, CI_low = SRL_G_dat$conf.low, CI_high = SRL_G_dat$conf.high, SPEI = SRL_G_dat$group, lab = "B"))

#make data for rug plot
RugDat_G <-  data.frame(rug = CO_grams$RTD_s, trait = "scaled(Root Tissue Density) (g/cm3)")
RugDat_G <- rbind(RugDat_G, data.frame(rug = CO_grams$SRL_s, trait = "scaled(Specific Root Length) (m/g)"))

#text for labels
dat_text <- data.frame(
  label = c("A", "D"),
  trait = c("scaled(Root Tissue Density) (g/cm3)", "scaled(Specific Root Length) (m/g)"),
  x    = c(min(RTD_G_dat$x),min(SRL_G_dat$x)),
  y     = c(1,1)
)

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
gramSurvFigure <- ggplot(data = GramDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, GramSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_G) +
  labs(title = "Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol =1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")


# make figure for forb survival

#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_F <- mean(CO_point_all$SPEI_s)
sdSPEI_F <- sd(CO_point_all$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5_F <- qnorm(.975, meanSPEI_F, sdSPEI_F) 
SPEI_2_5_G <- qnorm(.025, meanSPEI_F, sdSPEI_F)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)

#for RTD
RTD_vals <- seq(min(CO_point_all$RTD_s, na.rm = TRUE), max(CO_point_all$RTD_s, na.rm = TRUE), length.out = 20)
RTD_F_dat <- ggpredict(mSurvRTD_forbs , terms = c("RTD_s[RTD_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#for SRL
SRL_vals <- seq(min(CO_point_all$SRL_s, na.rm = TRUE), max(CO_point_all$SRL_s, na.rm = TRUE), length.out = 20)
SRL_F_dat <- ggpredict(mSurvSRL_forbs, terms = c("SRL_s[SRL_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
ForbDat <- data.frame(trait = c("scaled(Root Tissue Density) (g/cm3)"), x = RTD_F_dat$x, ForbSurv = RTD_F_dat$predicted, CI_low = RTD_F_dat$conf.low, CI_high = RTD_F_dat$conf.high, SPEI = RTD_F_dat$group, lab = "G")

ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Specific Root Length) (m/g)"), x = SRL_F_dat$x, ForbSurv = SRL_F_dat$predicted, CI_low = SRL_F_dat$conf.low, CI_high = SRL_F_dat$conf.high, SPEI = SRL_F_dat$group, lab = "H"))

#make data for rug plot
RugDat_F <-  data.frame(rug = CO_point_all$RTD_s, trait = "scaled(Root Tissue Density) (g/cm3)")
RugDat_F <- rbind(RugDat_F, data.frame(rug = CO_point_all$SRL_s, trait = "scaled(Specific Root Length) (m/g)"))

#text for labels
dat_text <- data.frame(
  label = c("C", "F"),
  trait = c("scaled(Root Tissue Density) (g/cm3)", "scaled(Specific Root Length) (m/g)"),
  x    = c(min(RTD_F_dat$x),min(SRL_F_dat$x)),
  y     = c(1,1)
)


#make a multipanel figure that shows only the graminoid survival probs for 3 traits
forbSurvFigure <- ggplot(data = ForbDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, ForbSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_F) +
  labs(title = "Forb Survival") +
  xlab(NULL) +
  ylab("Probability of Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))


# Make figure for graminoid growth models

#get 2.5 and 97.5 percentiles of the distribution
meanSPEI <- mean(CO_grow_TLP$SPEI_s)
sdSPEI <- sd(CO_grow_TLP$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for TLP_s
TLP_vals <- seq(min(CO_grow_TLP$TLP_s, na.rm = TRUE), max(CO_grow_TLP$TLP_s, na.rm = TRUE), length.out = 20)
TLP_grow_dat <- ggpredict(mGrowTLP, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for LDMC_s
LDMC_vals <- seq(min(CO_grow_LDMC$LDMC_s, na.rm = TRUE), max(CO_grow_LDMC$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_grow_dat <- ggpredict(mGrowLDMC, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for RDMC_s
RDMC_vals <- seq(min(CO_grow_RDMC$RDMC_s, na.rm = TRUE), max(CO_grow_RDMC$RDMC_s, na.rm = TRUE), length.out = 20)
RDMC_grow_dat <- ggpredict(mGrowRDMC, terms = c("RDMC_s[RDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for SLA_s
SLA_vals <- seq(min(CO_grow_SLA$SLA_s, na.rm = TRUE), max(CO_grow_SLA$SLA_s, na.rm = TRUE), length.out = 20)
SLA_grow_dat <- ggpredict(mGrowSLA, terms = c("SLA_s[SLA_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for RTD_s
RTD_vals <- seq(min(CO_grow_RTD$RTD_s, na.rm = TRUE), max(CO_grow_RTD$RTD_s, na.rm = TRUE), length.out = 20)
RTD_grow_dat <- ggpredict(mGrowRTD, terms = c("RTD_s[RTD_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#for SRL_s
SRL_vals <- seq(min(CO_grow_SRL$SRL_s, na.rm = TRUE), max(CO_grow_SRL$SRL_s, na.rm = TRUE), length.out = 20)
SRL_grow_dat <- ggpredict(mGrowSRL, terms = c("SRL_s[SRL_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#make a data.frame to contain all of the values for each trait
GrowthDat <- data.frame(trait = c("scaled(Root Tissue Density) (g/cm3)"), x = RTD_grow_dat$x, Growth = RTD_grow_dat$predicted, CI_low = RTD_grow_dat$conf.low, CI_high = RTD_grow_dat$conf.high, SPEI = RTD_grow_dat$group)
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Specific Root Length) (m/g)"), x = SRL_grow_dat$x, Growth = SRL_grow_dat$predicted, CI_low = SRL_grow_dat$conf.low, CI_high = SRL_grow_dat$conf.high, SPEI = SRL_grow_dat$group))

#make a data.frame with data for the rug plot
RugDat <- data.frame(rug = CO_grow_RTD$RTD_s, trait = "scaled(Root Tissue Density) (g/cm3)")
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_SRL$SRL_s, trait = "scaled(Specific Root Length) (m/g)"))

#text for labels
dat_text <- data.frame(
  label = c("B", "E"),
  trait = c("scaled(Root Tissue Density) (g/cm3)", "scaled(Specific Root Length) (m/g)"),
  x    = c(min(RTD_grow_dat$x), min(SRL_grow_dat$x)),
  y     = c(5.3,5.3))
#for RDiam_s
RDiam_vals <- seq(min(CO_grow_RDiam$RDiam_s, na.rm = TRUE), max(CO_grow_RDiam$RDiam_s, na.rm = TRUE), length.out = 20)
RDiam_grow_dat <- ggpredict(mGrowRDiam, terms = c("RDiam_s[RDiam_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#make a data.frame to contain all of the values for each trait
GrowthDat <- data.frame(trait = c("scaled(Root Tissue Density) (g/cm3)"), x = RTD_grow_dat$x, Growth = RTD_grow_dat$predicted, CI_low = RTD_grow_dat$conf.low, CI_high = RTD_grow_dat$conf.high, SPEI = RTD_grow_dat$group)
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Specific Root Length) (m/g)"), x = SRL_grow_dat$x, Growth = SRL_grow_dat$predicted, CI_low = SRL_grow_dat$conf.low, CI_high = SRL_grow_dat$conf.high, SPEI = SRL_grow_dat$group))

#make a data.frame with data for the rug plot
RugDat <- data.frame(rug = CO_grow_RTD$RTD_s, trait = "scaled(Root Tissue Density) (g/cm3)")
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_SRL$SRL_s, trait = "scaled(Specific Root Length) (m/g)"))

#make a multipanel figure
GrowthExtraFig <- ggplot(data = GrowthDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, Growth, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat) +
  labs(title = "Graminoid Growth") +
  xlab(NULL) +
  ylab(expression(ln(size[italic(t+1)])))  +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  facet_wrap(vars(trait), scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) 

# Combine into one figure

mainObs <- plot_grid( gramSurvFigure, GrowthExtraFig, forbSurvFigure, ncol = 3, align = "h", axis = "tb", rel_widths = c(1,1,1)) %>% 
  ggdraw() + draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .07, y = .529, width = .25, height = .035) +
  draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .39, y = .529, width = .29, height = .035) +
  draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .73, y = .529, width = .25, height = .035)

mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Root Tissue Density) (g/c"*m^3*")"),x = .55, y = 9, size = 9))
mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Root Tissue Density) (g/c"*m^3*")"),x = .19, y = 10, size = 9))
mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Root Tissue Density) (g/c"*m^3*")"),x = .873, y = 11, size = 9))
mainObs

#### make figure of SPEI (fig. S1) ####
#### make SPEI figure
# path <- ## name of the folder containing script2_output.RData
load(path)

## 50-year average prior to beginning of study
mean(CO_SPEI_uniform[CO_SPEI_uniform$Year %in% c(1946:1996),"SPEI_uniform"])
# 0.067

## max over that time period
max(CO_SPEI_uniform[CO_SPEI_uniform$Year %in% c(1946:1996),"SPEI_uniform"])
# 2.25

## min over that time period
min(CO_SPEI_uniform[CO_SPEI_uniform$Year %in% c(1946:1996),"SPEI_uniform"])
# -1.82

ggplot(data = CO_SPEI_uniform[CO_SPEI_uniform$Year %in% c(1990:2019),]) +
  geom_rect(aes(xmin = 1997, xmax = 2010, ymin = -2.5, ymax = 2.5, ), fill = "grey90")+ #1997-2010
  geom_hline(aes(yintercept = 0.067), lty = 2, col = "grey60") +
  geom_hline(aes(yintercept = 2.25), lty = 2, col = "#66c2a5") + 
  geom_hline(aes(yintercept = -1.82), lty = 2, col = "#fc8d62") + 
  geom_line(aes(x = Year, y = SPEI_uniform)) + 
  ylab("SPEI") +
  theme_classic()
  labs(title = NULL) +
  xlab(NULL) +
  ylab(expression(ln(size[italic(t+1)])))  +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5")) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  facet_wrap(vars(trait), scales = "free_x", strip.position =  "bottom", ncol = 1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) 

# Combine into one figure

mainObs <- plot_grid( gramSurvFigure, GrowthExtraFig, forbSurvFigure, ncol = 3, align = "h", axis = "tb", rel_widths = c(1,1,1)) %>% 
  ggdraw() + draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .07, y = .529, width = .25, height = .035) +
  draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .39, y = .529, width = .29, height = .035) +
  draw_grob(grid::rectGrob(gp = grid::gpar(fill = "white", col = "white")) ,hjust = 0, vjust = 0, x = .73, y = .529, width = .25, height = .035)

mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Root Tissue Density) (g/c"*m^3*")"),x = .55, y = 9, size = 9))
mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Root Tissue Density) (g/c"*m^3*")"),x = .19, y = 10, size = 9))
mainObs <- ggdraw(add_sub(mainObs, label = bquote("scaled(Root Tissue Density) (g/c"*m^3*")"),x = .873, y = 11, size = 9))
mainObs

#### make figure of SPEI (fig. S1) ####
#### make SPEI figure
# path <- ## name of the folder containing script2_output.RData
load(path)

## 50-year average prior to beginning of study
mean(CO_SPEI_uniform[CO_SPEI_uniform$Year %in% c(1946:1996),"SPEI_uniform"])
# 0.067

## max over that time period
max(CO_SPEI_uniform[CO_SPEI_uniform$Year %in% c(1946:1996),"SPEI_uniform"])
# 2.25

## min over that time period
min(CO_SPEI_uniform[CO_SPEI_uniform$Year %in% c(1946:1996),"SPEI_uniform"])
# -1.82

ggplot(data = CO_SPEI_uniform[CO_SPEI_uniform$Year %in% c(1990:2019),]) +
  geom_rect(aes(xmin = 1997, xmax = 2010, ymin = -2.5, ymax = 2.5, ), fill = "grey90")+ #1997-2010
  geom_hline(aes(yintercept = 0.067), lty = 2, col = "grey60") +
  geom_hline(aes(yintercept = 2.25), lty = 2, col = "#66c2a5") + 
  geom_hline(aes(yintercept = -1.82), lty = 2, col = "#fc8d62") + 
  geom_line(aes(x = Year, y = SPEI_uniform)) + 
  ylab("SPEI") +
  theme_classic()