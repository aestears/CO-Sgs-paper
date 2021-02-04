#//////////////
# CO Sgs Trait Project: Data Visualizations
# Alice Stears
# 8 September 2020
#//////////////

#### load packages ####

require(effects)
require(lattice)
require(latticeExtra)
require(gridExtra)
require(grid)
require(ggpubr)
require(lme4)
require(ggeffects)
require(tidyverse)

#### set wd ####
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis")
#get model result data into the environment
load("./SCRIPTS/models_11232020.RData") #change the file name to the most current version of model runs

#### make figure of model results and predictions ####
# lattice.options(
#   layout.heights=list(bottom.padding=list(x=0), top.padding=list(x=-.80)),
#   layout.widths=list(left.padding=list(x=0.3), right.padding=list(x=0))
# )
#for predictions
#make fake dataset
fakeDat_OLD <- data.frame(traitValues = seq(min(CO_grams$TLP_s),max(CO_grams$TLP_s),length.out = 20),
                      #LDMC = seq(max(CO_grams$LDMC_s),min(CO_grams$LDMC_s), length.out = 20),
                      #RDMC = seq(max(CO_poly_RDMC$RDMC_s), min(CO_poly_RDMC$RDMC_s), length.out = 20),
                      surv_moist = seq(.97,.97,length.out = 20),
                      surv_dry = seq(.75,.2,length.out = 20),
                      trait = "Turgor Loss Point (MPa)")
fakeDat <- rbind(fakeDat_OLD, 
                 data.frame(traitValues = seq(max(CO_grams$LDMC_s),min(CO_grams$LDMC_s), length.out = 20),
                            surv_moist = seq(.97,.97,length.out = 20),
                            surv_dry = seq(.75,.2,length.out = 20),
                            trait = "Leaf Dry Matter Content (g/g)"))
fakeDat <- rbind(fakeDat, 
                 data.frame(traitValues = seq(max(CO_grams$RDMC_s, na.rm = TRUE),min(CO_grams$RDMC_s, na.rm = TRUE), length.out = 20),
                            surv_moist = seq(.97,.97,length.out = 20),
                            surv_dry = seq(.75,.2,length.out = 20),
                            trait = "Root Dry Matter Content (g/g)"))
#make the data.frame into a long format 

# names(fakeDat_OLD) <- c("Turgor Loss Point", "Leaf Dry Matter Content", "Root Dry Matter Content", "wetYear", "dryYear")
# fakeDat <- fakeDat_OLD %>% pivot_longer(cols = c("Turgor Loss Point", "Leaf Dry Matter Content", "Root Dry Matter Content"), names_to = "trait", values_to = "traitValues")
# #fakeDat <- fakeDat %>% pivot_longer(cols = c("wet year", "dry year"), names_to = "SPEI", values_to = "survival")
# fakeDat$trait <- factor(fakeDat$trait, labels= unique(fakeDat$trait), ordered = TRUE)

#text for labels
dat_text <- data.frame(
  label = c("A", "B", "C"),
  trait = c("Turgor Loss Point (MPa)", "Leaf Dry Matter Content (g/g)", "Root Dry Matter Content (g/g)"),
  x    = c(min(CO_grams$TLP_s),min(CO_grams$LDMC_s),min(CO_poly_RDMC$RDMC_s)),
  y     = c(1.06,1.06,1.06)
)

predsFigure <- ggplot(data = fakeDat) +
  geom_line(aes(x = traitValues, y =surv_moist), col = "royalblue2") +
  geom_line(aes(x = traitValues, y = surv_dry), col = "goldenrod1") +
  ggtitle("Predictions") +
  xlab(NULL) +
  ylab("P(Survival)") +
  scale_y_continuous(limits = c(0,1.06))  +
  scale_fill_manual(values = c("goldenrod1", "royalblue2"), guide = FALSE) +
  facet_wrap(vars(trait), scales = "free_x", strip.position =  "bottom") +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")


#for grams
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_G <- mean(CO_grams$SPEI_s)
sdSPEI_G <- sd(CO_grams$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G) 
SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)

#for TLP_s
TLP_vals <- seq(min(CO_grams$TLP_s, na.rm = TRUE), max(CO_grams$TLP_s, na.rm = TRUE), length.out = 20)
TLP_G_dat <- ggpredict(m1_grams, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#for LDMC_s
LDMC_vals <- seq(min(CO_grams$LDMC_s, na.rm = TRUE), max(CO_grams$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_G_dat <- ggpredict(m2_grams, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for RDMC_s
RDMC_vals <- seq(min(CO_grams$RDMC_s, na.rm = TRUE), max(CO_grams$RDMC_s, na.rm = TRUE), length.out = 20)
RDMC_G_dat <- ggpredict(m9, terms = c("RDMC_s[RDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
GramDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_G_dat$x, GramSurv = TLP_G_dat$predicted, CI_low = TLP_G_dat$conf.low, CI_high = TLP_G_dat$conf.high, SPEI = TLP_G_dat$group, lab = "A")

GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_G_dat$x, GramSurv = LDMC_G_dat$predicted, CI_low = LDMC_G_dat$conf.low, CI_high = LDMC_G_dat$conf.high, SPEI = LDMC_G_dat$group, lab = "B"))

GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Root Dry Matter Content) (g/g)"), x = RDMC_G_dat$x, GramSurv = RDMC_G_dat$predicted, CI_low = RDMC_G_dat$conf.low, CI_high = RDMC_G_dat$conf.high, SPEI = RDMC_G_dat$group, lab = "C"))

#make data for rug plot
RugDat_G <-  data.frame(rug = CO_grams$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_G <- rbind(RugDat_G, data.frame(rug = CO_grams$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))
RugDat_G <- rbind(RugDat_G, data.frame(rug = CO_grams$RDMC_s, trait = "scaled(Root Dry Matter Content) (g/g)"))

#text for labels
dat_text <- data.frame(
  label = c("D", "E", "F"),
  trait = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)"),
  x    = c(min(TLP_G_dat$x),min(LDMC_G_dat$x),min(RDMC_G_dat$x)),
  y     = c(1,1,1)
)

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
gramSurvFigure <- ggplot(data = GramDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, GramSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_G) +
  ggtitle("Observations") +
  xlab(NULL) +
  ylab("P(Graminoid Survival)") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("goldenrod1", "royalblue2"), guide = FALSE) +
  scale_fill_manual(values = c("goldenrod1", "royalblue2"), guide = FALSE) +
  facet_wrap(vars(trait), scales = "free_x", strip.position =  "bottom") +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")



## for forbs
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_F <- mean(CO_point_all$SPEI_s)
sdSPEI_F <- sd(CO_point_all$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5_F <- qnorm(.975, meanSPEI_F, sdSPEI_F) 
SPEI_2_5_G <- qnorm(.025, meanSPEI_F, sdSPEI_F)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)

#for TLP_s
TLP_vals <- seq(min(CO_point_all$TLP_s, na.rm = TRUE), max(CO_point_all$TLP_s, na.rm = TRUE), length.out = 20)
TLP_F_dat <- ggpredict(m3, terms = c("TLP_s[TLP_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#for LDMC_s
LDMC_vals <- seq(min(CO_point_all$LDMC_s, na.rm = TRUE), max(CO_point_all$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_F_dat <- ggpredict(m4, terms = c("LDMC_s[LDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for RDMC_s
RDMC_vals <- seq(min(CO_point_all$RDMC_s, na.rm = TRUE), max(CO_point_all$RDMC_s, na.rm = TRUE), length.out = 20)
RDMC_F_dat <- ggpredict(m11, terms = c("RDMC_s[RDMC_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
ForbDat <- data.frame(trait = c("scaled(Turgor Loss Point) (MPa)"), x = TLP_F_dat$x, ForbSurv = TLP_F_dat$predicted, CI_low = TLP_F_dat$conf.low, CI_high = TLP_F_dat$conf.high, SPEI = TLP_F_dat$group, lab = "G")

ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Leaf Dry Matter Content) (g/g)"), x = LDMC_F_dat$x, ForbSurv = LDMC_F_dat$predicted, CI_low = LDMC_F_dat$conf.low, CI_high = LDMC_F_dat$conf.high, SPEI = LDMC_F_dat$group, lab = "H"))

ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Root Dry Matter Content) (g/g)"), x = RDMC_F_dat$x, ForbSurv = RDMC_F_dat$predicted, CI_low = RDMC_F_dat$conf.low, CI_high = RDMC_F_dat$conf.high, SPEI = RDMC_F_dat$group, lab = "I"))

#make data for rug plot
RugDat_F <-  data.frame(rug = CO_point_all$TLP_s, trait = "scaled(Turgor Loss Point) (MPa)")
RugDat_F <- rbind(RugDat_F, data.frame(rug = CO_point_all$LDMC_s, trait = "scaled(Leaf Dry Matter Content) (g/g)"))
RugDat_F <- rbind(RugDat_F, data.frame(rug = CO_point_all$RDMC_s, trait = "scaled(Root Dry Matter Content) (g/g)"))

#text for labels
dat_text <- data.frame(
  label = c("G", "H", "I"),
  trait = c("scaled(Turgor Loss Point) (MPa)", "scaled(Leaf Dry Matter Content) (g/g)", "scaled(Root Dry Matter Content) (g/g)"),
  x    = c(min(TLP_F_dat$x),min(LDMC_F_dat$x),min(RDMC_F_dat$x)),
  y     = c(1,1,1)
)


#make a multipanel figure that shows only the graminoid survival probs for 3 traits
forbSurvFigure <- ggplot(data = ForbDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, ForbSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat_F) +
  labs(title = NULL) +
  xlab(NULL) +
  ylab("P(Forb Survival Survival)") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("goldenrod1", "royalblue2")) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
  scale_fill_manual(values = c("goldenrod1", "royalblue2"), guide = FALSE) +
  facet_wrap(~trait, scales = "free_x", strip.position =  "bottom") +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0))) 

##combine into one mega-figure
predObs <- ggarrange(predsFigure, gramSurvFigure, forbSurvFigure, ncol = 1, nrow = 3, heights = c(.75,1,1))

#send to file
pdf("./Manuscript/Figures/PredResultsRugPlot.pdf", width = 7.5, height = 8)
predObs
dev.off()
# #plot of TLP prediction
# TLP_pred <- xyplot(surv_moist~TLP, data = fakeDat,
#                    ylim = c(-0.1,1.1),
#                    ylab = list(label = "P(Survival)", cex = .8),
#                    xlab = list(label = "Turgor Loss Point", cex = .8),
#                    panel =
#                      function( x, y, ... ) {
#                        panel.xyplot(x = fakeDat$TLP, y = fakeDat$surv_moist, data = fakeDat, col = "royalblue2", type = "l", lwd = 2)
#                        panel.lines( fakeDat$TLP, fakeDat$surv_dry, col = "goldenrod1", lwd = 2)
#                      },
#                    scales=list(y=list(labels=c()), x = list(labels=c())))
# #plot of LDMC prediction
# LDMC_pred <- xyplot(surv_moist~LDMC, data = fakeDat,
#                     ylim = c(-0.1,1.1),
#                     ylab = list(label = "P(Survival)", cex = .8),
#                     xlab = list(label = "Leaf Dry Matter Content", cex = .8),
#                     panel =
#                       function( x, y, ... ) {
#                         panel.xyplot(x = fakeDat$LDMC, y = fakeDat$surv_moist, data = fakeDat, col = "royalblue2", type = "l", lwd = 2)
#                         panel.lines( fakeDat$LDMC, fakeDat$surv_dry, col = "goldenrod1", lwd = 2)
#                       },
#                     scales=list(y=list(labels=c()), x = list(labels=c())))
# #plot of RDMC prediction
# RDMC_pred <- xyplot(surv_moist~RDMC, data = fakeDat,
#                     ylim = c(-0.1,1.1),
#                     ylab = list(label = "P(Survival)", cex = .8),
#                     xlab = list(label = "Root Dry Matter Content", cex = .8),
#                     panel =
#                       function( x, y, ... ) {
#                         panel.xyplot(x = fakeDat$RDMC, y = fakeDat$surv_moist, data = fakeDat, col = "royalblue2", type = "l", lwd = 2)
#                         panel.lines( fakeDat$RDMC, fakeDat$surv_dry, col = "goldenrod1", lwd = 2)
#                       },
#                     scales=list(y=list(labels=c()), x = list(labels=c())))
# 
# ##polygons
# #TLP
# #get 2.5 and 97.5 percentiles of the distribution
# meanSPEI_G <- mean(CO_grams$SPEI_s)
# sdSPEI_G <- sd(CO_grams$SPEI_s)
# #get 97.5 quantile of the distribution
# SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G) #2.10
# SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G) #-1.48
# 
# 
# #for TLP_s
# TLP_G <- plot(predictorEffect("TLP_s", m1_grams, xlevels=list(SPEI_s=c(round(SPEI_2_5_G,2), round(SPEI_97_5_G,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "royalblue2")),
#               confint = list(style = "auto"),
#               axes = list(y=list(type="response", lab = list(label = "P(Graminoid Survival)", cex = .8), ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(TLP_s=list(lab = list(label = "scaled(Turgor Loss Point)", cex = .8)))),
#               ylim = c(0,1),
#               lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0), columns=1, cex=.8, cex.title=1, text = list(lab = c("","")), lines = list(col = c("white", "white")),title = "")),
#               main = NA)
# 
# #for LDMC_s
# LDMC_G<- plot(predictorEffect("LDMC_s", m2_grams, xlevels=list(SPEI_s=c(round(SPEI_2_5_G,2), round(SPEI_97_5_G,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "royalblue2")),
#               confint = list(style = "auto"),
#               axes = list(y=list(type="response", lab = list(label = "P(Graminoid Survival)", cex = .8), ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(LDMC_s=list(lab = list(label = "scaled(Leaf Dry Matter Content)", cex = .8)))),
#               ylim = c(0,1),
#               lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0), columns=1, cex=.8, cex.title=1, text = list(lab = c("","")), lines = list(col = c("white", "white")),title = "")),
#               main = NA)
# #for RDMC_s
# RDMC_G <- plot(predictorEffect("RDMC_s", m9, xlevels=list(SPEI_s=c(round(SPEI_2_5_G,2), round(SPEI_97_5_G,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "royalblue2")),
#               confint = list(style = "auto"),
#               axes = list(y=list(type="response", lab = list(label = "P(Graminoid Survival)", cex = .8), ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(RDMC_s=list(lab = list(label = "scaled(Root Dry Matter Content)", cex = .8)))),
#               ylim = c(0,1),
#               lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0), columns=1, cex=.8, cex.title=1, text = list(lab = c("","")), lines = list(col = c("white", "white")),title = "")),
#               main = NA)
# 
# 
# #for forbs
# #get 2.5 and 97.5 percentiles of the distribution
# meanSPEI <- mean(CO_point_all$SPEI_s)
# sdSPEI <- sd(CO_point_all$SPEI_s)
# #get 97.5 quantile of the distribution
# SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
# SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)
# 
# #for TLP_s
# TLP_F <- plot(predictorEffect("TLP_s", m3Final, xlevels=list(SPEI_s=c(round(SPEI_2_5,2), round(SPEI_97_5,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "royalblue2")),
#               confint = list(style = "auto"),
#               axes = list(y=list(type="response", lab = list(label = "P(Forb Survival)", cex = .8), ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(TLP_s=list(lab = list(label = "scaled(Turgor Loss Point)", cex = .8)))),
#               ylim = c(0,1),lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0),
#                                                        columns=1,
#                                                        cex=.8,
#                                                        cex.title=1,
#                                                        text = list(lab = c("Dry Year","Wet Year")),title = "")), main = NA)
# 
# #for LDMC_s
# LDMC_F <- plot(predictorEffect("LDMC_s", m4Final, xlevels=list(SPEI_s=c(round(SPEI_2_5,2), round(SPEI_97_5,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "royalblue2")),
#                confint = list(style = "auto"),
#                axes = list(y=list(type="response", lab = list(label = "P(Forb Survival)", cex = .8), ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(LDMC_s=list(lab = list(label = "scaled(Leaf Dry Matter Content)", cex = .8)))),
#                ylim = c(0,1),
#                lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0), columns=1, cex=.8, cex.title=1, text = list(lab = c("","")), lines = list(col = c("white", "white")),title = "")),
#                main = NA)
# 
# #for RDMC_s
# RDMC_F <- plot(predictorEffect("RDMC_s", m11, xlevels=list(SPEI_s=c(round(SPEI_2_5,2), round(SPEI_97_5,2)))), lines = list(multiline = TRUE, col = c("goldenrod1", "royalblue2")),
#                confint = list(style = "auto"),
#                axes = list(y=list(type="response", lab = list(label = "P(Forb Survival)", cex = .8), ticks=list(at=c(0,.2,.4,.6,.8,1.0))), x = list(RDMC_s=list(lab = list(label = "scaled(Leaf Dry Matter Content)", cex = .8)))),
#                ylim = c(0,1),
#                lattice=list(key.args=list(x=.05, y=.83, corner=c(0, 0), columns=1, cex=.8, cex.title=1, text = list(lab = c("","")), lines = list(col = c("white", "white")),title = "")),
#                main = NA)
# 
# #combine everything into one plot
# grid.arrange(TLP_pred, LDMC_pred, RDMC_pred, TLP_G, LDMC_G, RDMC_G, TLP_F, LDMC_F, RDMC_F, nrow = 3, ncol = 3,
#              heights=c(.75,1,1))
# grid.text("Predictions", x = unit(0.52, "npc"), y = unit(.985, "npc"),
#           just = "centre", gp=gpar(fontface = 'bold'))
# grid.text("Observations", x = unit(0.52, "npc"), y = unit(.72, "npc"),
#           just = "centre", gp=gpar(fontface = 'bold'))
# grid.text("A", x = unit(.07,"npc"), y = unit(.975,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("B", x = unit(.4,"npc"), y = unit(.975,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("C", x = unit(.75,"npc"), y = unit(.975,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("D", x = unit(.1,"npc"), y = unit(.7,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("E", x = unit(.4,"npc"), y = unit(.7,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("F", x = unit(.75,"npc"), y = unit(.7,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("G", x = unit(.1,"npc"), y = unit(.34,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("H", x = unit(.4,"npc"), y = unit(.34,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("I", x = unit(.75,"npc"), y = unit(.34,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("Dry Year", x = unit(.1,"npc"), y = unit(.83,"npc"), just = "left", gp=gpar( fontsize = 8))
# grid.text("Wet Year", x = unit(.1,"npc"), y = unit(.85,"npc"), just = "left", gp=gpar( fontsize = 8))
# grid.lines(x = unit(c(.2,.23),"npc"), y = unit(c(.85,.85),"npc"), gp = gpar(col = "royalblue2", lex = 2))
# grid.lines(x = unit(c(.2,.23),"npc"), y = unit(c(.83,.83),"npc"), gp = gpar(col = "goldenrod1", lex = 2))
# grid.lines(x = unit(c(.03,.03),"npc"), y = unit(c(.81,.96),"npc"), arrow = arrow(angle = 30, length = unit(.02, "npc")))
# grid.lines(x = unit(c(.555,.555),"npc"), y = unit(c(.83,.95),"npc"),arrow = arrow(angle = 30, length = unit(.02, "npc")) )
# grid.lines(x = unit(c(.1,.43),"npc"), y = unit(c(.8,.8),"npc"),arrow = arrow(angle = 30, length = unit(.02, "npc")))
# grid.lines(x = unit(c(.6,.93),"npc"), y = unit(c(.8,.8),"npc"),arrow = arrow(angle = 30, length = unit(.02, "npc")))
# 
# #send figure to PDF
# pdf("./Manuscript/Figures/PredResultsRugPlot.pdf", width = 9, height = 7.5)
# # Make plot
# grid.arrange(TLP_pred, LDMC_pred, RDMC_pred, TLP_G, LDMC_G, RDMC_G, TLP_F, LDMC_F, RDMC_F, nrow = 3, ncol = 3,
#              heights=c(.75,1,1))
# grid.text("Predictions", x = unit(0.52, "npc"), y = unit(.985, "npc"),
#           just = "centre", gp=gpar(fontface = 'bold'))
# grid.text("Observations", x = unit(0.52, "npc"), y = unit(.72, "npc"),
#           just = "centre", gp=gpar(fontface = 'bold'))
# grid.text("A", x = unit(.06,"npc"), y = unit(.975,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("B", x = unit(.404,"npc"), y = unit(.975,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("C", x = unit(.73,"npc"), y = unit(.975,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("D", x = unit(.1,"npc"), y = unit(.71,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("E", x = unit(.405,"npc"), y = unit(.71,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("F", x = unit(.74,"npc"), y = unit(.71,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("G", x = unit(.1,"npc"), y = unit(.34,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("H", x = unit(.405,"npc"), y = unit(.34,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("I", x = unit(.74,"npc"), y = unit(.34,"npc"), just = "centre", gp=gpar(fontface = 'bold', fontsize = 10))
# grid.text("Dry Year", x = unit(.07,"npc"), y = unit(.83,"npc"), just = "left", gp=gpar( fontsize = 8))
# grid.text("Wet Year", x = unit(.07,"npc"), y = unit(.85,"npc"), just = "left", gp=gpar( fontsize = 8))
# grid.lines(x = unit(c(.13,.18),"npc"), y = unit(c(.85,.85),"npc"), gp = gpar(col = "royalblue2", lex = 2))
# grid.lines(x = unit(c(.13,.18),"npc"), y = unit(c(.83,.83),"npc"), gp = gpar(col = "goldenrod1", lex = 2))
# grid.lines(x = unit(c(.03,.03),"npc"), y = unit(c(.82,.96),"npc"), arrow = arrow(angle = 30, length = unit(.02, "npc")))
# grid.lines(x = unit(c(.365,.365),"npc"), y = unit(c(.82,.96),"npc"),arrow = arrow(angle = 30, length = unit(.02, "npc")))
# grid.lines(x = unit(c(.695,.695),"npc"), y = unit(c(.82,.96),"npc"),arrow = arrow(angle = 30, length = unit(.02, "npc")))
# grid.lines(x = unit(c(.05,.3),"npc"), y = unit(c(.78,.78),"npc"),arrow = arrow(angle = 30, length = unit(.02, "npc")))
# grid.lines(x = unit(c(.39,.63),"npc"), y = unit(c(.78,.78),"npc"),arrow = arrow(angle = 30, length = unit(.02, "npc")))
# grid.lines(x = unit(c(.72,.96),"npc"), y = unit(c(.78,.78),"npc"),arrow = arrow(angle = 30, length = unit(.02, "npc")))
# 
# #embed_fonts("plotname.pdf", outfile = "plotname_embed.pdf")
# dev.off()

#### Climate variability figure ####
source("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Climate Data/CrossSiteClimateComparison.R")

#figure of annual precip variability at CO site

setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis")
pdf("./Manuscript/Figures/CO_MAP.pdf", width = 3, height = 3)
ggplot(data = CO[!is.na(CO$Ann.Sum.Precip),])+
  geom_line(aes(x = Year, y = Ann.Sum.Precip), col = "gray25") +
  ylab(expression("MAP (mm)" %->% "")) + 
  scale_x_continuous(labels = NULL, breaks = NULL) + 
  scale_y_continuous(labels = NULL, breaks = NULL) +
  theme_classic() 
dev.off()


#### plot of random effects of individual plant size on survival for LDMC model ####
#get random effect data

#refit model w/ factors instead of logical values
m2_fac <- glmer(as.factor(survives_tplus1) ~ SPEI_s * LDMC_s +  area_s + neighbors_10_s + as.factor(nearEdge_t) + (area_s|species) + (1|quad) + (1|year_t), data=CO_poly_LDMC, family = binomial(link = logit), control=glmerControl(optimizer="bobyqa"))

sppAreaPreds_s <- ggpredict(m2_fac, terms = c("area_s[all]", "species"), type = "random")
sppAreaPreds_s <- data.frame("x" = sppAreaPreds_s$x, "preds" = sppAreaPreds_s$predicted,"spp" = sppAreaPreds_s$group )

globPreds_a_s <- ggpredict(m2_fac, terms = c("area_s[all]"), type = "random")
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


#make manual effects plot to make sure that ggEffects package is working correctly
#try to make effects plot manually
# newdata_2 <- data.frame(area_s = seq(min(CO_grams$area_s), max(CO_grams$area_s), length = 100))
# #predict values for low SPEI value
# newdata_2$linear.preds <- -1.203091 + -0.138959 * mean(CO_grams$SPEI_s) * mean(CO_grams$LDMC_s) +  2.165896 * newdata_2$area_s + -0.604892 * mean(CO_grams$neighbors_10_s) + 0.004885 * mean(CO_grams$nearEdge_t) +  0.238027 * mean(CO_grams$SPEI_s) + 0.223200 * mean(CO_grams$LDMC_s)
# newdata_2$linear.preds_ARILON <- -1.203091-0.09393829 + -0.138959 * mean(CO_grams$SPEI_s) * mean(CO_grams$LDMC_s) +  (2.165896-0.2743367) * newdata_2$area_s + -0.604892 * mean(CO_grams$neighbors_10_s) + 0.004885 * mean(CO_grams$nearEdge_t) +  0.238027 * mean(CO_grams$SPEI_s) + 0.223200 * mean(CO_grams$LDMC_s)
# 
# #transform to probabilites
# newdata_2$probs<- 1/(1+exp(-newdata_2$linear.preds))
# 
# newdata_2$probs_ARILON<- 1/(1+exp(-newdata_2$linear.preds_ARILON))
# 
# #predictions from ggpredict() function
# AriLonPreds <- ggpredict(m2_fac, terms = c("area_s[all]", "species[Aristida longiseta]"), type = "random")
# AreaPreds <- ggpredict(m2_fac, terms = c("area_s[all]"), type = "random")
# plot(probs~ area_s, data = newdata_2, type = "l",
#      ylim = c(0,1))
# lines(newdata_2$probs_ARILON ~ newdata_2$area_s, col = "blue")
# lines(AriLonPreds$predicted ~ AriLonPreds$x, col = "red")
# lines(AreaPreds$predicted ~ AreaPreds$x, col = "green")

# lines(x = c(-1.55,-1.45     , -1.35      ,-1.25      ,-1.15      ,-1.05     ,-0.953    , -0.854 ,
# -0.755  ,   -0.656 ,    -0.557  ,   -0.458  ,   -0.359  ,    -0.26 ,    -0.162,    -0.0627 ,
# 0.0361   ,   0.135  ,    0.234   ,   0.333   ,   0.432   ,    0.53  ,    0.629 ,     0.728 ,
# 0.827     , 0.926    ,   1.02     ,  1.12     ,  1.22     ,  1.32    ,   1.42   ,    1.52 ,
# 1.62       ,1.72      , 1.82       ,1.91       ,2.01       ,2.11      , 2.21     ,  2.31 ,
# 2.41  ,     2.51      , 2.61      , 2.71       , 2.8      ,  2.9       ,   3      ,  3.1,
# 3.2    ,    3.3 ), y = c(0.00859007 ,0.01064533 ,0.01318579 ,0.01632252 ,0.02019017 ,0.02495102 ,0.03060582 ,0.03764956 ,
# 0.04623705 ,0.05666792 ,0.06928102 ,0.08445021 ,0.10257467 ,0.12406186 ,0.14902643 ,0.17840609 ,
# 0.21195297 ,0.24992990 ,0.29223321 ,0.33846577 ,0.38800282 ,0.43943455 ,0.49274213 ,0.54621526 ,
# 0.59864296 ,0.64890686 ,0.69377592 ,0.73777220 ,0.77747485 ,0.81269244 ,0.84345861 ,0.86997978 ,
# 0.89257999 ,0.91165075 ,0.92761038 ,0.93965754 ,0.95083082 ,0.96002322 ,0.96755570 ,0.97370778 ,
# 0.97871895 ,0.98279190 ,0.98609641 ,0.98877359 ,0.99074348 ,0.99253263 ,0.99397806 ,0.99514508 ,
# 0.99608683 ,0.99684647))

#### Make plot of fixed effect of neighborhood density ####

sppNeighPreds_s <- ggpredict(m2_fac, terms = c("neighbors_10_s[all]", "species"), type = "random")
sppNeighPreds_s <- data.frame("x" = sppNeighPreds_s$x, "preds" = sppNeighPreds_s$predicted,"spp" = sppNeighPreds_s$group )

globPreds_n_s <- ggpredict(m2_fac, terms = c("neighbors_10_s[all]"), type = "fixed")
globPreds_n_s <- data.frame("x" = globPreds_n_s$x, "preds" = globPreds_n_s$predicted, "spp" = as.factor("Global"), "CI_low" = globPreds_n_s$conf.low, "CI_high" = globPreds_n_s$conf.high)

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

#### Make plot of random effect of area by species on growth ####
#refit model w/ factors instead of logical values
# mGrowTLP_fac<- lme4::lmer(logDiffArea ~  neighbors_10_s + TLP_s + SPEI_s * TLP_s + as.factor(nearEdge_t) + (1|species) + (1|quad) + (1|year_t), data = CO_grow_TLP_2 , control=lmerControl(optimizer="bobyqa"))
# 
# sppAreaPreds_g <- ggpredict(mGrowTLP_fac, terms = c("area_s[all]", "species"), type = "random")
# sppAreaPreds_g <- data.frame("x" = sppAreaPreds_g$x, "preds" = sppAreaPreds_g$predicted,"spp" = sppAreaPreds_g$group )
# 
# globPreds_a_g <- ggpredict(mGrowTLP_fac, terms = c("area_s[all]"), type = "fixed")
# globPreds_a_g <- data.frame("x" = globPreds_a_g$x, "preds" = globPreds_a_g$predicted, "spp" = as.factor("Global"), "CI_low" = globPreds_a_g$conf.low, "CI_high" = globPreds_a_g$conf.high)
# 
# AreaEffectGrowth <- ggplot() +
#   geom_line(data = sppAreaPreds_g, aes(x = x, y = preds, col = spp), alpha = .8)+
#   geom_line(data = globPreds_a_g, aes(x = x, y = preds), lwd = 1.25) +
#   geom_polygon(aes(x = c(globPreds_a_g$x,rev(globPreds_a_g$x)), y = c( globPreds_a_g$CI_low, rev(globPreds_a_g$CI_high))), col = NA, fill = "grey", alpha = .2) +
#   theme_classic() +
#   xlab(c(expression(size[year_t]))) +
#   ylab(c(expression(size[year_t+1]))) +
#   scale_color_manual(values = c("grey60", "grey60", "grey60", "grey60", "grey60", "grey60", "grey60", "grey60")) +
#   theme(axis.ticks = element_blank(),
#         axis.text = element_blank(),
#         legend.position = "none") 
# #send plot to file
# pdf("./Manuscript/Figures/AreaEffect_Growth.pdf", width = 4, height = 4)
# AreaEffectGrowth       
# dev.off()

#### Make plot of fixed effect of neighborhood by species on growth ####

sppNeighPreds_g <- ggpredict(mGrowTLP_fac, terms = c("neighbors_10_s[all]", "species"), type = "random")
sppNeighPreds_g <- data.frame("x" = sppNeighPreds_g$x, "preds" = sppNeighPreds_g$predicted,"spp" = sppNeighPreds_g$group )

globPreds_n_g <- ggpredict(mGrowTLP_fac, terms = c("neighbors_10_s[all]"), type = "fixed")
globPreds_n_g <- data.frame("x" = globPreds_n_g$x, "preds" = globPreds_n_g$predicted, "spp" = as.factor("Global"), "CI_low" = globPreds_n_g$conf.low, "CI_high" = globPreds_n_g$conf.high)

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
meanSPEI_G <- mean(CO_grams$SPEI_s)
sdSPEI_G <- sd(CO_grams$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G) #2.10
SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G) #-1.48

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)
SLA_vals <- seq(min(CO_grams$SLA_s), max(CO_grams$SLA_s), length.out = 20)

SLA_G_dat <- ggpredict(m5, terms = c("SLA_s[SLA_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for RTD_s
RTD_vals <- seq(min(CO_grams$RTD_s, na.rm = TRUE), max(CO_grams$RTD_s, na.rm = TRUE), length.out = 20)
RTD_G_dat <- ggpredict(m10, terms = c("RTD_s[RTD_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for SRL_s
SRL_vals <- seq(min(CO_grams$SRL_s, na.rm = TRUE), max(CO_grams$SRL_s, na.rm = TRUE), length.out = 20)
SRL_G_dat <- ggpredict(m13, terms = c("SRL_s[SRL_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for RDiam_s
RDiam_vals <- seq(min(CO_grams$RDiam_s, na.rm = TRUE), max(CO_grams$RDiam_s, na.rm = TRUE), length.out = 20)
RDiam_G_dat <- ggpredict(m14, terms = c("RDiam_s[RDiam_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#make a data.frame to contain all of the values for each trait
GramDat <- data.frame(trait = c("scaled(Specific Leaf Area)"), x = SLA_G_dat$x, GramSurv = SLA_G_dat$predicted, CI_low = SLA_G_dat$conf.low, CI_high = SLA_G_dat$conf.high, SPEI = SLA_G_dat$group, lab = "A")

GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Specific Root Length)"), x = SRL_G_dat$x, GramSurv = SRL_G_dat$predicted, CI_low = SRL_G_dat$conf.low, CI_high = SRL_G_dat$conf.high, SPEI = SRL_G_dat$group, lab = "B"))
GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Root Tissue Density)"), x = RTD_G_dat$x, GramSurv = RTD_G_dat$predicted, CI_low = RTD_G_dat$conf.low, CI_high = RTD_G_dat$conf.high, SPEI = RTD_G_dat$group, lab = "C"))
GramDat <- rbind(GramDat, data.frame(trait = c("scaled(Avg. Root Diameter)"), x = RDiam_G_dat$x, GramSurv = RDiam_G_dat$predicted, CI_low = RDiam_G_dat$conf.low, CI_high = RDiam_G_dat$conf.high, SPEI = RDiam_G_dat$group, lab = "D"))
#make data for rug plot
RugDat <-  data.frame(rug = CO_grams$SLA_s, trait = "scaled(Specific Leaf Area)")
RugDat <- rbind(RugDat, data.frame(rug = CO_grams$SRL_s, trait = "scaled(Specific Root Length)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grams$RTD_s, trait = "scaled(Root Tissue Density)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grams$RDiam_s, trait = "scaled(Avg. Root Diameter)"))
#text for labels
dat_text <- data.frame(
  label = c("A", "B", "C", "D"),
  trait = c("scaled(Specific Leaf Area)", "scaled(Specific Root Length)", "scaled(Root Tissue Density)", "scaled(Avg. Root Diameter)"),
  x    = c(min(SLA_G_dat$x),min(SRL_G_dat$x),min(RTD_G_dat$x),min(RDiam_G_dat$x)),
  y     = c(1,1,1,1)
)


#make a multipanel figure
GramSurvExtraFig <- ggplot(data = GramDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x, GramSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat) +
  labs(title = NULL) +
  xlab(NULL) +
  ylab("P(Graminoid Survival)") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("goldenrod1", "royalblue2")) +
  scale_fill_manual(values = c("goldenrod1", "royalblue2"), guide = FALSE) +
  facet_wrap(vars(trait), scales = "free_x", strip.position =  "bottom") +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0))) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")

#for forbs
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI <- mean(CO_point_all$SPEI_s)
sdSPEI <- sd(CO_point_all$SPEI_s)
#get 97.5 quantile of the distribution
SPEI_97_5 <- qnorm(.975, meanSPEI, sdSPEI) 
SPEI_2_5 <- qnorm(.025, meanSPEI, sdSPEI)

spei_vals <- c(SPEI_2_5, SPEI_97_5)

#for SLA_s
SLA_vals <- seq(min(CO_point_all$SLA_s, na.rm = TRUE), max(CO_point_all$SLA_s, na.rm = TRUE), length.out = 20)
SLA_F_dat <- ggpredict(m6, terms = c("SLA_s[SLA_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)


#for RTD_s
RTD_vals <- seq(min(CO_point_all$RTD_s, na.rm = TRUE), max(CO_point_all$RTD_s, na.rm = TRUE), length.out = 20)
RTD_F_dat <- ggpredict(m12, terms = c("RTD_s[RTD_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for SRL_s
SRL_vals <- seq(min(CO_point_all$SRL_s, na.rm = TRUE), max(CO_point_all$SRL_s, na.rm = TRUE), length.out = 20)
SRL_F_dat <- ggpredict(m15, terms = c("SRL_s[SRL_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#for RDiam_s
RDiam_vals <- seq(min(CO_point_all$RDiam_s, na.rm = TRUE), max(CO_point_all$RDiam_s, na.rm = TRUE), length.out = 20)
RDiam_F_dat <- ggpredict(m16, terms = c("RDiam_s[RDiam_vals]", "SPEI_s[spei_vals]"), type = "fixed", back.transform = TRUE)

#make a data.frame to contain all of the values for each trait
ForbDat <- data.frame(trait = c("scaled(Specific Leaf Area)"), x = SLA_F_dat$x, GramSurv = SLA_F_dat$predicted, CI_low = SLA_F_dat$conf.low, CI_high = SLA_F_dat$conf.high, SPEI = SLA_F_dat$group, lab = "A")

ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Specific Root Length)"), x = SRL_F_dat$x, GramSurv = SRL_F_dat$predicted, CI_low = SRL_F_dat$conf.low, CI_high = SRL_F_dat$conf.high, SPEI = SRL_F_dat$group, lab = "B"))
ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Root Tissue Density)"), x = RTD_F_dat$x, GramSurv = RTD_F_dat$predicted, CI_low = RTD_F_dat$conf.low, CI_high = RTD_F_dat$conf.high, SPEI = RTD_F_dat$group, lab = "C"))
ForbDat <- rbind(ForbDat, data.frame(trait = c("scaled(Avg. Root Diameter)"), x = RDiam_F_dat$x, GramSurv = RDiam_F_dat$predicted, CI_low = RDiam_F_dat$conf.low, CI_high = RDiam_F_dat$conf.high, SPEI = RDiam_F_dat$group, lab = "D"))
#make data for rug plot
RugDat <-  data.frame(rug = CO_point_all$SLA_s, trait = "scaled(Specific Leaf Area)")
RugDat <- rbind(RugDat, data.frame(rug = CO_point_all$SRL_s, trait = "scaled(Specific Root Length)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_point_all$RTD_s, trait = "scaled(Root Tissue Density)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_point_all$RDiam_s, trait = "scaled(Avg. Root Diameter)"))
#text for labels
dat_text <- data.frame(
  label = c("A", "B", "C", "D"),
  trait = c("scaled(Specific Leaf Area)", "scaled(Specific Root Length)", "scaled(Root Tissue Density)", "scaled(Avg. Root Diameter)"),
  x    = c(min(SLA_F_dat$x),min(SRL_F_dat$x),min(RTD_F_dat$x),min(RDiam_F_dat$x)),
  y     = c(1,1,1,1)
)

#make a multipanel figure
ForbSurvExtraFig <- ggplot(data = ForbDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, GramSurv, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat) +
  labs(title = NULL) +
  xlab(NULL) +
  ylab("P(Forb Survival)") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("goldenrod1", "royalblue2")) +
  scale_fill_manual(values = c("goldenrod1", "royalblue2"), guide = FALSE) +
  facet_wrap(vars(trait), scales = "free_x", strip.position =  "bottom") +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0))) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")

#send plot to file
#for graminoids
pdf("./Manuscript/Figures/AllTraitsSurv_Grams.pdf", width = 5, height = 5)
GramSurvExtraFig
dev.off()

#for forbs
pdf("./Manuscript/Figures/AllTraitsSurv_Forbs.pdf", width = 5, height = 5)
ForbSurvExtraFig
dev.off()

#### model of all traits for growth models ####
#for forbs
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

#for RDiam_s
RDiam_vals <- seq(min(CO_grow_RDiam$RDiam_s, na.rm = TRUE), max(CO_grow_RDiam$RDiam_s, na.rm = TRUE), length.out = 20)
RDiam_grow_dat <- ggpredict(mGrowRDiam, terms = c("RDiam_s[RDiam_vals]", "SPEI_s[spei_vals]"), type = "fixed")

#make a data.frame to contain all of the values for each trait
GrowthDat <- data.frame(trait = c("scaled(Turgor Loss Point)"), x = TLP_grow_dat$x, Growth = TLP_grow_dat$predicted, CI_low = TLP_grow_dat$conf.low, CI_high = TLP_grow_dat$conf.high, SPEI = TLP_grow_dat$group)
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Leaf Dry Matter Content)"), x = LDMC_grow_dat$x, Growth = LDMC_grow_dat$predicted, CI_low = LDMC_grow_dat$conf.low, CI_high = LDMC_grow_dat$conf.high, SPEI = LDMC_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Root Dry Matter Content)"), x = RDMC_grow_dat$x, Growth = RDMC_grow_dat$predicted, CI_low = RDMC_grow_dat$conf.low, CI_high = RDMC_grow_dat$conf.high, SPEI = RDMC_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Specific Leaf Area)"), x = SLA_grow_dat$x, Growth = SLA_grow_dat$predicted, CI_low = SLA_grow_dat$conf.low, CI_high = SLA_grow_dat$conf.high, SPEI = SLA_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Specific Root Length)"), x = SRL_grow_dat$x, Growth = SRL_grow_dat$predicted, CI_low = SRL_grow_dat$conf.low, CI_high = SRL_grow_dat$conf.high, SPEI = SRL_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Root Tissue Density)"), x = RTD_grow_dat$x, Growth = RTD_grow_dat$predicted, CI_low = RTD_grow_dat$conf.low, CI_high = RTD_grow_dat$conf.high, SPEI = RTD_grow_dat$group))
GrowthDat <- rbind(GrowthDat, data.frame(trait = c("scaled(Avg. Root Diameter)"), x = RDiam_grow_dat$x, Growth = RDiam_grow_dat$predicted, CI_low = RDiam_grow_dat$conf.low, CI_high = RDiam_grow_dat$conf.high, SPEI = RDiam_grow_dat$group))

#make a data.frame with data for the rug plot
RugDat <- data.frame(rug = CO_grow_TLP$TLP_s, trait = "scaled(Turgor Loss Point)")
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_LDMC$LDMC_s, trait = "scaled(Leaf Dry Matter Content)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_RDMC$RDMC_s, trait = "scaled(Root Dry Matter Content)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_SLA$SLA_s, trait = "scaled(Specific Leaf Area)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_SRL$SRL_s, trait = "scaled(Specific Root Length)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_RTD$RTD_s, trait = "scaled(Root Tissue Density)"))
RugDat <- rbind(RugDat, data.frame(rug = CO_grow_RDiam$RDiam_s, trait = "scaled(Avg. Root Diameter)"))

#text for labels
dat_text <- data.frame(
  label = c("A", "B", "C", "D","E","F","G"),
  trait = c("scaled(Turgor Loss Point)", "scaled(Leaf Dry Matter Content)", "scaled(Root Dry Matter Content)", "scaled(Specific Leaf Area)", "scaled(Specific Root Length)", "scaled(Root Tissue Density)", "scaled(Avg. Root Diameter)"),
  x    = c(min(TLP_grow_dat$x), min(LDMC_grow_dat$x), min(RDMC_grow_dat$x), min(SLA_grow_dat$x),min(SRL_grow_dat$x),min(RTD_grow_dat$x),min(RDiam_grow_dat$x)),
  y     = c(1.2,1.2,1.2,1.2,1.2,1.2,1.2)
)

#make a multipanel figure
GrowthExtraFig <- ggplot(data = GrowthDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, Growth, col = SPEI))  + 
  geom_rug(aes(x = rug), data = RugDat) +
  labs(title = NULL) +
  xlab(NULL) +
  ylab(expression("Graminoid Growth: log" ~ bgroup("(",frac(size[year_t+1],size[year_t]),")")))  +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("goldenrod1", "royalblue2")) +
  scale_fill_manual(values = c("goldenrod1", "royalblue2"), guide = FALSE) +
  facet_wrap(vars(trait), scales = "free_x", strip.position =  "bottom") +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0))) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")


pdf("./Manuscript/Figures/AllTraitsGrowth.pdf", width = 8.5, height = 6.5)
GrowthExtraFig
dev.off()
