#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Data Cleaning and file preparation
# Script 2 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

##### load packages #####
require(tidyverse) #v1.3.0

## clear workspace ##
rm(list=ls())

##### Load data files #####
#source the previous script ("0_NearestNeighborCalcs.R") to get "points" and "poly" data.frames
nearWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper"  #a path for the directory containing the /scripts folder
setwd(nearWD)
load("./scripts/script0_output.RData")

##load trait data 
# data source: Blumenthal, 2020 (https://doi.org/10.1111/1365-2745.13454) 
# Except: all trait values for Schedonnardus paniculatus, root trait values for Sitanion hystrix, Stipa comata RDMC, Allium textile RDMC and root diameter values, and all trait values for Vicia americana
datWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files" #set working directory for location of flowering time data file
setwd(datWD)
CO_traits <- read.csv("./CO_mean_traits.csv", stringsAsFactors = FALSE)

#load climate data
CO_climate <- read.csv("./CO_Climate_All.csv")
#reformat date
CO_climate$Date <- as.POSIXct(CO_climate$Date, format = "%m/%d/%y")
CO_climate$year <- lubridate::year(CO_climate$Date)
#get dataset w/ just annual averages
CO_climate<- CO_climate[is.na(CO_climate$Ann.Sum.Precip)==FALSE,c("Site", "Date", "year", "Ann.Sum.Precip", "Ann.Avg.T")]

#### merge datasets for polygon (grass) data ####
#merge survival data with trait data
CO_poly_surv_traits <- left_join(poly, CO_traits, by=c("species"="species"))
#merge cliamte data with trait/survival data
CO_poly_all <- left_join(CO_poly_surv_traits, CO_climate[,c("Ann.Sum.Precip", "Ann.Avg.T", "year")], by = c("year_t" = "year"))


#### merge datasets for point (forb) data ####
# merge trait data with the survival data ###
CO_point_surv_traits <- left_join (points, CO_traits, by=c("species"="species"))
CO_point_all <- left_join(CO_point_surv_traits, CO_climate[,c("Ann.Sum.Precip", "Ann.Avg.T", "year")], by = c("year" = "year"))

#### save workspace image for next script ####
rm(list = ls()[!(ls() %in% c('CO_point_all','CO_poly_all'))])

## for next script, need CO_point_surv_traits_quads and CO_poly_surv_traits_quads data.frames
#save as an .RData file 
path = "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper/scripts" #location where you'll put the environment data file
setwd(path)
save.image('script1_output.RData')

