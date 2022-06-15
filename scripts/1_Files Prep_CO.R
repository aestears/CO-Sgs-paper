#//////////////////////////
# Water availability dictates how plant traits predict demographic rates
# Data Cleaning and file preparation
# Script 2 of 6
# Alice Stears, alice.e.stears@gmail.com
# Revised 15 June 2022
# R version 4.0.3 
#//////////////////////////

##### load packages #####

library(tidyverse) #v1.3.0

##### Load data files #####
#source the previous script ("0_NearestNeighborCalcs.R") to get "points" and "poly" data.frames
# #a path for the directory containing the /scripts folder
# nearWD <- "name of file directory"
setwd(nearWD)

#load("./scripts/script0_output.RData")

##load trait data 
# data source: Blumenthal, 2020 (https://doi.org/10.1111/1365-2745.13454) 
# Except: all trait values for Schedonnardus paniculatus, root trait values for Sitanion hystrix, Stipa comata RDMC, Allium textile RDMC and root diameter values, and all trait values for Vicia americana
# datWD <- #set working directory for location of trait data file
setwd(datWD)
CO_traits <- read.csv(".//CO_mean_traits.csv", stringsAsFactors = FALSE)

#make sure TLP data is correct
#forbs: πTLP = 0.80πo–0.845
#for forbs
CO_traits[CO_traits$Functional_Group=="F", "TLP"] <- 0.8 * CO_traits[CO_traits$Functional_Group=="F", "LeafOsmoticPotential_Mpa"] - 0.845

#graminoids: πtlp = 0.944πo–0.611; r2 = 0.96
#for grams
CO_traits[CO_traits$Functional_Group=="G", "TLP"] <- 0.944 * CO_traits[CO_traits$Functional_Group=="G", "LeafOsmoticPotential_Mpa"] - 0.611

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
#merge climate data with trait/survival data
CO_poly_all <- left_join(CO_poly_surv_traits, CO_climate[,c("Ann.Sum.Precip", "Ann.Avg.T", "year")], by = c("year_t" = "year"))


#### merge datasets for point (forb) data ####
# merge trait data with the survival data ###
CO_point_surv_traits <- left_join (point, CO_traits, by=c("species"="species"))
CO_point_all <- left_join(CO_point_surv_traits, CO_climate[,c("Ann.Sum.Precip", "Ann.Avg.T", "year")], by = c("year" = "year"))

#### save workspace image for next script ####
rm(list = ls()[!(ls() %in% c('CO_point_all','CO_poly_all'))])

## for next script, need CO_point_surv_traits_quads and CO_poly_surv_traits_quads data.frames
#save as an .RData file 
#path = location where you'll put the environment data file

setwd(path)
save.image('script1_output.RData')

###############
# Preparing CO Shortgrass Steppe Data for Analysis
###############

library(tidyverse) #v1.3.0

##### Load data files #####
#source the previous script ("0_NearestNeighborCalcs.R") to get "points" and "poly" data.frames
# #a path for the directory containing the /scripts folder
setwd(nearWD)
source("/scripts/0_NearestNeighborCalcs.R")

#load point survival data
CO_point_surv <- read.csv("./Intermediate Analysis Files/point_demo_2_23_20.csv")
#load polygon survival data
CO_poly_surv <- read.csv("./Intermediate Analysis Files/polygon_demo_2_23_20.csv")
#load flowering survival data 
flowering <- read.csv("./all Flowering Time data.csv")
#load USDA ARS trait data
CO_traits <- read.csv("./Mean trait values_for Analysis.csv", stringsAsFactors = FALSE)
CO_traits$TLP <- as.numeric(CO_traits$TLP)
CO_traits$LDMC_g_g <- as.numeric(CO_traits$LDMC_g_g)
CO_traits$SLA_adj_cm2_g <- as.numeric(CO_traits$SLA_adj_cm2_g)
CO_traits$RDMC_g_g <- as.numeric(CO_traits$RDMC_g_g)
CO_traits$RTD_g_cm3 <- as.numeric(CO_traits$RTD_g_cm3)
CO_traits$SRL_best_m_g <- as.numeric(CO_traits$SRL_best_m_g)
CO_traits$AvgDiam_mm <- as.numeric(CO_traits$AvgDiam_mm)

#load quadrat information
CO_quads <- read.csv("./quad_info_all.csv")

##load trait data 
# data source: Blumenthal, 2020 (https://doi.org/10.1111/1365-2745.13454) 
# Except: all trait values for Schedonnardus paniculatus, root trait values for Sitanion hystrix, Stipa comata RDMC, Allium textile RDMC and root diameter values, and all trait values for Vicia americana
# datWD <- #set working directory for location of flowering time data file
setwd(datWD)
CO_traits <- read.csv("./CO_mean_traits.csv", stringsAsFactors = FALSE)


#make sure TLP data is correct
#forbs: πTLP = 0.80πo–0.845
#for forbs
CO_traits[CO_traits$Functional_Group=="F", "TLP"] <- 0.8 * CO_traits[CO_traits$Functional_Group=="F", "LeafOsmoticPotential_Mpa"] - 0.845

#graminoids: πtlp = 0.944πo–0.611; r2 = 0.96
#for grams
CO_traits[CO_traits$Functional_Group=="G", "TLP"] <- 0.944 * CO_traits[CO_traits$Functional_Group=="G", "LeafOsmoticPotential_Mpa"] - 0.611

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
