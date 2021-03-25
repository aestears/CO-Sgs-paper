#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Calculating SPEI w/ climate data from study site
# Alice Stears, astears@uwyo.edu
# Created 17 February 2021
# R version 4.0.3 
#//////////////////////////

#### load packages ####
require(tidyverse) #v1.3.0
require(SPEI)

## clear workspace ##
rm(list=ls())

## set working directory ##
path <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data"
setwd(path)

## read in climate data file
clim <- read.csv("./CO_daily_climate_data.csv")

#### Format data for SPEI function ####
## get mean monthly average temp (C)
  # meanT
## get mean monthly daily minimum temp (C)
  # meanMaxT
## get mean monthly daily maximum temp (C)
  # meanMinT
## get monthly total precip (mm)
  # precip
## get latitude of site 
  # 40.86813765	
# calculate mean values
climMonth <- clim %>% 
  group_by(year, month) %>% 
  summarize(meanT = mean(meanT, na.rm = TRUE),
            meanMaxT = mean(maxT, na.rm = TRUE),
            meanMinT = mean(minT, na.rm = TRUE),
            precip = sum(precip, na.rm = TRUE)) 


climMonth$Date <- as.character(paste0(climMonth$year, "-", str_pad(climMonth$month, width = 2, side = "left", pad = "0"), "-15"))

# format a date column as POSIXct
climMonth$Date <- as.POSIXct(climMonth$Date, tz = "GMT", format = "%Y-%m-%d")

#### Compute potential evapotranspiration (PET) and climatic water balance (BAL) #### 
# Compute potential evapotranspiration (PET)
climMonth$PET <- as.vector(thornthwaite(Tave = climMonth$meanT, lat = 40.86813765))
# Compute climatic water balance (BAL)
climMonth$BAL <- as.vector(climMonth$precip - climMonth$PET)

#### Calculate SPEI ####
for(i in 1:12) {
  #get SPEI for specified interval
  tmp <- spei(climMonth[,'BAL'], i)
  #get out SPEI results
   tmp2<- spei4$fitted %>% 
    as.data.frame() 
   #rename the column to reflect the SPEI interval
   names(tmp2)  <- paste0("SPEI_",i)
    #join to climMonth data.frame
   climMonth <- cbind(climMonth, tmp2)
  }



