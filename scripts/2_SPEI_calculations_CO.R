#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Calculating species-level SPEI intervals
# Script 3 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

#### load packages ####
library(tidyverse) #v1.3.0

## clear workspace ##
rm(list=ls())

#### Load data files ####
### load data from previous script ###
## set working directory ##
# datWD <-  #a path for the directory containing the /scripts folder
setwd(datWD)
load("./scripts/script1_output.RData")

### load climate data ###
## set working directory  ##
# path <-  ##file location of climate dataset
setwd(path)


## read in climate data file ##
# This climate data is measured at the LTER site
clim <- read.csv("./CO_daily_climate_data.csv")

#### Site-Data SPEI calculations ####
# The following code calculates SPEI using the "SPEI" package and precip and temp data measured at the LTER site. PET is estimated using the thornthwaite method
### Format data for SPEI function ###
climMonth <- clim %>% 
  group_by(year, month) %>% 
  summarize(meanT = mean(meanT, na.rm = TRUE),
            meanMaxT = mean(maxT, na.rm = TRUE),
            meanMinT = mean(minT, na.rm = TRUE),
            precip = sum(precip, na.rm = TRUE)) 


climMonth$Date <- as.character(paste0(climMonth$year, "-", str_pad(climMonth$month, width = 2, side = "left", pad = "0"), "-15"))

# format a date column as POSIXct
climMonth$Date <- as.POSIXct(climMonth$Date, tz = "GMT", format = "%Y-%m-%d")

### Compute potential evapotranspiration (PET) and climatic water balance (BAL) ####
# Compute potential evapotranspiration (PET) using thornthwaite method
climMonth$PET <- as.vector(thornthwaite(Tave = climMonth$meanT, lat = 40.86813765))
# Compute climatic water balance (BAL)
climMonth$BAL <- as.vector(climMonth$precip - climMonth$PET)

### Calculate SPEI ###
# for each month and each interval (from 1 to 12)
for(i in 1:12) {
  #get SPEI for specified interval
  tmp <- spei(climMonth[,'BAL'], i)
  #get out SPEI results
  tmp2<- tmp$fitted %>% 
    as.data.frame() 
  #rename the column to reflect the SPEI interval
  names(tmp2)  <- paste0("SPEI_",i)
  #join to climMonth data.frame
  climMonth <- cbind(climMonth, tmp2)
}

## trim data.frame to only have Date and SPEI values
CO_SPEI_all <- climMonth  %>% 
  as_tibble() %>% 
  mutate(Site = "CO") %>% 
  select(Site, Date, SPEI_1, SPEI_2, SPEI_3, SPEI_4, SPEI_5, SPEI_6, SPEI_7, SPEI_8, SPEI_9, SPEI_10, SPEI_11, SPEI_12)


#### SPEIbase SPEI calculations ####
# The following code imports SPEI values from the SPEIbase dataset (https://spei.csic.es/database.html), which uses PET values estimated using the Penman-Monteith method, which is far superior to the Thornthwaite method. This dataset uses modeled climate data. This might be a better SPEI value to use, even though we have site-level climate data available, since PM is a much better way of estimating PET. 
# the lat/long of the modeled data is (-104.89, 40.50)
# downloaded data is for a 4-month SPEI interval
## USE THIS METRIC ## (better estimate of PET)
### load data ###
## set wd
# path <-  ##file location of climate dataset
setwd(path)
## load dataset
SPEIbase_04 <- read.table("./SPEIbase_CO_LTER_NEG104.89_40.50_SPEI04.csv", sep = ";", header = TRUE) 
names(SPEIbase_04) <- c("Date", "SPEI_04")
# reformat date
SPEIbase_04$Date <- as.POSIXct(SPEIbase_04$Date, tz = "GMT", format = "%Y-%m-%d")

#### compare SPEI versions ####
# ggplot() +
#   geom_path(data = CO_SPEI_all, aes(x = Date, y = SPEI_4), col = "darkgreen") +
#   geom_path(data = SPEIbase_04[lubridate::year(SPEIbase_04$Date) %in% 1997:2010,], aes(x = Date, y = SPEI_04), col = "darkblue") +
#   theme_classic()

# I think that the SPEIbase version is a bit more reliable, since it uses Penman-Monteith PET. I'll use those values in the rest of the analysis. 


#### calculate SPEI for CO Dataset (uniformly for all species) ####
## get correct SPEI variables ##
# peak 'greenness' occurs around 215 DOY (according to CPER data)--beginning of August
# calculate SPEI with an interval from May to Sept. , SEPT w/ a 4 month interval


# get the appropriate SPEI data (ending in month 9, SPEI interval of 4 months)
CO_SPEI_uniform <- SPEIbase_04[lubridate::month(SPEIbase_04$Date)==9,c("Date","SPEI_04")]
# make year column
CO_SPEI_uniform$Year <- lubridate::year(CO_SPEI_uniform$Date)
names(CO_SPEI_uniform) <- c("Date", "SPEI_uniform", "Year")
#combine w/ poly and point datasets
CO_poly_all <- left_join(CO_poly_all, CO_SPEI_uniform[,c("SPEI_uniform", "Year")], by = c("year_t"= "Year"))

CO_point_all <- left_join(CO_point_all, CO_SPEI_uniform[,c("SPEI_uniform", "Year")], by = c("year"= "Year"))

#### save workspace image for next script ####
rm(list = ls()[!(ls() %in% c('CO_point_all','CO_poly_all'))])

## for next script, need CO_point_all and CO_poly_all data.frames
#save as an .RData file 
# path = #location where you'll put the environment data file
setwd(path)
save.image('script2_output.RData')
