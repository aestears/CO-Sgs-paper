<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> aa56e37... update SPEI script
#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Calculating species-level SPEI intervals
# Script 3 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////
<<<<<<< HEAD

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
=======
#####################
# Calculating SPEI on a by-species basis for CO dataset
#####################
require(tidyverse)
#### Load data files ####
#set working directory
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project")
#load polygon data
CO_poly_all <- read.csv("./Data/CO Analysis Data Files/Intermediate Analysis Files/poly_demo_Traits_2_24_20.csv")
#load point data
CO_point_all <- read.csv("./Data/CO Analysis Data Files/Intermediate Analysis Files/point_demo_Traits_2_24_20.csv")
=======
>>>>>>> aa56e37... update SPEI script

#### load packages ####
require(tidyverse) #v1.3.0

## clear workspace ##
rm(list=ls())

#### Load data files ####
#set working directory
datWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper"  #a path for the directory containing the /scripts folder
setwd(datWD)
load("./scripts/script1_output.RData")

CO_poly_all <-  CO_poly_surv_traits_quads
CO_point_all  <- CO_point_surv_traits_quads
rm(list=c("CO_point_surv_traits_quads", "CO_poly_surv_traits_quads"))

#load SPEI dataset
# source: https://spei.csic.es/database.html
climWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Climate Data/" #set wd to the location of the CO_SPEI.csv file
setwd(climWD)

CO_SPEI_all <- read.csv("./SPEI Dataframes/CO_SPEI.csv")

#format dates correctly
CO_SPEI_all$Date <- as.POSIXct(CO_SPEI_all$Date, tz="UTC", format="%m/%d/%Y %H:%M")
CO_SPEI_all$Year <- lubridate::year(CO_SPEI_all$Date)
CO_SPEI_all$Year[1:1188] <- CO_SPEI_all$Year[1:1188]+1900
CO_SPEI_all$Year[1189:1380] <- CO_SPEI_all$Year[1189:1380] + 2000
lubridate::year(CO_SPEI_all$Date) <- CO_SPEI_all$Year

#### Calculate SPEI for CO Dataset #####
### Set up Julian day/month info ###

## make a data.frame for first days of the month in the Julian Year
monthsJ <- data.frame(month = c("01","02","03","04","05","06",
                                "07", "08", "09", "10", "11", "12"),
                      day = c(1, 32, 60, 91, 121, 152, 182, 213, 
                              244, 274, 305, 335))

#determine the number of unique species in the dataset and then get the start and end of SPEI for each species
sppSPEI_poly <- unique(CO_poly_all[,c("species","Flwr_Start","Flwr_End","Flwr_Total","Flwr_Peak")])
sppSPEI_point <- unique(CO_point_all[,c("species","Flwr_Start","Flwr_End","Flwr_Total","Flwr_Peak")])
#Combine point and poly species lists
sppSPEI <- rbind(sppSPEI_poly, sppSPEI_point)

#### fill in gaps in phenology data ####
#calculate start and end for "Carex spp." by averaging dates for Carex eleocharis and Carex filifolia
sppSPEI[sppSPEI$species=="Carex spp.",] <- c("Carex spp.",
                                             mean(sppSPEI[sppSPEI$species=="Carex eleocharis","Flwr_Start"],sppSPEI[sppSPEI$species=="Carex filifolia","Flwr_Start"]),
                                             mean(sppSPEI[sppSPEI$species=="Carex eleocharis","Flwr_End"],sppSPEI[sppSPEI$species=="Carex filifolia","Flwr_End"]),
                                             (mean(sppSPEI[sppSPEI$species=="Carex eleocharis","Flwr_End"],sppSPEI[sppSPEI$species=="Carex filifolia","Flwr_End"])-mean(sppSPEI[sppSPEI$species=="Carex eleocharis","Flwr_Start"],sppSPEI[sppSPEI$species=="Carex filifolia","Flwr_Start"])),
                                             (mean(sppSPEI[sppSPEI$species=="Carex eleocharis","Flwr_Start"],sppSPEI[sppSPEI$species=="Carex filifolia","Flwr_Start"])+((mean(sppSPEI[sppSPEI$species=="Carex eleocharis","Flwr_End"],sppSPEI[sppSPEI$species=="Carex filifolia","Flwr_End"])-mean(sppSPEI[sppSPEI$species=="Carex eleocharis","Flwr_Start"],sppSPEI[sppSPEI$species=="Carex filifolia","Flwr_Start"])))/2))

#get dates for Astragalus spp. (use May to July, estimate from Ackerfield CO Flora)
sppSPEI[sppSPEI$species=="Astragalus/Oxytropis spp.",] <-  c("Astragalus/Oxytropis spp.", 121, 212, 91, 166.5)

sppSPEI[sppSPEI$species=="Astragalus spp.",] <-  c("Astragalus spp.", 121, 212, 91, 166.5)

#get dates for Thelesperma spp. (use June to Sept, estimate from Ackerfield CO Flora)
sppSPEI[sppSPEI$species=="Thelesperma spp.",] <-  c("Thelesperma spp.", 152, 273, 121, 212.5)

#get dates for Allium spp. (use May to Aug, estimate from Ackerfield CO Flora)
sppSPEI[sppSPEI$species=="Allium spp.",] <-  c("Allium spp.", 121, 243, 122, 182)

#### determine the closest month to the start and end date of flowering ####
#start of flowering date
sppSPEI$SPEI_start <- NA

sppSPEI$Flwr_Start <- as.numeric(sppSPEI$Flwr_Start)
sppSPEI$Flwr_End <- as.numeric(sppSPEI$Flwr_End)
sppSPEI$Flwr_Total <- as.numeric(sppSPEI$Flwr_Total)
sppSPEI$Flwr_Peak <- as.numeric(sppSPEI$Flwr_Peak)

#calculate start of SPEI
for (i in 1:nrow(sppSPEI)) {
    if(is.na(sppSPEI$Flwr_Start[i])==FALSE){
    if (sppSPEI$Flwr_Start[i] <=17) {
      sppSPEI$SPEI_start[i] <- 01
    } else {
      if(sppSPEI$Flwr_Start[i] > 17 & sppSPEI$Flwr_Start[i] <=46) {
        sppSPEI$SPEI_start[i] <- 02
      } else {
        if(sppSPEI$Flwr_Start[i] > 46 & sppSPEI$Flwr_Start[i] <= 76) {
          sppSPEI$SPEI_start[i] <- 03
        } else {
          if(sppSPEI$Flwr_Start[i] > 76 & sppSPEI$Flwr_Start[i] <= 106) {
            sppSPEI$SPEI_start[i] <- 04
          } else {
            if(sppSPEI$Flwr_Start[i] > 106 & sppSPEI$Flwr_Start[i] <= 137) {
              sppSPEI$SPEI_start[i] <- 05
            } else {
              if(sppSPEI$Flwr_Start[i] > 137 & sppSPEI$Flwr_Start[i] <= 167){
                sppSPEI$SPEI_start[i] <- 06
              } else {
                if(sppSPEI$Flwr_Start[i] > 167 & sppSPEI$Flwr_Start[i] <= 198) {
                  sppSPEI$SPEI_start[i] <- 07
                } else {
                  if(sppSPEI$Flwr_Start[i] > 198 & sppSPEI$Flwr_Start[i] <= 229) {
                    sppSPEI$SPEI_start[i] <- 08
                  } else {
                    if(sppSPEI$Flwr_Start[i] > 229 & sppSPEI$Flwr_Start[i] <= 259) {
                      sppSPEI$SPEI_start[i] <- 09
                    } else {
                      if(sppSPEI$Flwr_Start[i] > 259 & sppSPEI$Flwr_Start[i] <= 290) {
                        sppSPEI$SPEI_start[i] <- 10
                      } else {
                        if(sppSPEI$Flwr_Start[i] > 290 & sppSPEI$Flwr_Start[i] <= 315) {
                          sppSPEI$SPEI_start[i] <- 11
                        } else {
                          if(sppSPEI$Flwr_Start[i] > 315 & sppSPEI$Flwr_Start[i] <= 365) {
                            sppSPEI$SPEI_start[i] <- 12
                          } else {
                            sppSPEI$SPEI_start[i] <- NA
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    }
  }

#calculate end of SPEI
#end of flowering date for polygons
sppSPEI$SPEI_end <- NA
for (i in 1:nrow(sppSPEI)) {
  if(is.na(sppSPEI$Flwr_End[i])==FALSE){
  if (sppSPEI$Flwr_End[i] <=17) {
    sppSPEI$SPEI_end[i] <- 01
  } else {
    if(sppSPEI$Flwr_End[i] > 17 & sppSPEI$Flwr_End[i] <=46) {
      sppSPEI$SPEI_end[i] <- 02
    } else {
      if(sppSPEI$Flwr_End[i] > 46 & sppSPEI$Flwr_End[i] <= 76) {
        sppSPEI$SPEI_end[i] <- 03
      } else {
        if(sppSPEI$Flwr_End[i] > 76 & sppSPEI$Flwr_End[i] <= 106) {
          sppSPEI$SPEI_end[i] <- 04
        } else {
          if(sppSPEI$Flwr_End[i] > 106 & sppSPEI$Flwr_End[i] <= 137) {
            sppSPEI$SPEI_end[i] <- 05
          } else {
            if(sppSPEI$Flwr_End[i] > 137 & sppSPEI$Flwr_End[i] <= 167){
              sppSPEI$SPEI_end[i] <- 06
            } else {
              if(sppSPEI$Flwr_End[i] > 167 & sppSPEI$Flwr_End[i] <= 198) {
                sppSPEI$SPEI_end[i] <- 07
              } else {
                if(sppSPEI$Flwr_End[i] > 198 & sppSPEI$Flwr_End[i] <= 229) {
                  sppSPEI$SPEI_end[i] <- 08
                } else {
                  if(sppSPEI$Flwr_End[i] > 229 & sppSPEI$Flwr_End[i] <= 259) {
                    sppSPEI$SPEI_end[i] <- 09
                  } else {
                    if(sppSPEI$Flwr_End[i] > 259 & sppSPEI$Flwr_End[i] <= 290) {
                      sppSPEI$SPEI_end[i] <- 10
                    } else {
                      if(sppSPEI$Flwr_End[i] > 290 & sppSPEI$Flwr_End[i] <= 315) {
                        sppSPEI$SPEI_end[i] <- 11
                      } else {
                        if(sppSPEI$Flwr_End[i] > 315 & sppSPEI$Flwr_End[i] <= 365) {
                          sppSPEI$SPEI_end[i] <- 12
                        } else {
                          NA
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  }
}

#calculate the duration of flowering for SPEI calculations
sppSPEI$SPEI_duration <- sppSPEI$SPEI_end - sppSPEI$SPEI_start

#### add SPEI start, end, and duration to the CO_poly dataset
CO_poly_all <- CO_poly_all %>% 
  select(-c(Flwr_Start, Flwr_End, Flwr_Total, Flwr_Peak)) %>% 
  left_join(sppSPEI, by = c("species"))

#### add SPEI start, end, and duration to the CO_point dataset
CO_point_all <- CO_point_all %>% 
  select(-c(Flwr_Start, Flwr_End, Flwr_Total, Flwr_Peak)) %>% 
  left_join(sppSPEI, by = c("species"))

#### calculate unique SPEI for each species
#make a new column for SPEI_unique in polygon dataset
CO_poly_all$SPEI_unique <- NA

#make a vector of the possible years in the datasets
years <- unique(CO_SPEI_all$Year)
for (i in 1:nrow(CO_poly_all)){
  if(is.na(CO_poly_all$SPEI_duration[i])==FALSE){
    CO_poly_all$SPEI_unique[i] <- (select((CO_SPEI_all[(CO_SPEI_all$Year==CO_poly_all$year[i] & lubridate::month(CO_SPEI_all$Date)==CO_poly_all$SPEI_end[i]),
                                                       c(1,str_which(colnames(CO_SPEI_all), as.character(CO_poly_all$SPEI_duration[i])),9)]), 
                                          paste("SPEI_",CO_poly_all$SPEI_duration[i], sep="")))[1,1]
  }
}


CO_poly_all$SPEI_unique <- unlist(CO_poly_all$SPEI_unique)

#### Assign the appropriate SPEI values to each species in point Dataset ####
#make a new column for SPEI_unique in point dataset
CO_point_all$SPEI_unique <- NA
#make a vector of the possible years in the datasets
years <- unique(CO_SPEI_all$Year)
for (i in 1:nrow(CO_point_all)){
  CO_point_all$SPEI_unique[i] <- (select((CO_SPEI_all[(CO_SPEI_all$Year==CO_point_all$year[i] & lubridate::month(CO_SPEI_all$Date)==CO_point_all$SPEI_end[i]),
                                                      c(1,str_which(colnames(CO_SPEI_all), as.character(CO_point_all$SPEI_duration[i])),9)]), 
                                         paste("SPEI_",CO_point_all$SPEI_duration[i], sep="")))[1,1]
}
CO_point_all$SPEI_unique <- unlist(CO_point_all$SPEI_unique)

#### save workspace image for next script ####
rm(list = ls()[!(ls() %in% c('CO_point_all','CO_poly_all'))])

<<<<<<< HEAD
KS_min <- qnorm(.025 , mean = mean(KS_only$SPEI_6), sd(KS_only$SPEI_6))
KS_max <- qnorm(.975 , mean = mean(KS_only$SPEI_6), sd(KS_only$SPEI_6))
>>>>>>> 56c8fd3... added scripts for data analysis
=======
## for next script, need CO_point_all and CO_poly_all data.frames
#save as an .RData file 
path = "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper/scripts" #location where you'll put the environment data file
setwd(path)
save.image('script2_output.RData')
>>>>>>> aa56e37... update SPEI script
