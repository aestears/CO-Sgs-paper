#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Calculating species-level SPEI intervals
# Script 3 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

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

## for next script, need CO_point_all and CO_poly_all data.frames
#save as an .RData file 
path = "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/CO-Sgs-paper/scripts" #location where you'll put the environment data file
setwd(path)
save.image('script2_output.RData')
