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

#SPEI datasets
AZn_SPEI_all <- read.csv("./Data/Climate Data/SPEI Dataframes/AZn_SPEI.csv")
AZs_SPEI_all <- read.csv("./Data/Climate Data/SPEI Dataframes/AZs_SPEI.csv")
CO_SPEI_all <- read.csv("./Data/Climate Data/SPEI Dataframes/CO_SPEI.csv")
KS_SPEI_all <- read.csv("./Data/Climate Data/SPEI Dataframes/KS_SPEI.csv")
ID_SPEI_all <- read.csv("./Data/Climate Data/SPEI Dataframes/ID_SPEI.csv")
MT_SPEI_all <- read.csv("./Data/Climate Data/SPEI Dataframes/MT_SPEI.csv")
NM_SPEI_all <- read.csv("./Data/Climate Data/SPEI Dataframes/NM_SPEI.csv")

AZn_SPEI_all$Date <- as.POSIXct(AZn_SPEI_all$Date, tz="UTC", format="%m/%d/%Y %H:%M")
AZn_SPEI_all$Year <- lubridate::year(AZn_SPEI_all$Date)
AZn_SPEI_all$Year[1:1188] <- AZn_SPEI_all$Year[1:1188]+1900
AZn_SPEI_all$Year[1189:1380] <- AZn_SPEI_all$Year[1189:1380] + 2000
lubridate::year(AZn_SPEI_all$Date) <- AZn_SPEI_all$Year

AZs_SPEI_all$Date <- as.POSIXct(AZs_SPEI_all$Date, tz="UTC", format="%m/%d/%Y %H:%M")
AZs_SPEI_all$Year <- lubridate::year(AZs_SPEI_all$Date)
AZs_SPEI_all$Year[1:1188] <- AZs_SPEI_all$Year[1:1188]+1900
AZs_SPEI_all$Year[1189:1380] <- AZs_SPEI_all$Year[1189:1380] + 2000
lubridate::year(AZs_SPEI_all$Date) <- AZs_SPEI_all$Year

CO_SPEI_all$Date <- as.POSIXct(CO_SPEI_all$Date, tz="UTC", format="%m/%d/%Y %H:%M")
CO_SPEI_all$Year <- lubridate::year(CO_SPEI_all$Date)
CO_SPEI_all$Year[1:1188] <- CO_SPEI_all$Year[1:1188]+1900
CO_SPEI_all$Year[1189:1380] <- CO_SPEI_all$Year[1189:1380] + 2000
lubridate::year(CO_SPEI_all$Date) <- CO_SPEI_all$Year

KS_SPEI_all$Date <- as.POSIXct(KS_SPEI_all$Date, tz="UTC", format="%m/%d/%Y %H:%M")
KS_SPEI_all$Year <- lubridate::year(KS_SPEI_all$Date)
KS_SPEI_all$Year[1:1188] <- KS_SPEI_all$Year[1:1188]+1900
KS_SPEI_all$Year[1189:1380] <- KS_SPEI_all$Year[1189:1380] + 2000
lubridate::year(KS_SPEI_all$Date) <- KS_SPEI_all$Year

ID_SPEI_all$Date <- as.POSIXct(ID_SPEI_all$Date, tz="UTC", format="%m/%d/%Y %H:%M")
ID_SPEI_all$Year <- lubridate::year(ID_SPEI_all$Date)
ID_SPEI_all$Year[1:1188] <- ID_SPEI_all$Year[1:1188]+1900
ID_SPEI_all$Year[1189:1380] <- ID_SPEI_all$Year[1189:1380] + 2000
lubridate::year(ID_SPEI_all$Date) <- ID_SPEI_all$Year

MT_SPEI_all$Date <- as.POSIXct(MT_SPEI_all$Date, tz="UTC", format="%m/%d/%Y %H:%M")
MT_SPEI_all$Year <- lubridate::year(MT_SPEI_all$Date)
MT_SPEI_all$Year[1:1188] <- MT_SPEI_all$Year[1:1188]+1900
MT_SPEI_all$Year[1189:1380] <- MT_SPEI_all$Year[1189:1380] + 2000
lubridate::year(MT_SPEI_all$Date) <- MT_SPEI_all$Year

NM_SPEI_all$Date <- as.POSIXct(NM_SPEI_all$Date, tz="UTC", format="%m/%d/%Y %H:%M")
NM_SPEI_all$Year <- lubridate::year(NM_SPEI_all$Date)
NM_SPEI_all$Year[1:1188] <- NM_SPEI_all$Year[1:1188]+1900
NM_SPEI_all$Year[1189:1380] <- NM_SPEI_all$Year[1189:1380] + 2000
lubridate::year(NM_SPEI_all$Date) <- NM_SPEI_all$Year

#### Calculate SPEI for CO Dataset ####
#### Set up Julian day/month info ####

## make a data.frame for first days of the month in the Julian Year
monthsJ <- data.frame(month = c("01","02","03","04","05","06",
                                "07", "08", "09", "10", "11", "12"),
                      day = c(1, 32, 60, 91, 121, 152, 182, 213, 
                              244, 274, 305, 335))

#### SPEI Month calculations for POLYGONS ####

names(CO_poly_all)[87] <- "flowering_start"
names(CO_poly_all)[88] <- "flowering_end"
names(CO_poly_all)[89] <- "flowering_total"
names(CO_poly_all)[90] <- "flowering_peak"

#### SPEI Month calculations for POINTS ####
names(CO_point_all)[82] <- "flowering_start"
names(CO_point_all)[83] <- "flowering_end"
names(CO_point_all)[84] <- "flowering_total"
names(CO_point_all)[85] <- "flowering_peak"

## determine the closest month to the start and end date of flowering
##using a for loop

#determine the number of unique species in the dataset and then get the start and end of SPEI for each species
sppSPEI_poly <- unique(CO_poly_all[,c("species","flowering_start","flowering_end","flowering_total","flowering_peak")])
sppSPEI_point <- unique(CO_point_all[,c("species","flowering_start","flowering_end","flowering_total","flowering_peak")])
#Combine point and poly species lists
sppSPEI <- rbind(sppSPEI_poly, sppSPEI_point)

#calculate start and end for "Carex spp." by averaging dates for Carex eleocharis and Carex filifolia
sppSPEI[sppSPEI$species=="Carex spp.",] <- c("Carex spp.",
                                             mean(sppSPEI[sppSPEI$species=="Carex eleocharis","flowering_start"],sppSPEI[sppSPEI$species=="Carex filifolia","flowering_start"]),
                                             mean(sppSPEI[sppSPEI$species=="Carex eleocharis","flowering_end"],sppSPEI[sppSPEI$species=="Carex filifolia","flowering_end"]),
                                             (mean(sppSPEI[sppSPEI$species=="Carex eleocharis","flowering_end"],sppSPEI[sppSPEI$species=="Carex filifolia","flowering_end"])-mean(sppSPEI[sppSPEI$species=="Carex eleocharis","flowering_start"],sppSPEI[sppSPEI$species=="Carex filifolia","flowering_start"])),
                                             (mean(sppSPEI[sppSPEI$species=="Carex eleocharis","flowering_start"],sppSPEI[sppSPEI$species=="Carex filifolia","flowering_start"])+((mean(sppSPEI[sppSPEI$species=="Carex eleocharis","flowering_end"],sppSPEI[sppSPEI$species=="Carex filifolia","flowering_end"])-mean(sppSPEI[sppSPEI$species=="Carex eleocharis","flowering_start"],sppSPEI[sppSPEI$species=="Carex filifolia","flowering_start"])))/2))

#get dates for Astragalus spp. (use May to July, estimate from Ackerfield CO Flora)
sppSPEI[sppSPEI$species=="Astragalus/Oxytropis spp.",] <-  c("Astragalus/Oxytropis spp.", 121, 212, 91, 166.5)

sppSPEI[sppSPEI$species=="Astragalus spp.",] <-  c("Astragalus spp.", 121, 212, 91, 166.5)

#get dates for Thelesperma spp. (use June to Sept, estimate from Ackerfield CO Flora)
sppSPEI[sppSPEI$species=="Thelesperma spp.",] <-  c("Thelesperma spp.", 152, 273, 121, 212.5)

#get dates for Allium spp. (use May to Aug, estimate from Ackerfield CO Flora)
sppSPEI[sppSPEI$species=="Allium spp.",] <-  c("Allium spp.", 121, 243, 122, 182)

#start of flowering date 
sppSPEI$SPEI_start <- NA

sppSPEI$flowering_start <- as.numeric(sppSPEI$flowering_start)
sppSPEI$flowering_end <- as.numeric(sppSPEI$flowering_end)
sppSPEI$flowering_total <- as.numeric(sppSPEI$flowering_total)
sppSPEI$flowering_peak <- as.numeric(sppSPEI$flowering_peak)

#calculate start of SPEI
for (i in 1:nrow(sppSPEI)) {
    if(is.na(sppSPEI$flowering_start[i])==FALSE){
    if (sppSPEI$flowering_start[i] <=17) {
      sppSPEI$SPEI_start[i] <- 01
    } else {
      if(sppSPEI$flowering_start[i] > 17 & sppSPEI$flowering_start[i] <=46) {
        sppSPEI$SPEI_start[i] <- 02
      } else {
        if(sppSPEI$flowering_start[i] > 46 & sppSPEI$flowering_start[i] <= 76) {
          sppSPEI$SPEI_start[i] <- 03
        } else {
          if(sppSPEI$flowering_start[i] > 76 & sppSPEI$flowering_start[i] <= 106) {
            sppSPEI$SPEI_start[i] <- 04
          } else {
            if(sppSPEI$flowering_start[i] > 106 & sppSPEI$flowering_start[i] <= 137) {
              sppSPEI$SPEI_start[i] <- 05
            } else {
              if(sppSPEI$flowering_start[i] > 137 & sppSPEI$flowering_start[i] <= 167){
                sppSPEI$SPEI_start[i] <- 06
              } else {
                if(sppSPEI$flowering_start[i] > 167 & sppSPEI$flowering_start[i] <= 198) {
                  sppSPEI$SPEI_start[i] <- 07
                } else {
                  if(sppSPEI$flowering_start[i] > 198 & sppSPEI$flowering_start[i] <= 229) {
                    sppSPEI$SPEI_start[i] <- 08
                  } else {
                    if(sppSPEI$flowering_start[i] > 229 & sppSPEI$flowering_start[i] <= 259) {
                      sppSPEI$SPEI_start[i] <- 09
                    } else {
                      if(sppSPEI$flowering_start[i] > 259 & sppSPEI$flowering_start[i] <= 290) {
                        sppSPEI$SPEI_start[i] <- 10
                      } else {
                        if(sppSPEI$flowering_start[i] > 290 & sppSPEI$flowering_start[i] <= 315) {
                          sppSPEI$SPEI_start[i] <- 11
                        } else {
                          if(sppSPEI$flowering_start[i] > 315 & sppSPEI$flowering_start[i] <= 365) {
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
  if(is.na(sppSPEI$flowering_end[i])==FALSE){
  if (sppSPEI$flowering_end[i] <=17) {
    sppSPEI$SPEI_end[i] <- 01
  } else {
    if(sppSPEI$flowering_end[i] > 17 & sppSPEI$flowering_end[i] <=46) {
      sppSPEI$SPEI_end[i] <- 02
    } else {
      if(sppSPEI$flowering_end[i] > 46 & sppSPEI$flowering_end[i] <= 76) {
        sppSPEI$SPEI_end[i] <- 03
      } else {
        if(sppSPEI$flowering_end[i] > 76 & sppSPEI$flowering_end[i] <= 106) {
          sppSPEI$SPEI_end[i] <- 04
        } else {
          if(sppSPEI$flowering_end[i] > 106 & sppSPEI$flowering_end[i] <= 137) {
            sppSPEI$SPEI_end[i] <- 05
          } else {
            if(sppSPEI$flowering_end[i] > 137 & sppSPEI$flowering_end[i] <= 167){
              sppSPEI$SPEI_end[i] <- 06
            } else {
              if(sppSPEI$flowering_end[i] > 167 & sppSPEI$flowering_end[i] <= 198) {
                sppSPEI$SPEI_end[i] <- 07
              } else {
                if(sppSPEI$flowering_end[i] > 198 & sppSPEI$flowering_end[i] <= 229) {
                  sppSPEI$SPEI_end[i] <- 08
                } else {
                  if(sppSPEI$flowering_end[i] > 229 & sppSPEI$flowering_end[i] <= 259) {
                    sppSPEI$SPEI_end[i] <- 09
                  } else {
                    if(sppSPEI$flowering_end[i] > 259 & sppSPEI$flowering_end[i] <= 290) {
                      sppSPEI$SPEI_end[i] <- 10
                    } else {
                      if(sppSPEI$flowering_end[i] > 290 & sppSPEI$flowering_end[i] <= 315) {
                        sppSPEI$SPEI_end[i] <- 11
                      } else {
                        if(sppSPEI$flowering_end[i] > 315 & sppSPEI$flowering_end[i] <= 365) {
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
  select(-c(flowering_start, flowering_end, flowering_total, flowering_peak)) %>% 
  left_join(sppSPEI, by = c("species"))

#### add SPEI start, end, and duration to the CO_point dataset
CO_point_all <- CO_point_all %>% 
  select(-c(flowering_start, flowering_end, flowering_total, flowering_peak)) %>% 
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

#for previous year's SPEI!! (SPEI in the last year in which the plant was alive)
#make a new column for SPEI_unique_prev in polygon dataset
CO_poly_all$SPEI_unique_prev <- NA

#make a vector of the possible years in the datasets
years <- unique(CO_SPEI_all$Year)
for (i in 1:nrow(CO_poly_all)){
  if(is.na(CO_poly_all$SPEI_duration[i])==FALSE){
    CO_poly_all$SPEI_unique_prev[i] <- (select((CO_SPEI_all[(CO_SPEI_all$Year==(CO_poly_all$year[i]-1) & lubridate::month(CO_SPEI_all$Date)==CO_poly_all$SPEI_end[i]),
                                                            c(1,str_which(colnames(CO_SPEI_all), as.character(CO_poly_all$SPEI_duration[i])),9)]), 
                                               paste("SPEI_",CO_poly_all$SPEI_duration[i], sep="")))[1,1]
  }
}
CO_poly_all$SPEI_unique_prev <- unlist(CO_poly_all$SPEI_unique_prev)


#### Assign the appropriate SPEI values to each species in point Dataset ####
# use CO_SPEI_all dataset

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

#for previous year's SPEI!! (SPEI in the last year in which the plant was alive)
#make a new column for SPEI_unique_prev in point dataset
#make a new column for SPEI_unique in point dataset
CO_point_all$SPEI_unique_prev <- NA
#make a vector of the possible years in the datasets
years <- unique(CO_SPEI_all$Year)
for (i in 1:nrow(CO_point_all)){
  CO_point_all$SPEI_unique_prev[i] <- (select((CO_SPEI_all[(CO_SPEI_all$Year==(CO_point_all$year[i]-1) & lubridate::month(CO_SPEI_all$Date)==CO_point_all$SPEI_end[i]),
                                                           c(1,str_which(colnames(CO_SPEI_all), as.character(CO_point_all$SPEI_duration[i])),9)]), 
                                              paste("SPEI_",CO_point_all$SPEI_duration[i], sep="")))[1,1]
}
CO_point_all$SPEI_unique <- unlist(CO_point_all$SPEI_unique)


#### calculate growth from year t to year t+1 ####

#### Save files for analysis ####
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files")
write.csv(CO_point_all,"./Intermediate Analysis Files/Point_Demo_SPEI_2_24_20.csv",row.names = FALSE)
write.csv(CO_poly_all,"./Intermediate Analysis Files/Poly_Demo_SPEI_2_24_20.csv",row.names = FALSE)

#### plot SPEI for all sites (get mean SPEI accross years of sampling) ####
#use 1 month SPEI
NM_only <- NM_SPEI_all[NM_SPEI_all$Year %in% seq(1915,1950,1),c("Date","SPEI_6")]
MT_only <- MT_SPEI_all[MT_SPEI_all$Year %in% seq(1932,1945,1),c("Date","SPEI_6")]
ID_only <- ID_SPEI_all[ID_SPEI_all$Year %in% seq(1923,1957,1),c("Date","SPEI_6")]
CO_only <- CO_SPEI_all[CO_SPEI_all$Year %in% seq(1997,2010,1),c("Date","SPEI_6")]
AZs_only <- AZs_SPEI_all[AZs_SPEI_all$Year %in% seq(1915,1935,1),c("Date","SPEI_6")]
AZn_only <- AZn_SPEI_all[AZn_SPEI_all$Year %in% seq(2002,2018,1),c("Date","SPEI_6")]
KS_only <- KS_SPEI_all[KS_SPEI_all$Year %in% seq(1932,1972,1),c("Date","SPEI_6")]

#plot
palette <- c("#56b4e9", "#E69F00","#009e73", "#f0e442","#0072b2","#D55e00","#cc79a7")
plot(SPEI_6 ~ Date, data = NM_only, type = "l", col = palette[1], 
     xlim = c(as.POSIXct("1900-01-01 17:00:00 UTC"),as.POSIXct("2015-01-01 17:00:00 UTC")),
     ylim = c(-2.5,2.5))
lines(SPEI_6 ~ Date, data = MT_only, type = "l", col = palette[2])
lines(SPEI_6 ~ Date, data = ID_only, type = "l", col = palette[3])
lines(SPEI_6 ~ Date, data = CO_only, type = "l", col = palette[4])
lines(SPEI_6 ~ Date, data = AZs_only, type = "l", col = palette[5])
lines(SPEI_6 ~ Date, data = KS_only, type = "l", col = palette[6])
lines(SPEI_6 ~ Date, data = AZn_only, type = "l", col = palette[7])

#calculate the 2.5 and 97.5 
NM_min <- qnorm(.025 , mean = mean(NM_only$SPEI_6), sd(NM_only$SPEI_6))
NM_max <- qnorm(.975 , mean = mean(NM_only$SPEI_6), sd(NM_only$SPEI_6))


MT_min <- qnorm(.025 , mean = mean(MT_only$SPEI_6), sd(MT_only$SPEI_6))
MT_max <- qnorm(.975 , mean = mean(NM_only$SPEI_6), sd(NM_only$SPEI_6))

ID_min <- qnorm(.025 , mean = mean(ID_only$SPEI_6), sd(ID_only$SPEI_6))
ID_max <- qnorm(.975 , mean = mean(NM_only$SPEI_6), sd(NM_only$SPEI_6))

CO_min <- qnorm(.025 , mean = mean(CO_only$SPEI_6), sd(CO_only$SPEI_6))
CO_max <- qnorm(.975 , mean = mean(CO_only$SPEI_6), sd(NM_only$SPEI_6))

AZs_min <- qnorm(.025 , mean = mean(AZs_only$SPEI_6), sd(AZs_only$SPEI_6))
AZs_max <- qnorm(.975 , mean = mean(AZs_only$SPEI_6), sd(AZs_only$SPEI_6))

AZn_min <- qnorm(.025 , mean = mean(AZn_only$SPEI_6), sd(AZn_only$SPEI_6))
AZn_max <- qnorm(.975 , mean = mean(AZn_only$SPEI_6), sd(AZn_only$SPEI_6))

KS_min <- qnorm(.025 , mean = mean(KS_only$SPEI_6), sd(KS_only$SPEI_6))
KS_max <- qnorm(.975 , mean = mean(KS_only$SPEI_6), sd(KS_only$SPEI_6))