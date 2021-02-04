###############
# Preparing CO Shortgrass Steppe Data for Analysis
###############

##### LOAD PACKAGES #####
library(dplyr)
library(tidyverse)


##### LOAD DATA FILES #####
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files")

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
#load climate information
CO_climate <- read.csv("./CO_Climate_All.csv")
#load data on families and tribes
families <- read.csv("./Families_Tribes.csv")

#################################################################
###### PREPARE DATASET FOR ANALYSIS #####

### clean up CO_trait datset ###
## clean-up and subset flowering datset 
flowering[flowering$site == "AZ","site"] <- "AZ_s"

#fix species names in traits and flowering datasets
# CO_traits[CO_traits$Species %in% prob_sp_names,]
# CO_traits[14,"Species"] <- "Carex eleocharis"
# CO_traits[17, "Species"] <- "Chrysopsis villosa"
# CO_traits[17, "sp_code_4"] <- "CHVI"
# CO_traits[20,"Species"] <- "Comandra umbellata"
# CO_traits[38, "Species"] <- "Lithospermum incisum"
# CO_traits[50, "Species"] <- "Psoralidium tenuiflorum"
# CO_traits[52,"Species"] <- "Sitanion hystrix"
# CO_traits[55,"Species"] <- "Stipa comata"
# 
# flowering[flowering$Species %in% prob_sp_names,]
# flowering[4,"Species"] <- "Ambrosia psilostachya"
# flowering[22, "other_name"] <- "Chenopodium album"
# flowering[5, "other_name"] <- "Aristida fendleriana"
# flowering[4, "other_name"] <- "Ambrosia psilostachya"
# flowering[11, "sp_code_6"] <- "ASTMIS2"
# flowering[33, "sp_code_6"] <- "ERIFLA2"
# flowering[72, "sp_code_4"] <- "SIHY"
# flowering[90, "other_name"] <- "Aristida fendleriana"
# flowering[119, "Species"] <- "Opuntia polyacantha"
# flowering[119, "other_name"] <- "Opuntia polyacantha"
# flowering[125, "other_name"] <- "Muhlenbergia paniculata"
# flowering[172, "other_name"] <- "Vulpia octoflora"
# flowering[183, "sp_code_6"] <- "ARTTRIP2"
# flowering[201, "other_name"] <- "Sitanion hystrix"
# flowering[223, "other_name"] <- "Koeleria cristata"
# flowering[258, "other_name"] <- "Aristida fendleriana"
# flowering[261, "sp_code_6"] <- "ASCVIR2"
# flowering[282,"Species"] <- "Chenopodium leptophyllum"
# flowering[304, "other_name"] <- "Vulpia octoflora"
# flowering[326, "other_name"] <- "Chaetopappa ericoides"
# flowering[332, "Species"] <- "Oenothera macrocarpa"
# flowering[353, "Species"] <- "Psoralidium tenuiflorum"
# flowering[379, "other_name"] <- "Tragopogon lamottei"
# flowering[337, "other_name"] <- "Megapterium fremontii"
# flowering[65, "other_name"] <- "Pediomelum tenuiflorum"
# flowering <- flowering[-337,]

#subset flowering data to only have CO rows
CO_flowering <- flowering[flowering$site=="CO",]

###update species names in trait dataset
CO_traits$species <- as.character(CO_traits$species)
CO_traits$other_name <- as.character(NA)
#update some more names in the trait dataset
CO_traits[CO_traits$species=="Stipa comata/Hesperostipa comata","species"] <- "Stipa comata"
CO_traits[CO_traits$species=="Stipa comata","other_name"] <- "Hesperostipa comata"

CO_traits[CO_traits$species=="Carex eleocharis/C. duriuscula","species"] <- "Carex eleocharis"
CO_traits[CO_traits$species=="Carex eleocharis","other_name"] <- "Carex duriuscula"

CO_traits[CO_traits$species=="Chrysopsis villosa/Heterotheca villosa","species"] <- "Chrysopsis villosa"
CO_traits[CO_traits$species=="Chrysopsis villosa","other_name"] <- "Heterotheca villosa"

CO_traits[CO_traits$species=="Chrysothamnus nauseosus/Ericameria nauseosa","species"] <- "Chrysothamnus nauseosus"
CO_traits[CO_traits$species=="Chrysothamnus nauseosus","other_name"] <- "Ericameria nauseosa"

CO_traits[CO_traits$species=="Lappula redowskii/Lappula occidentalis","species"] <- "Lappula redowskii"
CO_traits[CO_traits$species=="Lappula redowskii","other_name"] <- "Lappula occidentalis"

CO_traits[CO_traits$species=="Sitanion hystrix/Elymus elymoides","species"] <- "Sitanion hystrix"
CO_traits[CO_traits$species=="Sitanion hystrix","other_name"] <- "Elymus elymoides"

CO_traits[CO_traits$species=="Koeleria cristata/Koeleria macrantha","species"] <- "Koeleria cristata"
CO_traits[CO_traits$species=="Koeleria cristata","other_name"] <- "Koeleria macrantha"

#when there are duplicates for species with trait measurements, only use data from CPER (not HPGRS)
dups <- sort(CO_traits$species[duplicated(CO_traits$species)]) #identify the species that are duplicated
traits_dups <- CO_traits[CO_traits$species %in% dups & CO_traits$Site=="CPER",] #for those duplicate species, get the values only for the measurements at CPER
traits_Not_dups <- CO_traits[!(CO_traits$species %in% dups),] #get the values for the species that were measured only at one site
#add trait data for duplicated species (CPER only) to non-duplicate-measured species
CO_traits <- rbind(traits_dups, traits_Not_dups)

#### add trait data for species w/out trait data from CO ####
#load trait data from other sites
all_traits <- read.csv("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Raw Trait Data/Data Entry Files/AllTraits_2_12_20.csv")

#get average trait values by species
all_traits_avg <- aggregate(all_traits[,c("Species","TLP","LDMC_g_g","SLA_cm2_g","RDMC","RTD_g_cm3", "AvgDiam_mm_AUTO", "SRL_m_g")], by = list(all_traits$Species), FUN = mean(na.rm = TRUE))

all_traits_avg <- all_traits %>% 
  select(c("Species","TLP","LDMC_g_g","SLA_cm2_g","RDMC","RTD_g_cm3", "AvgDiam_mm_AUTO", "SRL_m_g")) %>% 
  group_by(Species) %>% 
  summarise(TLP = mean(TLP, na.rm = TRUE),
            LDMC_g_g = mean(LDMC_g_g, na.rm = TRUE),
            SLA_cm2_g = mean(SLA_cm2_g, na.rm = TRUE),
            RDMC = mean(RDMC, na.rm = TRUE),
            RTD_g_cm3 = mean(RTD_g_cm3, na.rm = TRUE),
            AvgDiam_mm_AUTO  = mean(AvgDiam_mm_AUTO , na.rm = TRUE),
            SRL_m_g = mean(SRL_m_g, na.rm = TRUE))
  

#get trait data for Schedonnardus paniculatus
SCHPAN <- all_traits_avg[all_traits_avg$Species=="SCHPAN",]
SCHPAN <- SCHPAN[1,]
#make a blank row in the CO_traits dataset
blank <- CO_traits[1,]
blank[1,] <- rep_len(NA, length.out = 66)
SCHPAN_row <- blank
SCHPAN_row$species <- "Schedonnardus paniculatus"
SCHPAN_row$sp_code <- "SCHPAN"
SCHPAN_row$TLP <- SCHPAN$TLP
SCHPAN_row$LDMC_g_g <- SCHPAN$LDMC_g_g
SCHPAN_row$SLA_adj_cm2_g <- SCHPAN$SLA_cm2_g
SCHPAN_row$RDMC_g_g <- SCHPAN$RDMC
#add to trait dataset
CO_traits <- rbind(CO_traits, SCHPAN_row)

#get root trait data for Sitanion hystrix
SITHYS <- all_traits_avg[all_traits_avg$Species=="ELYELY",]
SITHYS <- SITHYS[1,]
#make a blank row in the CO_traits dataset
CO_traits[CO_traits$species == "Sitanion hystrix","RDMC_g_g"] <- SITHYS$RDMC
CO_traits[CO_traits$species == "Sitanion hystrix","AvgDiam_mm"] <- SITHYS$AvgDiam_mm_AUTO

#get root trait data for Stipa comata (only RDMC)
STICOM <- all_traits_avg[all_traits_avg$Species=="HESCOM",]
STICOM <- STICOM[1,]
#make a blank row in the CO_traits dataset
CO_traits[CO_traits$species == "Stipa comata", "RDMC_g_g"] <- STICOM$RDMC

#get root trait data for Allium Textile (only RDMC and root diameter)
ALLTEX <- all_traits_avg[all_traits_avg$Species=="ALLTEX",]
ALLTEX <- ALLTEX[1,]
#make a blank row in the CO_traits dataset
CO_traits[CO_traits$species == "Allium textile","RDMC_g_g"] <- ALLTEX$RDMC
CO_traits[CO_traits$species == "Allium textile","AvgDiam_mm"] <- ALLTEX$AvgDiam_mm_AUTO

#get trait data for Vicia americana
VICAME <- all_traits_avg[all_traits_avg$Species=="VICAME",]
VICAME <- VICAME[1,]
#make a blank row in the CO_traits dataset
blank <- CO_traits[1,]
blank[1,] <- rep_len(NA, length.out = 66)
VICAME_row <- blank
VICAME_row$species <- "Vicia americana"
VICAME_row$sp_code <- "VICAME"
VICAME_row$TLP <- VICAME$TLP
VICAME_row$LDMC_g_g <- VICAME$LDMC_g_g
VICAME_row$SLA_adj_cm2_g <- VICAME$SLA_cm2_g
VICAME_row$RDMC_g_g <- VICAME$RDMC
#add to trait dataset
CO_traits <- rbind(CO_traits, VICAME_row)


### combine flowering and trait data ###
all_CO_traits <- full_join (CO_traits, CO_flowering, by = c("species" = "Species"))
#all_CO_traits <- subset(all_CO_traits_1, select = -c(species.y,sp_code_4.x) ) #drop the extra "species name" column
#add the 'families' data.frame
all_CO_traits <- left_join(all_CO_traits, families[,c("sp_code_6","Tribe")], by = "sp_code_6" )

names(all_CO_traits)[1] <- "trait_site"
#combine both "other_name" columns
all_CO_traits$other_name <- NA
all_CO_traits$other_name.x <- as.character(all_CO_traits$other_name.x)
all_CO_traits$other_name.y <- as.character(all_CO_traits$other_name.y)

for(i in 1:nrow(all_CO_traits)){
  if(is.na(all_CO_traits$other_name.x[i])==FALSE){
  all_CO_traits$other_name[i] <- all_CO_traits$other_name.x[i]
  } else {
    all_CO_traits$other_name[i] <- all_CO_traits$other_name.y[i]
  }
}

all_CO_traits <- all_CO_traits %>% 
  select(-c(other_name.x, other_name.y))


all_CO_traits$site <- "CO"
all_CO_traits<- all_CO_traits[order(all_CO_traits$species),]


####Clean-up the CO_quads dataset###
## make a new variable "grazing" in the CO_quads dataset that combines the grazing history into one vaiable
#create an empty vector ***make this a categorical variable, rather than 
grazing <- vector(length=nrow(CO_quads))
#make a for loop that combines the grazing data (pre1997 THEN post1997) into one variable
for(i in 1:nrow(CO_quads)) {
  grazing[i] <- (paste(CO_quads$before1997[i],"THEN",CO_quads$from1997[i]))
}


#make a new column for grazing data
CO_quads <- mutate(CO_quads, grazing=grazing)

###Clean-up climate dataset###
## Organize climate data set and create new growing season variables ##

#change format of date column
CO_climate$Date <- as.POSIXct(CO_climate$Date, format = "%m/%d/%Y")
#fix years 
lubridate::year(CO_climate$Date)<- lubridate::year(CO_climate$Date)+1900
lubridate::year(CO_climate$Date)[37:162] <- lubridate::year(CO_climate$Date)[37:162]+100

#create a new vector for mean growing season temp
grow.mean.temp <- data.frame(growing.mean.temp = vector(length=nrow(CO_climate)), 
                             year = vector(length=nrow(CO_climate)), 
                             site = vector(length=nrow(CO_climate)))

#calculate mean growing season temp
for(i in 1:nrow(CO_climate)) {
  if(lubridate::month(CO_climate$Date)[i]=="12") {
    grow.mean.temp$growing.mean.temp[i] <- mean(year<-c(CO_climate$Mean.Monthly.T[i-3],
                                       CO_climate$Mean.Monthly.T[i-4],
                                       CO_climate$Mean.Monthly.T[i-5],
                                       CO_climate$Mean.Monthly.T[i-6],
                                       CO_climate$Mean.Monthly.T[i-7]))
    grow.mean.temp$year[i] <- lubridate::year(CO_climate$Date[i])
    grow.mean.temp$site[i] <- paste(CO_climate$Site[i])
  } else {
    grow.mean.temp$growing.mean.temp[i] <- NA
    grow.mean.temp$year[i] <- NA
    grow.mean.temp$site[i] <- NA
  }
}

drop_na(grow.mean.temp)

#make a new variable in the climate dataset to show mean growing season (May to Sept.) temperature
CO_climate <- mutate(CO_climate, mean.grow.temp= grow.mean.temp$growing.mean.temp)

#subset climate data to include only annual data
CO_clim_yr <- CO_climate[lubridate::month(CO_climate$Date)=="12",]

#calculate climate variables for t (the last year the plant was alive) and t+1 (the year that the plant died, the first year it wasn't found)
#add new columns for each (t1) climate variable
CO_clim_yr <- mutate(CO_clim_yr, Ann.Sum.Precip_t1="", Ann.Avg.T_t1="", mean.grow.temp_t1="")
#make sure that the dataset is ordered by year from 1997 to 2009
CO_clim_yr <- arrange(CO_clim_yr,lubridate::year(CO_clim_yr$Date))
#put ann.sum.precip value from previous year into Ann.Sum.Precip_t1 column
for (i in 1:nrow(CO_clim_yr)) {
  CO_clim_yr[i,"Ann.Sum.Precip_t1"] <- CO_clim_yr[i+1,"Ann.Sum.Precip"]
} 
#put ann.avg.t value from previous year into Ann.Avg.T_t1 column
for (i in 1:nrow(CO_clim_yr)) {
  CO_clim_yr[i,"Ann.Avg.T_t1"] <- CO_clim_yr[i+1,"Ann.Avg.T"]
} 
#put mean.grow.temp value from previous year into mean.grow.temp_t1 column
for (i in 1:nrow(CO_clim_yr)) {
  CO_clim_yr[i,"mean.grow.temp_t1"] <- CO_clim_yr[i+1,"mean.grow.temp"]
} 

#convert the new variables to "numeric" rather than "character" (for some reason they became characters when transformed)
CO_clim_yr$Ann.Sum.Precip_t1 <- as.numeric(CO_clim_yr$Ann.Sum.Precip_t1)
CO_clim_yr$Ann.Avg.T_t1 <- as.numeric(CO_clim_yr$Ann.Avg.T_t1)
CO_clim_yr$mean.grow.temp_t1 <- as.numeric(CO_clim_yr$mean.grow.temp_t1)

############ merge datasets for polygon (grass) data ############
#merge survival data with trait data
CO_poly_surv_traits <- left_join (CO_poly_surv, all_CO_traits[,-11], by=c("species"="species"))
#merge quadrat data with trait/survival data
CO_poly_surv_traits_quads <- left_join(CO_poly_surv_traits,CO_quads[CO_quads$Site=="CO",], by=c("quad"="quad"))

### Merge climate and polygon dataset ###
CO_clim_yr$year <- lubridate::year(CO_clim_yr$Date)

CO_poly_all <- left_join(CO_poly_surv_traits_quads,CO_clim_yr,by=c("year_t"="year"))
#drop the monthly climate variables that we don't need
CO_poly_all <- subset(CO_poly_all,select = -c(Mean.Monthly.T,Monthly.Max.T,Monthly.Min.T,Monthly.ppt))
                                              #Site.y, Site.y.y, Site.x.x))
names(CO_poly_all)[21] <- "trait_site"

############ merge datasets for point (forb) data ############

### merge trait data with the survival data ###
CO_point_surv_traits <- left_join (CO_point_surv, all_CO_traits[,-11], by=c("species"="species"))

### merge trait/survival data with quadrat data ### 
CO_point_surv_traits_quads <- left_join(CO_point_surv_traits, CO_quads, by=c("quad"="quad"))


### Merge climate and point dataset ###
CO_point_all <- left_join(CO_point_surv_traits_quads,CO_clim_yr,by="year")
#drop the monthly climate variables that we don't need
CO_point_all <- subset(CO_point_all,select = -c(Mean.Monthly.T,Monthly.Max.T,Monthly.Min.T,Monthly.ppt))
                                                #,Site.y, Site.y.y, Site.x.x, sp_code_4.y, site, Species))
#names(CO_point_all)[12] <- "sp_code_4"
names(CO_point_all)[17] <- "trait_site"


#determining sample size for forbs
forb_names <- unique(CO_point_all$species)
sample_size_forb <- as.vector(seq(0,0,length=length(forb_names)))
for(i in 1:length(forb_names)) {
  sample_size_forb[i] <- as.numeric(nrow(CO_point_all[CO_point_all$species==forb_names[i],]))
}
forbs <- data.frame(x=forb_names,sample_size_forb)

write.csv(forbs,file="forb sample size",row.names=FALSE)

#determining sample size for graminoids
gram_names <- unique(CO_poly_all$species)
sample_size_gram <- as.vector(seq(0,0,length=length(gram_names)))
for(i in 1:length(gram_names)) {
  sample_size_gram[i] <- as.numeric(nrow(CO_poly_all[CO_poly_all$species==gram_names[i],]))
}
graminoids <- data.frame(x=gram_names,sample_size_gram)


#remove variables that I won't use in this analysis
CO_poly_all <- subset(CO_poly_all, select = -c(latitude, longitude, elevation,Site.y,Site.x))
CO_poly_all <- CO_poly_all[,-102]
CO_point_all <- subset(CO_point_all, select = -c(latitude, longitude, elevation))
CO_point_all <- subset(CO_point_all, select = -c(Site.x))

write.csv(CO_poly_all, "./Intermediate Analysis Files/poly_demo_Traits_2_24_20.csv", row.names = FALSE)
write.csv(CO_point_all, "./Intermediate Analysis Files/point_demo_Traits_2_24_20.csv", row.names = FALSE)

