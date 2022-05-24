#//////////////////////////
# Demographic trade-offs affect how leaf turgor loss point and tissue dry matter content mediate the effect of drought on herbaceous perennial survival and growth
# Nearest Neighbor Calculations
# Script 1 of 6
# Alice Stears, astears@uwyo.edu
# Revised 9 February 2021
# R version 4.0.3 
#//////////////////////////

#clear workspace
rm(list=ls())

#### load packages ####
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> dc34618... cleaning up documentation/updating figures
library(tidyverse) #v1.3.0
library(sf) #v0.9-7
library(mapview) #v2.9.0
library(lwgeom) #v0.2-5
<<<<<<< HEAD

#### Data Sources ####
# Polygon and Point demographic tracking data from output of 'tracking script' set of scripts
#### Calculating Conspecific Nearest Neighbor Area for Polygon Dataset
## set wd and read in data file
# workDir <-  #change to the path of your file "polygon_species_survD_IPM.csv"
=======
require(tidyverse) #v1.3.0
require(sf) #v0.9-7
require(mapview) #v2.9.0
require(lwgeom) #v0.2-5
=======
>>>>>>> dc34618... cleaning up documentation/updating figures

#### Data Sources ####
# Polygon and Point demographic tracking data from output of 'tracking script' set of scripts
#### Calculating Conspecific Nearest Neighbor Area for Polygon Dataset
## set wd and read in data file
<<<<<<< HEAD
workDir <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/SurvivalData" #change to the path of your file "polygon_species_survD_IPM.csv"
>>>>>>> 00232cd... renamed file
=======
# workDir <-  #change to the path of your file "polygon_species_survD_IPM.csv"
>>>>>>> dc34618... cleaning up documentation/updating figures
setwd(workDir) 

#read in polygon survival data file
poly <- read.csv("polygonSpeciesSurvData.csv")

## create empty columns in the data.frame to hold values for each nearest neighbor radius (in cm)
poly$neighbor_area_5 <- NA
poly$neighbor_area_10 <- NA
poly$neighbor_area_15 <- NA
poly$neighbor_area_20 <- NA



#make a vector for years in the dataset
year <- sort(unique(poly$year))
#make a vector for quadrats in the dataset
quad <- unique(poly$quad)
#make a vector for species in the dataset
species <- unique(poly$species)

#### get the data for SP_ID from tracking files and merge this with the poly dataset (for individuals with only one observation--haven't been clumped at genet scale) ####

#load all tracking data files with the following loop

#set the working directory to the location of the folder "PolygonTrackingResults"
<<<<<<< HEAD
<<<<<<< HEAD
# trackingDatWD <- # change the location of the folder "PolygonTrackingResults"
#change to appropriate working directory
=======
trackingDatWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/InputData/PolygonTrackingResults" #change to appropriate working directory
>>>>>>> 00232cd... renamed file
=======
# trackingDatWD <- # change the location of the folder "PolygonTrackingResults"
#change to appropriate working directory
>>>>>>> dc34618... cleaning up documentation/updating figures
setwd(trackingDatWD)

for(k in 1:length(species)) { #loop through all species
  trackTemp <- read.csv(paste("./",
                              str_to_upper(paste(str_sub(species[k],1,3), 
                                                 str_sub(species[k],str_locate(species[k]," ")[1]+1,str_locate(species[k]," ")[1]+3),sep = ""))
                              ,"_buf5_dorm1.csv", sep = ""))
  if (k == 1) {
    trackSP <- trackTemp
  } else {
    trackSP <- rbind(trackSP, trackTemp)
  }
}
#trackSP data file has all raw tracking results

## aggregate this data.frame by quad, year, species, and trackID to get the trackIDs of individuals that are singletons (i.e. not mapped as multiple polygons)
#will use this data to identify the SP_ID's that are 'good' in the trackSP data.set
trackGOOD <- aggregate(trackSP, by = list(trackSP$quad, trackSP$year, trackSP$Species, trackSP$trackID), FUN = length)
# drop all IDs that have more than one individual (would have >1 SP_ID)
trackGOOD <- filter(trackGOOD,quad==1)
#add a column so we know the individual record is 'good'
trackGOOD$need <- "need"
#fix the names of the data.frame
trackGOOD <- trackGOOD[,c("Group.1", "Group.2", "Group.3", "Group.4", "need")]
names(trackGOOD) <- c("quad","year","Species","trackID","need")
#merge with trackSP dataset to ID 'good' records (denoted by "need" in the need column)
trackSP <- left_join(trackSP, trackGOOD, by = c("quad", "year", "Species", "trackID"))
#filter the trackSP data for those that we "need"
trackSP <- filter(trackSP, need=="need")
#merge with poly dataset to get SP_IDs
poly<- left_join(poly, trackSP[,c("quad","year","SP_ID","Species","trackID")], by = c("quad", "year_t"="year", "species"="Species", "trackID"))

#### calculate nearest neighbor for polygons with only one SP_ID ####
<<<<<<< HEAD
<<<<<<< HEAD
# shpWD <- #change to your file that contains the CO shapefiles
=======
shpWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles" #change to your file that contains the CO shapefiles
>>>>>>> 00232cd... renamed file
=======
# shpWD <- #change to your file that contains the CO shapefiles
>>>>>>> dc34618... cleaning up documentation/updating figures
setwd(shpWD)

#make a bounding box that is the shape and size of the quadrat
baseShape <- st_read(dsn = "gzgz_5a", layer = "poly_gzgz_5a_1997")
box <- st_as_sfc(st_bbox(baseShape)) #make a box that is the size of a quadrat (1mx1m)
boxBuffer <- st_buffer(box, dist = -0.05)#make a 5cm buffer inside the box
boxBuffer <- st_difference(box, boxBuffer)

## Calculate nearest neighbor for 5cm radius
for(j in 1:length(quad)) { #loop throught the quadrats
  temp1 <- poly[poly$quad==quad[j],]
  for (i in 1:length(year)) { #loop through the years
    temp2 <- temp1[temp1$year==year[i],]
    for (k in 1:length(species)) { #loop through the species
      temp3 <- temp2[temp2$species == species[k],]
      if (nrow(temp3)>0) {
        #now find the appropriate shapefile 
        shape <- st_read(dsn = paste(quad[j]), layer = paste("poly_",quad[j],"_",year[i], sep = ""), stringsAsFactors = FALSE)
        shape$SP_ID <- as.numeric(shape$SP_ID)
        shapeFocal <- drop_na(shape[shape$Species == as.character(species[k]),],"Species") #all of the polygons for the target species
        shapeFocal <- st_make_valid(shapeFocal)
        if(nrow(shapeFocal)>0){
          for (m in 1:nrow(shapeFocal)) {
            buffer <- st_buffer(shapeFocal[m,],.05)
            area <- st_intersection(buffer, shapeFocal)
            area2 <- st_difference(area, shapeFocal[m,])
            area3 <- sum(st_area(area2))
            poly[poly$quad==quad[j] &
                   poly$year==year[i] &
                   poly$SP_ID == shapeFocal[m,]$SP_ID &
                   is.na(poly$SP_ID)==FALSE , 
                 "neighbor_area_5"] <- area3
          }
        }
      }
    }
  }
}

#for 10cm nearest neighbor radius
for(j in 1:length(quad)) {
  temp1 <- poly[poly$quad==quad[j],]
  for (i in 1:length(year)) {
    temp2 <- temp1[temp1$year==year[i],]
    for (k in 1:length(species)) {
      temp3 <- temp2[temp2$species == species[k],]
      if (nrow(temp3)>0) {
        #now find the appropriate shapefile 
        shape <- st_read(dsn = paste(quad[j]), layer = paste("poly_",quad[j],"_",year[i], sep = ""), stringsAsFactors = FALSE)
        shape$SP_ID <- as.numeric(shape$SP_ID)
        shapeFocal <- drop_na(shape[shape$Species == as.character(species[k]),],"Species") #all of the polygons
        shapeFocal <- st_make_valid(shapeFocal)
        if(nrow(shapeFocal)>0){
          for (m in 1:nrow(shapeFocal)) {
            buffer <- st_buffer(shapeFocal[m,],.1)
            area <- st_intersection(buffer, shapeFocal)
            area2 <- st_difference(area, shapeFocal[m,])
            area3 <- sum(st_area(area2))
            poly[poly$quad==quad[j] &
                   poly$year==year[i] &
                   poly$SP_ID == shapeFocal[m,]$SP_ID &
                   is.na(poly$SP_ID)==FALSE , 
                 "neighbor_area_10"] <- area3
          }
        }
      }
    }
  }
}

#for 15cm nearest neighbor radius
for(j in 1:length(quad)) {
  temp1 <- poly[poly$quad==quad[j],]
  for (i in 1:length(year)) {
    temp2 <- temp1[temp1$year==year[i],]
    for (k in 1:length(species)) {
      temp3 <- temp2[temp2$species == species[k],]
      if (nrow(temp3)>0) {
        #now find the appropriate shapefile 
        shape <- st_read(dsn = paste(quad[j]), layer = paste("poly_",quad[j],"_",year[i], sep = ""), stringsAsFactors = FALSE)
        shape$SP_ID <- as.numeric(shape$SP_ID)
        shapeFocal <- drop_na(shape[shape$Species == as.character(species[k]),],"Species") #all of the polygons
        shapeFocal <- st_make_valid(shapeFocal)
        if(nrow(shapeFocal)>0){
          for (m in 1:nrow(shapeFocal)) {
            buffer <- st_buffer(shapeFocal[m,],.15)
            area <- st_intersection(buffer, shapeFocal)
            area2 <- st_difference(area, shapeFocal[m,])
            area3 <- sum(st_area(area2))
            poly[poly$quad==quad[j] &
                   poly$year==year[i] &
                   poly$SP_ID == shapeFocal[m,]$SP_ID &
                   is.na(poly$SP_ID)==FALSE , 
                 "neighbor_area_15"] <- area3
          }
        }
      }
    }
  }
}

#for 20cm nearest neighbor radius
for(j in 1:length(quad)) {
  temp1 <- poly[poly$quad==quad[j],]
  for (i in 1:length(year)) {
    temp2 <- temp1[temp1$year==year[i],]
    for (k in 1:length(species)) {
      temp3 <- temp2[temp2$species == species[k],]
      if (nrow(temp3)>0) {
        #now find the appropriate shapefile 
        shape <- st_read(dsn = paste(quad[j]), layer = paste("poly_",quad[j],"_",year[i], sep = ""), stringsAsFactors = FALSE)
        shape$SP_ID <- as.numeric(shape$SP_ID)
        shapeFocal <- drop_na(shape[shape$Species == as.character(species[k]),],"Species") #all of the polygons
        shapeFocal <- st_make_valid(shapeFocal)
        if(nrow(shapeFocal)>0){
          for (m in 1:nrow(shapeFocal)) {
            buffer <- st_buffer(shapeFocal[m,],.2)
            area <- st_intersection(buffer, shapeFocal)
            area2 <- st_difference(area, shapeFocal[m,])
            area3 <- sum(st_area(area2))
            poly[poly$quad==quad[j] &
                   poly$year==year[i] &
                   poly$SP_ID == shapeFocal[m,]$SP_ID &
                   is.na(poly$SP_ID)==FALSE , 
                 "neighbor_area_20"] <- area3
          }
        }
      }
    }
  }
}

#### calculate nearest neighbor area for individuals with multiple SP ID's (mapped as multiple polygons) ####
#set up a buffer box 
baseShape <- st_read(dsn = "gzgz_5a", layer = "poly_gzgz_5a_1997")
box <- st_as_sfc(st_bbox(baseShape)) #make a box that is the size of a quadrat (1mx1m)
boxBuffer <- st_buffer(box, dist = -0.05)#make a 5cm buffer inside the box
boxBuffer <- st_difference(box, boxBuffer)

#reminders: 
# trackingDatWD is an object that stores the path of the "PolygonTrackingResults" directory
# shpWD is an object that stores the path of the "CO_shapefiles" directory

#for 5cm radius
for (k in 1:length(species)){ ##loop through species
  setwd(trackingDatWD)
  tempSP <- read.csv(paste(str_to_upper(paste(str_sub(species[k],1,3), 
                                              str_sub(species[k],str_locate(species[k]," ")[1]+1,str_locate(species[k]," ")[1]+3),sep = ""))
                           ,"_buf5_dorm1.csv", sep = ""))
  tempSP$area <- round(tempSP$area, 3)
  #load datafile that has nearest neighbor data (data after genets have been grouped)
  #filter for species
  neSP <- poly[poly$species==species[k],]
  neSP$area <- round(neSP$area_t, 3)
  neSP <- neSP[is.na(neSP$neighbor_area_5)==TRUE,]
  for(j in 1:length(quad)) {
    ##loop through quadrats
    quadNeSP <- neSP[neSP$quad==paste(quad[j]),]
    quadtempSP <- tempSP[tempSP$quad==paste(quad[j]),]
    for(i in 1:length(year)) {
      ##loop through years
      yearNeSP <- quadNeSP[quadNeSP$year==year[i],]
      yeartempSP <- quadtempSP[quadtempSP$year==year[i],]
      #get the SP_IDs of the individuals that do not have nearest neighbor info 
      # (the SP_ID is in the 'tracking' data file, but the species ID is in the 'ne' data file)
      yearSP <- yeartempSP[yeartempSP$trackID %in% unique(yearNeSP$trackID),]
      if(nrow(yearSP)>length(unique(yearSP$trackID))) {
        #load appropriate shape file
        setwd(shpWD)
        shape <- st_read(dsn = paste(quad[j]), layer = paste("poly_",quad[j],"_",year[i], sep=""))
        #get focal species from shape file
        shapeFocal <- drop_na(shape[shape$Species == paste(species[k]),],"Species")
        shapeFocal <- st_make_valid(shapeFocal)
        if(nrow(shape)>0) {
          for(m in 1:length(unique(yearSP$trackID))) {
            ##loop through the groups of individuals that have the same track ID
            #identify all of the indviduals that have the same track ID 
            oneSP <- yearSP[yearSP$trackID==unique(yearSP$trackID)[m],]
            #identify the individuals that have the same track ID (by their SP_ID in the shape file)
            if(nrow(oneSP)>1) {
              oneShape <- shapeFocal[shapeFocal$SP_ID %in% (oneSP$SP_ID) ,] 
              # draw buffers around all of the shapes
              buffer <- st_buffer(oneShape,.05)
              # remove the overlap and combine into one shapefile
              buffer2 <- st_union(buffer)
              finalBuffer<- st_difference(buffer2, st_union(oneShape))
              # calculate overlap with other 'individuals'
              area <- st_intersection(st_make_valid(finalBuffer), st_make_valid(shapeFocal))
              area2 <- sum(st_area(area))
              #insert this value into the appropriate place in the dataframe for nearest neighbor values
              poly[poly$species==species[k] & 
                     poly$quad==quad[j] & 
                     poly$year==year[i] & 
                     poly$trackID==unique(yearSP$trackID)[m],
                   "neighbor_area_5"]<- area2
            } 
          }
        }
      }
    }
  }
}

#for 10cm radius
for (k in 1:length(species)){
  setwd(trackingDatWD)
  tempSP <- read.csv(paste(
                           str_to_upper(paste(str_sub(species[k],1,3), 
                                              str_sub(species[k],str_locate(species[k]," ")[1]+1,str_locate(species[k]," ")[1]+3),sep = ""))
                           ,"_buf5_dorm1.csv", sep = ""))
  tempSP$area <- round(tempSP$area, 3)
  #load datafile that has nearest neighbor data (data after genets have been grouped)
  #filter for species
  neSP <- poly[poly$species==species[k],]
  neSP$area <- round(neSP$area_t, 3)
  neSP <- neSP[is.na(neSP$neighbor_area_10)==TRUE,]
  for(j in 1:length(quad)) {
    ##loop through quadrats
    quadNeSP <- neSP[neSP$quad==paste(quad[j]),]
    quadtempSP <- tempSP[tempSP$quad==paste(quad[j]),]
    for(i in 1:length(year)) {
      ##loop through years
      yearNeSP <- quadNeSP[quadNeSP$year==year[i],]
      yeartempSP <- quadtempSP[quadtempSP$year==year[i],]
      #get the SP_IDs of the individuals that do not have nearest neighbor info 
      # (the SP_ID is in the 'tracking' data file, but the species ID is in the 'ne' data file)
      yearSP <- yeartempSP[yeartempSP$trackID %in% unique(yearNeSP$trackID),]
      if(nrow(yearSP)>length(unique(yearSP$trackID))) {
        #load appropriate shape file
        setwd(shpWD)
        shape <- st_read(dsn = paste(quad[j]), layer = paste("poly_",quad[j],"_",year[i], sep=""))
        #get focal species from shape file
        shapeFocal <- drop_na(shape[shape$Species == paste(species[k]),],"Species")
        shapeFocal <- st_make_valid(shapeFocal)
        if(nrow(shape)>0) {
          for(m in 1:length(unique(yearSP$trackID))) {
            ##loop through the groups of individuals that have the same track ID
            #identify all of the indviduals that have the same track ID 
            oneSP <- yearSP[yearSP$trackID==unique(yearSP$trackID)[m],]
            #identify the individuals that have the same track ID (by their SP_ID in the shape file)
            if(nrow(oneSP)>1) {
              oneShape <- shapeFocal[shapeFocal$SP_ID %in% (oneSP$SP_ID) ,] 
              # draw buffers around all of the shapes
              buffer <- st_buffer(oneShape,.1)
              # remove the overlap and combine into one shapefile
              buffer2 <- st_union(buffer)
              finalBuffer<- st_difference(buffer2, st_union(oneShape))
              # calculate overlap with other 'individuals'
              area <- st_intersection(st_make_valid(finalBuffer), st_make_valid(shapeFocal))
              area2 <- sum(st_area(area))
              #insert this value into the appropriate place in the dataframe for nearest neighbor values
              poly[poly$species==species[k] & 
                     poly$quad==quad[j] & 
                     poly$year==year[i] & 
                     poly$trackID==unique(yearSP$trackID)[m],
                   "neighbor_area_10"]<- area2
            } 
          }
        }
      }
    }
  }
}

#for 15cm radius
for (k in 1:length(species)){
  setwd(trackingDatWD)
  tempSP <- read.csv(paste(str_to_upper(paste(str_sub(species[k],1,3), 
                                              str_sub(species[k],str_locate(species[k]," ")[1]+1,str_locate(species[k]," ")[1]+3),sep = ""))
                           ,"_buf5_dorm1.csv", sep = ""))
  tempSP$area <- round(tempSP$area, 3)
  #load datafile that has nearest neighbor data (data after genets have been grouped)
  #filter for species
  neSP <- poly[poly$species==species[k],]
  neSP$area <- round(neSP$area_t, 3)
  neSP <- neSP[is.na(neSP$neighbor_area_15)==TRUE,]
  for(j in 1:length(quad)) {
    ##loop through quadrats
    quadNeSP <- neSP[neSP$quad==paste(quad[j]),]
    quadtempSP <- tempSP[tempSP$quad==paste(quad[j]),]
    for(i in 1:length(year)) {
      ##loop through years
      yearNeSP <- quadNeSP[quadNeSP$year==year[i],]
      yeartempSP <- quadtempSP[quadtempSP$year==year[i],]
      #get the SP_IDs of the individuals that do not have nearest neighbor info 
      # (the SP_ID is in the 'tracking' data file, but the species ID is in the 'ne' data file)
      yearSP <- yeartempSP[yeartempSP$trackID %in% unique(yearNeSP$trackID),]
      if(nrow(yearSP)>length(unique(yearSP$trackID))) {
        #load appropriate shape file
        setwd(shpWD)
        shape <- st_read(dsn = paste(quad[j]), layer = paste("poly_",quad[j],"_",year[i], sep=""))
        #get focal species from shape file
        shapeFocal <- drop_na(shape[shape$Species == paste(species[k]),],"Species")
        shapeFocal <- st_make_valid(shapeFocal)
        if(nrow(shape)>0) {
          for(m in 1:length(unique(yearSP$trackID))) {
            ##loop through the groups of individuals that have the same track ID
            #identify all of the indviduals that have the same track ID 
            oneSP <- yearSP[yearSP$trackID==unique(yearSP$trackID)[m],]
            #identify the individuals that have the same track ID (by their SP_ID in the shape file)
            if(nrow(oneSP)>1) {
              oneShape <- shapeFocal[shapeFocal$SP_ID %in% (oneSP$SP_ID) ,] 
              # draw buffers around all of the shapes
              buffer <- st_buffer(oneShape,.15)
              # remove the overlap and combine into one shapefile
              buffer2 <- st_union(buffer)
              finalBuffer<- st_difference(buffer2, st_union(oneShape))
              # calculate overlap with other 'individuals'
              area <- st_intersection(st_make_valid(finalBuffer), st_make_valid(shapeFocal))
              area2 <- sum(st_area(area))
              #insert this value into the appropriate place in the dataframe for nearest neighbor values
              poly[poly$species==species[k] & 
                     poly$quad==quad[j] & 
                     poly$year==year[i] & 
                     poly$trackID==unique(yearSP$trackID)[m],
                   "neighbor_area_15"]<- area2
            } 
          }
        }
      }
    }
  }
}

#for 20cm radius
for (k in 1:length(species)){
  setwd(trackingDatWD)
  tempSP <- read.csv(paste(str_to_upper(paste(str_sub(species[k],1,3), 
                                              str_sub(species[k],str_locate(species[k]," ")[1]+1,str_locate(species[k]," ")[1]+3),sep = ""))
                           ,"_buf5_dorm1.csv", sep = ""))
  tempSP$area <- round(tempSP$area, 3)
  #load datafile that has nearest neighbor data (data after genets have been grouped)
  #filter for species
  neSP <- poly[poly$species==species[k],]
  neSP$area <- round(neSP$area_t, 3)
  neSP <- neSP[is.na(neSP$neighbor_area_20)==TRUE,]
  for(j in 1:length(quad)) {
    ##loop through quadrats
    quadNeSP <- neSP[neSP$quad==paste(quad[j]),]
    quadtempSP <- tempSP[tempSP$quad==paste(quad[j]),]
    for(i in 1:length(year)) {
      ##loop through years
      yearNeSP <- quadNeSP[quadNeSP$year==year[i],]
      yeartempSP <- quadtempSP[quadtempSP$year==year[i],]
      #get the SP_IDs of the individuals that do not have nearest neighbor info 
      # (the SP_ID is in the 'tracking' data file, but the species ID is in the 'ne' data file)
      yearSP <- yeartempSP[yeartempSP$trackID %in% unique(yearNeSP$trackID),]
      if(nrow(yearSP)>length(unique(yearSP$trackID))) {
        #load appropriate shape file
        setwd(shpWD)
        shape <- st_read(dsn = paste(quad[j]), layer = paste("poly_",quad[j],"_",year[i], sep=""))
        #get focal species from shape file
        shapeFocal <- drop_na(shape[shape$Species == paste(species[k]),],"Species")
        shapeFocal <- st_make_valid(shapeFocal)
        if(nrow(shape)>0) {
          for(m in 1:length(unique(yearSP$trackID))) {
            ##loop through the groups of individuals that have the same track ID
            #identify all of the indviduals that have the same track ID 
            oneSP <- yearSP[yearSP$trackID==unique(yearSP$trackID)[m],]
            #identify the individuals that have the same track ID (by their SP_ID in the shape file)
            if(nrow(oneSP)>1) {
              oneShape <- shapeFocal[shapeFocal$SP_ID %in% (oneSP$SP_ID) ,] 
              oneShape <- st_union(oneShape, by_feature = FALSE)
              # draw buffers around all of the shapes
              buffer <- st_buffer(oneShape,.20)
              # remove the overlap and combine into one shapefile
              buffer2 <- st_union(buffer)
              finalBuffer<- st_difference(buffer2, oneShape)
              # calculate overlap with other 'individuals'
              area <- st_intersection(st_make_valid(finalBuffer), st_make_valid(shapeFocal))
              area2 <- sum(st_area(area))
              #insert this value into the appropriate place in the dataframe for nearest neighbor values
              poly[poly$species==species[k] & 
                     poly$quad==quad[j] & 
                     poly$year==year[i] & 
                     poly$trackID==unique(yearSP$trackID)[m],
                   "neighbor_area_20"]<- area2
            } 
          }
        }
      }
    }
  }
}


# #write  to csv file
# write.csv(poly,
#           "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files/Intermediate Analysis Files/polygon_demo_2_12_18.csv", 
#           row.names = FALSE)


#### Calculate Nearest Neighbor for Points Dataset ####
<<<<<<< HEAD
<<<<<<< HEAD
# pointsWD <-#change the location of your 'point_species_survD.csv" file 
=======
pointsWD <- "/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/SurvivalData" #change the location of your 'point_species_survD.csv" file 
>>>>>>> 00232cd... renamed file
=======
# pointsWD <-#change the location of your 'point_species_survD.csv" file 
>>>>>>> dc34618... cleaning up documentation/updating figures
setwd(pointsWD)

#read in point survival data
points <- read.csv("/pointSpeciesSurvData.csv", stringsAsFactors = FALSE)
#add a column for 'site' 
points$Site <- "CO"

#create an empty column for nearest neighbor density
points$neighbors_5 <- NA
points$neighbors_10 <- NA
points$neighbors_15 <- NA
points$neighbors_20 <- NA

#make a vector for years in the dataset
year <- sort(unique(points$year_t))
quad <- unique(points$quad) #make a vector of quads in the dataset
species <- unique(points$species) #make a vector of species in the dataset

setwd(shpWD) #change WD to the path for the CO_shapefiles folder (which is the same as "shpWD", defined above)

#calculate nearest neighbor density for 5cm radius
for(j in 1:length(quad)) {
  temp1 <- points[points$quad==quad[j],]
  for (i in 1:length(year)) {
    temp2 <- temp1[temp1$year==year[i],]
    for (k in 1:length(species)) {
      temp3 <- temp2[temp2$species == species[k],]
      if (nrow(temp3)>0) {
        shape <- st_read(dsn = paste(quad[j]), layer = paste("pnt_",quad[j],"_",year[i], sep = ""))
        shape <- shape[shape$Species == as.character(species[k]),]
        for (m in 1:nrow(shape)) {
          buffer <- st_buffer(shape[m,],.05)
          count <- ifelse(length(unlist(st_intersects(buffer, shape)))==0,
                          yes = (0), no = (length(unlist(st_intersects(buffer, shape)))-1))
          points[points$x < (as.data.frame(shape[m,"coords_x1"])[1,1]+.0001) &
                   points$x > (as.data.frame(shape[m,"coords_x1"])[1,1]-.0001) &
                   points$quad == quad[j] &
                   points$year == year[i] &
                   points$species == species[k], "neighbors_5"] <- count
          }
      }
    }
  }
}

#calculate nearest neighbor density for 10cm radius
for(j in 1:length(quad)) {
  temp1 <- points[points$quad==quad[j],]
  for (i in 1:length(year)) {
    temp2 <- temp1[temp1$year==year[i],]
    for (k in 1:length(species)) {
      temp3 <- temp2[temp2$species == species[k],]
      if (nrow(temp3)>0) {
        shape <- st_read(dsn = paste(quad[j]), layer = paste("pnt_",quad[j],"_",year[i], sep = ""))
        shape <- shape[shape$Species == as.character(species[k]),]
        for (m in 1:nrow(shape)) {
          buffer <- st_buffer(shape[m,],.10)
          count <- ifelse(length(unlist(st_intersects(buffer, shape)))==0,
                          yes = (0), no = (length(unlist(st_intersects(buffer, shape)))-1))
          points[points$x < (as.data.frame(shape[m,"coords_x1"])[1,1]+.0001) &
                   points$x > (as.data.frame(shape[m,"coords_x1"])[1,1]-.0001) &
                   points$quad == quad[j] &
                   points$year == year[i] &
                   points$species == species[k], "neighbors_10"] <- count
        }
      }
    }
  }
}

#calculate nearest neighbor density for 15cm radius
for(j in 1:length(quad)) {
  temp1 <- points[points$quad==quad[j],]
  for (i in 1:length(year)) {
    temp2 <- temp1[temp1$year==year[i],]
    for (k in 1:length(species)) {
      temp3 <- temp2[temp2$species == species[k],]
      if (nrow(temp3)>0) {
        shape <- st_read(dsn = paste(quad[j]), layer = paste("pnt_",quad[j],"_",year[i], sep = ""))
        shape <- shape[shape$Species == as.character(species[k]),]
        for (m in 1:nrow(shape)) {
          buffer <- st_buffer(shape[m,],.15)
          count <- ifelse(length(unlist(st_intersects(buffer, shape)))==0,
                          yes = (0), no = (length(unlist(st_intersects(buffer, shape)))-1))
          points[points$x < (as.data.frame(shape[m,"coords_x1"])[1,1]+.0001) &
                   points$x > (as.data.frame(shape[m,"coords_x1"])[1,1]-.0001) &
                   points$quad == quad[j] &
                   points$year == year[i] &
                   points$species == species[k], "neighbors_15"] <- count
        }
      }
    }
  }
}

#calculate nearest neighbor density for 20cm radius
for(j in 1:length(quad)) {
  temp1 <- points[points$quad==quad[j],]
  for (i in 1:length(year)) {
    temp2 <- temp1[temp1$year==year[i],]
    for (k in 1:length(species)) {
      temp3 <- temp2[temp2$species == species[k],]
      if (nrow(temp3)>0) {
        shape <- st_read(dsn = paste(quad[j]), layer = paste("pnt_",quad[j],"_",year[i], sep = ""))
        shape <- shape[shape$Species == as.character(species[k]),]
        for (m in 1:nrow(shape)) {
          buffer <- st_buffer(shape[m,],.20)
          count <- ifelse(length(unlist(st_intersects(buffer, shape)))==0,
                          yes = (0), no = (length(unlist(st_intersects(buffer, shape)))-1))
          points[points$x < (as.data.frame(shape[m,"coords_x1"])[1,1]+.0001) &
                   points$x > (as.data.frame(shape[m,"coords_x1"])[1,1]-.0001) &
                   points$quad == quad[j] &
                   points$year == year[i] &
                   points$species == species[k], "neighbors_20"] <- count
        }
      }
    }
  }
}


# remove individuals from the 'points' dataset that are <5cm from the edges of a plot (won't include in the analysis, since we cannot accurately estimate nearest neighbor density)
points$edgeAS <- NA
points[points$x<0.05 | points$x>0.95 | points$y<0.05 | points$y>0.95,"edgeAS"] <- TRUE
points[points$x>=0.05 & points$x<=0.95 & points$y>=0.05 & points$y<=0.95,"edgeAS"] <- FALSE

#### remove files that aren't needed for further analysis ####
rm(list = ls()[!(ls() %in% c('points','poly'))])

#### for next script, need 'points' and 'poly' data.frames ####
<<<<<<< HEAD
<<<<<<< HEAD
#save as an .RData file 
# path =  #location where you'll put the environment data file
setwd(path)
save.image('script0_output.RData')
=======
>>>>>>> 00232cd... renamed file
=======
#save as an .RData file 
# path =  #location where you'll put the environment data file
setwd(path)
save.image('script0_output.RData')
>>>>>>> f43506c... updating script 0 and script 1
