#### load packages ####
require(tidyverse)
require(sf)
require(mapview)
require(lwgeom)

#### Calculating Nearest Neighbor Area for Polygon Dataset####
poly <- read.csv("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/SurvivalData/polygon_species_survD_IPM.csv")
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

#get the data for SP_ID from tracking files and merge this with the poly dataset (for individuals with only one observation--haven't been clumped at genet scale)
#load all tracking data files
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/InputData/PolygonTrackingResults")
for(k in 1:length(species)) {
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
#aggregate this data.frame by quad, year, species, and trackID to get the trackIDs of numbers that are singletons
#will use this data to identify the SP_ID's that are 'good' in the trackSP data.set
trackGOOD <- aggregate(trackSP, by = list(trackSP$quad, trackSP$year, trackSP$Species, trackSP$trackID), FUN = length)
# drop all IDs that have more than one individual (would have >1 SP_ID)
trackGOOD <- filter(trackGOOD,quad==1)
#add a column so we know the individual record is 'good'
trackGOOD$need <- "need"
trackGOOD <- trackGOOD[,c("Group.1", "Group.2", "Group.3", "Group.4", "need")]
names(trackGOOD) <- c("quad","year","Species","trackID","need")
#merge with trackSP dataset to ID 'good' records
trackSP <- left_join(trackSP, trackGOOD, by = c("quad", "year", "Species", "trackID"))
#filter the trackSP data for those that we "need"
trackSP <- filter(trackSP, need=="need")
#merge with poly dataset to get SP_IDs
poly<- left_join(poly, trackSP[,c("quad","year","SP_ID","Species","trackID")], by = c("quad", "year_t"="year", "species"="Species", "trackID"))

### calculate nearest neighbor for polygons with only one SP_ID ###

setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles")
baseShape <- st_read(dsn = "gzgz_5a", layer = "poly_gzgz_5a_1997")
box <- st_as_sfc(st_bbox(baseShape)) #make a box that is the size of a quadrat (1mx1m)
boxBuffer <- st_buffer(box, dist = -0.05)#make a 5cm buffer inside the box
boxBuffer <- st_difference(box, boxBuffer)

# for 5cm radius
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

#for 10cm radius
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

#for 15cm radius
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

#for 20cm radius
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



##problem: the individuals that have "NA"s for nearest neighbor values are those that have been 
# aggregated into a single genet. 
# So they are actually two polygons, and their area has been summed. 
# Need to figure out how to calculate area values for those individuals that are actually multiple polygons.  
# Could sum the area of the buffer polygons for each , but need to make sure they don't overlap, 
# and need to figure out how to identify those two polygons in the first place

## Workflow: 
# identify those polygons that have been aggregated into one individual 
#do this by pulling out the data files of raw polygon tracking by species, and merging these data with the data from the nearest neighbor calculations that don't have any nearest neighbors (the individuals that have been grouped!)
#can merge them by the track IDs! (I think... should be able to do a full join to identify the individuals that have been grouped)
#load raw tracking files

#set up a buffer box 
baseShape <- st_read(dsn = "gzgz_5a", layer = "poly_gzgz_5a_1997")
box <- st_as_sfc(st_bbox(baseShape)) #make a box that is the size of a quadrat (1mx1m)
boxBuffer <- st_buffer(box, dist = -0.05)#make a 5cm buffer inside the box
boxBuffer <- st_difference(box, boxBuffer)

##loop through species
#for 5cm radius
for (k in 1:length(species)){
  setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/InputData/PolygonTrackingResults")
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
        setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles")
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
  setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/InputData/PolygonTrackingResults")
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
        setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles")
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
  setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/InputData/PolygonTrackingResults")
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
        setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles")
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
  setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/InputData/PolygonTrackingResults")
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
        setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles")
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

#### Calculating Change in Area for Polygons####
poly$delta_area <- NA
# #track <- seq(1,807,1)
# 
# for (k in 1:length(species)){ #loop through all possible species
#   temp1 <- poly[poly$species==species[k],]
#   for(j in 1:length(quad)) { #within each species, loop throuhgh all quadrats
#     temp2 <- temp1[temp1$quad==quad[j],]
#     track <- unique(temp2$trackID)
#     for(n in 1:length(track)) { #within each quadrat and species, loop through each individual
#       temp3 <- temp2[temp2$trackID==track[n],]
#       if(nrow(temp3)>1){ #make sure that the individual stays alive for more than one year
#         temp3 <- arrange(temp3,year_t) #make sure that the rows are in order according to year (from earliest to latest)
#         #make sure that there are no gaps
#         #if there is a gap, re-start the counting process
#         for(l in 2:nrow(temp3)){ #for all but the first observation, subtract the area of the individual in the previous year from the area of the individual in the current year (get delta area)
#           if (temp3[l,"year_t"]-temp3[l-1,"year_t"]==1){
#             temp3[l,"delta_area"] <- temp3[l,"area_t"]-temp3[(l-1),"area_t"]
#             }
#           }
#       }
#       for(p in 1:nrow(temp3)){
#             poly[poly$species==species[k] & 
#                    poly$quad==quad[j] & 
#                    poly$trackID==track[n] &
#                    poly$year_t==temp3$year_t[p],"delta_area"] <- temp3[p,"delta_area"]
#           }
#       }
#     }
#   }
# ##for years after they are dormant, should have growth be an 'NA'
# # new recruits should also have an 'NA' 

#use area in t+1 in demographic tracking scripts to determine change in size from year t to year t+1 (size in year t+1 - size year t)
poly$delta_area <- poly$area_tplus1-poly$area_t


#write to csv file
write.csv(poly,
          "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files/Intermediate Analysis Files/polygon_demo_2_12_18.csv", 
          row.names = FALSE)

#### Calculate Nearest Neighbor for Points Dataset ####
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData")
points <- read.csv("SurvivalData/point_species_survD.csv", stringsAsFactors = FALSE)
points$Site <- "CO"
#have to do it separately for each quadrat for each year (loop through each quadrat and year)
#create an empty column for nearest neighbor density
points$neighbors_20 <- NA
points$neighbors_10 <- NA
#make a vector for years in the dataset
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles")
year <- sort(unique(points$year))
quad <- unique(points$quad)
species <- unique(points$species)
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

points$neighbors_20 <- NA
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Raw Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles")
year <- sort(unique(points$year))
quad <- unique(points$quad)
species <- unique(points$species)
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
points$neighbors_10 <- NA
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Raw Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles")
year <- sort(unique(points$year))
quad <- unique(points$quad)
species <- unique(points$species)
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
#there are some individuals that do not have an "NA" for nearest neigbhor-- not a match in the spatial dataset (in terms of x-y coordinates)
#could this be data entry error?

# remove individuals from the 'points' dataset that are 5cm from the edges of a plot
points$edgeAS <- NA
points[points$x<0.05 | points$x>0.95 | points$y<0.05 | points$y>0.95,"edgeAS"] <- TRUE
points[points$x>=0.05 & points$x<=0.95 & points$y>=0.05 & points$y<=0.95,"edgeAS"] <- FALSE

#write to csv
write.csv(points, 
          "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/CO Analysis Data Files/Intermediate Analysis Files/point_demo_2_23_20.csv", row.names = FALSE)
