#/////////////////////
# Upadating Flagstaff Chart Quadrat Shapefiles to work with Adler scripts
# Prepared by Alice Stears
# 3 December 2019 
#/////////////////////
rm(list=ls())

#load required packages
require(tidyverse)
require(sf)
require(mapview)

#set working directory
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/FlagstaffSpatialData") #set the working directory to be the folder that contains the "RawShapefiles" folder


#### changes needed to make shapefiles work with Adler's survival scripts ####
#make sure that the column names are the same (capitalize Species)
#convert the scale to cm, rather than m
#divide the data into "cover" and "density" files

#### read in data files in their intial form ###
#this is assuming that you have all of the shapefiles for a given species in a single folder, labeled by that species name (instead of AF test, for example)

species <- c("Festuca arizonica", "Muhlenbergia montana") #define the species that you want to work with (i.e. the names of the folders that contain the shapefile data for each species)
dir.create("./polygons")
for(spI in 1:length(species)){
#list of file names in the directory
files <- list.files(paste0("./RawShapefiles/", species[spI]), pattern = ".shp$")
#generate a list of all of the quads
quads <- unique(paste(str_split(files,"_",n=3,simplify = TRUE)[,1],str_split(files,"_",n=3,simplify = TRUE)[,2],sep = "_"))
#create a directory within 'polygons' to hold the files for this species
dir.create(paste0("./polygons","/",species[spI]))
for(i in 1:length(quads)) {
  thisQuad <- quads[i]
  years <-files[str_detect(files,paste0(thisQuad,"_"))]
  #make outfile directory for Cover file
  OutDirCover <- paste0("./polygons/",species[spI],"/",thisQuad)
  dir.create(OutDirCover)
  for(j in 1:length(years)){
    #read in the files
    tempShape <- st_read(dsn = paste0("./RawShapefiles/",species[spI]), str_sub(years[j], start = 1, end = nchar(years[j])-4))
    if(nrow(tempShape)>0){
    #calculate the area of the polygon
    tempShape$area <- as.numeric(st_area(tempShape))
    #calculate the centroid coordinates of the polygon
    centroids_sf <- st_centroid(tempShape$geometry)
    centroids <- do.call(rbind, st_geometry(centroids_sf)) %>% 
      as_tibble() %>% setNames(c("x","y"))
    #put these centroids into the sf data frame as x and y
    tempShape$x <- centroids$x
    tempShape$y <- centroids$y
      #make directories for this year
      yearDirCover <- paste0(OutDirCover,"/",str_sub(years[j], start = 1, end = nchar(years[j])-4))
      dir.create(yearDirCover)
        #write Cover shapefile to the directory
      st_write(tempShape, 
             dsn = yearDirCover, 
             layer = str_sub(years[j], start = 1, end = nchar(years[j])-4),
             driver = "ESRI Shapefile")
    }
  }
  }
}


