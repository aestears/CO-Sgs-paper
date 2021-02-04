rm(list=ls())
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData")

library(sp)
library(maptools)
library(rgeos)
require(tidyverse)

##Polygons, Polygon are not inhereted from Spatial class
##this is to keep the same bbox slot for rawD and buffD
##if not, the bbox will be extrated from the coordinates by default.
species <- c("Aristida longiseta","Bouteloua gracilis", "Buchloe dactyloides","Carex eleocharis","Carex filifolia","Carex spp.","Ceratoides lanata", "Eriogonum effusum","Muhlenbergia torreyi","Schedonnardus paniculatus","Sitanion hystrix",  "Sporobolus cryptandrus","Stipa comata")     
for(spI in 1:length(species)) {
  doSpp <- species[spI]

  
myBox<-matrix(c(0,0,100,100),nrow=2,dimnames=list(NULL,c("min","max")))
rownames(myBox)<-c("x","y")

startDir="polygons"
outDir<-"polygons"
BUFF_DIST<-5

outDir=paste(outDir,"/",doSpp," buf5",sep="")
dir.create(outDir)
quadList=list.files(paste(startDir,"/",doSpp,sep=""))

for(i in :length(quadList)){
  tmpQuad<-quadList[i]
  buffOutDir<-paste(outDir,"/",tmpQuad,sep="")
  dir.create(buffOutDir)
  tmpShp<-list.files(paste(startDir,"/",doSpp,"/",tmpQuad,sep=""), pattern = ".shp$")
  if(length(tmpShp)>0){
    for(j in 1:length(tmpShp)){
      thisYear<-str_sub(tmpShp[j],start = 1, end = -5L)
      thisShp<-paste("./",startDir,"/",doSpp,"/",tmpQuad,"/",sep="")
      files <- list.files(thisShp,pattern = thisYear)
      if(length(files)>0){
      rawD=readShapePoly(paste0(thisShp,"/",thisYear))
      rawD@bbox<-myBox
      
      ##warnings "Polygons object missing comment attribute ignoring hole(s). 
      ##See function createSPComment."
      rawD<-createSPComment(rawD)##there is another function named createPolygonsComment()
      
      
      
      buffD=gBuffer(rawD,width=5,byid=T) #just SpatialPolygons
      buffD@bbox<-myBox
      
      
#       plot(rawD) #not even buffer around the original polygons
#       plot(buffD)
      
      
      
      buffDData<-SpatialPolygonsDataFrame(buffD,rawD@data,match.ID=TRUE) #SpatialPolygonsDataFrame
      buffDData@data$BUFF_DIST<-5  #following the similar style of Python...

      writePolyShape(buffDData,paste(buffOutDir,"/",thisYear,sep=""))
      }
      }
      }
    }
  }


