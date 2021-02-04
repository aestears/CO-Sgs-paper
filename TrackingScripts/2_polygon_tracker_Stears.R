# This script tracks the identity of polygons in quadrats over time
# It assumes polygon holes are correctly flagged
# Change handling of missing vs. dormant yrs ???

rm(list=ls())
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData") #define the working directory as the folder that contains the polygon file
library(sp)
library(maptools)
library(rgeos)
library(tidyverse)
library(lwgeom)

species <- c("Aristida longiseta","Bouteloua gracilis", "Buchloe dactyloides","Carex eleocharis","Carex filifolia","Carex spp.","Ceratoides lanata", "Eriogonum effusum","Muhlenbergia torreyi","Schedonnardus paniculatus","Sitanion hystrix",  "Sporobolus cryptandrus","Stipa comata")    #input a vector of the names of the species you want to track
dir.create("./InputData") #create the folder that will contain the tracking output
dir.create("./InputData/PolygonTrackingResults") #define the sub-folder that will contain the tracking output
#---------------------------------------------------------
#define this function, which will tell us whether a polygon is close to the quadrat edge
checkEdge=function(x,edgeDist,minCoord,maxCoord){
# check whether polygon is close to quadrat edge
# x is a SpatialPolygons list object of length 1 (1 feature)
   N1=length(x@Polygons)
   out=F
   ii=1
   while(ii<=N1 & out==F){
        p1=x@Polygons[[ii]]@coords
        if(min(p1)<=(minCoord+edgeDist) |  max(p1)>=(maxCoord-edgeDist)) out=T
        ii=ii+1
   } # next ii
   out
}
#---------------------------------------------------------
for(spI in 1:length(species)) {
  doSpp <- species[spI]

maxDorm=1 #we only want to allow a dormancy of 1 year before considering the plant as 'dead'
maxArea=10000 # 100 cm^2
edgeDist=5 # define the edge of the quadrat as 5cm from edge
minCoord=0 #0 cm 
maxCoord=100 #1 cm
sppShapes=paste0("polygons/",doSpp,"/")
sppBuffers=paste0("polygons/",doSpp," buf5/")
outfile=paste("InputData/PolygonTrackingResults/",
              str_sub(str_to_upper(str_split(species[spI]," ", simplify = TRUE))[1],1,3), 
              str_sub(str_to_upper(str_split(species[spI]," ", simplify = TRUE))[2],1,3),
              "_buf5_dorm1.csv",sep = "")

#loop through quadrats
quadFiles=list.files(path=sppBuffers) ##define all of the quadrats that have data available in them 
#quadFiles="B6" ## for one specific quadrat

firstQuad=T
for(qI in 1:length(quadFiles)){
  quad=quadFiles[qI] #get the name of this quadrat/year combo
  rawDir=paste(sppShapes,"",quad,sep="") #define the directory that contains the raw (unbuffered) shapefiles
  bufDir=paste(sppBuffers,quad,sep="")
  # load all polygons in the quad into one list of SpatialPolygonsDataFrame
  # with a new year attribute
  #loop through years in quadrat
  yrFiles=list.files(path=bufDir,pattern=".shp$")
  
  if(length(yrFiles)>0){
    begin=T
    for(yrI in 1:length(yrFiles)){
      if(begin==T){ ##for the first year
        #read in original data
        tmp=substr(yrFiles[yrI],1,nchar(yrFiles[yrI])-4)
        infile=paste(rawDir,"/",tmp,sep="")
        rawD=readShapePoly(infile)
        #check to make sure that rawD polygons are valid
        if(gIsValid(rawD)==FALSE){
          #check to make sure that the shape is valid
            rawD<- gBuffer(rawD, byid=TRUE, width=0)
        }
        rawD@data$year=as.numeric(substr(infile,nchar(infile)-3,nchar(infile))) #make sure the 'year' data is in the correct format
        rawD@data$quad=as.character(quad)      #make sure the 'quadrat' name is in the correct format
        rawD@data=rawD@data[,c("SP_ID","area","Species","x","y","year","quad")] #remove any columns we don't need
        #read in buffered data
        infile=paste(bufDir,"/",tmp,sep="")
        bufD=readShapePoly(infile)
        bufD@data$year=as.numeric(substr(infile,nchar(infile)-3,nchar(infile)))  #make sure the 'year' data is in the correct format
        bufD@data$quad=as.character(quad) #make sure the 'quadrat' name is in the correct format
        #bufD@data$area=bufD@data$area_1
        #bufD@data$area=bufD@data$area*maxArea
        bufD@data=bufD@data[,c("SP_ID","area","Species","x","y","year","quad")]  #remove any columns we don't need

        begin=F
        
      }else{ ##for the following year
        
        tmp=substr(yrFiles[yrI],1,nchar(yrFiles[yrI])-4)
        #read in original data
        infile=paste(rawDir,"/",tmp,sep="")
        print(infile)
        tmp1=readShapePoly(infile)
        tmp1@data$year=as.numeric(substr(infile,nchar(infile)-3,nchar(infile)))
        tmp1@data$quad=as.character(quad)
        tmp1@data=tmp1@data[,c("SP_ID","area","Species","x","y","year","quad")]
        #assign track IDs
        ID1=getSpPPolygonsIDSlots(rawD)
        ID2=getSpPPolygonsIDSlots(tmp1)
        ID2=as.character(as.numeric(ID2)+1+max(as.numeric(ID1))) 
        tmp2=spChFIDs(tmp1,ID2)
        rawD=spRbind(rawD,tmp2) #add data for new year to data from previous year (rawD)
        rm(tmp1,tmp2)
        #make sure that new data added to rawD is geometrically valid
        if(gIsValid(rawD)==FALSE){
          #check to make sure that the shape is valid
          rawD<- gBuffer(rawD, byid=TRUE, width=0)
        }
        #buffered data
        infile=paste(bufDir,"/",tmp,sep="")
        tmp1=readShapePoly(infile)
        tmp1@data$year=as.numeric(substr(infile,nchar(infile)-3,nchar(infile)))
        tmp1@data$quad=as.character(quad)
        #tmp1@data$area=tmp1@data$area_1
        #tmp1@data$area=tmp1@data$Area*maxArea
        
        tmp1@data=tmp1@data[,c("SP_ID","area","Species","x","y","year","quad")]

        ID1=getSpPPolygonsIDSlots(bufD)
        ID2=getSpPPolygonsIDSlots(tmp1)
        ID2=as.character(as.numeric(ID2)+1+max(as.numeric(ID1)))
        tmp2=spChFIDs(tmp1,ID2) ##change feature IDs in spatial objects
        bufD=spRbind(bufD,tmp2) ##rbind for spatial objects
        rm(tmp1,tmp2)
      }  # end if
    } # next yrI
    # look for duplicates
    totN=dim(rawD)[1] #find the number of rows (observations) in the rawD dataset
    duplicate=rep(NA,totN); duplicate[1]=F #the first observation can't be a duplicate(?)
    if(totN>1){ #if the raw data for years added so far is greater than 1 (can't have a duplicate with only one datapoint)
      for(i in 2:totN){
        #determine if the there are duplicate polygons in the same year (including the year is a change from Adler's version)
        #identify the year in which you're looking for duplicates
          year <- rawD$year[i]
          #identify any potential duplicates
          bad <- (rawD$year == year & #are the observations in the same year?
               rawD$x == rawD$x[i] & #do the observations have the same x coordinate?
               rawD$y == rawD$y[i] & #do the observations have the same y coordinate?
               rawD$area == rawD$area[i] 
               ) 
          #excluding the 'TRUE' value for the 'ith' observation (shares it's own X, Y, area, and year values), are there any "TRUE" values?
          sum(bad[-i])
          ifelse(test = sum(bad[-i])>=1,
                 yes = duplicate[i] <- TRUE,
                 no = duplicate[i] <- FALSE)
            
          #Adler code  
          # ( #are the year values the same?
          #   rawD$x[i]==rawD$x[i-1] #are the x values the same? 
          #  & rawD$y[i]==rawD$y[i-1] #are the y values the same?
          #  & rawD$area[i]==rawD$area[i-1]) #is the area value the same?
      }
      #if there are duplicates, then we must identify only one of them as a duplicate (i.e. remove the "TRUE" from the duplicates argument for one of the observations so both aren't deleted)
      #get the row names in the rawD dataset of the TRUE (duplicate) observations
      dupsTRUE <- which(duplicate==TRUE)
      #make a temporary dataset with only the duplicated data
      if(length(dupsTRUE) > 0) {
      dupsDat <- rawD@data[dupsTRUE,]
      #add a column for the rowIDs from the duplicate vector
      dupsDat$dupID <- dupsTRUE
      #identify which are duplicates and get the rowID for the duplicate vector
       BADdupIDs <- dupsDat[duplicated(dupsDat[,1:7]),"dupID"]
       #change the TRUE's to FALSE's in the duplicate dataset for one of the two duplicated values
       duplicate[BADdupIDs] <- FALSE
      }
       }
    # remove duplicates
    tmpp=which(duplicate==T);print(tmpp)
    if(length(tmpp)>0){
      rawD=rawD[-tmpp,]
      bufD=bufD[-tmpp,]
     }
    
    # create matrix of year lags
    if(sum(bufD@data$year!=rawD@data$year)>0) stop("Raw and buffer data do not match")
    year=bufD@data$year
    dorm<-outer(year,year,"-")
    dorm[dorm<1]<-NA
    dorm[dorm>(1+maxDorm)]<-NA
    dorm<-(dorm-1)*maxArea*-1

    # create matrix of centroid distances (for tiebreaker)
    xdiff<-outer(rawD@data$x,rawD@data$x,FUN="-")
    ydiff<-outer(rawD@data$y,rawD@data$y,FUN="-")
    dist<-sqrt(xdiff^2+ydiff^2)
    rm(xdiff,ydiff)

    # create matrix of polygons overlaps
    if(dim(dorm)[1]>1){
      overlap=dorm
      overlap[is.na(overlap)==F]=-99
      for(j in 1:(dim(overlap)[1]-1)){
        for(k in (j+1):dim(overlap)[1]){
            if(is.na(overlap[k,j])==F){
              tmp=gIntersection(rawD[j,],bufD[k,])
              if(is.null(tmp)){
                overlap[k,j]=0
              }else{
                overlap[k,j]=gArea(tmp)
              }    
            }
        } # next k
      } # next j
      overlap[overlap==0]=NA
    }else{
      overlap=0
    }    
    
    # assign identities
    track<-overlap+dorm
    
    ##here could generate some warnings, but it is OK.
    ###Warning in FUN(newX[, i], ...) :
    ###no non-missing arguments to max; returning -Inf
    rowMax<-apply(track,1,max,na.rm=T) #more than 1 max, exactly equal
    rowMax<-matrix(rowMax,dim(track)[1],dim(track)[2])
    track[track!=rowMax]<-NA    
    track<-(is.na(track)==F) 
    track[rawD$seedling=="Y",]<-F
    
    # check for multiple parents
    tmp=rowSums(track)
    tmp2=which(tmp>1)
    if(length(tmp2)>0){
      for(j in 1:length(tmp2)){
        tmp3=which(track[tmp2[j],]==T)
        tmp4=which(dist[tmp2[j],tmp3]==min(dist[tmp2[j],tmp3]))
        track[tmp2[j],]=F
        track[tmp2[j],tmp3[tmp4]]=T
      } # next j
    } # end if
    
    if(sum(rowSums(track)>1)) stop("Multiple parents")
    
    isColonist<-ifelse(rowSums(track)==0,1,0)
    survives<-1*(colSums(track)>0)

    # add to output data table
    age<-rep(NA,length(isColonist))
    dormancy<-rep(NA,length(isColonist))
    trackID<-rep(NA,length(isColonist))
    nearEdge<-rep(NA,length(isColonist))
    firstYear<-rep(NA,length(isColonist))
    trackN<-0
    for(i in 1:length(isColonist)){
       if(isColonist[i]==1)
       {
           trackN<-trackN+1
           trackID[i]<-trackN
           age[i]<-1
           dormancy[i]<-0
           nearEdge[i]=checkEdge(rawD@polygons[[i]],edgeDist,minCoord,maxCoord)
       }else{
           tmp<-which(track[i,]==T)
           delta<-dorm[i,tmp]/(-1*maxArea)+1
           ifelse(delta>1,dormancy[i]<-(delta-1),dormancy[i]<-0)
           age[i]<-age[tmp]+delta
           trackID[i]<-trackID[tmp]
           nearEdge[i]=checkEdge(rawD@polygons[[i]],edgeDist,minCoord,maxCoord)
       }
    }

    out<-data.frame(cbind(rawD@data,age,survives,dormancy,trackID,nearEdge))
    out=out[,c("quad","year","SP_ID","Species","area","x","y",
               "age","survives","dormancy","trackID","nearEdge")]
    #write to output
    if(firstQuad==T){
        write.table(out,paste0("./",outfile),row.names=F,sep=",")
        firstQuad=F
    }else{
       write.table(out,outfile,row.names=F,col.names=F,sep=",",append=T)
    } # end if

  } # end if length(yrFiles)
  
  print(paste("Finished",quad,sep=" "))
  flush.console()
} 
}#next quad




