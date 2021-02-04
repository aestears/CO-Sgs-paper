
rm(list=ls())

setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project")


# pull out polygons for focal species
#defining the species that you want to extract shapefiles for
doSpp = c("Aristida longiseta","Bouteloua gracilis", "Buchloe dactyloides","Carex eleocharis","Carex filifolia","Carex spp.","Ceratoides lanata", "Eriogonum effusum","Muhlenbergia torreyi","Schedonnardus paniculatus","Sitanion hystrix",  "Sporobolus cryptandrus","Stipa comata")


startDir <- "./Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles" #defining the name of the directory that contains the complete shapefiles
outDir1 <- "./CO_sgs Analysis/trackingData/polygons" #defining the name of the directory that will contain the subsette shapefiles
dir.create(outDir1) #creating the outdirectory in the working directory
rescale=T #defining your argument to the option 'rescale'. T = need to rescale measurement system of quadrat
scaleFactor=100   # convert to m from cm
myBbox=cbind(c(0,0),c(1,1))  #define the boundaries of the quadrat

library(sp) #load packages
library(maptools)
for(spI in 1:length(doSpp)){
outDir=paste(outDir1,"//",doSpp[spI],sep="") #define a folder for the output for each specific species
dir.create(outDir) #create the output directory

# loop through quads and data frames and write shapefiles
empty=NULL
quadList<-list.files("./Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data/CO_shapefiles") #make a list of all of the possible quadrats
# quadList=list.files(startDir,pattern="poly") #just need poly files

#quadList=quadList[-length(quadList)]  # drop readme file
for(q in quadList){
    print(q) # identifying the specific quadrat that you're using in this iteration of the loop
    dir.create(paste(outDir,"//",q,sep=""))
    
    yrList_all=list.files(paste(startDir,"//",q,sep=""),pattern=".shp$")#list both the point and polygon shapefiles that have data for this quad (years)
    tmp_poly<-grep("poly",yrList_all) #identify those file names that are for polygon shapefiles
    tmp_poly<-yrList_all[tmp_poly]#just polygons
    
    yrList=unique(as.numeric(substr(tmp_poly,start=nchar(tmp_poly)-7,stop=nchar(tmp_poly)-3))) #year
    
    for(yr in yrList){
        print(yr)
        flush.console()
        
        outfile=paste(outDir,"//",q,"//","poly_",q,"_",yr,sep="")
        infile=paste(startDir,"//",q,"//","poly_",q,"_",yr,".shp",sep="") 
        cover=readShapePoly(infile,delete_null_obj=TRUE) #for all species of the quadrat in this year yr
        
        tmp<-which(cover$Species==doSpp[spI])
        
        if(length(tmp)>0){
          cover=cover[tmp,]
          if(rescale==T){
            out=elide(cover,scale=scaleFactor,bb=myBbox)
            # correct areas in attribute data
            tmp=sapply(slot(out,"polygons"),FUN=function(i) slot(i,"area"))
            out$area=tmp
            # add corrected x,y coords to attribute data
            centroids = coordinates(out)
            out@data$x=centroids[,1]
            out@data$y=centroids[,2]
            cover=out
          }
          #get rid of redundant columns
          tmp=grep("SP_ID",names(cover))
          if(length(tmp)>0) cover@data=cover@data[,-tmp]
          
          #write shapefile
          writePolyShape(cover,outfile)
          
        }else{
          empty=c(empty,yr)
        }
    }
}
}

print(empty)

