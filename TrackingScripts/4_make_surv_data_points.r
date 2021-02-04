# INPUTS
rm(list=ls())

setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/Adler Dowloaded Datasets/Adler_CO_Downloaded Data")

startRun<-1997
endRun<-2010
bigFile<-"./CO_allrecords_density.csv"
inventory<-"./CO_quad_inventory.csv"
outfile<-paste0("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData/SurvivalData/point_species_survD.csv")

genet=T   # track genets (T) or assume each point is an individual (F)

maxDist<-0.05  # max distance a plant can move between years (check units!)
maxDorm<-1    # max dormancy a plant is allowed (0 = no dormancy)

tmp <- read.csv("./point_species_todo.csv")
sppList <- as.character(tmp$species)
sppCodes <- as.character(tmp$code)


# IMPORT ALL RECORDS FILE------------------------------------------------
allD<-read.csv(bigFile)

#reorder & edit
allD<-allD[,c("quad","year","Species","X","Y")]
names(allD)<-c("quad","year","Species","x","y")

# format quadrat inventory
Qyrs<-read.table(inventory,header=T)
Qnames <- names(Qyrs)

absMin<-min(Qyrs,na.rm=T)
absMax<-max(Qyrs,na.rm=T)


# DEFINE TRACKING FUNCTION-------------------------------------------------------------

track<-function(x,y,year,maxDist,maxDorm,seedling,genet){

  # This function is for ONE species in ONE quadrat in ALL years.
    
  # This function determines which plants are colonists and
  # which survive based on coordinates and year of observation.
  # Each plant can only have one "parent" and one "child".
  
  # Inputs:
  # x = vector of x coordinates
  # y = vector of y coordinates
  # year = vector of year of observation
  # maxDist = max distance a plant can move and still be one individ.
  # maxDorm = max number of yrs a plant can remain dormant (0= no dormancy)
  # seedling = "Y" means we know this point is a seedling
  # genet = Boolean variable; T = track genets, F = track ramets
  
  # out = matrix with 4 columns, first is the age of the plant,
  # second is whether or not it survives,
  #	third is the period of dormancy a plant is emerging from,
  #	and fourth is a unique tracking number given to each individual
  
  xdiff<-outer(x,x,FUN="-")
  ydiff<-outer(y,y,FUN="-")
  dist<-sqrt(xdiff^2+ydiff^2)
  rm(xdiff,ydiff)
  dist[dist>maxDist]<-9999
  tmp<-diag(9999,dim(dist)[1])
  dist<-dist+tmp
  
  dorm<-outer(year,year,"-")
  dorm[dorm<1]<-NA
  dorm[dorm>(1+maxDorm)]<-NA
  dorm<-(dorm-1)*100
  dorm[is.na(dorm)]<-9999
  
  track<-dist+dorm
  track[track>9999]<-9999
  track[seedling=="Y",]<-9999
  
  if(genet==F){
    colMin<-apply(track,2,min)
    colMin<-matrix(colMin,dim(track)[1],dim(track)[2],byrow=T)
    track[track!=colMin]<-9999
  }
  # make sure there's only one parent
  rowMin<-apply(track,1,min)
  rowMin<-matrix(rowMin,dim(track)[1],dim(track)[2])
  track[track!=rowMin]<-9999
  
  track<-track!=9999 # TRUE or FALSE---1 or 0
  isColonist<-ifelse(rowSums(track)==0,1,0)
  survives<-1*(colSums(track)>0)
  
  # do output
  age<-rep(NA,length(isColonist))
  dormancy<-rep(NA,length(isColonist))
  trackID<-rep(NA,length(isColonist))
  trackN<-0
  for(i in 1:length(isColonist)){
     if(isColonist[i]==1)
     {
         trackN<-trackN+1
         trackID[i]<-trackN
         age[i]<-1
         dormancy[i]<-0
     }else{
         tmp<-which(track[i,]==T)
         delta<-dorm[i,tmp]/100+1 # delta: 1 or 2
         ifelse(delta>1,dormancy[i]<-(delta-1),dormancy[i]<-0)
         age[i]<-age[tmp]+delta
         trackID[i]<-trackID[tmp]
     }
  }
  out<-cbind(age,survives,dormancy,trackID)
  out
} # end function --------------------------------------


#----------------------------------------------------------

# GET LIST OF GOOD QUAD-YEARS (TWO CONSECUTIVE CENSUSES)
quadInfo=read.csv("./CO_quad_inventory.csv", sep = "\t") #inventory
quadInfo=stack(quadInfo)
names(quadInfo)=c("year","quad")
quadInfo=subset(quadInfo,is.na(year)==F)
tmp=quadInfo
tmp$year=tmp$year-1
goodRecords=merge(quadInfo,tmp)
goodRecords=goodRecords[order(goodRecords$quad,goodRecords$year),]
goodRecords=goodRecords[,c("quad","year")]

# MAIN LOOP

#let's remove the duplicates from allD
totN=dim(allD)[1]
duplicate=rep(NA,totN); duplicate[1]=F
if(totN>1){
  for(i in 2:totN){
    duplicate[i]=(allD$x[i]==allD$x[i-1] & allD$y[i]==allD$y[i-1] & allD$year[i]==allD$year[i-1])
  }
}
tmp=which(duplicate==T);print(tmp) ## to find which quadrat and year
if(length(tmp)>0){
    allD=allD[-tmp,]
}

for(iSpp in 1:length(sppList)){
  dospp = sppList[iSpp]
  D<-subset(allD,Species==dospp)
  if(dim(D)[1]==0) stop(paste("Check spelling of ",dospp))
  
  # TRACK GENETS 
  D$year<-as.numeric(as.vector(D$year))
  origColN<-dim(D)[2]
  D$age<-rep(NA,dim(D)[1])
  D$survives<-rep(NA,dim(D)[1])
  D$dormancy<-rep(NA,dim(D)[1])
  D$trackID<-rep(NA,dim(D)[1])
  
  quadList<-unique(D$quad)
  for(qI in 1:length(quadList)){
    rows<-which(D$quad==quadList[qI])
    tmpD<-D[rows,1:origColN]    
    out<-track(tmpD$x,tmpD$y,tmpD$year,maxDist,maxDorm,seedling=tmpD$Seedling,genet=genet)
    D[rows,]<-cbind(tmpD,out)
  } # next qI
  
  # FORMAT SURVIVAL DATA
  
  # aggregate individuals points to genet scale
  D$stems=1
  genD=aggregate(D[,c("stems","survives")],by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=sum)
  genD$survives[genD$survives>1]<-1
  tmp=aggregate(cbind(D[,c("x","y","age")]),by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=mean)
  genD=merge(genD,tmp)
  
  # flag individuals within 5 cm of edges
  minXY=aggregate(D[,c("x","y")],by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=min)
  names(minXY)[4:5]=c("minx","miny")
  maxXY=aggregate(D[,c("x","y")],by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=max)
  names(maxXY)[4:5]=c("maxx","maxy")
  tmp=merge(minXY,maxXY)
  tmp$nearEdge = ifelse((tmp$minx<0.05 | tmp$miny<0.05 | tmp$maxx>0.95 | tmp$maxy > 0.95),TRUE,FALSE)
  
  genD=merge(genD,tmp[,c("quad","year","trackID","nearEdge")])

  # genet survival as a function of size and year------------------------------------
  survD=merge(goodRecords,genD)
  survD$species=sppList[iSpp]
  survD <- survD[,c(NCOL(survD),1:(NCOL(survD)-1))]
  
  # calcualte and merge in quadrat stem density
  quadDens=aggregate(D$stems,by=list("quad"=D$quad,"year"=D$year),FUN=sum)
  names(quadDens)[3]="quadDensity"
  quadDens$species=sppList[iSpp]
  quadDens <- quadDens[,c(NCOL(quadDens),1:(NCOL(quadDens)-1))]
  survD <- merge(survD,quadDens)
  
  if(iSpp==1){
    survD_all_species = survD
  }else{
    survD_all_species = rbind(survD_all_species,survD)
  }
    
  rm(tmpD,out,rows,qI,quadList,origColN,minXY,maxXY,quadDens)
  gc()

} # next species

write.csv(survD_all_species, outfile,row.names=F)
