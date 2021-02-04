
# output survival data sets for Daniel Laughlin

rm(list=ls())
require(tidyverse)

#define working directory (change for your file structure!)
setwd("/Users/Alice/Dropbox/Grad School/Research/Trait Project/CO_sgs Analysis/trackingData")

# #you can make a csv file of the species that you want to do, but in this case I'll just make character vectors by hand--simpler when we only have a few species (or one!)
# tmp <- read.csv("polygon_species_todo.csv")
# sppNames <- as.character(tmp$species)
# sppCodes <- as.character(tmp$code)
sppNames <- c("Aristida longiseta","Bouteloua gracilis", "Buchloe dactyloides","Carex eleocharis","Carex filifolia","Carex spp.","Ceratoides lanata", "Eriogonum effusum","Muhlenbergia torreyi","Schedonnardus paniculatus","Sitanion hystrix",  "Sporobolus cryptandrus","Stipa comata")
sppCodes <- c("ARILON", "BOUGRA", "BUCDAC", "CARELE", "CARFIL", "CARSPP", "CERLAN", "ERIEFF", "MUHTOR", "SCHPAN", "SITHYS","SPOCRY", "STICOM")

#make folder to hold survival data
dir.create("./SurvivalData")
outfile<-"SurvivalData/polygon_species_survD.csv"

for(i in 1:length(sppCodes)){
  
  infile1<-paste0("./InputData/PolygonTrackingResults/",sppCodes[i],"_buf5_dorm1.csv") #sppFile

  D<-read.csv(infile1)
  
  # get quad inventory & good years for survival and growth (if yr 30 & 31 are present, 30 is a good year)
  # This just had to do with missing data. Observing 
  # survival/growth/recruitment requires having a map for "two consecutive 
  # years", which means that one missing year actually causes us to lose two 
  # years of observations. If we have a map for 1930, but no map for 1931, 
  # then 1930 is a "bad" year because we cannot observe survival, growth or 
  # recruitment. 1931 is obviously a bad year too. If we have a map for 1932 
  # and 1933, then 1932 is a "good year," meaning we have a complete 
  # observation of a survival/growth/recruitment transition.
  
  quadInfo=read.table("./InputData/CO_quad_inventory.csv", sep = "\t", header = TRUE) #inventory
  quadInfo=stack(quadInfo)
  names(quadInfo)=c("year","quad")
  quadInfo=subset(quadInfo,is.na(year)==F)
  tmp=quadInfo
  tmp$year=tmp$year-1
  goodRecords=merge(quadInfo,tmp)
  goodRecords=goodRecords[order(goodRecords$quad,goodRecords$year),]
  goodRecords=goodRecords[,c("quad","year")]
  
  # aggregate plants (polygons) to genet scale------------------------------------
  sizeD=aggregate(cbind(D[,c("area","survives")]),by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=sum)
  sizeD$survives[sizeD$survives>1]<-1
  
  tmp=aggregate(cbind(D[,c("x","y","age")]),by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=mean)
  sizeD=merge(sizeD,tmp)
  
  tmp=aggregate(D$nearEdge,by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=sum)
  names(tmp)[4]="nearEdge"
  tmp$nearEdge = ifelse(tmp$nearEdge==0,FALSE,TRUE)
  
  sizeD=merge(sizeD,tmp)
  #tmp=aggregate(D$allEdge,by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=min)
  #names(tmp)[4]="allEdge"
  #sizeD=merge(sizeD,tmp)
  
  # no information for seedling status
  # tmp=aggregate(D$seedling=="Y",by=list("quad"=D$quad,"year"=D$year,"trackID"=D$trackID),FUN=min)
  # names(tmp)[4]="seedling"
  # sizeD=merge(sizeD,tmp)
  
  sizeD$year=as.numeric(as.character(sizeD$year))   
  
  quadList=sort(unique(sizeD$quad))
  
  #calculate size of the individual in the next year
  quads <- unique(sizeD$quad)
  rm(sizeD_size)
    for(j in 1:length(quads)){
      tmp2 <- sizeD[sizeD$quad==quads[j],] #get data for one quadrat
      trackIDs <- unique(tmp2$trackID) #determine the unique track ID's that exist in that quadrat
      for(k in 1:length(trackIDs)){
        tmp3 <- tmp2[tmp2$trackID==trackIDs[k],] #get the rows of data for only one unique track ID
        tmp3 <- tmp3[order(tmp3$year),] #make sure that the data.frame is ordered in sequence by year
        if(nrow(tmp3)>1){
          for(l in 1:(nrow(tmp3)-1)){
            age_1 <- tmp3[l,"age"] #get the age of the focal individual
            ageMin_1 <- age_1+1 #get the age of the focal individual in the next year
            #make sure that there is a row of data for the appropriate 'year after' age
            if((tmp3[l+1,"age"]==ageMin_1)==TRUE){
              #calculate the growth of the individual to the next year
              area_tplus1 <- tmp3[l+1,"area"]
              #make a temporary data.frame that contains the growth data
              tmp4 <- tmp3[l,]
              tmp4$area_tplus1 <- area_tplus1
              #put the results into a new data frame
              if(exists("sizeD_size")==FALSE){
                sizeD_size <- tmp4
              }else{
                sizeD_size <- rbind(sizeD_size,tmp4)
              }
            }
          }
        }
      }
    }
  if(exists("sizeD_size")==TRUE){
    sizeD <- left_join(sizeD,sizeD_size)
  } else{
      sizeD$area_tplus1 <- NA
    }
  
  # genet survival as a function of size and year------------------------------------
  survD=merge(goodRecords,sizeD)
  survD$species=sppNames[i]
  survD <- survD[,c(NCOL(survD),1:(NCOL(survD)-1))]

  # calculate and merge in quadrat cover
  quadCov=aggregate(D$area,by=list("quad"=D$quad,"year"=D$year),FUN=sum)
  names(quadCov)[3]="quadCover"
  quadCov$species=sppNames[i]
  quadCov <- quadCov[,c(NCOL(quadCov),1:(NCOL(quadCov)-1))]
  survD <- merge(survD,quadCov)
  
  if(i==1){
    survD_all_species = survD
  }else{
    survD_all_species = rbind(survD_all_species,survD)
  }

} # next i

# convert x, y coordinates from cm to m (not necessary here, since the scale is already in m!)
#survD_all_species$x <- survD_all_species$x/100
#survD_all_species$y <- survD_all_species$y/100

write.csv(survD_all_species, outfile,row.names=F)


#### Make a column to indicate whether the plant is a recruit or not ####
survD_recruits <- survD_all_species
#unless we know otherwise, assume all plants are not recruits
survD_recruits$recruit <- 0

#for those individuals that have an age of 1 in 2002 (for most plots) or 2009, we cannot know whether they were recruited in the year they were measured (i.e. could have recruited in 2000, 2001, or 2008)
#there are also plots that started measuring later, so we need to change which year is the first year for some plots (use a loop to do this)
survD_recruits$age_t_fixed <- survD_recruits$age
species <- unique(survD_recruits$species)
quads <- unique(survD_recruits$quad)
for(spI in 1:length(species)){
  for(i in 1:length(quads)){
    #identify the start of data collection for this year
    datYears <- quadInfo[quadInfo$quad==quads[i],"year"]
    startYear <- min(datYears)
    #identify any gaps in the sequence of years
    sequential <- c(datYears,NA) - c(NA,datYears)
    startYear_2 <- datYears[which(sequential != 1)]
    
    #for those individuals that were measured first in start years, give an 'NA' in 'recruit' column
    survD_recruits[survD_recruits$species==species[spI] &
                     survD_recruits$quad==quads[i] &
                     survD_recruits$year %in% c(startYear, startYear_2),
                     'recruit'] <- NA
    #give these individuals an 'NA' in the 'age_t_fixed' column
    survD_recruits[survD_recruits$species==species[spI] &
                     survD_recruits$quad==quads[i] &
                     survD_recruits$year %in% c(startYear, startYear_2),
                   'age_t_fixed'] <- NA
    #follow these individuals through time to change 'age' argument into NA (since we don't know when it was 'born')
    #get their track IDs
     badTrackIDs <- survD_recruits[survD_recruits$species==species[spI] &
                     survD_recruits$quad==quads[i] &
                     survD_recruits$year %in% c(startYear, startYear_2),'trackID'] 
     #find other records of these individuals through time
     survD_recruits[survD_recruits$species==species[spI] &
                      survD_recruits$quad==quads[i] &
                      survD_recruits$trackID %in% badTrackIDs,
                      'age_t_fixed'] <- NA 
    #for those individuals that were NOT measured in start years, and are 1 year old, give a 'recruit' value of '1'
     survD_recruits[survD_recruits$species==species[spI] & #for our target species
                      survD_recruits$quad==quads[i] & #for our target quad
                      survD_recruits$year %in% c(datYears[datYears!=startYear & datYears!=startYear_2]) & #in those years that are not 'start' years
                      survD_recruits$age==1, #when age of the individual is 1
                      'recruit'] <- 1
  }
}
  


survD_IPM <- left_join(survD_all_species, survD_recruits, by = c("species","quad","year","trackID","area","survives","x","y","age","nearEdge","quadCover","area_tplus1"))

#change the column names to something more intuitive
names(survD_IPM) <- c("species", "quad", "year_t", "trackID", "area_t", "survives_tplus1", "x_t", "y_t", "age_t", "nearEdge_t" ,"area_tplus1", "quadCover", "recruit_t",  "age_t_fixed")


#check to make sure that there are no years for which the plant survives but there is no size in year t+1
sizeBad <- survD_IPM %>% 
  filter(survives_tplus1==1 &
          is.na(area_tplus1)==TRUE)
#(no years without a good explanation) (i.e. has skipped a year due to one-year buffer of skipping)


write.csv(survD_IPM, "SurvivalData/polygon_species_survD_IPM.csv", row.names = F)




#compare this new dataset to the old version
#new: survD_all_species
#old
survOld <- read.csv("/Users/Alice/Box Sync/traits-rates/sgs/SurvivalData/polygon_species_survD.csv", stringsAsFactors = FALSE)
survOld$type <- "old"
survOld$quad <- as.factor(survOld$quad)

survNew <- survD_all_species

survCompare <- left_join(survNew[,c("species","quad","year","trackID")], survOld[,c("species","quad","year","trackID","type")], by = c("species", "quad", "year", "trackID"))

survCompare[is.na(survCompare$type)==TRUE,"type"] <- "new"

table(survCompare[,c("species","type")])

survNew$type <- "New"
survOld$type <- "Old"
survNewAnti <- anti_join(survNew[,c("species","quad","year","trackID", "type")], survOld[,c("species","quad","year","trackID","type")], by = c("species", "quad", "year", "trackID"))
survOldAnti <- anti_join(survOld[,c("species","quad","year","trackID", "type")], survNew[,c("species","quad","year","trackID","type")], by = c("species", "quad", "year", "trackID"))


