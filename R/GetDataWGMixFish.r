
GetDataWGMixFish <- function(syear=2003,eyear=2010,which.lib='RODBC'){

 ##Create a vector of dates to loop over
#
dats <- data.frame(start.date=paste('1','Jan',syear:eyear,sep="-"), end.date= paste('31','Dec',syear:eyear,sep="-"))
#

#Attach libraries
library(visstatExtraction)
library(vmstools)

for (i in 1:8)  {     # 2010

print(dats[i,])
data <- GetDataLandingValueByRegistration(Cstart=dats$start.date[i],Cstop=dats$end.date[i],which.lib='RODBC')

#Add on year

data$YEAR <- format(data$ARRIVAL_DATE,"%Y")

curyear <- as.numeric(unique(data$YEAR))

#Add on quarter

data <- qtr.f(data)

#Add on gear category

data <- DCFGearCodes(input=data,data.type="visstat")

#Pasive or static?

data <- PassiveOrStatic(input=data)

#Vessel length category

data <- WGMixFishLengthCats(data)

#Mesh size category

data <- DCFMeshCategory(input=data,data.type="visstat")

#Areas in format required

data$narea <- as.character(data$ICES_SUBAREA)

data$narea[data$narea %in% c('IVa','IVb','IVc')] <- "4"
data$narea[data$narea %in% c('IV')] <- "4"
data$narea[data$narea %in% 'VIIa'] <- "7a"
data$narea[data$narea %in% 'VIIc'] <- "7c"
data$narea[data$narea %in% 'VIIb'] <- "7b"
data$narea[data$narea %in% 'VIId'] <- "7d"
data$narea[data$narea %in% 'VIIe'] <- "7e"
data$narea[data$narea %in% 'VIIh'] <- "7h"
data$narea[data$narea %in% 'VIIj'] <- "7j"
data$narea[data$narea %in% 'IIa'] <- "2a"
data$narea[data$narea %in% 'VIIIb'] <- "8b"
data$narea[data$narea %in% 'IIIa'] <- "3a"
data$narea[data$narea %in% 'Vb'] <- "5b"
data$narea[data$narea %in% 'VIa'] <- "6a"

data$narea[data$narea %in% 'UNK'] <- NA

#Take out Dutch boats

data<-data[data$RGN_TRP_PPY_PLM_CNY_CODE=='nld',]

#Put on new country code

data$RGN_TRP_PPY_PLM_CNY_CODE <- 'NED'

#Nephrops functional groups

data <- Nephrops.FU(data)


### LANDINGS ###

#Extract species required  for this call

data.l <- data[data$TXN_ICES_CODE   %in% c('COD','SOL','HAD','PLE','POK','WHG','NEP') ,]

#New species coding

data.l$species <- as.character(data.l$TXN_ICES_CODE)
ww <- grep('NEP',as.character(data.l$species))
data.l$species[ww] <- paste(data.l$species[ww],as.character(data.l$NEPFU[ww]),sep="")


#Sum over year, quarter, country etc.
#Table A.

lst <- list(data.l$YEAR,data.l$QUARTER,data.l$VESSEL_LENGTH,data.l$GEAR,data.l$MESH_SIZE_RANGE,data.l$narea,data.l$species)
tC <- vectorise(tapply(data.l$WEIGHT,lst,sum,na.rm=T))
tV <- vectorise(tapply(data.l$VALUE,lst,sum,na.rm=T))
tC$VALUE<-tV$value

id <- paste(tC$V2,tC$V3,tC$V4,tC$V5,tC$V6,tC$V7,tC$V8,sep='|')

#Make Table A

tabA <- data.frame(ID=id,
COUNTRY=rep('NED',length(tC[,1])),YEAR =  tC$V2, QUARTER= tC$V3, VESSEL_LENGTH = tC$V4, GEAR = tC$V5, MESH_SIZE_RANGE = tC$V6, AREA = tC$V7,
SPECIES = tC$V8, LANDINGS = tC$value, DISCARDS=rep(NA,length(tC[,1])), VALUE = tC$VALUE)

tabA<-tabA[!is.na(tabA$LANDINGS),]

#Check 

print(tapply(tabA$LANDINGS,list(tabA$SPECIES,tabA$YEAR),sum,na.rm=T)/1000)



###### Make Table B (effort) ##########

data.e    <- data[!duplicated(data$TRIP_NUMBER),]; #Extract unique trips

#Check numbers look ok

print(tapply(data.e$KWDAS,list(data.e$GEAR),sum,na.rm=T))     

lst <- list(data.e$YEAR,data.e$QUARTER,data.e$VESSEL_LENGTH,data.e$GEAR,data.e$MESH_SIZE_RANGE,data.e$narea)

tkwd <- vectorise(tapply(data.e$KWDAS,lst,sum,na.rm=T))
tdas <- vectorise(tapply(data.e$DAS,lst,sum,na.rm=T))
tkwd$das<-tdas$value

id <- paste(tkwd$V2,tkwd$V3,tkwd$V4,tkwd$V5,tkwd$V6,tkwd$V7,sep='|')

tabB <- data.frame(ID=id,
COUNTRY=rep('NED',length(tkwd[,1])),YEAR =  tkwd$V2, QUARTER= tkwd$V3, 
VESSEL_LENGTH = tkwd$V4, GEAR = tkwd$V5, MESH_SIZE_RANGE = tkwd$V6, AREA = tkwd$V7, KW_DAYS_EFFORT = tkwd$value, DAYS_AT_SEA_EFFORT=tkwd$das, NO_VESSELS = '-1')

tabB<-tabB[!is.na(tabB$KW_DAYS_EFFORT),]

#Check 

print(tapply(tabB$KW_DAYS_EFFORT,list(tabB$GEAR),sum,na.rm=T))




if (i == 1){

  write.table (tabA, file = 'tabA.csv',col.names=TRUE, sep=",",row.names=F)
  write.table (tabB, file = 'tabB.csv',col.names=TRUE, sep=",",row.names=F)
  }

else {

  write.table (tabA, file = 'tabA.csv', col.names=FALSE, sep=",", row.names =F,append=T)
  write.table (tabB, file = 'tabB.csv', col.names=FALSE, sep=",", row.names =F,append=T)

  }
# EOF
}
}
 
 
setwd('/media/n/Projecten/ICES WG/WKMIXFISH/2010')        #On nemo

GetDataWGMixFish(syear=2003,eyear=2010)

################################################################
###########  Add on DISCARD estimates available ################ 
################################################################

### Use the discard data supplied to the STECF effort meeting ###

library(gdata)
dat <- read.xls("/media/n/Projecten/STECF/EWG-11-06/Datacall-2011/Input/DataSent/EWG-11-06_A.new.discards.xls")

discards <- dat[dat$DISCARDS != -1,]
discards <- discards[,c(1:13)]
discards$VESSEL_LENGTH <- 'o40m'
discards$ID <- paste(discards$YEAR,discards$QUARTER,discards$VESSEL_LENGTH,discards$GEAR,discards$MESH_SIZE_RANGE,discards$AREA,discards$SPECIES,sep='|')

#Read in the landings component

landings <- read.table('/media/n/Projecten/ICES WG/WKMIXFISH/2010/tabA.csv',sep=",",header=T)
landings <- landings[landings$YEAR == '2010',]

landings$DISCARDS <- discards$DISCARDS[match(landings$ID,discards$ID)]

landings$LANDINGS <- round(landings$LANDINGS/1000)

landings[!is.na(landings$DISCARDS),]



#
#library(vmstools)
#
#curyear <- 2010
#
#print(paste('Doing discard data',curyear)) #Only have the raising data for 2010 but this can be improved
#
#discard <- read.table('W:/IMARES/IJmuiden/WOT/WOT Discards demersaal/4 Rapportage/resultaten/STECF/discard/n_disc_trip.csv',sep=',',header=T)
#
###### Effort data needed for the raising #######
#
#effort.fleet <- read.table("W:/IMARES/IJmuiden/WOT/WOT Discards demersaal/4 Rapportage/resultaten/VanHelmond_2010/Effort/effort_fleet.csv",sep=",",header=T)
#effort.trip  <- read.table("W:/IMARES/IJmuiden/WOT/WOT Discards demersaal/4 Rapportage/resultaten/VanHelmond_2010/Effort/effort_trip.csv",sep=",",header=T)
#
##Add on 3-letter species code from the database
#
#visstat <- odbcConnect(dsn="visstatp", uid="doug",pwd="oneover");
#frisbe  <- odbcConnect(dsn="frisbep", uid="doug",pwd="ninethirty");
#
#taxons1 <- sqlQuery(visstat,"SELECT * from TAXONS")
#taxons2 <- sqlQuery(frisbe,"SELECT * from VIS_TAXONS")
#
#discard$SPECIES <- taxons2$ICES_CODE[match(discard$SCIENTIFIC_NAME,taxons2$SCIENTIFIC_NAME)]
#
##Extract data with a 3 letter code available
#
#discard <- discard[!is.na(discard$SPECIES),]
#
#####  Get weights per hour instead of numbers per hour ####
#
#lwt <- read.table('W:/IMARES/IJmuiden/WOT/WOT Discards demersaal/4 Rapportage/resultaten/STECF/discard/lwrel.csv',sep=',',header=T)
#
##Put on A & B
#
#x <- tolower(discard$SCIENTIFIC_NAME) 
#y <- tolower(lwt$SPEC)
#
#discard$a <- lwt$LWA[match(x,y)]
#discard$b <- lwt$LWB[match(x,y)]
#
##Chuck out things that don't match (mostly invertebrates)
#
#discard <- discard [!is.na(discard$a),]
#
## Apply non-linear model parameters, eg. W = aL^b
#
#discard$wt_total_trip <- (discard$a*(discard$class_length^discard$b) * discard$n_total_trip) /1000 #kilos
#
##Sum over species 
#
#discard.sums <- aggregate(list(dwt = discard$wt_total_trip), 
#list(SHIP = discard$SHIP, week = discard$week, 
#SCIENTIFIC_NAME = discard$SCIENTIFIC_NAME,year = discard$year,QUARTER = discard$quar),sum,na.rm=T)
#
#d1 <- discard.sums[discard.sums$year == curyear,]
#
#d1[d1$SCIENTIFIC_NAME == "Pleuronectes platessa",]
#
##Put the hpeffort on d1
#
##discard.sums$hpeffort <- effort.trip$hpeffort[match(paste(discard.sums$SHIP,discard.sums$week),paste(effort.trip$SHIP,effort.trip$week))]
#
##Put the fleet effort on
#
#dimnames(effort.fleet)[[2]][2] <- 'QUARTER'
#
#effort.fleet <- effort.fleet[effort.fleet$hpsegment2 == 'Groot' & effort.fleet$meshsegment2 == '80-99mm',]
#
##Aggregate over QUARTER and species 
#
#d2 <- aggregate (list(dwt=d1$dwt), list(SCIENTIFIC_NAME = d1$SCIENTIFIC_NAME,QUARTER = d1$QUARTER),sum,na.rm=T)
#e2 <- aggregate (list(hpeffort=effort.trip$hpeffort), list(QUARTER = effort.trip$qtr),sum,na.rm=T)
#
#d2$trip.hpeffort <- e2$hpeffort[match(d2$QUARTER,e2$QUARTER)]
#d2$fleet.hpeffort <- effort.fleet$hpeffort[match(d2$QUARTER,effort.fleet$QUARTER)]
#
#d2$discards <- (d2$fleet.hpeffort/d2$trip.hpeffort)*d2$dwt
#
#d2$discards <- round(d2$discards/1000)
#
# d2[d2$SCIENTIFIC_NAME == "Pleuronectes platessa",]
# d2[d2$SCIENTIFIC_NAME == "Gadus morhua",]
# d2[d2$SCIENTIFIC_NAME == "Gadus morhua",]
#
#
#
## Add on the 3-letter codes 
#
#d2$SPECIES <- ac(taxons2$ICES_CODE[match(d2$SCIENTIFIC_NAME,taxons2$SCIENTIFIC_NAME)] )
#
## JAX is the code for horse mackerel, ANF for Lophius piscatorius and FLX for flounders.
#
#d2$species[d2$SPECIES == 'HOM'] <- 'JAX'
#d2$species[d2$SPECIES == 'FLE'] <- 'FLX'
#d2$species[d2$SPECIES == 'MON'] <- 'ANF'
#
##Put on gear codes for large beam trawlers
#d2$COUNTRY <- 'NED';
#d2$YEAR <- curyear;
#d2$VESSEL_LENGTH <- 'o40m'
#d2$GEAR <- 'BEAM';
#d2$MESH_SIZE_RANGE <- '80-89';
#d2$AREA <- '4';
#d2$DISCARDS <- d2$discards * 1000 # put data into kilos
#
#discard.sums3 <- data.frame(COUNTRY=d2$COUNTRY,YEAR=d2$YEAR,QUARTER = d2$QUARTER,
#VESSEL_LENGTH = d2$VESSEL_LENGTH, GEAR = d2$GEAR,
#MESH_SIZE_RANGE = d2$MESH_SIZE_RANGE,AREA = d2$AREA,SPECIES = d2$SPECIES,DISCARDS = d2$DISCARDS)
#
##Match in the discard data 
#
##Read in table A
#
#tabA <- read.table("D:/bearedo/WorkingGroups/WGMIXFISH/tabA.csv",sep=",",header=T)
#
##tabA <- tabA[tabA$YEAR == 2010,]
#
#idl <- paste(tabA$YEAR,tabA$QUARTER,tabA$AREA,tabA$SPECIES,tabA$GEAR,tabA$VESSEL_LENGTH,tabA$MESH_SIZE_RANGE)
#idd <- paste(discard.sums3$YEAR,discard.sums3$QUARTER,discard.sums3$AREA,discard.sums3$SPECIES,discard.sums3$GEAR,discard.sums3$VESSEL_LENGTH,discard.sums3$MESH_SIZE_RANGE)
#
#tabA$DISCARDS <- discard.sums3$DISCARDS[match(idl,idd)]
#
##Check discards have gone in at the right place
#
#tabA[tabA$YEAR == 2010 & tabA$GEAR == 'BEAM' & tabA$AREA == '4' & tabA$VESSEL_LENGTH == 'o40m' & tabA$MESH_SIZE_RANGE == '80-89', ]
#
#setwd("D:/bearedo/WorkingGroups/WGMIXFISH")
##
#
#tabB <- read.table('tabB.csv',sep=',',header=T)
#tabB <- tabB[!is.na(tabB$YEAR),]
##
#
#bt2 <- tabB[tabB$AREA == '4' & tabB$MESH_SIZE_RANGE %in% c('80-89','90-99','100-109','>=120'),]
##
#round(tapply(bt2$KW_DAYS_EFFORT,list(bt2$GEAR,bt2$YEAR),sum)*1.34)
##
##
##### Write out the data with discards 
#  
#  write.table(tabA, file='tabA.csv',sep=',',row.names=F)
#  write.table(tabB, file='tabB.csv',sep=',',row.names=F)
#  
##### Just the 2010 data
#
write.table(landings, file='tabA.2010.csv',sep=',',row.names=F)


write.table(tabB[tabB$YEAR == 2010,], file='tabB.2010.csv',sep=',',row.names=F)
##
## ## Check numbers against eflalo 
## 
## load("N:/Projecten/Cod-Closures-2011/eflalo.10.rda")
##
## library(visstatExtraction)
## 
##  eflalo.10 <- DCFGearCodes(input=eflalo.10,data.type="eflalo")
##  test <- eflalo.10[,c(1:31,506)]
##
##  test <- PassiveOrStatic(input=test)
##
###
###   ##  Mesh size category
###
##test <- DCFMeshCategory(input=test,data.type="eflalo")
###
##bt2e <- test[test$GEAR == 'BEAM',]
###
###tapply(bt2e$LE_EFF,list(bt2e$GEAR),sum,na.rm=T)
####