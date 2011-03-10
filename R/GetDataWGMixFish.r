
GetDataWGMixFish <- function(syear=2003,eyear=2009){

 ##Create a vector of dates to loop over
#
dats <- data.frame(start.date=paste('1','Jan',syear:eyear,sep="-"), end.date= paste('31','Dec',syear:eyear,sep="-"))
#
#
for (i in 1:7) {

print(dats$start.date[i])
data <- GetDataLandingValueByRegistration(Cstart=dats$start.date[i],Cstop=dats$end.date[i])

#Just species required

data <- data[data$TXN_ICES_CODE   %in% c('COD','SOL','HAD','PLE','POK','WHG','NEP') ,]


#Add on year

data$YEAR <- format(data$ARRIVAL_DATE,"%Y")

#Add on quarter

data <- qtr.f(data)

#Add on gear category

data <- DCFGearCodes(data=data,data.type="visstat")

#Pasive or static?

data <- PassiveOrStatic(data)

#Vessel length category

data <- WGMixFishLengthCats(data)

#Mesh size category

data <- DCFMeshCategory(data,data.type="visstat")

#Nephrops function groups

data <- Nephrops.FU(data)

#New species coding

data$species <- as.character(data$TXN_ICES_CODE)
ww <- grep('NEP',as.character(data$species))
data$species[ww] <- paste(data$species[ww],as.character(data$NEPFU[ww]),sep="")


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

data<-data[data$RGN_TRP_PPY_PLM_CNY_CODE=='NLD',]

#Put on new country code

data$RGN_TRP_PPY_PLM_CNY_CODE <- 'NED'

#Sum over year, quarter, country etc.
#Table A.

lst <- list(data$YEAR,data$QUARTER,data$VESSEL_LENGTH,data$GEAR,data$MESH_SIZE_RANGE,data$narea,data$species)
tC <- vectorise(tapply(data$WEIGHT,lst,sum.na))
tV <- vectorise(tapply(data$VALUE,lst,sum.na))
tC$VALUE<-tV$value

id <- paste(tC$V2,tC$V3,tC$V4,tC$V5,tC$V6,tC$V7,tC$V8,sep='|')

#Make Table A

tabA <- data.frame(ID=id,
COUNTRY=rep('NED',length(tC[,1])),YEAR =  tC$V2, QUARTER= tC$V3, VESSEL_LENGTH = tC$V4, GEAR = tC$V5, MESH_SIZE_RANGE = tC$V6, AREA = tC$V7,
SPECIES = tC$V8, LANDINGS = tC$value, DISCARDS=rep(NA,length(tC[,1])), VALUE = tC$VALUE)

tabA<-tabA[!is.na(tabA$LANDINGS),]

#Make Table B (effort)

data1  <- unique(paste(data$YEAR,data$QUARTER,data$VESSEL_LENGTH,data$GEAR,data$MESH_SIZE_RANGE,data$narea,data$COARSE_DAS,data$KWDAS,sep="|"))


data1  <- data.frame(matrix(unlist(strsplit(data1,"\\|")),ncol=8,byrow=T))
data1$DAYS_AT_SEA_EFFORT <- as.numeric(as.vector(data1$X7))
data1$KW_DAYS_EFFORT <- as.numeric(as.vector(data1$X8))

lst <- list(data1$X1,data1$X2,data1$X3,data1$X4,data1$X5,data1$X6)
tkwd <- vectorise(tapply(data1$KW_DAYS_EFFORT,lst,sum.na))
tdas <- vectorise(tapply(data1$DAYS_AT_SEA_EFFORT,lst,sum.na))
tkwd$das<-tdas$value

id <- paste(tkwd$V2,tkwd$V3,tkwd$V4,tkwd$V5,tkwd$V6,tkwd$V7,sep='|')

tabB <- data.frame(ID=id,
COUNTRY=rep('NED',length(tkwd[,1])),YEAR =  tkwd$V2, QUARTER= tkwd$V3, VESSEL_LENGTH = tkwd$V4, GEAR = tkwd$V5, MESH_SIZE_RANGE = tkwd$V6, AREA = tkwd$V7, KW_DAYS_EFFORT = tkwd$value, DAYS_AT_SEA_EFFORT=tkwd$das, NO_VESSELS = '-1')

tabB<-tabB[!is.na(tabB$KW_DAYS_EFFORT),]

setwd("D:/bearedo/WorkingGroups/WGMIXFISH")


if (i == 1){

  write.table (tabA, file = 'tabA.csv',col.names=TRUE, sep=",",row.names=F)
  write.table (tabB, file = 'tabB.csv',col.names=TRUE, sep=",",row.names=F)
  }

else {

  write.table (tabA, file = 'tabA.csv', col.names=FALSE, sep=",", row.names =F,append=T)
  write.table (tabB, file = 'tabB.csv', col.names=FALSE, sep=",", row.names =F,append=T)

  }

}

# EOF
}




  #save(tacsat,file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//tacsat.rda",compress=T)




