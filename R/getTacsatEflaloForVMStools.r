
library(visstatExtraction)
library(vmstools)

  #-Get the data
tacsat              <- GetDataTacsat(paste('01-jan-','2003',sep=""),paste('31-jan-','2004',sep=""),flag_nations=c('nld'),which.lib="RODBC")
tacsat              <- formatTacsat(tacsat)
eflalo              <- GetDataEflalo(paste('01-jan-','2003',sep=""),paste('31-jan-','2004',sep=""),flag_nations=c('nld'),which.lib="RODBC")
eflalo              <- formatEflalo(eflalo)

  #-Look at distribution of vessels and gears by month
tacsat$SI_DATIM     <- as.POSIXct(paste(tacsat$SI_DATE,  tacsat$SI_TIME,   sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")
eflalo$SI_DATIM     <- as.POSIXct(paste(eflalo$FT_DDAT,  eflalo$FT_DTIME,  sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M")

tacsatDistri        <- table(tacsat$VE_REF,months(tacsat$SI_DATIM),year(tacsat$SI_DATIM))
eflaloDistri        <- table(eflalo$VE_REF,eflalo$LE_MET_level6,months(eflalo$SI_DATIM),year(eflalo$SI_DATIM))
eflaloDistri[which(eflaloDistri>0)] <- 1; 
eflaloDistri <- data.frame(eflaloDistri); colnames(eflaloDistri) <- c("Vessel","Metier","Month","Year","Freq")
eflaloDistri        <- aggregate(eflaloDistri$Freq,by=list(eflaloDistri$Metier,eflaloDistri$Month,eflaloDistri$Year),FUN=sum);
 colnames(eflaloDistri) <- c("Metier","Month","Year","N")

  #-Make sure that per metier at least 5 vessels are present in a months time
eflaloSelect        <- which(eflaloDistri$N > 5,arr.ind=T)

  #-Make a selection of the data, based on the eflaloSelect
selectEflalo        <- numeric()
selectTacsat        <- numeric()
forbiddenVessels03  <- character()
forbiddenVessels04  <- character()

for(iSel in 1:length(eflaloSelect)){
  yr              <- anf(eflaloDistri[eflaloSelect[iSel],"Year"])
  mnth            <- ac(eflaloDistri[eflaloSelect[iSel],"Month"])
  met             <- ac(eflaloDistri[eflaloSelect[iSel],"Metier"])
  maxSampleSize   <- anf(eflaloDistri[eflaloSelect[iSel],"N"]) - 5
  if(maxSampleSize > 0){
    res             <- eflalo[which(year(eflalo$SI_DATIM) == yr & months(eflalo$SI_DATIM) == mnth & eflalo$LE_MET_level6 == met),]
    if(yr == 2003)  vessels2Choose  <- res$VE_REF[which(!res$VE_REF %in% forbiddenVessels03)]
    if(yr == 2004)  vessels2Choose  <- res$VE_REF[which(!res$VE_REF %in% forbiddenVessels04)]

    chosenVessels   <- sample(unique(vessels2Choose),ifelse(maxSampleSize > 50,ifelse(length(unique(vessels2Choose))<50,length(unique(vessels2Choose)),50),ifelse(maxSampleSize>length(unique(vessels2Choose)),length(unique(vessels2Choose)),maxSampleSize)))
    if(yr == 2003)  forbiddenVessels03 <- sort(unique(c(forbiddenVessels03,chosenVessels)))
    if(yr == 2004)  forbiddenVessels04 <- sort(unique(c(forbiddenVessels04,chosenVessels)))

    selectEflalo    <- rbind(selectEflalo,res[which(res$VE_REF %in% chosenVessels),])
  }
}

  #- Merge tacsat to get good set of matching eflalo-tacsat records
tacsatp             <- mergeEflalo2Tacsat(selectEflalo,tacsat)
selectTacsat        <- tacsatp[which(tacsatp$FT_REF != 0),-dim(tacsatp)[2]]

 
#Create a fictional country 

selectTacsat$VE_COU <- 'Narnia'
selectEflalo$VE_COU <- 'Narnia'

#Noise on the locations 

selectTacsat$SI_LATI <- jitter(selectTacsat$SI_LATI)
selectTacsat$SI_LONG <- jitter(selectTacsat$SI_LONG)

#Noise on the landings & values data 

 for(i in c(grep("_KG_",colnames(selectEflalo)),grep("_EURO_",colnames(selectEflalo)))) selectEflalo[,i][selectEflalo[,i]>0] <- jitter(selectEflalo[,i][selectEflalo[,i]>0])
 
#Make certain there are still no negative catches

 for(i in c(grep("_KG_",colnames(selectEflalo)),grep("_EURO_",colnames(selectEflalo)))) selectEflalo[,i][selectEflalo[,i] < 0] <- 0

#Add on time to date string

selectTacsat$SI_DATE <- gsub('2003','1989',selectTacsat$SI_DATE)
selectEflalo$FT_LDAT <- gsub('2003','1989',selectEflalo$FT_LDAT)
selectEflalo$FT_DDAT <- gsub('2003','1989',selectEflalo$FT_DDAT)
selectEflalo$LE_CDAT <- gsub('2003','1989',selectEflalo$LE_CDAT)

selectTacsat$SI_DATE <- gsub('2004','1990',selectTacsat$SI_DATE)
selectEflalo$FT_LDAT <- gsub('2004','1990',selectEflalo$FT_LDAT)
selectEflalo$FT_DDAT <- gsub('2004','1990',selectEflalo$FT_DDAT)
selectEflalo$LE_CDAT <- gsub('2004','1990',selectEflalo$LE_CDAT)

   #- set up location of repository

setwd("D:\\bearedo\\Projects\\vmstools\\vmstools\\data")

selectEflalo  <-  selectEflalo[,c(1:183)]
selectTacsat <- selectTacsat[,c(1:8)]

 
  #-Make sure formats are right

selectEflalo        <- formatEflalo(selectEflalo)
selectTacsat        <- formatTacsat(selectTacsat)


 #- write out data
 
save(selectEflalo,file='eflalo.rda',compress=T)
save(selectTacsat,file='tacsat.rda',compress=T) 
  
  
  
  
  
  
  
  
  