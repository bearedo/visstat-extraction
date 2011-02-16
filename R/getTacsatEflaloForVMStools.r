

#1. Install libraries and maximum memory

detach(package:vmstools)

install.packages("visstatExtraction_0.15.tar.gz",repos=NULL)                                             # from nemo

install.packages("/media/n/Projecten/VMS tools/9 Repository/VMS_tools/vmstools_0.46.tar.gz",repos=NULL)  # from nemo

setwd('/media/n/Projecten/bearedo/Projects')


library(visstatExtraction)
library(vmstools)
memory.size(4000)

#-Get the data

tacsat1              <- GetDataTacsat(paste('01-jan-','2003',sep=""),paste('31-dec-','2003',sep=""),flag_nations=c('nld'),which.lib="RODBC")
gc(reset=TRUE)
tacsat2              <- GetDataTacsat(paste('01-jan-','2004',sep=""),paste('31-dec-','2004',sep=""),flag_nations=c('nld'),which.lib="RODBC")

  #-Combine them
tacsat <- rbind(tacsat1,tacsat2)

  #-Tidy up
  
rm(tacsat1,tacsat2)
gc(reset=T)

   #-Format
tacsat              <- formatTacsat(tacsat)

#save(tacsat,file='tacsat.rda',compress=T)

load('tacsat.rda')

eflalo1              <- GetDataEflalo(paste('01-jan-','2003',sep=""),paste('31-dec-','2003',sep=""),flag_nations=c('nld'),which.lib="RODBC")

#save(eflalo1,file='eflalo1.rda',compress=T)
load('eflalo1.rda')

eflalo2              <- GetDataEflalo(paste('01-jan-','2004',sep=""),paste('31-dec-','2004',sep=""),flag_nations=c('nld'),which.lib="RODBC")

save(eflalo2,file='eflalo2.rda',compress=T)

load('eflalo2.rda')

   #-combine the two eflalo datasets

d1 <- dimnames(eflalo1)[[2]]
d2 <- dimnames(eflalo2)[[2]]

d <- unique(c(d1,d2))

m1 <- match(d1,d)
m2 <- match(d2,d)

df1 <- data.frame(matrix(NA,nrow=nrow(eflalo1),ncol=length(d)))
colnames(df1)<-d1
for(i in 1:length(m1)){df1[,m1[i]] <- eflalo1[,i] }
colnames(df1) <- d


df2 <- data.frame(matrix(NA,nrow=nrow(eflalo2),ncol=length(d)))
colnames(df2)<-d2
for(i in 1:length(m2)){df2[,m2[i]] <- eflalo2[,i] }
colnames(df2) <- d

eflalo <- rbind(df1,df2)


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

reorderEflaloDistri <- orderBy(~-N+Month+Year+Metier,data=eflaloDistri[eflaloSelect,])
thres               <- 205

for(iSel in 1:dim(reorderEflaloDistri)[1]){
  yr              <- anf(reorderEflaloDistri[iSel,"Year"])
  mnth            <- ac(reorderEflaloDistri[iSel,"Month"])
  met             <- ac(reorderEflaloDistri[iSel,"Metier"])
  maxSampleSize   <- anf(reorderEflaloDistri[iSel,"N"]) - 5
  if(maxSampleSize > 0){
    res             <- eflalo[which(year(eflalo$SI_DATIM) == yr & months(eflalo$SI_DATIM) == mnth & eflalo$LE_MET_level6 == met),]
    if(yr == 2003)  vessels2Choose  <- res$VE_REF[which(!res$VE_REF %in% forbiddenVessels03)]
    if(yr == 2004)  vessels2Choose  <- res$VE_REF[which(!res$VE_REF %in% forbiddenVessels04)]

    chosenVessels   <- sample(unique(vessels2Choose),ifelse(maxSampleSize > thres,ifelse(length(unique(vessels2Choose))<thres,length(unique(vessels2Choose)),thres),ifelse(maxSampleSize>length(unique(vessels2Choose)),length(unique(vessels2Choose)),maxSampleSize)))
    if(yr == 2003)  forbiddenVessels03 <- sort(unique(c(forbiddenVessels03,chosenVessels)))
    if(yr == 2004)  forbiddenVessels04 <- sort(unique(c(forbiddenVessels04,chosenVessels)))

    selectEflalo    <- rbind(selectEflalo,res[which(res$VE_REF %in% chosenVessels),])
  }
}

print(dim(selectEflalo))

table(selectEflalo$LE_GEAR)

  #- Merge tacsat to get good set of matching eflalo-tacsat records

selectEflalo$FT_DDATIM <- as.POSIXct(paste(selectEflalo$FT_DDAT, selectEflalo$FT_DTIME,sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M:%S")
selectEflalo$FT_LDATIM <- as.POSIXct(paste(selectEflalo$FT_LDAT, selectEflalo$FT_LTIME,sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M:%S")
selectEflalo$FT_DDATIM <- selectEflalo$FT_DDATIM - (12*60*60)
selectEflalo$FT_LDATIM <- selectEflalo$FT_LDATIM + (12*60*60)
selectEflalo$FT_LDAT   <- ac(format(selectEflalo$FT_LDATIM,format="%d/%m/%Y"))
selectEflalo$FT_LTIME  <- ac(format(selectEflalo$FT_LDATIM,format="%H:%M:%S"))
selectEflalo$FT_DDAT   <- ac(format(selectEflalo$FT_DDATIM,format="%d/%m/%Y"))
selectEflalo$FT_DTIME  <- ac(format(selectEflalo$FT_DDATIM,format="%H:%M:%S"))

tacsatp             <- mergeEflalo2Tacsat(selectEflalo,tacsat)
selectTacsat        <- tacsatp[which(tacsatp$FT_REF != 0),-dim(tacsatp)[2]]

selectEflalo$FT_DDATIM <- selectEflalo$FT_DDATIM + (12*60*60)
selectEflalo$FT_LDATIM <- selectEflalo$FT_LDATIM - (12*60*60)
selectEflalo$FT_LDAT   <- ac(format(selectEflalo$FT_LDATIM,format="%d/%m/%Y"))
selectEflalo$FT_LTIME  <- ac(format(selectEflalo$FT_LDATIM,format="%H:%M:%S"))
selectEflalo$FT_DDAT   <- ac(format(selectEflalo$FT_DDATIM,format="%d/%m/%Y"))
selectEflalo$FT_DTIME  <- ac(format(selectEflalo$FT_DDATIM,format="%H:%M:%S"))

 
#Create a fictional country 

selectTacsat$VE_COU <- 'Atlantis'
selectEflalo$VE_COU <- 'Atlantis'

#Noise on the locations 

selectTacsat$SI_LATI <- jitter(selectTacsat$SI_LATI,0.25)
selectTacsat$SI_LONG <- jitter(selectTacsat$SI_LONG,5)
#range(selectTacsat2$SI_LATI - selectTacsat$SI_LATI)[2]*60*1852 #should equal approx 500
#range(selectTacsat2$SI_LONG - selectTacsat$SI_LONG)[2]*30*1852 #should equal approx 500

 #- Replace NAs with Zeros

for(i in 32:dim(selectEflalo)[2]){ 
selectEflalo[,i] <- ifelse(is.na(selectEflalo[,i]),0,selectEflalo[,i]) }


#Noise on the landings & values data 

 for(i in c(grep("_KG_",colnames(selectEflalo)),grep("_EURO_",colnames(selectEflalo)))) selectEflalo[,i][selectEflalo[,i]>0] <- jitter(selectEflalo[,i][selectEflalo[,i]>0])
 
#Make certain there are still no negative catches

 for(i in c(grep("_KG_",colnames(selectEflalo)),grep("_EURO_",colnames(selectEflalo)))) selectEflalo[,i][selectEflalo[,i] < 0] <- 0

#Add on time to date string

selectTacsat$SI_DATE <- gsub('2003','1800',selectTacsat$SI_DATE)
selectEflalo$FT_LDAT <- gsub('2003','1800',selectEflalo$FT_LDAT)
selectEflalo$FT_DDAT <- gsub('2003','1800',selectEflalo$FT_DDAT)
selectEflalo$LE_CDAT <- gsub('2003','1800',selectEflalo$LE_CDAT)

selectTacsat$SI_DATE <- gsub('2004','1801',selectTacsat$SI_DATE)
selectEflalo$FT_LDAT <- gsub('2004','1801',selectEflalo$FT_LDAT)
selectEflalo$FT_DDAT <- gsub('2004','1801',selectEflalo$FT_DDAT)
selectEflalo$LE_CDAT <- gsub('2004','1801',selectEflalo$LE_CDAT)


 
  #- Remove extra dates

selectEflalo <- selectEflalo[,1:length(d)]

   #- set up location to dump data

setwd("/media/n/Projecten/bearedo/Projects/vmstoolstestdatasets")

selectEflalo  <-  selectEflalo[,c(1:189)]
selectTacsat <- selectTacsat[,c(1:8)]

 #-Make sure formats are right

selectEflalo        <- formatEflalo(selectEflalo)
selectTacsat        <- formatTacsat(selectTacsat)

 #- write out data
 
 eflalo <- selectEflalo
 tacsat <- selectTacsat
 
save(eflalo,file='eflalo.rda',compress=T)
save(tacsat,file='tacsat.rda',compress=T) 
  
  
  
  