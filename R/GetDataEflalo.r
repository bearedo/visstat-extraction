
#Cstart="01-jan-2006";Cstop="31-jan-2006"  
#flag_nations <- c('bel','deu','dnk','eng','fra','fro','gbr','irl','ltu','nld','nor','sco') 
#which.lib = 'RODBC'
##flag_nations <- c('nld')

GetDataEflalo <- function(Cstart=Cstart,Cstop=Cstop,flag_nations = flag_nations,which.lib=which.lib) 
{
# Get data 
#memory.size(4000)
 
valuebyreg <- GetDataLandingValueByRegistration(Cstart=Cstart, Cstop=Cstop,which.lib=which.lib)

#Reformat time strings

valuebyreg$ARRIVEL_TIME <- ReformatTime(valuebyreg$ARRIVEL_TIME,which.lib=which.lib)
valuebyreg$DEPARTURE_TIME <- ReformatTime(valuebyreg$DEPARTURE_TIME,which.lib=which.lib)

#Reformat date strings

valuebyreg$ARRIVEL_DATE <- ReformatDate( valuebyreg$ARRIVEL_DATE,which.lib=which.lib)
valuebyreg$DEPARTURE_DATE <- ReformatDate( valuebyreg$DEPARTURE_DATE,which.lib=which.lib)

#Select flag nations

valuebyreg <- valuebyreg[valuebyreg$RGN_TRP_PPY_PLM_CNY_CODE %in% flag_nations,]

# Transform the data frame into the crappy 'matrix' format required by eflalo.

mm<-paste(valuebyreg$TRIP_NUMBER,valuebyreg$VESSEL_ID2,
valuebyreg$LEVEL5,valuebyreg$RGN_TRP_PPY_PLM_CNY_CODE,valuebyreg$PRT_CNY_CODE,
valuebyreg$PRT_CNY_CODE_DEPARTED_FROM,
valuebyreg$GPY_CODE,valuebyreg$MESHSIZE,valuebyreg$QUADRANT,
valuebyreg$PRT_CODE_DEPARTED_FROM,valuebyreg$PRT_CODE,valuebyreg$DEPARTURE_DATE,valuebyreg$DEPARTURE_TIME,
valuebyreg$ARRIVEL_DATE,valuebyreg$ARRIVEL_TIME,valuebyreg$ICES_SUBAREA,valuebyreg$POWER,valuebyreg$LENGTH,valuebyreg$KWDAS,sep=",")

col.labels<-c("TRIP_NUMBER","VESSEL_ID2","LEVEL5","RGN_TRP_PPY_PLM_CNY_CODE","PRT_CNY_CODE",
"PRT_CNY_CODE_DEPARTED_FROM",
"GPY_CODE","MESHSIZE","QUADRANT",
"PRT_CODE_DEPARTED_FROM","PRT_CODE","DEPARTURE_DATE","DEPARTURE_TIME",
"ARRIVEL_DATE","ARRIVEL_TIME","ICES_SUBAREA","POWER","LENGTH","KWDAS")

sum.na <- function(x) { x<-sum(x[!is.na(x)]);x}

####################################
# First by weight of landings 
####################################

ef0<-tapply(valuebyreg$WEIGHT,list(mm,valuebyreg$TXN_ICES_CODE),sum.na)
mm1<-dimnames(ef0)[[1]]

 #Get the number of variables selected
ll<-length(unlist(strsplit(mm1[1],",")))


ef1<-matrix(unlist(strsplit(mm1,",")),ncol=ll,byrow=T)
dimnames(ef1)[[1]]<-1:dim(ef1)[1]
dimnames(ef1)[[2]][1:ll]<-col.labels


ef2 <- data.frame(VE_REF=ef1[,"VESSEL_ID2"],VE_FLT=ef1[,"LEVEL5"],VE_COU = ef1[,"RGN_TRP_PPY_PLM_CNY_CODE"], VE_LEN = ef1[,"LENGTH"], 
VE_KW= ef1[,"POWER"],VE_TON=rep(NA,dim(ef1)[1]),
FT_REF=ef1[,"TRIP_NUMBER"],FT_DCOU=ef1[,"PRT_CNY_CODE_DEPARTED_FROM"],FT_DHAR=ef1[,"PRT_CODE_DEPARTED_FROM"],
FT_DDAT=ef1[,"DEPARTURE_DATE"],FT_DTIME=ef1[,"DEPARTURE_TIME"],FT_LCOU = ef1[,"PRT_CNY_CODE"],
FT_LHAR = ef1[,"PRT_CODE"], FT_LDAT = ef1[,"ARRIVEL_DATE"], FT_LTIME=ef1[,"ARRIVEL_TIME"],
LE_ID = paste(ef1[,"TRIP_NUMBER"],ef1[,"GPY_CODE"],ef1[,"QUADRANT"],sep="-"),
LE_CDAT=rep(NA,dim(ef1)[1]),
LE_STIME=rep(NA,dim(ef1)[1]), 
LE_ETIME =rep(NA,dim(ef1)[1]),
LE_SLAT=rep(NA,dim(ef1)[1]),
LE_SLON=rep(NA,dim(ef1)[1]),
LE_ELAT=rep(NA,dim(ef1)[1]),
LE_ELON=rep(NA,dim(ef1)[1]),
LE_GEAR =  ef1[,"GPY_CODE"], LE_MSZ = ef1[,"MESHSIZE"], LE_RECT = ef1[,"QUADRANT"], 
LE_DIV = ef1[,"ICES_SUBAREA"], 
LE_MET_level6=rep(NA,dim(ef1)[1]),
LE_UNIT=rep("KWDAYS",dim(ef1)[1]),
LE_EFF=ef1[,"KWDAS"],LE_EFF_VMS=rep(NA,dim(ef1)[1]))
                                                             
ef3<-cbind(ef2,ef0)

#Replace NAs with Zeros
for(i in 32:dim(ef3)[2]){ ef3[,i] <- ifelse(is.na(ef3[,i]),0,ef3[,i])  }

dimnames(ef3)[[2]][32:dim(ef3)[2]]<-paste("LE_KG_",dimnames(ef3)[[2]][32:dim(ef3)[2]],sep="")

ef3$LE_EFF <- as.numeric(as.character(ef3$LE_EFF))
ef3$VE_LEN <- as.numeric(as.character(ef3$VE_LEN))
ef3$VE_KW  <- as.numeric(as.character(ef3$VE_KW))


############################
# Now by value of landings##
############################

ef0<-tapply(valuebyreg$VALUE,list(mm,valuebyreg$TXN_ICES_CODE),sum.na)
mm1<-dimnames(ef0)[[1]]

 #Get the number of variables selected
ll<-length(unlist(strsplit(mm1[1],",")))


ef1<-matrix(unlist(strsplit(mm1,",")),ncol=ll,byrow=T)
dimnames(ef1)[[1]]<-1:dim(ef1)[1]
dimnames(ef1)[[2]][1:ll]<-col.labels

ef2 <- data.frame(VE_REF=ef1[,"VESSEL_ID2"],VE_FLT=ef1[,"LEVEL5"],VE_COU = ef1[,"RGN_TRP_PPY_PLM_CNY_CODE"], VE_LEN = ef1[,"LENGTH"], 
VE_KW= ef1[,"POWER"],VE_TON=rep(NA,dim(ef1)[1]),
FT_REF=ef1[,"TRIP_NUMBER"],FT_DCOU=ef1[,"PRT_CNY_CODE_DEPARTED_FROM"],FT_DHAR=ef1[,"PRT_CODE_DEPARTED_FROM"],
FT_DDAT=ef1[,"DEPARTURE_DATE"],FT_DTIME=ef1[,"DEPARTURE_TIME"],FT_LCOU = ef1[,"PRT_CNY_CODE"],
FT_LHAR = ef1[,"PRT_CODE"], FT_LDAT = ef1[,"ARRIVEL_DATE"], FT_LTIME=ef1[,"ARRIVEL_TIME"],
LE_ID = paste(ef1[,"TRIP_NUMBER"],ef1[,"GPY_CODE"],ef1[,"QUADRANT"],sep="-"),
LE_CDAT=rep(NA,dim(ef1)[1]),
LE_STIME=rep(NA,dim(ef1)[1]), 
LE_ETIME =rep(NA,dim(ef1)[1]),
LE_SLAT=rep(NA,dim(ef1)[1]),
LE_SLON=rep(NA,dim(ef1)[1]),
LE_ELAT=rep(NA,dim(ef1)[1]),
LE_ELON=rep(NA,dim(ef1)[1]),
LE_GEAR =  ef1[,"GPY_CODE"], LE_MSZ = ef1[,"MESHSIZE"], LE_RECT = ef1[,"QUADRANT"], 
LE_DIV = ef1[,"ICES_SUBAREA"], 
LE_MET_level6=rep(NA,dim(ef1)[1]),
LE_UNIT=rep("KWDAYS",dim(ef1)[1]),
LE_EFF=ef1[,"KWDAS"],LE_EFF_VMS=rep(NA,dim(ef1)[1]))

ef4<-cbind(ef2,ef0)
#Replace NAs with Zeros
for(i in 32:dim(ef4)[2]){ ef4[,i] <- ifelse(is.na(ef4[,i]),0,ef4[,i])  }

dimnames(ef4)[[2]][32:dim(ef4)[2]]<-paste("LE_EURO_",dimnames(ef4)[[2]][32:dim(ef4)[2]],sep="")

ef4$LE_EFF <- as.numeric(as.character(ef4$LE_EFF))
ef4$VE_LEN <- as.numeric(as.character(ef4$VE_LEN))
ef4$VE_KW  <- as.numeric(as.character(ef4$VE_KW))

#Get original dimensions of ef4

dd <- dim(ef4)

#Partition the effort among the statistical rectangles

ctotals<-apply(ef4[32:dim(ef4)[2]],1,sum.na)

ptab <- tapply(ctotals,list(as.character(ef4$LE_RECT), as.character(ef4$FT_REF)),sum.na);

ptab[is.na(ptab)] <- 0;

ptab <- vectorise(prop.table(ptab,margin=2));
dimnames(ptab)[[2]] <- c("P","LE_RECT","FT_REF")
ptab <- ptab[!is.na(ptab$P),]

ef5<-merge(ef4,ptab,all.x=T)

ef3$LE_EFF <- ef3$LE_EFF * ef5$P

#ef5<- ef5[,1:dd[2]]

#Combine the weights and values into a single dataframe

eflalo2 <- cbind(ef3,ef5[,32:dim(ef4)[2]])

#Put on the metier

#Get the metier data from the database

 if(which.lib=="RODBC"){
  #dasbyreg <- sqlQuery(visstat,query) 
   metiers <- sqlQuery(dBConnect("visstat",which.lib=which.lib),"SELECT * from METIERS WHERE CODE = 'DCF';")

  }
  if(which.lib=="DBI"){
  metiers <-  dbGetQuery(dBConnect("visstat",which.lib=which.lib),"SELECT * from METIERS WHERE CODE = 'DCF';")
   }

#Get rid of the \r
metiers$METIER <- as.character(metiers$METIER)

metiers$METIER <- gsub("\r","",metiers$METIER)

eflalo2$LE_MET_level6 <- as.character(metiers$METIER[match(eflalo2$FT_REF,metiers$TRIP_NUMBER)])

#Put on ICES area 

#lon_lat <- rectangle.lon.lat(eflalo2$LE_RECT,midpoint=T)
#eflalo2$LE_DIV <- ICESarea(long=lon_lat[,2],lat=lon_lat[,1])

departure <- as.POSIXct(paste(eflalo2$FT_DDAT, eflalo2$FT_DTIME, sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M:%S")
arrival   <- as.POSIXct(paste(eflalo2$FT_LDAT, eflalo2$FT_LTIME, sep=" "), tz="GMT", format="%d/%m/%Y  %H:%M:%S") 
 
 mid.time <- rep(NA, length(departure))
 
 for(r in 1:length(departure)){
 mid.time[r] <- as.character(seq(from=departure[r], to=arrival[r], length.out = 3)[2])
              }

y <- substr(mid.time,1,4)
m <- substr(mid.time,6,7)
d <- substr(mid.time,9,10)

eflalo2$LE_CDAT <- paste(d,m,y,sep="/")

countries <- data.frame(old=c('bel','deu','dnk','eng','irl','fra','gbr','nld','scd','swe','nor','fro','ltu'),new=c("BEL","DEU","DNK","GBR","IRL","FRA","GBR","NLD","GBR","SWE","NOR","FRO","LTU"))

eflalo2$VE_COU <- as.character(countries$new[match(eflalo2$VE_COU,countries$old)])

eflalo2

}


#######################Example##################

# Quick way to load a load of r-files in a directory 

# lapply(list.files(),source)
# #eflalo2.10 <- GetDataEflalo(Cstart='01-jan-2010',Cstop='31-aug-2010',flag_nations=c('nld'),which.lib="RODBC")
# eflalo2 <-    GetDataEflalo(Cstart='01-jan-2009',Cstop='15-jan-2009',flag_nations=c('nld'))
  #Just Dutch data
  
#  save(eflalo2,    file="D://bearedo//Projects//VMS-Tools//vmstools2//vmstools//data//eflalo2.rda",compress=T)
#  save(eflalo2,    file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//eflalo2.rda",compress=T) 
# 
 #eflalo2.08 <- GetDataEflalo(Cstart='01-jan-2008',Cstop='31-Dec-2008')
#   save(eflalo2.08, file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//eflalo2.08.rda",compress=T)
#   print(eflalo2.08[1:5,])
## 
# eflalo2.07 <- GetDataEflalo(Cstart='01-jan-2007',Cstop='31-dec-2007')
#   save(eflalo2.07, file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//eflalo2.07.rda",compress=T)
#    print(eflalo2.07[1:5,])
## 
#      eflalo2.06.1 <- GetDataEflalo(Cstart='01-jan-2006',Cstop='30-jun-2006')
#      eflalo2.06.2 <- GetDataEflalo(Cstart='1-jul-2006',Cstop='31-dec-2006')
#      
#      
#      eflalo2.06 <- formatEflalo2(eflalo2.06)
#     eflalo2.06$VE_FLT[eflalo2.06$VE_COU == "NLD"] <- substr(eflalo2.06$LE_MET_level6[eflalo2.06$VE_COU == "NLD"],start=1,stop=7) 
#    save(eflalo2.06, file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//eflalo2.06.rda",compress=T)
#    print(eflalo2.06[1:5,])
#  
#  eflalo2.05 <- GetDataEflalo(Cstart='01-jan-2005',Cstop='31-dec-2005')
#    save(eflalo2.05, file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//eflalo2.05.rda",compress=T)
#
#
## print(dim(eflalo2))  

# Write out the data for the vmstools library 

# Make vessel anon first

#  
   #eflalo2$VE_REF    <- matrix(unlist(strsplit(as.character(eflalo2$VE_REF),":")),ncol=2,byrow=T)[,2]
   #eflalo2.07$VE_REF <- matrix(unlist(strsplit(as.character(eflalo2.07$VE_REF),":")),ncol=2,byrow=T)[,2]
   #eflalo2.08$VE_REF <- matrix(unlist(strsplit(as.character(eflalo2.08$VE_REF),":")),ncol=2,byrow=T)[,2]
   
   #          library(vmstools)
   #eflalo2    <- formatEflalo2(eflalo2)
   #eflalo2.08 <- formatEflalo2(eflalo2.08)
   #eflalo2.07 <- formatEflalo2(eflalo2.07)
   
