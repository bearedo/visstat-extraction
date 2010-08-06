
#Cstart="01-jan-2009";Cstop="31-jan-2009"  


GetDataEflalo <- function(Cstart=tstart,Cstop=tstop,Cmeshmin=Cmeshmin,Cmeshmax=Cmeshmax) 

{

# Get data 

valuebytrip <- GetDataLandingValueByTrip(Cstart=Cstart, Cstop=Cstop)

#Reformat time strings

valuebytrip$ARRIVEL_TIME <- ReformatTime(valuebytrip$ARRIVEL_TIME)
valuebytrip$DEPARTURE_TIME <- ReformatTime(valuebytrip$DEPARTURE_TIME)

#Reformat date strings

valuebytrip$ARRIVEL_DATE <- ReformatDate( valuebytrip$ARRIVEL_DATE)
valuebytrip$DEPARTURE_DATE <- ReformatDate( valuebytrip$DEPARTURE_DATE)


# Transform the data frame into the crappy 'matrix' format required by eflalo.

mm<-paste(valuebytrip$TRIP_NUMBER,paste(valuebytrip$TRP_PPY_PLM_CODE,valuebytrip$VESSEL_ID1,sep=":"),
valuebytrip$LEVEL5,valuebytrip$RGN_TRP_PPY_PLM_CNY_CODE,valuebytrip$PRT_CNY_CODE,
valuebytrip$PRT_CNY_CODE_DEPARTED_FROM,
valuebytrip$GPY_CODE,valuebytrip$MESHSIZE,valuebytrip$QUADRANT,
valuebytrip$PRT_CODE_DEPARTED_FROM,valuebytrip$PRT_CODE,valuebytrip$DEPARTURE_DATE,valuebytrip$DEPARTURE_TIME,
valuebytrip$ARRIVEL_DATE,valuebytrip$ARRIVEL_TIME,valuebytrip$POWER,valuebytrip$LENGTH,valuebytrip$KWDAS,sep=",")

col.labels<-c("TRIP_NUMBER","TRP_PPY_PLM_CODE","LEVEL5","RGN_TRP_PPY_PLM_CNY_CODE","PRT_CNY_CODE",
"PRT_CNY_CODE_DEPARTED_FROM",
"GPY_CODE","MESHSIZE","QUADRANT",
"PRT_CODE_DEPARTED_FROM","PRT_CODE","DEPARTURE_DATE","DEPARTURE_TIME",
"ARRIVEL_DATE","ARRIVEL_TIME","POWER","LENGTH","KWDAS")

sum.na <- function(x) { x<-sum(x[!is.na(x)]);x}

####################################
# First by weight of landings 
####################################

ef0<-tapply(valuebytrip$WEIGHT,list(mm,valuebytrip$TXN_ICES_CODE),sum.na)
mm1<-dimnames(ef0)[[1]]

 #Get the number of variables selected
ll<-length(unlist(strsplit(mm1[1],",")))


ef1<-matrix(unlist(strsplit(mm1,",")),ncol=ll,byrow=T)
dimnames(ef1)[[1]]<-1:dim(ef1)[1]
dimnames(ef1)[[2]][1:ll]<-col.labels


ef2 <- data.frame(VE_REF=ef1[,"TRP_PPY_PLM_CODE"],VE_FLT=ef1[,"LEVEL5"],VE_COU = ef1[,"RGN_TRP_PPY_PLM_CNY_CODE"], VE_LEN = ef1[,"LENGTH"], 
VE_KW= ef1[,"POWER"],VE_TON=rep(NA,dim(ef1)[1]),
FT_REF=ef1[,"TRIP_NUMBER"],FT_DCOU=ef1[,"PRT_CNY_CODE_DEPARTED_FROM"],FT_DHAR=ef1[,"PRT_CODE_DEPARTED_FROM"],
FT_DDAT=ef1[,"DEPARTURE_DATE"],FT_DTIME=ef1[,"DEPARTURE_TIME"],FT_LCOU = ef1[,"PRT_CNY_CODE"],
FT_LHAR = ef1[,"PRT_CODE"], FT_LDAT = ef1[,"ARRIVEL_DATE"], FT_LTIME=ef1[,"ARRIVEL_TIME"],LE_CDAT=rep(NA,dim(ef1)[1]),LE_STIME=rep(NA,dim(ef1)[1]), 
LE_ETIME =rep(NA,dim(ef1)[1]),
LE_SEQNUM=rep(NA,dim(ef1)[1]),LE_GEAR =  ef1[,"GPY_CODE"], LE_MSZ = ef1[,"MESHSIZE"], LE_RECT = ef1[,"QUADRANT"], LE_MET_level6=rep(NA,dim(ef1)[1]),
LE_UNIT=rep("KWDAYS",dim(ef1)[1]),
LE_EFF=ef1[,"KWDAS"],LE_EFF_VMS=rep(NA,dim(ef1)[1]))

ef3<-cbind(ef2,ef0)
#Replace NAs with Zeros
for(i in 27:dim(ef3)[2]){ ef3[,i] <- ifelse(is.na(ef3[,i]),0,ef3[,i])  }

dimnames(ef3)[[2]][27:dim(ef3)[2]]<-paste("LE_KG_",dimnames(ef3)[[2]][27:dim(ef3)[2]],sep="")

ef3$LE_EFF <- as.numeric(as.vector(ef3$LE_EFF))
ef3$VE_LEN <- as.numeric(as.vector(ef3$VE_LEN))
ef3$VE_KW  <- as.numeric(as.vector(ef3$VE_KW))


############################
# Now by value of landings##
############################

ef0<-tapply(valuebytrip$VALUE,list(mm,valuebytrip$TXN_ICES_CODE),sum.na)
mm1<-dimnames(ef0)[[1]]

 #Get the number of variables selected
ll<-length(unlist(strsplit(mm1[1],",")))


ef1<-matrix(unlist(strsplit(mm1,",")),ncol=ll,byrow=T)
dimnames(ef1)[[1]]<-1:dim(ef1)[1]
dimnames(ef1)[[2]][1:ll]<-col.labels


ef2 <- data.frame(VE_REF=ef1[,"TRP_PPY_PLM_CODE"],VE_FLT=ef1[,"LEVEL5"],VE_COU = ef1[,"RGN_TRP_PPY_PLM_CNY_CODE"], VE_LEN = ef1[,"LENGTH"], 
VE_KW= ef1[,"POWER"],VE_TON=rep(NA,dim(ef1)[1]),
FT_REF=ef1[,"TRIP_NUMBER"],FT_DCOU=ef1[,"PRT_CNY_CODE_DEPARTED_FROM"],FT_DHAR=ef1[,"PRT_CODE_DEPARTED_FROM"],
FT_DDAT=ef1[,"DEPARTURE_DATE"],FT_DTIME=ef1[,"DEPARTURE_TIME"],FT_LCOU = ef1[,"PRT_CNY_CODE"],
FT_LHAR = ef1[,"PRT_CODE"], FT_LDAT = ef1[,"ARRIVEL_DATE"], FT_LTIME=ef1[,"ARRIVEL_TIME"],LE_CDAT=rep(NA,dim(ef1)[1]),LE_STIME=rep(NA,dim(ef1)[1]), 
LE_ETIME =rep(NA,dim(ef1)[1]),
LE_SEQNUM=rep(NA,dim(ef1)[1]),LE_GEAR =  ef1[,"GPY_CODE"], LE_MSZ = ef1[,"MESHSIZE"], LE_RECT = ef1[,"QUADRANT"], LE_MET_level6=rep(NA,dim(ef1)[1]),
LE_UNIT=rep("KWDAYS",dim(ef1)[1]),
LE_EFF=ef1[,"KWDAS"],LE_EFF_VMS=rep(NA,dim(ef1)[1]))

ef4<-cbind(ef2,ef0)
#Replace NAs with Zeros
for(i in 27:dim(ef4)[2]){ ef4[,i] <- ifelse(is.na(ef4[,i]),0,ef4[,i])  }

dimnames(ef4)[[2]][27:dim(ef4)[2]]<-paste("LE_EURO_",dimnames(ef4)[[2]][27:dim(ef4)[2]],sep="")

ef4$LE_EFF <- as.numeric(as.vector(ef4$LE_EFF))
ef4$VE_LEN <- as.numeric(as.vector(ef4$VE_LEN))
ef4$VE_KW  <- as.numeric(as.vector(ef4$VE_KW))

#Combine the weights and values into a single dataframe

eflalo2 <- cbind(ef3,ef4[,27:dim(ef4)[2]])

eflalo2

}


#######################Example##################

##  source("GetDataDaysAtSeaByTrip.r")
##  source("GetDataLandingValueByTrip.r")
##  source("GetDataEflalo.r")
##  source("GetDataPrice.r")

## eflalo2 <- GetDataEflalo(Cstart='01-jan-2009',Cstop='31-jan-2009')  

## Write out the data 

## Make vessel anon first

##  eflalo2$VE_REF <- matrix(unlist(strsplit(as.character(eflalo2$VE_REF),":")),ncol=2,byrow=T)[,2]

##  write.table (eflalo2, file='eflalo2.csv',sep=",",row.names=F)