
#Cstart <- '01-jan-2009'
#Cstop  <- '31-jan-2009'

GetDataTacsat <- function(Cstart=Cstart,Cstop=Cstop)

{
# Connect to database for which you will need an account and permission from Peter Van der Kamp
  
print(paste("Start at", Cstart, "and end at", Cstop))

vms <- GetDataVMS(Cstart=Cstart,Cstop=Cstop)
print('Got VMS data')
# Extract the columns

#Reformat time

vms$RGN_UTC_TIME <- ReformatTime(vms$RGN_UTC_TIME)
vms$RGN_LOCAL_TIME <- ReformatTime(vms$RGN_LOCAL_TIME)

#Tacsat wants a '/' in the data rather than a '-'
#So reformat date strings

vms$RGN_UTC_DATE <- ReformatDate(vms$RGN_UTC_DATE)


#Read in platform properties to get a more anonymous ID for the TACSAT table

tacsat <- data.frame(VE_REF=paste(vms$PPY_PLM_CODE,vms$PPY_ID,sep=":"),SI_LATI=vms$LATITUDE,SI_LONG=vms$LONGITUDE,
SI_DATE=vms$RGN_UTC_DATE,SI_TIME=vms$RGN_UTC_TIME,SI_SP = vms$SPEED,SI_HE=vms$HEADING)

print(head(tacsat))

tacsat

}

############Example############


#  tacsat <- GetDataTacsat(Cstart='01-jan-2006',Cstop='31-Dec-2006')

#Create a vector of dates to loop over

#syear <- 2009
#eyear <- 2010
#yrs<-paste(rep(syear:eyear,rep(12,length(syear:eyear))))
#mths<-rep(month.abb,length(syear:eyear))
#dys1 <- 1
#dys2 <- c(31,28,31,30,31,30,31,31,30,31,30,31)
#
#dats <- data.frame(start.date=paste(dys1,mths,yrs,sep="-"), end.date= paste(dys2,mths,yrs,sep="-"))
#
#setwd("D://bearedo//Projects//visstat-raising//")
#
#for (i in 1:12) {
#
#tacsat2 <- GetDataTacsat(Cstart=dats$start.date[i],Cstop=dats$end.date[i])
#
# 
##tacsat$VE_REF <- matrix(unlist(strsplit(as.character(tacsat$VE_REF),":")),ncol=2,byrow=T)[,2]
#tacsat <- formatTacsat(tacsat)
##
#if (i == 1){
#
#  write.table (tacsat2, file = 'tacsat.csv',col.names=TRUE, sep=",",row.names=F) 
#  
#  }
#  
#else {
#
#  write.table (tacsat2, file = 'tacsat.csv', col.names=FALSE, sep=",", row.names =F,append=T)}
#
#}  
#  
#
#
#tacsat <- read.table('tacsat.csv',sep=",",header=T)
#  
#
#tacsat$VE_REF <- matrix(unlist(strsplit(as.character(tacsat$VE_REF),":")),ncol=2,byrow=T)[,1]
#
#tacsat06 <- tacsat
#save(tacsat06,file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//tacsat06.rda",compress=T)

#save(tacsat,file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//tacsat07.rda",compress=T)
#
#save(tacsat,file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//tacsat08.rda",compress=T)
#
#save(tacsat,file="D://bearedo//Projects//visstat-raising//visstat-extraction//data//tacsat.rda",compress=T)
#
#save(tacsat,file="D://bearedo//Projects//VMS-Tools//vmstools2//vmstools//data//tacsat.rda",compress=T)
#
#

tacsat$VE_COU <- eflalo2.06$VE_COU[match(eflalo2.06$VE_REF,tacsat$VE_REF)]