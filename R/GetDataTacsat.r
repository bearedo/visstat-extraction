
#Cstart <- '01-jan-2009'
#Cstop  <- '31-jan-2009'

GetDataTacsat <- function(Cstart=Cstart,Cstop=Cstop)

{

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

tacsat <- data.frame(VE_REF=paste(vms$TRP_PPY_PLM_CODE,vms$VESSEL_ID1,sep=":"),SI_LATI=vms$LATITUDE,SI_LONG=vms$LONGITUDE,
SI_DATE=vms$RGN_UTC_DATE,SI_TIME=vms$RGN_UTC_TIME,SI_SP = vms$SPEED,SI_HE=vms$HEADING)

tacsat

}

############Example############


#  tacsat <- GetDataTacsat(Cstart='01-jan-2009',Cstop='31-jan-2009')

#   tacsat$VE_REF <- matrix(unlist(strsplit(as.character(tacsat$VE_REF),":")),ncol=2,byrow=T)[,2]

#  write.table (tacsat, file='tacsat.csv',sep=",",row.names=F)