
# Cstart="01-jan-2009";Cstop="31-jan-2009"  

GetDataVMS <- function(Cstart=Cstart, Cstop=Cstop) {

# This function extracts all raw VMS data from VISSTAT by time interval.
# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- dBConnect(which.database="visstat")
# Format the time strings
  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
#Set up the Query
  
   query <- paste("SELECT vms.* FROM vms                   
  WHERE vms.rgn_local_date between ",Cstart," and ",Cstop,"")
  
 
  #WHERE vms.RGN_LOCAL_DATE between platform_properties.START_DATE AND nvl(platform_properties.END_DATE,sysdate))

#Attach to DB and get the data out according to query
  vms <- sqlQuery(visstat,query);
  
#Get date-time string

vms$ntim <- ReformatTime(vms$RGN_LOCAL_TIME)
vms$date <- as.POSIXct(paste(vms$RGN_LOCAL_DATE,vms$ntim),tz="CET")

#Put on the platform id 

#Make sure vms data are ordered in time
oo <- order(vms$date)
vms <- vms[oo,]

  vms
  }

#Example: extract all vms data for January 2010

#  vms <- GetDataVMS(Cstart="01-jan-2010",Cstop="31-jan-2010")