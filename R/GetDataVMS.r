
#Cstart="01-jan-2010";Cstop="15-jan-2010"  
#flag_nations <- c('bel','deu','dnk','eng','fra','fro','gbr','irl','ltu','nld','nor','sco') 

GetDataVMS <- function(Cstart=Cstart, Cstop=Cstop, flag_nations=flag_nations,which.lib=which.lib) 
{

# This function extracts all raw VMS data from VISSTAT by time interval.
# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- dBConnect(which.lib=which.lib,which.database="visstat")
# Format the time strings
  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
#Set up the Query
  
   query <- paste("SELECT vms.* FROM vms WHERE vms.rgn_local_date BETWEEN ",Cstart," and ",Cstop,"")
  
 
  #WHERE vms.RGN_LOCAL_DATE between platform_properties.START_DATE AND nvl(platform_properties.END_DATE,sysdate))

#Attach to DB and get the data out according to query
  if(which.lib=="RODBC"){
  vms <- sqlQuery(visstat,query) 
  }
  if(which.lib=="DBI"){
  vms <- dbGetQuery(visstat,query)
  }
  
  
  vms <- vms[vms$PPY_PLM_CNY_CODE %in% flag_nations,]
  
 
  vms
  }

#Example: extract all vms data for January 2010

#  vms <- GetDataVMS(Cstart="01-jan-2008",Cstop="10-jan-2008",flag_nations=c("nld"),which.lib="DBI")

# vms <- GetDataVMS(Cstart="01-jan-2008",Cstop="10-jan-2008",flag_nations=c("nld"),which.lib="RODBC")