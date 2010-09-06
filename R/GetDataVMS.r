

GetDataVMS <- function(Cstart=Cstart, Cstop=Cstop) {

# This function extracts all raw VMS data from VISSTAT by time interval.
# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- dBConnect(which.database="visstat")
# Format the time strings
  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
#Set up the Query
  query <- paste("SELECT * from vms WHERE vms.rgn_local_date between ",Cstart," and ",Cstop,"")
#Attach to DB and get the data out according to query
  vms <- sqlQuery(visstat,query);
  vms

}

#Example: extract all vms data for January 2010

#  vms <- GetDataVMS(Cstart="01-jan-2010",Cstop="31-jan-2010")