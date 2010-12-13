
GetDataPrice <- function(Cstart=Cstart, Cstop=Cstop, which.lib=which.lib) {

# This function extracts landings data from VISSTAT by species, time interval and mesh size range.
# If meshes are null or void you get -1 in the output file

# Load required R libraries
  library(reshape)

# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- dBConnect(which.lib=which.lib, which.database="visstat")



  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)

query <-paste("

  SELECT
    trip_number,
       plm_code,
       sales_date,
       auction,
       prt_code,
       TO_CHAR(unload_date,'J') AS julian,
       TO_CHAR(unload_date,'WW') AS week,
       TO_CHAR(unload_date,'MM') AS month,
       txn_ices_code,
       presentation,
       size_class,
       weight,
       price_kg,
       valuta,
       plm_cny_code,
       weight*price_kg AS value
  FROM size_classes
  WHERE unload_date between ",Cstart," and ",Cstop,"
")


  if(which.lib=="RODBC"){
  value <- sqlQuery(visstat,query) 
  }
  if(which.lib=="DBI"){
  value <- dbGetQuery(visstat,query)
  }




}

###############################################################################
##################             Example of DATA EXTRACTION        #########################
###############################################################################


#value <- GetDataPrice(Cstart="01-jan-05",Cstop="31-jan-05")

