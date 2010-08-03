
GetDataPrice <- function(Cstart=tstart, Cstop=tstop, Cmeshmin=min_mesh_size, Cmeshmax=max_mesh_size) {

# This function extracts landings data from VISSTAT by species, time interval and mesh size range.
# If meshes are null or void you get -1 in the output file

# Load required R libraries
  library(RODBC)
  library(reshape)
# Close any old odbc connections
  odbcCloseAll()
# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- odbcConnect(dsn="visstatp", uid="",pwd="")


  Cspec  <-WriteSQLString(Cspec)
  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)

query <-paste("

  SELECT
    trip_number,
       plm_code,
       TO_CHAR(unload_date,'J') AS julian,
       TO_CHAR(unload_date,'WW') AS week,
       TO_CHAR(unload_date,'MM') AS month,
       txn_ices_code,
       weight,
       price_kg,
       weight*price_kg AS value
  FROM size_classes
  WHERE unload_date between ",Cstart," and ",Cstop,"
")

value <-sqlQuery(visstat,query);

}