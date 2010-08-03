# REMOVE OLD OBJECTS
#rm(list=(ls()))       


# FOR THIS STEP YOU NEED AN ACCOUNT AND LOGIN NAME (PROVIDED BY PETER VD KAMP)


###############################################################################
##################                SELECTION 1         #########################
###############################################################################

#tstart <- "01-jan-2004"
#tstop  <- "31-mar-2004"
#
#species <- c("PLE")
#
#min_mesh_size <- -1
#max_mesh_size <- 1000
#
###############################################################################
##################                QUERY               #########################
###############################################################################

GetDataLandings <- function(Cspec=species,Cstart=tstart, Cstop=tstop) {
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

    query<-paste("SELECT catches.rgn_trp_arrivel_date arr_date,
       TO_CHAR(catches.rgn_trp_arrivel_date,'Q') quarter,
       TO_CHAR(catches.rgn_trp_arrivel_date,'YYYY') year,
       TO_CHAR(catches.rgn_trp_arrivel_date,'MM') month,
       catches.txn_ices_code species,
       catches.weight,
       registrations.GPY_code gear,
       registrations.MESHSIZE,
       nvl(Quadrant_properties.ICES_AREA,'UNKNOWN') ices_area,
       nvl(Quadrant_properties.ICES_SUBAREA,'UNKNOWN') ices_subarea,
       nvl(Quadrant_properties.ICES_QUADRANT,'UNKNOWN') ices_quadrant,
       catches.rgn_trp_ppy_plm_cny_code country,
       trips.ppy_plm_cny_code country2,
       metiers.metier,
       platform_properties.length vessel_length,
       platform_properties.power     
    FROM registrations
    JOIN catches ON (registrations.sre_code = catches.rgn_sre_code
       and registrations.trp_ppy_plm_cny_code = catches.rgn_trp_ppy_plm_cny_code
       and registrations.trp_prt_code = catches.rgn_trp_prt_code
       and registrations.trp_prt_cny_code = catches.rgn_trp_prt_cny_code
       and registrations.trp_arrivel_date = catches.rgn_trp_arrivel_date
       and registrations.trp_arrivel_time = catches.rgn_trp_arrivel_time
       and registrations.trp_ppy_id = catches.rgn_trp_ppy_id
       and registrations.trp_ppy_plm_code = catches.rgn_trp_ppy_plm_code
       and registrations.rgn_date = catches.rgn_rgn_date )
    LEFT OUTER join Quadrant_properties ON (registrations.QPY_ICES_QUADRANT =
       Quadrant_properties.ICES_QUADRANT)
    INNER JOIN trips ON (trips.arrivel_date = registrations.trp_arrivel_date
             and trips.arrivel_time = registrations.trp_arrivel_time
             and trips.ppy_plm_code = registrations.trp_ppy_plm_code
             and trips.prt_code = registrations.trp_prt_code)
    INNER JOIN metiers ON (trips.trip_number = metiers.trip_number)       
    LEFT OUTER JOIN platform_properties ON (platform_properties.id = trips.ppy_id
    and trips.ARRIVEL_DATE between platform_properties.START_DATE
    and nvl(platform_properties.END_DATE,sysdate))
    WHERE catches.txn_ices_code IN ",Cspec," 
       and registrations.trp_arrivel_date between ",Cstart," and ",Cstop,"
       and nvl(registrations.MESHSIZE,-1) BETWEEN ",Cmeshmin," AND ",Cmeshmax,"
       
       ")
    
  landings <- sqlQuery(visstat, query)
 

}


###############################################################################
##################             Example of DATA EXTRACTION        #########################
###############################################################################


#landings <- GetDataCatches(Cspec=c("PLE"),Cstart="01-jan-2004",
 #          Cstop="31-jan-2008",Cmeshmin=min_mesh_size, Cmeshmax=max_mesh_size)
 

# create length classes from vessel lengths
#Note: we don't want this here. We should have functions that calculate length categories. They are always changing too so we'd need options
#TheData$LENGTHCLASS <- TheData$VESSEL_LENGTH
#TheData$LENGTHCLASS[TheData$VESSEL_LENGTH < 15] <- "<15"
#TheData$LENGTHCLASS[TheData$VESSEL_LENGTH >= 15 & TheData$LENGTH < 30] <- "15-29"
#TheData$LENGTHCLASS[TheData$VESSEL_LENGTH >= 30] <- ">=30"
#TheData$LENGTHCLASS[is.na(TheData$VESSEL_LENGTH)] <- "unknown"
#
# 
# create power classes from vessel power
#Note: we don't want this here. We should have functions that calculate length categories. They are always changing too so we'd need options
#TheData$POWERCLASS <- TheData$POWER
#TheData$POWERCLASS[TheData$POWER < 1000] <- "<1000"
#TheData$POWERCLASS[TheData$POWER >= 1000 & TheData$POWER < 2000] <- "1000-2000"
#TheData$POWERCLASS[TheData$POWER >= 2000] <- ">=2000"
#TheData$POWERCLASS[is.na(TheData$POWER)] <- "unknown"  
#
###############################################################################
##################                SELECTION 2         #########################
###############################################################################

# Select from: "ICES_AREA", "ICES_SUBAREA", "ICES_QUADRANT",
#"YEAR", "QUARTER", "MONTH", "GEAR", "COUNTRY", "SPECIES", "POWERCLASS",
#"LENGTHCLASS" and "METIER", see table1

#selection <- c("ICES_AREA", "ICES_SUBAREA", "YEAR",  "GEAR", "QUARTER", "COUNTRY", "SPECIES", "METIER")
#
#selection_table <- aggregate(TheData$WEIGHT,TheData[,selection],sum)
#names(selection_table)[length(selection)+1] <- "LANDED_WEIGHT"
#                           
#
##############################################################################
###################         Write data to Files         #########################
################################################################################
#
## WARNING!!!, these file overwrite the existing file. 
## To prevent loosing the existing file, change file name
#
#setwd("M:/My Documents/VisstatRaising/")
#
##write.csv(TheData, "VisstatCatches_Total.csv")
#write.csv(selection_table, "VisstatCatches_selection.csv")
#
##show the first 6 rows of boths tables in your R window
#  head(TheData)
#  head(selection_table)
# 

###############################################################################
##################              END                   #########################
############################################################################### 




 