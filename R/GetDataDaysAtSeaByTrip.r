  
#Cstart="01-jan-2010";Cstop="31-jan-2010"  
  
GetDataDaysAtSeaByTrip <- function(Cstart=Cstart,Cstop=Cstop) {

# This function extracts landings and effort (days at sea, kw days at sea) data from VISSTAT by species, time interval and mesh size range.
# If meshes are null or void you get -1 in the output file

# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- dBConnect(which.database="visstat")

  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)

  # Right let's hit the database
  
  query <-paste("
SELECT
    trips.trip_number
,   trips.prt_code
,   trips.prt_code_departed_from 
,   trips.prt_cny_code
,   trips.prt_cny_code_departed_from
,   trips.arrivel_date
,   trips.arrivel_time
,   trips.departure_date
,   trips.departure_time
,   TO_CHAR(catches.rgn_trp_arrivel_date,'J') AS julianday
,   TO_CHAR(catches.rgn_trp_arrivel_date,'WW') AS week
,   TO_CHAR(catches.rgn_trp_arrivel_date,'MM') AS month
,   catches.txn_ices_code
,   catches.weight
,   catches.rgn_trp_ppy_plm_cny_code
,   registrations.GPY_code
,   registrations.MESHSIZE
,   registrations.trp_ppy_plm_code
,   registrations.trp_ppy_id as vessel_id1
,   nvl(Quadrant_properties.ICES_QUADRANT,'UNKNOWN') AS quadrant
,   nvl(Quadrant_properties.ICES_AREA,'UNKNOWN') AS ices_area
,   nvl(Quadrant_properties.ICES_SUBAREA,'UNKNOWN') AS ices_subarea
,   platform_properties.length
,   platform_properties.power
,   platform_properties.id as vessel_id2
,   metiers.metier
,   ROUND(to_date(to_char(arrivel_date,'yyyy.mm.dd')||' '||substr(to_char(arrivel_time,'0999'),2,2)||'.'||substr(to_char(arrivel_time,'0999'),4,2),'yyyy.mm.dd hh24.mi') -
to_date(to_char(departure_date,'yyyy.mm.dd')||' '||substr(to_char(departure_time,'0999'),2,2)||'.'||substr(to_char(departure_time,'0999'),4,2),'yyyy.mm.dd hh24.mi'),2) AS das
FROM registrations
    LEFT OUTER JOIN platform_properties ON (platform_properties.id = registrations.trp_ppy_id
                                           and registrations.TRP_ARRIVEL_DATE between platform_properties.START_DATE and nvl(platform_properties.END_DATE,sysdate))
    INNER JOIN catches ON (registrations.sre_code  = catches.rgn_sre_code
             and registrations.trp_ppy_plm_cny_code = catches.rgn_trp_ppy_plm_cny_code
             and registrations.trp_prt_code = catches.rgn_trp_prt_code
             and registrations.trp_prt_cny_code = catches.rgn_trp_prt_cny_code
             and registrations.trp_arrivel_date = catches.rgn_trp_arrivel_date
             and registrations.trp_arrivel_time = catches.rgn_trp_arrivel_time
             and registrations.trp_ppy_id = catches.rgn_trp_ppy_id
             and registrations.trp_ppy_plm_code = catches.rgn_trp_ppy_plm_code
             and registrations.rgn_date = catches.rgn_rgn_date )
    INNER JOIN trips ON (trips.arrivel_date = registrations.trp_arrivel_date
             and trips.arrivel_time = registrations.trp_arrivel_time
             and trips.ppy_plm_code = registrations.trp_ppy_plm_code
             and trips.prt_code = registrations.trp_prt_code)
    INNER JOIN metiers ON (trips.trip_number = metiers.trip_number)
    LEFT OUTER join Quadrant_properties ON (registrations.QPY_ICES_QUADRANT = Quadrant_properties.ICES_QUADRANT)
WHERE  catches.rgn_trp_arrivel_date between ",Cstart," and ",Cstop,"
       and catches.RGN_TRP_PPY_PLM_CNY_CODE IN ('nld')
")

dasbytrip <-sqlQuery(visstat,query);

# attach level 5 metier variables ??!!
dasbytrip$LEVEL5 <- substr(dasbytrip$METIER,start=1,stop=7);

#sum(as.numeric(is.na(dasbytrip$POWER)))
#dasbytrip$LENGTH[is.na(dasbytrip$POWER)]

#Replace missing power values with median. Better possibilities for future? 1. use EU Fleet Register 2. ).

dasbytrip$POWER[is.na(dasbytrip$POWER)] <- median(na.omit(dasbytrip$POWER));


dasbytrip$KWDAS <- dasbytrip$POWER*dasbytrip$DAS;

#Replace 2 letter country codes (make this into a function)

countries <- data.frame(old=c('be','de','dk','eng','fr','gb','nl','nld','scd'),new=c("BEL","DEU","DNK","GBR","FRA","GBR","NLD","NLD","GBR"))

dasbytrip$PRT_CNY_CODE <- countries$new[match(dasbytrip$PRT_CNY_CODE,countries$old)]
dasbytrip$PRT_CNY_CODE_DEPARTED_FROM <- countries$new[match(dasbytrip$PRT_CNY_CODE_DEPARTED_FROM,countries$old)]
dasbytrip$RGN_TRP_PPY_PLM_CNY_CODE <- countries$new[match(dasbytrip$RGN_TRP_PPY_PLM_CNY_CODE,countries$old)]
dasbytrip

}



###############################################################################
##################             Example of DATA EXTRACTION        #########################
###############################################################################


#dasbytrip <- GetDataDaysAtSeaByTrip(Cstart="01-jan-2009",Cstop="31-jan-2009")
