

GetDataVMS <- function(Cstart=tstart, Cstop=tstop) {

# This function extracts all raw VMS data from VISSTAT by time interval.

# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- dBConnect(which.database="visstat")

  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
  
  query <- paste("
SELECT
    trips.trip_number
,   vms.tripnumber
,   TO_CHAR(vms.rgn_local_date,'MM')  AS month
,   vms.rgn_local_time
,   registrations.GPY_code
,   registrations.MESHSIZE
,   registrations.trp_ppy_plm_code
,   nvl(Quadrant_properties.ICES_QUADRANT,'UNKNOWN') AS quadrant
,   nvl(Quadrant_properties.ICES_AREA,'UNKNOWN') AS ices_area
,   nvl(Quadrant_properties.ICES_SUBAREA,'UNKNOWN') AS ices_subarea
,   platform_properties.length
,   platform_properties.power
,   platform_properties.id as vessel_id
,   ROUND(to_date(to_char(arrivel_date,'yyyy.mm.dd')||' '||substr(to_char(arrivel_time,'0999'),2,2)||'.'||substr(to_char(arrivel_time,'0999'),4,2),'yyyy.mm.dd hh24.mi') -
to_date(to_char(departure_date,'yyyy.mm.dd')||' '||substr(to_char(departure_time,'0999'),2,2)||'.'||substr(to_char(departure_time,'0999'),4,2),'yyyy.mm.dd hh24.mi'),2) AS das
,   vms.latitude
,   vms.longitude
,   vms.rgn_utc_date_time_sec
,   vms.rgn_utc_date
,   vms.rgn_utc_time
,   vms.rgn_local_date
,   vms.utc_year
,   vms.heading
,   vms.speed
,   vms.permission
,   vms.QPY_ICES_AREA
,   vms.unique_flag
FROM registrations
    LEFT OUTER JOIN platform_properties ON (platform_properties.id = registrations.trp_ppy_id
                                           and registrations.TRP_ARRIVEL_DATE between platform_properties.START_DATE and nvl(platform_properties.END_DATE,sysdate))
    INNER JOIN trips ON (trips.arrivel_date = registrations.trp_arrivel_date
             and trips.arrivel_time = registrations.trp_arrivel_time
             and trips.ppy_plm_code = registrations.trp_ppy_plm_code
             and trips.prt_code = registrations.trp_prt_code)
    LEFT OUTER join Quadrant_properties ON (registrations.QPY_ICES_QUADRANT = Quadrant_properties.ICES_QUADRANT)
    LEFT OUTER join vms ON (vms.tripnumber = trips.trip_number)
    WHERE vms.rgn_local_date between ",Cstart," and ",Cstop,"")


vms <- sqlQuery(visstat,query);

vms

}

#Example: extract all vms data for January 2010

#  vms <- GetDataVMS(Cstart="01-jan-2010",Cstop="10-jan-2010")