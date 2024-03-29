


GetDataLandings <- function(Cspec=species,Cstart=Cstart, Cstop=Cstop, Cmeshmin=min_mesh_size, Cmeshmax=max_mesh_size,which.lib='RODBC',user=user,passwd=passwd) {

# This function extracts landings data from VISSTAT by species and time interval. 
# If meshes are null or void you get -1 in the output file

# Load required R libraries 

  #library(reshape)

# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- dBConnect(which.database="visstat",user=user,passwd=passwd)


  Cspec  <-WriteSQLString(Cspec)
  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)

    query<-paste("SELECT catches.rgn_trp_arrival_date arr_date,
       TO_CHAR(catches.rgn_trp_arrival_date,'Q') quarter,
       TO_CHAR(catches.rgn_trp_arrival_date,'YYYY') year,
       TO_CHAR(catches.rgn_trp_arrival_date,'MM') month,
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
       and registrations.trp_arrival_date = catches.rgn_trp_arrival_date
       and registrations.trp_arrival_time = catches.rgn_trp_arrival_time
       and registrations.trp_ppy_id = catches.rgn_trp_ppy_id
       and registrations.trp_ppy_plm_code = catches.rgn_trp_ppy_plm_code
       and registrations.rgn_date = catches.rgn_rgn_date )
    LEFT OUTER join Quadrant_properties ON (registrations.QPY_ICES_QUADRANT =
       Quadrant_properties.ICES_QUADRANT)
    INNER JOIN trips ON (trips.arrival_date = registrations.trp_arrival_date
             and trips.arrival_time = registrations.trp_arrival_time
             and trips.ppy_plm_code = registrations.trp_ppy_plm_code
             and trips.prt_code = registrations.trp_prt_code)
    INNER JOIN metiers ON (trips.trip_number = metiers.trip_number)       
    LEFT OUTER JOIN platform_properties ON (platform_properties.PLM_CODE = trips.PPY_PLM_CODE
    AND platform_properties.PLM_CNY_CODE = trips.PPY_PLM_CNY_CODE
    

AND trips.ARRIVAL_DATE between platform_properties.START_DATE
    and nvl(platform_properties.END_DATE,sysdate))
    WHERE catches.txn_ices_code IN ",Cspec," 
       and registrations.trp_arrival_date between ",Cstart," and ",Cstop,"
       and nvl(registrations.MESHSIZE,-1) BETWEEN ",Cmeshmin," AND ",Cmeshmax,"
       
       ")
    
  landings <- sqlQuery(visstat, query)
 

}


###############################################################################
##################             Example of DATA EXTRACTION        #########################
###############################################################################


#landings <- GetDataLandings(Cspec=c("PLE","SOL"),Cstart="01-jan-2008",Cstop="31-jan-2008",Cmeshmin= -1, Cmeshmax= 200)
 


###############################################################################
##################              END                   #########################
############################################################################### 




 