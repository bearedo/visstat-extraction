  
#Cstart="01-jan-2009";Cstop="31-dec-2009"  
  
GetDataDaysAtSeaByRegistration <- function(Cstart=Cstart,Cstop=Cstop) {

# This function extracts effort (days at sea, kw days at sea) data from VISSTAT by trip.
# Some trips have different gears used on the trip which is why sometimes you get the data repeated.
# If meshes are null or void you get -1 in the output file

# Connect to database for which you will need an account and permission from Peter Van der Kamp
  visstat <- dBConnect(which.database="visstat",which.lib=which.lib)

  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)

 
  query <-paste("
SELECT trips.trip_number,trips.prt_code,trips.prt_code_departed_from,trips.prt_cny_code,trips.prt_cny_code_departed_from,trips.arrivel_date,
trips.arrivel_time
,trips.departure_date,trips.departure_time,registrations.GPY_code,registrations.MESHSIZE,registrations.trp_ppy_plm_code
,registrations.sre_code,registrations.TRP_PPY_PLM_CNY_CODE ,platform_properties.length,
platform_properties.power,platform_properties.id as vessel_id2,metiers.metier
,ROUND(to_date(to_char(arrivel_date,'yyyy.mm.dd')||' '||substr(to_char(arrivel_time,'0999'),2,2)||'.'||substr(to_char(arrivel_time,'0999'),4,2),'yyyy.mm.dd hh24.mi') -
to_date(to_char(departure_date,'yyyy.mm.dd')||' '||substr(to_char(departure_time,'0999'),2,2)||'.'||substr(to_char(departure_time,'0999'),4,2),'yyyy.mm.dd hh24.mi'),2) AS das
,arrivel_date - departure_date AS coarse_das
FROM registrations
LEFT OUTER JOIN platform_properties ON (platform_properties.PLM_CODE = registrations.trp_ppy_plm_code
     AND platform_properties.PLM_CNY_CODE = registrations.PPY_PLM_CNY_CODE 
    AND registrations.TRP_ARRIVEL_DATE between platform_properties.START_DATE  AND nvl(platform_properties.END_DATE,sysdate))

INNER JOIN trips ON (trips.arrivel_date = registrations.trp_arrivel_date
     AND trips.arrivel_time = registrations.trp_arrivel_time
     AND trips.ppy_plm_code = registrations.trp_ppy_plm_code
     AND trips.prt_code = registrations.trp_prt_code)
    INNER JOIN metiers ON (trips.trip_number = metiers.trip_number)
    WHERE  registrations.trp_arrivel_date between ",Cstart," and ",Cstop,"")

#and registrations.TRP_PPY_PLM_CNY_CODE IN ('nld')

#AND catches.RGN_TRP_PPY_PLM_CNY_CODE IN ('nld')
  if(which.lib=="RODBC"){
  dasbyreg <- sqlQuery(visstat,query) 
  }
  if(which.lib=="DBI"){
  dasbyreg <- dbGetQuery(visstat,query)
  }

dasbyreg$COARSE_DAS <- ifelse(dasbyreg$COARSE_DAS==0,1,dasbyreg$COARSE_DAS)

# attach level 5 metier variables ??!!
dasbyreg$LEVEL5 <- substr(dasbyreg$METIER,start=1,stop=7);

#Deal with missing lengths

#First make everything that is zero or missing equal to NA

dasbyreg$LENGTH[!is.na(dasbyreg$LENGTH) & dasbyreg$LENGTH == 0] <- NA
dasbyreg$POWER[!is.na(dasbyreg$POWER) & dasbyreg$POWER == 0] <- NA

#Replace length categories using parameters from linear models between length and engine power categories

ll <- log(dasbyreg$LENGTH)
lp <- log(dasbyreg$POWER)
m1 <- lm(ll ~ lp,na.action='na.omit')

dasbyreg$LENGTH[is.na(dasbyreg$LENGTH)] <- exp(coef(m1)[1]+coef(m1)[2]*log(dasbyreg$POWER[is.na(dasbyreg$LENGTH) ]))

ll <- log(dasbyreg$LENGTH)
lp <- log(dasbyreg$POWER)
m2 <- lm(lp ~ ll,na.action='na.omit')

dasbyreg$POWER[is.na(dasbyreg$POWER)] <- exp(coef(m2)[1]+coef(m2)[2]*log(dasbyreg$POWER[is.na(dasbyreg$POWER) ]))

# If still not known then leave them unknown at this stage

# Calculate kwdays

dasbyreg$KWDAS <- dasbyreg$POWER*dasbyreg$DAS;

#Replace 2 letter country codes (make this into a function)

countries <- data.frame(old=c('be','de','dk','eng','fr','gb','nl','nld','scd'),new=c("BEL","DEU","DNK","GBR","FRA","GBR","NLD","NLD","GBR"))

dasbyreg$PRT_CNY_CODE <- countries$new[match(dasbyreg$PRT_CNY_CODE,countries$old)]
dasbyreg$PRT_CNY_CODE_DEPARTED_FROM <- countries$new[match(dasbyreg$PRT_CNY_CODE_DEPARTED_FROM,countries$old)]

dasbyreg

}



###############################################################################
##################             Example of DATA EXTRACTION        #########################
###############################################################################


#dasbyreg <- GetDataDaysAtSeaByRegistration(Cstart="01-jan-2009",Cstop="31-jan-2009")
#
##Add on approximate lats and longs
#
#dasbyreg$lat <-convert.statsq.lat.lon(dasbyreg$QUADRANT)$lat
#
#dasbyreg <- GetMissingMeshSize(data=dasbyreg,latitude.name="lat")
#table(dasbyreg$MESHSIZE)
#
#
#library(visstatExtraction)
#dasbyreg <- DCFGearCodes(dasbyreg);
#dasbyreg <- PassiveOrStatic(dasbyreg)
#dasbyreg <- DCFMeshCategory(dasbyreg)
#dasbyreg <- CodRegsGearCodes(dasbyreg)


#u <- unique(paste(dasbyreg$TRIP_NUMBER,dasbyreg$REG_GEAR,dasbyreg$DAS,dasbyreg$COARSE_DAS,dasbyreg$KWDAS,sep="|"))
#xx <- data.frame(matrix(unlist(strsplit(u,"\\|")),ncol=5,byrow=T))
#dimnames(xx)[[2]] <- c("TRIP_NUMBER","REG_GEAR","DAS","COARSE_DAS","KWDAS")
#for(i in 3:5){xx[,i] <- as.numeric(as.character(xx[,i])) }
#tapply(xx$COARSE_DAS,list(xx$REG_GEAR),sum.na)
#tapply(xx$DAS,list(xx$REG_GEAR),sum.na)
#tapply(xx$KWDAS,list(xx$REG_GEAR),sum.na)