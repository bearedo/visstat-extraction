#Cstart="01-nov-2010";Cstop="30-jan-2011"


GetDataHALAEggSurvey <- function(Cstart=Cstart,Cstop=Cstop,user=user,passwd=passwd) {

   odbcCloseAll()
   frisbe <- dBConnect(which.database="frisbe",user=user,passwd=passwd)
   
  # Connect to database for which you will need an account and permission from Peter Van der Kamp
  #Cstart="01-nov-2010";Cstop="02-feb-2011";

  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
  
 #qwgmegs <- paste("select st.*, sa.*, ss.*, cl.*, ta.*, vp.*
#  FROM VIS_STATIONS st, VIS_SAMPLES sa, VIS_SUBSAMPLES ss, VIS_CLASSES
#cl, VIS_TAXONS ta, VIS_POSITIONS vp
#  WHERE
#  st.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
#  AND st.PGM_CODE IN 'WGMEGS'
#  AND st.id=sa.stn_ID AND sa.id = ss.SPE_ID  AND ss.ID = cl.SSE_ID AND cl.TXN_NODC_CODE = ta.NODC_CODE
#  AND vp.stn_id = st.id AND vp.seq_no = 0
#  ")
#  
#  wgmegs <- sqlQuery(frisbe,qwgmegs)
  
   #Take out missing values
  
  #wgmegs <- wgmegs [!is.na(wgmegs$QUANTITY.1),] 
 
 
 qhala <- paste("select st.*, sa.*, ss.*, cl.*, ta.*, vp.*
  FROM VIS_STATIONS st, VIS_SAMPLES sa, VIS_SUBSAMPLES ss, VIS_CLASSES
cl, VIS_TAXONS ta, VIS_POSITIONS vp
  WHERE
  st.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
  AND st.PGM_CODE IN 'HALA'
  AND st.id=sa.stn_ID AND sa.id = ss.SPE_ID  AND ss.ID = cl.SSE_ID AND cl.TXN_NODC_CODE = ta.NODC_CODE
  AND vp.stn_id = st.id AND vp.seq_no = 0
  ")
  
  hala <- sqlQuery(frisbe,qhala)
  
 
  # Get VIS_STATIONS and POSITIONS

qstvp <- paste("select VIS_STATIONS.*,VIS_POSITIONS.*, vis_samples.year, vis_ctd_measurements.*,
vis_samples.seq_no sample_id FROM VIS_STATIONS, VIS_POSITIONS, vis_samples,vis_ctd_measurements
  WHERE VIS_STATIONS.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
    AND
  VIS_STATIONS.PGM_CODE IN ('HALA')
   AND VIS_STATIONS.ID = VIS_POSITIONS.STN_ID AND VIS_POSITIONS.seq_no IN (0,999)
   and vis_samples.stn_id = vis_stations.id
   AND vis_ctd_measurements.ptn_id = vis_positions.id")
 
 vis_chrons <- sqlQuery(frisbe,qstvp)  
 
 vis_chrons<- vis_chrons[vis_chrons$LONGITUDE >0,]
 vis_chrons<- vis_chrons[vis_chrons$LATITUDE >0,]




 #check 
 
 plot(vis_chrons$LONGITUDE,vis_chrons$LATITUDE)
 
 #Put on ship IDs
  
  vis_p_p <- sqlQuery(frisbe,"SELECT * from VIS_PLATFORM_PROPERTIES")
  
  vis_chrons$SHIP <- vis_p_p$NAME[match(vis_chrons$PPY_ID,vis_p_p$ID)]
 
 print(paste('number of distinct stations = ',dim(vis_chrons)[[1]]))

 #The chrons are a definitive list of stations 
 
 
 #subfact <- bts$TOTAL_UNITS/bts$USED_UNITS
 #bts$QUANTITY.1  <- subfact*bts$QUANTITY.1 

 out <- list(hala=hala,chrons=vis_chrons)
 
 }
 
 
#Cstart="01-nov-2010";Cstop="02-feb-2011";


#eg <- GetDataHALAEggSurvey(Cstart=Cstart,Cstop=Cstop)

#write.table(eg$chrons,'W:/IMARES/IJmuiden/Afdeling/Projecten/PLACES II - werkgroep onderwatergeluid/4 Rapportage/4-4 Manuscripts/hala_chons.csv',sep=",",row.names=F)
#write.table(eg$hala,'W:/IMARES/IJmuiden/Afdeling/Projecten/PLACES II - werkgroep onderwatergeluid/4 Rapportage/4-4 Manuscripts/hala.csv',sep=",",row.names=F)

