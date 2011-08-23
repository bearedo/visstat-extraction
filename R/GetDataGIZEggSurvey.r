Cstart="01-jan-1900";Cstop="31-dec-2011"


GetDataGIZEggSurvey <- function(Cstart=Cstart,Cstop=Cstop) {

   odbcCloseAll()
   frisbe <- dBConnect(which.database="frisbe")
   
  # Connect to database for which you will need an account and permission from Peter Van der Kamp
  
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
 
 
 qgiz <- paste("select st.*, sa.*, ss.*, cl.*, ta.*, vp.*
  FROM VIS_STATIONS st, VIS_SAMPLES sa, VIS_SUBSAMPLES ss, VIS_CLASSES
cl, VIS_TAXONS ta, VIS_POSITIONS vp
  WHERE
  st.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
  AND st.PGM_CODE = 'GIZ'
  AND st.id=sa.stn_ID AND sa.id = ss.SPE_ID  AND ss.ID = cl.SSE_ID AND cl.TXN_NODC_CODE = ta.NODC_CODE
  AND vp.stn_id = st.id AND vp.seq_no = 0
  ")
  
  giz <- sqlQuery(frisbe,qgiz)
  
   #Take out missing values
  
  #giz <- giz [!is.na(giz$QUANTITY.1),] 
 
 
 
  # Get VIS_STATIONS and POSITIONS

qstvp <- paste("select VIS_STATIONS.*,VIS_POSITIONS.* FROM VIS_STATIONS, VIS_POSITIONS
 WHERE VIS_STATIONS.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
   AND
 VIS_STATIONS.PGM_CODE IN ('GIZ')
  AND VIS_STATIONS.ID = VIS_POSITIONS.STN_ID AND VIS_POSITIONS.seq_no = 0
 ")
 
 vis_chrons <- sqlQuery(frisbe,qstvp)  
 
 #check 
 
 plot(vis_chrons$LONGITUDE,vis_chrons$LATITUDE)
 
 #Put on ship IDs
  
  vis_p_p <- sqlQuery(frisbe,"SELECT * from VIS_PLATFORM_PROPERTIES")
  
  vis_chrons$SHIP <- vis_p_p$NAME[match(vis_chrons$PPY_ID,vis_p_p$ID)]
 
 print(paste('number of distinct stations = ',dim(vis_chrons)[[1]]))

 #The chrons are a definitive list of stations 
 
 
 #subfact <- bts$TOTAL_UNITS/bts$USED_UNITS
 #bts$QUANTITY.1  <- subfact*bts$QUANTITY.1 

 out <- list(giz=giz,chrons=vis_chrons)
 
 }
 
 
#Cstart="01-jan-1900";Cstop="31-dec-2011"


#eg <- GetDataGIZEggSurvey(Cstart=Cstart,Cstop=Cstop)

#write.table(eg$chrons,'W:/IMARES/IJmuiden/Afdeling/Projecten/PLACES II - werkgroep onderwatergeluid/4 Rapportage/4-4 Manuscripts/giz_chons.csv',sep=",",row.names=F)

#write.table(eg$giz,'W:/IMARES/IJmuiden/Afdeling/Projecten/PLACES II - werkgroep onderwatergeluid/4 Rapportage/4-4 Manuscripts/giz.csv',sep=",",row.names=F)

