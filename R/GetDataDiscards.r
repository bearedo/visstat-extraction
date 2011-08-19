#Cstart="01-jan-2008";Cstop="31-mar-2008"


GetDataDiscards <- function(Cstart=Cstart,Cstop=Cstop) {

   frisbe <- dBConnect(which.database="frisbe",which.lib='RODBC')
   
  # Connect to database for which you will need an account and permission from Peter Van der Kamp
  

  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
  
 qdis <- paste("select st.*, sa.*, ss.*, cl.*, ta.*, vp.*
  FROM VIS_STATIONS st, VIS_SAMPLES sa, VIS_SUBSAMPLES ss, VIS_CLASSES
cl, VIS_TAXONS ta, VIS_POSITIONS vp
  WHERE
  st.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
  AND st.PGM_CODE IN ('DISOT','DISBT','DISN','DISCRAN','DISGILL','DISP')
  AND st.id=sa.stn_ID AND sa.id = ss.SPE_ID  AND ss.ID = cl.SSE_ID AND cl.TXN_NODC_CODE = ta.NODC_CODE
  AND vp.stn_id = st.id AND vp.seq_no = 0
  ")
  
  #AND
  #ss.sse_category IN ('d','l')

  dis <- sqlQuery(frisbe,qdis)
  
  #Take out missing values
  
  dis <- dis [!is.na(dis$QUANTITY.1),] 
 
  # Get VIS_STATIONS and POSITIONS

qstvp <- paste("select VIS_STATIONS.*,VIS_POSITIONS.* FROM VIS_STATIONS, VIS_POSITIONS
 WHERE VIS_STATIONS.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
   AND
 VIS_STATIONS.PGM_CODE IN ('DISOT','DISBT','DISN','DISCRAN','DISGILL','DISP')
  AND VIS_STATIONS.ID = VIS_POSITIONS.STN_ID AND VIS_POSITIONS.seq_no = 0
 ")
 
 vis_chrons <- sqlQuery(frisbe,qstvp)  
 
 #List of extras in the chrons
 
 usd <- unique(dis$ID)
 
 mm <- match(usd,vis_chrons$ID)
 
 discards <- merge(vis_chrons[-mm,],dis,all=T)
 
 discards$QUANTITY.1[is.na(discards$QUANTITY.1)] <- 0
   
 discards
 
 }
 
 
 #xxx <- GetDataDiscards(Cstart='01-JAN-2010',Cstop='31-DEC-2010') 