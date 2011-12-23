#Cstart="01-jan-2008";Cstop="31-mar-2008"


GetDataSNS <- function(Cstart=Cstart,Cstop=Cstop,species="'DAB'",user=user,passwd=passwd) {

   frisbe <- dBConnect(which.database="frisbe",user=user,passwd=passwd)
   
  # Connect to database for which you will need an account and permission from Peter Van der Kamp
  

  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
  
 qsns <- paste("select st.*, sa.*, ss.*, cl.*, ta.*, vp.*
  FROM VIS_STATIONS st, VIS_SAMPLES sa, VIS_SUBSAMPLES ss, VIS_CLASSES
cl, VIS_TAXONS ta, VIS_POSITIONS vp
  WHERE ta.ICES_CODE = ",species," AND
  st.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
  AND st.PGM_CODE IN ('SNS')
  AND st.id=sa.stn_ID AND sa.id = ss.SPE_ID  AND ss.ID = cl.SSE_ID AND cl.TXN_NODC_CODE = ta.NODC_CODE
  AND vp.stn_id = st.id AND vp.seq_no = 0
  ")
  
  sns <- sqlQuery(frisbe,qsns)
  
  #Take out missing values
  
  sns <- sns [!is.na(sns$QUANTITY.1),] 
 
  # Get VIS_STATIONS and POSITIONS

qstvp <- paste("select VIS_STATIONS.*,VIS_POSITIONS.* FROM VIS_STATIONS, VIS_POSITIONS
 WHERE VIS_STATIONS.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
   AND
 VIS_STATIONS.PGM_CODE IN ('SNS')
  AND VIS_STATIONS.ID = VIS_POSITIONS.STN_ID AND VIS_POSITIONS.seq_no = 0
 ")
 
 vis_chrons <- sqlQuery(frisbe,qstvp)  
 
 
 print(paste('number of distinct stations = ',dim(vis_chrons)[[1]]))

 #List of extras in the chrons
 
 usd <- unique(sns$ID)
 
 mm <- match(usd,vis_chrons$ID)
 
 sns <- merge(vis_chrons[-mm,],sns,all=T)
 
 sns$QUANTITY.1[is.na(sns$QUANTITY.1)] <- 0
   
 sns
 
 }
 
 
#sns <- GetDataSNS(Cstart="01-jan-2007",Cstop="31-dec-2007",species="'DAB'") 