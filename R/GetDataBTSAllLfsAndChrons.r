#Cstart="01-jan-2008";Cstop="31-mar-2008"


GetDataBTSAllLfsAndChrons <- function(Cstart=Cstart,Cstop=Cstop,user=user,passwd=passwd) {

   frisbe <- dBConnect(which.database="frisbe",user=user,passwd=passwd)
   
  # Connect to database for which you will need an account and permission from Peter Van der Kamp
  
  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
  
 qbts <- paste("select st.*, sa.*, ss.*, cl.*, ta.*, vp.*
  FROM VIS_STATIONS st, VIS_SAMPLES sa, VIS_SUBSAMPLES ss, VIS_CLASSES
cl, VIS_TAXONS ta, VIS_POSITIONS vp
  WHERE
  st.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
  AND st.PGM_CODE IN 'BTS'
  AND st.id=sa.stn_ID AND sa.id = ss.SPE_ID  AND ss.ID = cl.SSE_ID AND cl.TXN_NODC_CODE = ta.NODC_CODE
  AND vp.stn_id = st.id AND vp.seq_no = 0
  ")
  
  bts <- sqlQuery(frisbe,qbts)
  
   #Take out missing values
  
  bts <- bts [!is.na(bts$QUANTITY.1),] 
 
  # Get VIS_STATIONS and POSITIONS

qstvp <- paste("select VIS_STATIONS.*,VIS_POSITIONS.* FROM VIS_STATIONS, VIS_POSITIONS
 WHERE VIS_STATIONS.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
   AND
 VIS_STATIONS.PGM_CODE IN ('BTS')
  AND VIS_STATIONS.ID = VIS_POSITIONS.STN_ID AND VIS_POSITIONS.seq_no = 0
 ")
 
 vis_chrons <- sqlQuery(frisbe,qstvp)  
 
 #Put on ship IDs
  
  vis_p_p <- sqlQuery(frisbe,"SELECT * from VIS_PLATFORM_PROPERTIES")
  
  vis_chrons$SHIP <- vis_p_p$NAME[match(vis_chrons$PPY_ID,vis_p_p$ID)]
 
 print(paste('number of distinct stations = ',dim(vis_chrons)[[1]]))

 #The chrons are a definitive list of stations 
 
 subfact <- bts$TOTAL_UNITS/bts$USED_UNITS
 bts$QUANTITY.1  <- subfact*bts$QUANTITY.1 

 out <- list(bts=bts,chrons=vis_chrons)
 
 }
 
 
#bts <- GetDataBTSAllLfsAndChrons(Cstart="01-jan-2003",Cstop="31-dec-2010") 

#write.table(bts$chrons,file='bts-chrons-2003-2010.csv',row.names=F,sep=",")