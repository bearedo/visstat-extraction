#Cstart="01-jan-2009";Cstop="31-jan-2009"


GetDataDaysAtSeaByRegistration <- function(Cstart=Cstart,Cstop=Cstop) {

# This function extracts effort (days at sea, kw days at sea) data from VISSTAT by trip.
# Some trips have different gears used on the trip which is why sometimes you get the data repeated.
# If meshes are null or void you get -1 in the output file

# Connect to database for which you will need an account and permission from Peter Van der Kamp
  frisbe <- dBConnect(which.database="frisbe")

  Cstop  <-WriteSQLString(Cstop)
  Cstart <-WriteSQLString(Cstart)
  

 qdis <- paste("select st.*, sa.*, ss.*, cl.*, ta.*, vp.*
  FROM VIS_STATIONS st, VIS_SAMPLES sa, VIS_SUBSAMPLES ss, VIS_CLASSES cl, VIS_TAXONS ta, VIS_POSITIONS vp
  WHERE   st.STN_DATE BETWEEN ",Cstart," and ",Cstop,"
  AND st.id=sa.stn_ID AND sa.id = ss.SPE_ID AND
  ss.sse_category IN ('l','d') AND ss.ID = cl.SSE_ID AND cl.TXN_NODC_CODE = ta.NODC_CODE
  AND vp.stn_id = st.id AND vp.seq_no = 0")


  dis <- sqlQuery(frisbe,qdis)
  
  dis
  
  }