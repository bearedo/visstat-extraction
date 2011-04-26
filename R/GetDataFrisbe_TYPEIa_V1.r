
########################################################################
#
#
# Extraction of Frisbe (Market sampling) data
#
# The following needs to be specified:
#
#  OPTION 1: Select all data from a User-specified list of snijmonster id's
#  To use this option, set: SnijMonsterList <- TRUE;
#  If this option is used, a straightforward selection from Frisbe is made: all data from these snijmonsters is taken
#
#  OPTION 2: Fully automatic selection based on specification of 'species' 'period' and 'area'
#  To use this option, set: FRISBEautoselect <- TRUE;
#  In this option, A selection of all Frisbe is made in which PGM_CODE='MARKET' (market sampling)
#  and the given period and species.
#  A shapefile of the ICES subarea polygons is used to define which point locations of the snijmonsters fall into the given (user-specified) list of subareas.
#
#  The following needs to be specified:
# - start date
# - end date
# - species code
# - a list of ICES subareas
#
# OPTION 3: as option 2, but with the added possibility for the user to
# specify snijmonster id's which need to be included over and above the automatically selected snijmonsters
# To use this option, set: FRISBEautoselect <- TRUE;
#
# The following needs to be specified:
#
# - a list of snijmonsters; e.g. snijmonsterforcein <- c(9900112,9900113);
#
#
########################################################################
#
## header of the function 'GetAllData'
#
#GetAllData <- function(GNC=NC, gear=geartype, Catcharea=catchareas, spec=species,
#                    tstart=startdate, tstop=enddate, tcuryear=curyear, tplusgroupage=plusgroupage
#                    SnijMonsterList=FrisbeOption1, FRISBEautoselect=FrisbeOption2, snijmonsterforcein=FrisbeOption3, ICESAreas=ICESshapefile)
#
## To use the options, in the masterfile set:
## For option 1 (user-specified list of snijmonster numbers)
#
#FrisbeOption1 <- c(214441, 214444, 214446, 214450, 214451, 214456, 214493, 214514, 214442, 214443, 214445, 214447, 214448);
#FrisbeOption2 <- "FALSE";
#FrisbeOption3 <- "FALSE";
#
## For option 2 (automatic selection of snijmonsters)
#FrisbeOption1 <- "FALSE;
#FrisbeOption2 <- "TRUE";
#FrisbeOption3 <- "FALSE";
#
#
## For option 3  (automatic selection of snijmonsters + additional user-specified snijonster numbers)
#FrisbeOption1 <- "FALSE;
#FrisbeOption2 <- "TRUE";
#FrisbeOption3 <- c(214441,214451);
#                    
                    
###############################################################################################################

#########################################################################################################
#
# Option 1 (user-specified list of snijmonster numbers)
#
#########################################################################################################


GetDataFrisbeOption1 <- function(TheSnijIDvect=SnijMonsterList, FWarningsFile=Wfile) {

SNIJSQLList <- WriteSQLString(TheSnijIDvect);
query <- paste("SELECT
      sa.ID id
,     ss.ID subsampleid
,     cl.ID classesid
,     st.ID stationid
,     pos.ID positionid
,     pos.position_type
,     ta.ICES_CODE
,     st.CODE stationcode
,     st.AREA_CODE 
,     st.STM_CODE
,     st.STN_DATE
,     TO_CHAR(st.STN_DATE,'Q') quarter
,     TO_CHAR(st.STN_DATE,'MM') month
,     TO_CHAR(st.STN_DATE,'YYYY') year
,     st.PGM_CODE
,     ss.SSE_CATEGORY
,     cl.GENDER
,     cl.FISH_NUMBER
,     cl.QUANTITY
,     cl.CSS_WEIGHT
,     cl.LENGTH
,     age.YEARCLASS
,     ss.SSE_WEIGHT
,     sa.handling
,     ss.TOTAL_UNITS
,     ss.USED_UNITS
,     pos.LONGITUDE
,     pos.LATITUDE
 FROM VIS_PLATFORMS pla 
   INNER JOIN VIS_PLATFORM_PROPERTIES plpr ON (pla.CODE = plpr.PFM_CODE)
   INNER JOIN VIS_STATIONS st ON (st.PPY_ID = plpr.ID)
   INNER JOIN VIS_SAMPLES sa ON (st.id=sa.stn_ID)
   INNER JOIN VIS_SUBSAMPLES ss ON (sa.id = ss.SPE_ID)
   INNER JOIN VIS_CLASSES cl ON (ss.ID = cl.SSE_ID)
   INNER JOIN VIS_TAXONS ta ON (cl.TXN_NODC_CODE = ta.NODC_CODE)
   LEFT OUTER JOIN vis_ages age ON (cl.ID=age.CSS_ID)
   LEFT OUTER JOIN VIS_POSITIONS pos ON (st.ID=pos.STN_ID)
 WHERE st.CODE IN ",SNIJSQLList,"
")

snij <- sqlQuery(frisbe,query);

write("finished reading frisbe data", file=FWarningsFile, append=T);
write(paste("number of rows of data: ", nrow(snij)), file=FWarningsFile, append=T);
if(nrow(snij)==0) { write("ERROR: no frisbe data has been read!", file=FWarningsFile, append=T); };

# exceptions in snijdata
snij <- SNIJExceptions(EXsnij=snij, EXSNIJspecies=SNIJspecies);

return(snij);

};


########################################################################################################  
#
# Option 2 (automatic selection of snijmonsters)
#
# and Option 3 (automatic selection of snijmonsters + additional user-specified snijonster numbers)
#
#########################################################################################################

GetDataFrisbeOption2and3 <- function(SnijArea=AreaSnij, SNIJspecies=spec, SNIJstart=tstart, SnijAreadat=ICESAreadat,
                                      SNIJstop=tstop, ICESpolygons=ICESAreas, forcein=snijmonsterforcein, FWarningsFile=Wfile, FMapFile=Mfile) {

query <- paste("SELECT
      sa.ID id
,     ss.ID subsampleid
,     cl.ID classesid
,     st.ID stationid
,     pos.ID positionid
,     pos.position_type
,     ta.ICES_CODE
,     st.CODE stationcode
,     st.AREA_CODE 
,     st.STM_CODE
,     st.STN_DATE
,     TO_CHAR(st.STN_DATE,'Q') quarter
,     TO_CHAR(st.STN_DATE,'MM') month
,     TO_CHAR(st.STN_DATE,'YYYY') year
,     st.PGM_CODE
,     ss.SSE_CATEGORY
,     cl.GENDER
,     cl.FISH_NUMBER
,     cl.QUANTITY
,     cl.CSS_WEIGHT
,     cl.LENGTH
,     age.YEARCLASS
,     ss.SSE_WEIGHT
,     sa.handling
,     ss.TOTAL_UNITS
,     ss.USED_UNITS
,     pos.LONGITUDE
,     pos.LATITUDE
 FROM VIS_PLATFORMS pla 
   INNER JOIN VIS_PLATFORM_PROPERTIES plpr ON (pla.CODE = plpr.PFM_CODE)
   INNER JOIN VIS_STATIONS st ON (st.PPY_ID = plpr.ID)
   INNER JOIN VIS_SAMPLES sa ON (st.id=sa.stn_ID)
   INNER JOIN VIS_SUBSAMPLES ss ON (sa.id = ss.SPE_ID)
   INNER JOIN VIS_CLASSES cl ON (ss.ID = cl.SSE_ID)
   INNER JOIN VIS_TAXONS ta ON (cl.TXN_NODC_CODE = ta.NODC_CODE)
   LEFT OUTER JOIN vis_ages age ON (cl.ID=age.CSS_ID)
   LEFT OUTER JOIN VIS_POSITIONS pos ON (st.ID=pos.STN_ID)
 WHERE ta.ICES_CODE = ",SNIJspecies,"
 AND st.STN_DATE <= ",SNIJstop,"
 AND  st.STN_DATE >= ",SNIJstart,"
 AND st.PGM_CODE='MARKET'
 AND pla.CTY_CODE = 'ned'
")

snij <- sqlQuery(frisbe,query)
write("finished reading frisbe data", file=FWarningsFile, append=T);
write(paste("number of rows of snijdata: ", nrow(snij)), file=FWarningsFile, append=T);
if(nrow(snij)==0) { write("ERROR: no frisbe data has been read!", file=FWarningsFile, append=T); };

#Fudge by Doug Beare for Cod
print(SNIJspecies)
if (SNIJspecies == "'COD'") snij$GENDER <- rep("m",length(snij[,1]));print(snij[1:5,])

# exceptions in snijdata
snij <- SNIJExceptions(EXsnij=snij, EXSNIJspecies=SNIJspecies, EXFWarningsFile=FWarningsFile);

# geografische selectie
snij <- SNIJSelectGeo(GEOsnij=snij, GEOSnijArea=SnijArea,  GEOforcein=forcein,  GEOSnijAreadat=SnijAreadat, GEOICESpolygons=ICESpolygons, GEOFWarningsFile=FWarningsFile, GEOFMapFile=FMapFile);

return(snij);

};



