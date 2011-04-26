
ALL_GPY <- c("OTB","TBB","TBS","DRB","PTB","SDN","SSC","PTM","PS","LL","LLS","GN","GNS",
            "GND","GTR","LHP","FPO","MIS","LLD","OTM","OTT","BNT","NVT","TGB","LHM");
            

GetDataSizeClasses <- function(SCNC=NC, SCgearselect=FALSE, SCgear=gear, SCmeshmin=min_mesh_size, SCmeshmax=max_mesh_size,
                      SCCatcharea=catchareas, SCspec=species, SCstart=startdate, SCstop=enddate, SWarningsFile=Wfile) {
                                                      


############################################################################################
#"trips_selection" query to select for: trips in ices_area IV,meshsize,geartype and country#
############################################################################################

if (SCgearselect=="TRUE") {

SQLgear <- WriteSQLString(SCgear);

query <- paste("select registrations.qpy_ices_quadrant,
                        quadrant_properties.ices_subarea,
                        trips.trip_number,
                        registrations.meshsize,
                        registrations.GPY_code,
                        registrations.trp_ppy_plm_cny_code

from      registrations,
          quadrant_properties,
          trips
where     registrations.qpy_ices_quadrant = quadrant_properties.ices_quadrant
          and registrations.gpy_code IN ",SQLgear,"
          and registrations.meshsize between ",SCmeshmin," AND ",SCmeshmax,"
          and registrations.trp_ppy_plm_cny_code = 'nld'
          and trips.arrival_date = registrations.trp_arrival_date
          and trips.arrival_time = registrations.trp_arrival_time
          and trips.ppy_plm_code = registrations.trp_ppy_plm_code
          and trips.prt_code = registrations.trp_prt_code
          and trips.arrival_date between ",SCstart," and ",SCstop,"
         ");

trips_selection <- sqlQuery(visstat,query);
};

if (SCgearselect=="FALSE") {

query <- paste("select registrations.qpy_ices_quadrant,
                        quadrant_properties.ices_subarea,
                        trips.trip_number,
                        registrations.trp_ppy_plm_cny_code

from      registrations,
          quadrant_properties,
          trips
where     registrations.qpy_ices_quadrant = quadrant_properties.ices_quadrant
          and registrations.trp_ppy_plm_cny_code = 'nld'
          and trips.arrival_date = registrations.trp_arrival_date
          and trips.arrival_time = registrations.trp_arrival_time
          and trips.ppy_plm_code = registrations.trp_ppy_plm_code
          and trips.prt_code = registrations.trp_prt_code
          and trips.arrival_date between ",SCstart," and ",SCstop,"
         ");

trips_selection <- sqlQuery(visstat,query);


};


######## Select for trips with 100% catches from area IV ########

AreaOfInterest <- as.character(trips_selection$ICES_SUBAREA);
AreaOfInterest[AreaOfInterest%in%SCCatcharea] <- 1;
AreaOfInterest[AreaOfInterest!=1] <- 0;


trips_selection <- data.frame(trips_selection,AreaOfInterest);
trips_selection <- na.omit(trips_selection);

trips_selection$AreaOfInterest <- as.numeric(as.character(trips_selection$AreaOfInterest));


if ( sum(trips_selection$AreaOfInterest) < nrow(trips_selection) ) {

  tab <- table(trips_selection$TRIP_NUMBER,trips_selection$AreaOfInterest);
  thetripnumbers <- dimnames(tab)[[1]];

  AreaOItrips <- thetripnumbers[tab[,1]==0 & tab[,2]>0];
  AreaOItrips <- unique(AreaOItrips);

};

if ( sum(trips_selection$AreaOfInterest)== nrow(trips_selection) ) {

  tab <- table(trips_selection$TRIP_NUMBER,trips_selection$AreaOfInterest);
  thetripnumbers <- dimnames(tab)[[1]];
  AreaOItrips <- unique(thetripnumbers);

};

write("finished reading trips_selection table", file=SWarningsFile, append=T);
write(paste("number of trips selected: ", length(AreaOItrips)), file=SWarningsFile, append=T);
if(length(AreaOItrips)==0) { write("ERROR: no trips were selected!", file=SWarningsFile, append=T);};


# "size_classes" query to obtain the total landed weight per market category #

query <- paste("select
          size_classes.trip_number,
          size_classes.auction,
          size_classes.prt_code,
          size_classes.txn_ices_code,
          size_classes.size_class,
          size_classes.weight
from      size_classes
where
          size_classes.txn_ices_code=",SCspec,"
          and size_classes.unload_date between ",SCstart," and ",SCstop,"
")

size_classes <- sqlQuery(visstat,query);
size_classes <- size_classes[size_classes$TRIP_NUMBER%in%AreaOItrips,];
write("finished reading visstat size classes data", file=SWarningsFile, append=T);
if(nrow(size_classes)==0) { write("ERROR: no size class data has been read!", file=SWarningsFile, append=T);};
write(paste("number of rows in size_classes table: ", nrow(size_classes)),file=SWarningsFile, append=T);
write(paste("total weight in size_classes table: ", sum(size_classes$WEIGHT)),file=SWarningsFile, append=T);
write(paste("number of rows in size_classes table: ",nrow(size_classes)),file=SWarningsFile, append=T);

# total landed weight per market category
tab_W_c <- tapply(size_classes$WEIGHT, as.factor(size_classes$SIZE_CLASS),sum);

return(tab_W_c);

};