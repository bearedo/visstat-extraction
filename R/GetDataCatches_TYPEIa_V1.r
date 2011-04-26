

GetDataCatches <- function(CArea=Catcharea, gearselect=GearOption, Cgear=geartype, Cspec=species,
                Cstart=tstart, Cstop=tstop, Cmeshmin=meshmin, Cmeshmax=meshmax, CRaisingsFile=Rfile, CWarningsFile=Wfile, FRC=FeedRetCatch) {

if ( FRC == "FALSE" ) {
  SQLcatchareas <- WriteSQLString(CArea);

  # Option 1: gearselect=FALSE
  # meest eenvoudige selectie zonder specificatie geartype en meshsize

  if (gearselect=="FALSE") {

    query<-paste("SELECT catches.rgn_trp_arrival_date,
       TO_CHAR(catches.rgn_trp_arrival_date,'Q') quarter, 
       catches.txn_ices_code,
       catches.txn_ices_code,
       catches.weight,
       catches.rgn_trp_ppy_plm_cny_code,
       registrations.GPY_code,
       registrations.MESHSIZE,
       registrations.trp_ppy_plm_code,
       nvl(Quadrant_properties.ICES_AREA,'UNKNOWN') ices_area,
       nvl(Quadrant_properties.ICES_SUBAREA,'UNKNOWN') ices_subarea
    FROM registrations
    INNER JOIN catches ON (registrations.sre_code = catches.rgn_sre_code
       and registrations.trp_ppy_plm_cny_code =
            catches.rgn_trp_ppy_plm_cny_code
       and registrations.trp_prt_code = catches.rgn_trp_prt_code
       and registrations.trp_prt_cny_code = catches.rgn_trp_prt_cny_code
       and registrations.trp_arrival_date = catches.rgn_trp_arrival_date
       and registrations.trp_arrival_time = catches.rgn_trp_arrival_time
       and registrations.trp_ppy_id = catches.rgn_trp_ppy_id
       and registrations.trp_ppy_plm_code = catches.rgn_trp_ppy_plm_code
       and registrations.rgn_date = catches.rgn_rgn_date )
    LEFT OUTER join Quadrant_properties ON (registrations.QPY_ICES_QUADRANT = Quadrant_properties.ICES_QUADRANT)
    WHERE catches.txn_ices_code = ",Cspec,"
          catches.ices_subarea IN ",Catcharea,"
       and registrations.trp_arrival_date between ",Cstart," and ",Cstop,"
       and registrations.trp_ppy_plm_cny_code = 'nld'
       ")

#Execute the query

  catches <-sqlQuery(visstat,query);
  write("finished reading visstat catches data", file=CWarningsFile, append=T);
  write("no selection for gear type or meshsize", file=CWarningsFile, append=T);
  write(paste("number of rows of data in catches table: ", nrow(catches)), file=CWarningsFile, append=T);

  };
  

  if (gearselect==TRUE) {

    SQLgear <- WriteSQLString(Cgear);

    query<-paste("SELECT catches.rgn_trp_arrival_date,
       TO_CHAR(catches.rgn_trp_arrival_date,'Q') quarter, 
       catches.txn_ices_code,
       catches.weight,
       catches.rgn_trp_ppy_plm_cny_code,
       registrations.GPY_code,
       registrations.MESHSIZE,
       registrations.trp_ppy_plm_code,
       nvl(Quadrant_properties.ICES_AREA,'UNKNOWN') ices_area,
       nvl(Quadrant_properties.ICES_SUBAREA,'UNKNOWN') ices_subarea
    FROM registrations
    INNER JOIN catches ON (registrations.sre_code = catches.rgn_sre_code
       and registrations.trp_ppy_plm_cny_code =
            catches.rgn_trp_ppy_plm_cny_code
       and registrations.trp_prt_code = catches.rgn_trp_prt_code
       and registrations.trp_prt_cny_code = catches.rgn_trp_prt_cny_code
       and registrations.trp_arrival_date = catches.rgn_trp_arrival_date
       and registrations.trp_arrival_time = catches.rgn_trp_arrival_time
       and registrations.trp_ppy_id = catches.rgn_trp_ppy_id
       and registrations.trp_ppy_plm_code = catches.rgn_trp_ppy_plm_code
       and registrations.rgn_date = catches.rgn_rgn_date )
    LEFT OUTER join Quadrant_properties ON (registrations.QPY_ICES_QUADRANT = Quadrant_properties.ICES_QUADRANT)
    WHERE catches.txn_ices_code = ",Cspec,"
       and registrations.trp_arrival_date between ",Cstart," and ",Cstop,"
       and registrations.trp_ppy_plm_cny_code = 'nld'
       and registrations.GPY_CODE IN ",SQLgear,"
       and registrations.MESHSIZE BETWEEN ",Cmeshmin," AND ",Cmeshmax,"
       ")

    #Execute the query

    catches <-sqlQuery(visstat,query);
    write("finished reading visstat catches data", file=CWarningsFile, append=T);
    write("selection for the following gear types", file=CWarningsFile, append=T);
    write(WriteSQLString(Cgear), file=CWarningsFile, append=T);
    write(paste("with meshsizes between ", Cmeshmin, "and ", Cmeshmax), file=CWarningsFile, append=T);
    write(paste("number of rows of data in catches table: ", nrow(catches)), file=CWarningsFile, append=T);

  };
  


write("", file=CRaisingsFile, append=T);
write("selected landings (1000 kg):", file=CRaisingsFile, append=T);
ctab <- tapply(catches$WEIGHT,list(as.factor(catches$ICES_SUBAREA),as.factor(catches$QUARTER)),sum)/1000;
ctab[is.na(ctab)] <- 0;
write.table(round(ctab,digits=2), file=CRaisingsFile,append=TRUE, sep=" : ", col.names=FALSE);
write("", file=CRaisingsFile, append=T);

dareanames <- dimnames(ctab)[[1]]
lflag <- c("UNKNOWN" %in% dimnames(ctab)[[1]]);
newctab <- array(data=NA,dim=c(length(CArea)),dimnames=list(CArea));

if ( lflag ) { 
write("", file=CRaisingsFile, append=T);
write("selected landings after proportional allocation of category 'UNKNOWN' (kg):", file=CRaisingsFile, append=T);
pctab <- (ctab[dareanames[dareanames!="UNKNOWN"],]/sum(ctab[dareanames[dareanames!="UNKNOWN"],]))*ctab["UNKNOWN",];
temtabA <- pctab + ctab[dareanames[dareanames!="UNKNOWN"],];
newctab[dareanames[dareanames%in%CArea]] <- temtabA[dareanames[dareanames%in%CArea]]; newctab[is.na(newctab)] <- 0;
write.table(round(newctab,digits=2), file=CRaisingsFile,append=TRUE, sep=" : ", col.names=FALSE);
write("", file=CRaisingsFile, append=T);
};

if (!lflag ) { newctab[dareanames[dareanames%in%CArea]] <- ctab[dareanames[dareanames%in%CArea],]; newctab[is.na(newctab)] <- 0; };

write("", file=CRaisingsFile, append=T);
temmes <- paste("Sum of landings selected from ICES subareas (kg): ", WriteSQLString(CArea),sep="");
write(temmes, file=CRaisingsFile, append=T);
TOTWEIGHT <- sum(newctab);
write(round(TOTWEIGHT,digits=2), file=CRaisingsFile, append=T);
write("", file=CRaisingsFile, append=T);



# imares conversiefactor gedeeld door wettelijke (LNV) conversiefactor
if ( Cspec == "'PLE'") { convf <- 1.11/1.07; };
if ( Cspec == "'SOL'") { convf <- 1.11/1.04; };
if ( Cspec == "'DAB'") { convf <- 1; };
if ( Cspec == "'FLE'") { convf <- 1; };
if ( Cspec == "'TUR'") { convf <- 1; };
if ( Cspec == "'BLL'") { convf <- 1; };

totalweight <- TOTWEIGHT*convf;
write(paste("Finally, as used for Raising: total landed weight after application of conversion factor ","(",round(convf,digits=3),") (kg) :"), file=CRaisingsFile, append=T);
write(round(totalweight,digits=2), file=CRaisingsFile, append=T);
write("", file=CRaisingsFile, append=T);

} # if FRC==FALSE


if ( FRC != FALSE ) {
      totalweight <- FRC;
      };

return(totalweight);

};





