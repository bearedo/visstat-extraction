

######################################################################
#
# Function to read in all data needed for raising from Visstat and Frisbe
# This function uses a number of functions
# 'GetDataCatches'
# 'GetDataSizeclasses'
# 'GetDataFrisbe'
#
# The user has a number of options
#
######################################################################

#
# NC: number of market categories, eg. NC <- 4
# geartype: vector of geartypes, e.g. geartype <- c("TBB","OTB","TBS");
# Catcharea: vector of ICES subareas, e.g. Catcharea <- c("IVa","IVAb","IVc");
# species: taxon code, e.g. species <- "PLE";
# startdate
# enddate
# curyear
# plusgroupage
#
# FrisbeOption1,FrisbeOption2,FrisbeOption3 : various options to user to select Frisbe data
#
# ICESshapefile: shapefile of polygons of ICES subareas (needed for Frisbe options 2 and 3)
#

GetAllData <- function(GNC=NC, GearOption=FALSE, gear=geartype, Catcharea=catchareas, AreaSnij=catchareas,spec=species,
                        meshmin=min_mesh_size, meshmax=max_mesh_size,
                    tstart=startdate, tstop=enddate, tcuryear=curyear, tplusgroupage=plusgroupage,
                    SnijMonsterList=FrisbeOption1, FRISBEautoselect=FrisbeOption2,
                    snijmonsterforcein=FrisbeOption3, ICESAreas=ICESshapefile, ICESAreadat=Areadat, Rfile=QRaisingFile, Wfile=QWarningsFile, Mfile=QMapFile, totalweight=FeedLandOpt, FeedSizeComp=FALSE) {

###
# Open files for writing
###

write("Information, Warning and Error Messages", file=Wfile);
write(paste("Raising for ",spec," year: ",tcuryear," quarter: ",tstart, " to ", tstop,sep=""), file=Wfile, append=TRUE);
write("##########################", file=Wfile, append=TRUE);

write(paste("Raising Results for year ",tcuryear," quarter ",tstart, " to ", tstop,sep=""), file=Rfile);
write(paste("Raising for ",spec,sep=""), file=Rfile, append=TRUE);
write("##########################", file=Rfile, append=TRUE);

print("opened files for writing");



# imares conversiefactor gedeeld door wettelijke (LNV) conversiefactor
if ( spec == "'PLE'") { totalweight <- totalweight*(1.11/1.07); 

write(paste(""), file=Rfile, append=TRUE);
write(paste("total landed weight multiplied by the following factor: ",1.11/1.07,sep=""), file=Rfile, append=TRUE);
write(paste("total landed weight after this conversion: ",totalweight,sep=""), file=Rfile, append=TRUE);
write(paste(""), file=Rfile, append=TRUE);

};

if ( spec == "'SOL'") { totalweight <- totalweight*(1.11/1.04); 

write(paste(""), file=Rfile, append=TRUE);
write(paste("total landed weight multiplied by the following factor: ",1.11/1.04,sep=""), file=Rfile, append=TRUE);
write(paste("total landed weight after this conversion: ",totalweight,sep=""), file=Rfile, append=TRUE);
write(paste(""), file=Rfile, append=TRUE);

};


###
# Calling Functions to Read sizeclasses data
###

if (FeedSizeComp[1]==FALSE) {

  if ( spec %in% c("'PLE'","'SOL'","'TUR'","'BLL'","'RJC'","'RJH'","'RJM'","'RJN'") ) {
    tab_W_c <- GetDataSizeClasses(SCNC=GNC, SCgearselect=GearOption, SCgear=gear, SCmeshmin=meshmin, SCmeshmax=meshmax,
                      SCCatcharea=Catcharea, SCspec=spec, SCstart=tstart, SCstop=tstop, SWarningsFile=Wfile);

    print("obtained market category composition of landings from VISSTAT Size-Classes table");
  };


# three market categories in auction data (four in biological sampling data)
if (spec=="'BLL'" ) { Scaleratio <- totalweight/sum(tab_W_c[c("1","2","3")]); 
      write("% landings per market category", file=Rfile, append=TRUE);
      write(round(tab_W_c[c("1","2","3")]/sum(tab_W_c[c("1","2","3")]),digits=2), file=Rfile, append=TRUE);
      write("", file=Rfile, append=TRUE);
      tab_W_c <- tab_W_c[c("1","2","3")]*Scaleratio;
      outm <- c(tab_W_c,sum(tab_W_c)); names(outm)[4] <- "total";
      write("", file=Rfile, append=TRUE);
      write("total landings per market category as used in raising (kg):", file=Rfile, append=TRUE);
      write.table(round(outm,digits=2), sep="\t", file=Rfile, append=TRUE, col.names=FALSE, row.names=TRUE);
      write("", file=Rfile, append=TRUE);
};

# four market categories
if (spec %in% c("'PLE'","'RJC'","'RJH'") ) { Scaleratio <- totalweight/sum(tab_W_c[c("1","2","3","4")]); 
      write("% landings per market category", file=Rfile, append=TRUE);
      write(round(tab_W_c[c("1","2","3","4")]/sum(tab_W_c[c("1","2","3","4")]),digits=2), file=Rfile, append=TRUE);
      write("", file=Rfile, append=TRUE);
      tab_W_c <- tab_W_c[c("1","2","3","4")]*Scaleratio;
      outm <- c(tab_W_c,sum(tab_W_c)); names(outm)[5] <- "total";
      write("", file=Rfile, append=TRUE);
      write("total landings per market category as used in raising (kg):", file=Rfile, append=TRUE);
      write.table(round(outm,digits=2), sep="\t", file=Rfile, append=TRUE, col.names=FALSE, row.names=TRUE);
      write("", file=Rfile, append=TRUE);
     
};


# four market categories gevlekte rog   Raja montagui: no biological samples from category 1 but landings are tiny
# for this reason the following assumption: bioloigcal characteristics of fish in category 1 are the same as in category 2
# thus add landings in categories 1 and 2 together
if (spec %in% c("'RJM'") ) { Scaleratio <- totalweight/sum(tab_W_c[c("1","2","3","4")]); 
      write("% landings per market category before adding market categories 1 and 2:", file=Rfile, append=TRUE);
      write(round(tab_W_c[c("1","2","3","4")]/sum(tab_W_c[c("1","2","3","4")]),digits=2), file=Rfile, append=TRUE);
      write("", file=Rfile, append=TRUE);
      
      tab_W_c["2"] <- sum(tab_W_c[c("1","2")])
      tab_W_c <- tab_W_c[c("2","3","4")];
      write("% landings per market category", file=Rfile, append=TRUE);
      write(round(tab_W_c[c("2","3","4")]/sum(tab_W_c[c("2","3","4")]),digits=2), file=Rfile, append=TRUE);
      write("", file=Rfile, append=TRUE);
      tab_W_c <- tab_W_c[c("2","3","4")]*Scaleratio;
      outm <- c(tab_W_c,sum(tab_W_c)); names(outm)[4] <- "total";
      write("", file=Rfile, append=TRUE);
      write("total landings per market category as used in raising (kg):", file=Rfile, append=TRUE);
      write.table(round(outm,digits=2), sep="\t", file=Rfile, append=TRUE, col.names=FALSE, row.names=TRUE);
      write("", file=Rfile, append=TRUE);
     
};
      
# five market categories
if (spec=="'SOL'" | (spec=="'TUR'" & tcuryear <= 1999) ) { Scaleratio <- totalweight/sum(tab_W_c[c("1","2","3","4","5")]); 
      write("% landings per market category", file=Rfile, append=TRUE);
      write(round(tab_W_c[c("1","2","3","4","5")]/sum(tab_W_c[c("1","2","3","4","5")]),digits=2), file=Rfile, append=TRUE);
      write("", file=Rfile, append=TRUE);
      tab_W_c <- tab_W_c[c("1","2","3","4","5")]*Scaleratio;
      outm <- c(tab_W_c,sum(tab_W_c)); names(outm)[6] <- "total";
      write("", file=Rfile, append=TRUE);
      write("total landings per market category as used in raising (kg):", file=Rfile, append=TRUE);
      write.table(round(outm,digits=2), sep="\t", file=Rfile, append=TRUE, col.names=FALSE, row.names=TRUE);
      write("", file=Rfile, append=TRUE);
};


# six market categories in auction statistics (seven in biological sampling data)
if (spec=="'TUR'" & tcuryear>= 2000) { Scaleratio <- totalweight/sum(tab_W_c[c("1","2","3","4","5","6")]); 
      write("% landings per market category", file=Rfile, append=TRUE);
      write(round(tab_W_c[c("1","2","3","4","5","6")]/sum(tab_W_c[c("1","2","3","4","5","6")]),digits=2), file=Rfile, append=TRUE);
      write("", file=Rfile, append=TRUE);
      tab_W_c <- tab_W_c[c("1","2","3","4","5","6")]*Scaleratio;
      outm <- c(tab_W_c,sum(tab_W_c)); names(outm)[7] <- "total";
      write("", file=Rfile, append=TRUE);
      write("total landings per market category as used in raising (kg):", file=Rfile, append=TRUE);
      write.table(round(outm,digits=2), sep="\t", file=Rfile, append=TRUE, col.names=FALSE, row.names=TRUE);
      write("", file=Rfile, append=TRUE);
};


};

  # no market categories
if ( spec %in% c("'DAB'","'FLE'","'HER'","'MAC'","'HOM'","'WHB'","'ARU'","'COD'","'ARG'") ) {
     Scaleratio <- 1;
     tab_W_c <- array(data=totalweight,dim=c(1),dimnames=list("1"));
};     


if (FeedSizeComp[1]!=FALSE) {
Scaleratio <- 1;
tab_W_c <- totalweight*FeedSizeComp;

};

write(paste("The factor by which total weights within market categories have been scaled up: ",round(Scaleratio,digits=2)),file=Wfile,append=T);


###
# Calling Functions to Read Frisbe data
###


  if ( SnijMonsterList[1] != "FALSE" & FRISBEautoselect=="FALSE") {
    snij <- GetDataFrisbeOption1(TheSnijIDvect=SnijMonsterList, FWarningsFile=Wfile);
  };


  if ( SnijMonsterList[1] == "FALSE" & FRISBEautoselect=="TRUE") {
    snij <- GetDataFrisbeOption2and3(SnijArea=AreaSnij, SNIJspecies=spec, SNIJstart=tstart, SnijAreadat=ICESAreadat,
                                      SNIJstop=tstop, ICESpolygons=ICESAreas, forcein=snijmonsterforcein, FWarningsFile=Wfile, FMapFile=Mfile);
  };


  if ( SnijMonsterList[1] != "FALSE" & FRISBEautoselect=="TRUE") {
    write("ERROR: Frisbe options 1 and 2 chosen at the same time!!", file=Wfile);
  };

print("obtained biological market sampling data from Frisbe");


temmw1 <- mean(snij$CSS_WEIGHT[snij$HANDLING=="g"]);

# list of conversion factors of biological data in FRISBE: from gutted to fresh (more than 1) and from whole to fresh (typically 1)
if ( spec == "'PLE'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.11; }; # 1.11 is IMARES conversion factor; 1.05 is the LNV factor although historically this was 1.07
if ( spec == "'SOL'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.11; }; # 1.11 is IMARES conversion factor; 1.04 is the LNV conversion factor
if ( spec == "'BLL'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.09; }; # LNV conversion factor
if ( spec == "'DAB'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.11; }; # LNV conversion factor
if ( spec == "'FLE'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.08; }; # LNV conversion factor
if ( spec == "'TUR'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.09; }; # LNV conversion factor
if ( spec == "'COD'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.17; }; # LNV conversion factor
if ( spec == "'RJC'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.13; }; # LNV conversion factor
if ( spec == "'RJM'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.13; }; # LNV conversion factor
if ( spec == "'RJN'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.13; }; # LNV conversion factor
if ( spec == "'RJH'") { snij$CSS_WEIGHT[snij$HANDLING=="g"] <- snij$CSS_WEIGHT[snij$HANDLING=="g"]*1.13; }; # LNV conversion factor
if ( spec == "'WHG'") { snij$CSS_WEIGHT <- snij$CSS_WEIGHT*1.18; }; # LNV conversion factor
if ( spec == "'HAD'") { snij$CSS_WEIGHT <- snij$CSS_WEIGHT*1.17; }; # LNV conversion factor
if ( spec == "'HER'") { snij$CSS_WEIGHT <- snij$CSS_WEIGHT*1; };   # always presented whole in frozen blocks
if ( spec == "'HOM'") { snij$CSS_WEIGHT <- snij$CSS_WEIGHT*1; };   # always presented whole in frozen blocks 
if ( spec == "'MAC'") { snij$CSS_WEIGHT <- snij$CSS_WEIGHT*1; };   # always presented whole in frozen blocks 
if ( spec == "'WHB'") { snij$CSS_WEIGHT <- snij$CSS_WEIGHT*1; };   # always presented whole in frozen blocks 
if ( spec == "'ARG'") { snij$CSS_WEIGHT <- snij$CSS_WEIGHT*1; };   # always presented whole in frozen blocks 


temmw2 <- mean(snij$CSS_WEIGHT[snij$HANDLING=="g"]);

write("  ", file=Wfile, append=TRUE);
write(paste("The chosen conversion factor from gutted to fresh weight: ", temmw2/temmw1), file=Wfile, append=TRUE);
write(" ", file=Wfile, append=TRUE);
          
     
dummy <- unlist(strsplit(as.character(snij$STN_DATE),"-"));

year <- as.numeric(dummy[seq(1,length(dummy),3)]);
month <- as.numeric(dummy[seq(2,length(dummy),3)]);
day <- as.numeric(dummy[seq(3,length(dummy),3)]);

season <- cut(month, breaks=c(0,3,6,9,12), labels=c(1,2,3,4))


#
idlevtab <- table(as.factor(snij$SSE_CATEGORY),as.factor(snij$ID));
NSamples <- ncol(idlevtab);
idlevtabT <- idlevtab; idlevtabT[idlevtab==0] <- 1; idlevtabT[idlevtab!=0] <- 0;
NSamplesAllLevels <- sum(as.numeric(colSums(idlevtabT)==0));

write(paste("the number of samples in the time-interval: ", NSamples), file=Wfile, append=T);
write(paste("the number of samples in the time-interval with all market categories represented: ", NSamplesAllLevels), file=Wfile, append=T);

reslist <- list(tab_W_c=tab_W_c, year=year, month=month, day=day, snij=snij, NID=NSamples, NIDAll=NSamplesAllLevels);

return(reslist);

                    
};



#
#############################################
##
## Example to call function 'GetAllData'
##
#
#
#
##rm(list=(ls()));
#
#library(RODBC);
#library(PBSmapping);
#library(sp);
#odbcCloseAll();
#
## load the library of raising and precision functions
#source(file="N:/Projecten/VisstatRaising/Code/PrecisionTypeILibraryPlaice5.txt");
#
#source(file="N:/Projecten/VisstatRaising/Code/GetDataSizeClasses/GetDataSizeClasses_V2.txt");
#source(file="N:/Projecten/VisstatRaising/Code/GetDataFrisbe/GetDataFrisbe_V2.r");
#source(file="N:/Projecten/VisstatRaising/Code/GetDataCatches/GetDataCatches_V2.r");
#
#
## make connections to the relevant databases
##visstat <- odbcConnect(dsn="visstatp", uid="stijn",pwd="zesover")
##frisbe  <- odbcConnect(dsn="frisbep", uid="stijn",pwd="zesover")
#
#visstat <- odbcConnect(dsn="visstatp", uid="tessa",pwd="drieover")
#frisbe  <- odbcConnect(dsn="frisbep", uid="tessa",pwd="tweevoor")
#
## File for Raising results
#RaisingFile = "N:/Projecten/VisstatRaising/Raising.txt"
## File for warnings
#WarningsFile = "N:/Projecten/VisstatRaising/Warnings.txt"
#
## catch subareas (for selection of 'catches' table): see ICES areas map
##catchareas <- c("IVa","IVb","IVc","VIId","VIIe","IIIa");
#catchareas <- c("IVa","IVb","IVc");
## Species: schol
#species <- "'PLE'";
## number of market categories
#NC <- 4;
## age of plus group
#plusgroupage <- 7;
#
## geartype
#geartype <- ALL_GPY;
##geartype <- c("OTB","TBB","TBS","GN","GNS");
#geartype <- c("TBB");
## meshsize
#min_mesh_size <- -1;
#max_mesh_size <- 1000;
#
## beginning and end of period
#startdate <- "'01-apr-2008'";
#enddate <- "'30-jun-2008'";
#
## year in which period falls
#curyear <- 2008;
#
#ICESshapefile <- importShapefile("N:/Projecten/VisstatRaising/shapefile/ICES_Areas_20051103");
#Areadat <- read.dbf("N:/Projecten/VisstatRaising/shapefile/ICES_Areas_20051103.dbf");
#
## Frisbe option 1
#
#
##FrisbeOption1 <- as.character(c(214441, 214444, 214446, 214450, 214451, 214456, 214493, 214514, 214442, 214443, 214445, 214447, 214448));
##FrisbeOption2 <- "FALSE";
##FrisbeOption3 <- "FALSE";
#
## For option 2 (automatic selection of snijmonsters)
#FrisbeOption1 <- "FALSE";
#FrisbeOption2 <- "TRUE";
#FrisbeOption3 <- "FALSE";
#
#dat <- GetAllData(GNC=NC, GearOption=TRUE, gear=geartype, Catcharea=catchareas, meshmin=min_mesh_size, meshmax=max_mesh_size,
#                    spec=species,tstart=startdate, tstop=enddate, tcuryear=curyear, tplusgroupage=plusgroupage,
#                    SnijMonsterList=FrisbeOption1, FRISBEautoselect=FrisbeOption2,
#                    snijmonsterforcein=FrisbeOption3, ICESAreas=ICESshapefile);
#
#RaiseN <- RaiseNumbers(RNC=NC, Nsnij=dat$snij, Ntab_W_c=dat$tab_W_c, Nagesexplus=dat$agesexplus, Ntpr=TRUE)
#
#write.table(RaiseN$tab_N_ID, file=RaisingFile,quote=F, sep=" N= ", col.names= "snijID", append=T)
#
## The final product: the landed numbers of fish per age-sex
#
#write.table(RaiseN$tab_Numb_agesexplus, file=RaisingFile, quote=F, sep=" ; ", col.names= "", append=T)
#            