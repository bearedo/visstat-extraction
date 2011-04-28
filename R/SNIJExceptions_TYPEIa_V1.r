
SNIJExceptions <- function(EXsnij=snij, EXSNIJspecies=SNIJspecies, EXFWarningsFile=FWarningsFile) {


# double position records (for example "shoot" and "haul"): create unique key and delete one of each of duplicates
mykey <- paste(EXsnij$CLASSESID,EXsnij$FISH_NUMBER, sep="");
if ( sum(as.numeric(duplicated(mykey)))!=0 ) {
  write("duplicates in position records!", file=EXFWarningsFile, append=T);
  EXsnij <- EXsnij[!duplicated(mykey),];
  write("afer deletion of duplicates there are : ",nrow(EXsnij), " fish left in the snijfiles", file=EXFWarningsFile, append=T);
};

# voor roggen geef een arbitraire leeftijd (alleen lengte metingen)
if ( EXSNIJspecies %in% c("'RJC'","'RJH'","'RJM'") ) {
  EXsnij$YEARCLASS <- 2000;
  write("", file=EXFWarningsFile, append=T);
  write("All yearclasses set to 2000!! ", file=EXFWarningsFile, append=T);
  write("", file=EXFWarningsFile, append=T);
};


if ( EXSNIJspecies %in% c("'DAB'","'FLE'","'HER'","'WHB'","'MAC'","'HOM'","'ARU'","'COD'","'MAC'","'ARG'") ) {
write("", file=EXFWarningsFile, append=T);
write("all market categories set to 1", file=EXFWarningsFile, append=T);
EXsnij$SSE_CATEGORY <- 1; EXsnij$SSE_CATEGORY <- EXsnij$SSE_CATEGORY[,drop=TRUE];
};

# delete market categories that are of unknown type
checksum <- sum(as.numeric(!as.character(EXsnij$SSE_CATEGORY)%in%c("+","1","2","3","4","5","6","7")));
if ( checksum !=0 ) {
  EXsnij <- EXsnij[as.character(EXsnij$SSE_CATEGORY)%in%c("+","1","2","3","4","5","6","7"),];
  write("", file=EXFWarningsFile, append=T);
  write("unknown market category in snijfiles!", file=EXFWarningsFile, append=T);
  write(paste("These data were deleted; after deletion there are",nrow(EXsnij)," rows of data left",sep=""), file=EXFWarningsFile, append=T);
  write("", file=EXFWarningsFile, append=T);
};



# delete length-only data from FRISBE for pelagic sampling scheme
# in other words: assume aged fish have been drawn completely at random from the population
if ( EXSNIJspecies %in% c("'COD'","'HER'","'MAC'","'WHB'","'HOM'","'ARU'","'PLE'","'SOL'","'WHG'","'TUR'","'DAB'","'BLL'") ) {
write("", file=EXFWarningsFile, append=T);
write(paste(sum(as.numeric(is.na(EXsnij$FISH_NUMBER))), " fish with length only measurements: these have been deleted from the snijfiles",sep=" "), file=EXFWarningsFile, append=T);
EXsnij <- EXsnij[!is.na(EXsnij$FISH_NUMBER),];
};


if (sum(as.numeric(is.na(EXsnij$GENDER)))!=0) {
    write("", file=EXFWarningsFile, append=T);
    write(paste(sum(as.numeric(is.na(EXsnij$GENDER))), " fish without gender assigned: these have been deleted from the snijfiles",sep=" "), file=EXFWarningsFile, append=T);
    EXsnij <- EXsnij[is.na(EXsnij$GENDER)=="FALSE",];
};

if (sum(as.numeric(is.na(EXsnij$CSS_WEIGHT)))!=0) {
    write("", file=EXFWarningsFile, append=T);
    write(paste(sum(as.numeric(is.na(EXsnij$CSS_WEIGHT))), " fish without measured weight: these have been deleted from the snijfiles",sep=" "), file=EXFWarningsFile, append=T);
    EXsnij <- EXsnij[is.na(EXsnij$CSS_WEIGHT)=="FALSE",];
};

if (sum(as.numeric(is.na(EXsnij$LENGTH)))!=0) {
    write("", file=EXFWarningsFile, append=T);
    write(paste(sum(as.numeric(is.na(EXsnij$LENGTH))), " fish without measured length: these have been deleted from the snijfiles",sep=" "), file=EXFWarningsFile, append=T);
    EXsnij <- EXsnij[is.na(EXsnij$LENGTH)=="FALSE",];
};

if (sum(as.numeric(is.na(EXsnij$YEARCLASS)))!=0) {
    write("", file=EXFWarningsFile, append=T);
    write(paste(sum(as.numeric(is.na(EXsnij$YEARCLASS))), " fish without yearclass assigned: these have been deleted from the snijfiles",sep=" "), file=EXFWarningsFile, append=T);
    EXsnij <- EXsnij[is.na(EXsnij$YEARCLASS)=="FALSE",];
};

if (sum(1-as.numeric(EXsnij$GENDER%in%c("m","f")))!=0) {
    write("", file=EXFWarningsFile, append=T);
    write(paste(sum(as.numeric(is.na(EXsnij$YEARCLASS))), " fish with gender neq male or female: these have been deleted from the snijfiles",sep=" "), file=EXFWarningsFile, append=T);
    EXsnij <- EXsnij[(EXsnij$GENDER%in%c("m","f"))=="TRUE",];
};

write("", file=EXFWarningsFile, append=T);
write(paste("number of rows of snijdata after deletions of fish with missing data: ", nrow(EXsnij)), file=EXFWarningsFile, append=T);
write("", file=EXFWarningsFile, append=T);

return(EXsnij);

};

