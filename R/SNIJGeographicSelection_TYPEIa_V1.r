
SNIJSelectGeo <- function(GEOsnij=snij, GEOSnijArea=SnijArea,  GEOforcein=forcein, GEOSnijAreadat=SnijAreadat, GEOICESpolygons=ICESpolygons, GEOFWarningsFile=FWarningsFile, GEOFMapFile=FMapFile) {

temX <- tapply(GEOsnij$LONGITUDE,as.factor(GEOsnij$ID),mean);
temY <- tapply(GEOsnij$LATITUDE,as.factor(GEOsnij$ID),mean);
temID <- as.numeric(levels(as.factor(GEOsnij$ID)));

check1 <- c(dimnames(temX)[[1]]==dimnames(temY)[[1]]);
check2 <- c(temID==dimnames(temY)[[1]]);
if ( length(check1[check1==FALSE])!=0 ) { write("warning: snijfiles ID not unique for latitude and longitude", file=GEOFWarningsFile, append=T); };
if ( length(check2[check2==FALSE])!=0 ) { write("warning: snijfiles ID not unique for latitude and longitude", file=GEOFWarningsFile, append=T); };

pointlocs <- as.EventData(data.frame(EID=as.numeric(temID), X=temX, Y=temY));

polres <- findPolys(pointlocs,GEOICESpolygons);

#graphics.off();
#png(GEOFMapFile, width=1000, height=1000);
#plotMap(GEOICESpolygons);
#points(pointlocs$X,pointlocs$Y, pch=16, col="red", cex=0.75);
#dev.off();

inind <- temID%in%polres$EID;

notin <- pointlocs[inind==FALSE,]

if( nrow(notin)!=0 ) { write("warning: snijmonster(s) not in an ICES area!", file=GEOFWarningsFile, append=T); write(notin$EID, file=GEOFWarningsFile, append=T); };

if( nrow(notin)==0 ) { write("all snijmonsters allocated to an ICES area!", file=GEOFWarningsFile, append=T); };

#print("The following ICES areas have been allocated to the snijmonsters:");
Npointlocs <- data.frame(snijID=polres$EID,IcesArea=GEOSnijAreadat$ICES_area[polres$PID]);

USEind <- c(Npointlocs$IcesArea%in%GEOSnijArea);
write("The following snijmonsters have been automatically selected for inclusion:", file=GEOFWarningsFile, append=T);
NpointlocsUSE <- Npointlocs[USEind,]
write.table(NpointlocsUSE, file=GEOFWarningsFile, quote=F, sep=" ", row.names=FALSE, col.names=FALSE, append=T);

if (  sum(as.numeric(USEind=="FALSE")) == 0 ) {
  temmes <- paste("All of these snijmonsters fall into: ", WriteSQLString(GEOSnijArea),sep="");
  write.table(temmes, file=GEOFWarningsFile, quote=F, sep="", col.names=FALSE,append=T);
};

if (  sum(as.numeric(USEind=="FALSE")) > 0 ) {
  temmes <- paste("The following snijmonsters do not fall into: ", WriteSQLString(GEOSnijArea),sep="");
  write.table(temmes, file=GEOFWarningsFile, append=T, col.names=FALSE);
  write.table(Npointlocs[USEind==FALSE,], file=GEOFWarningsFile, col.names=FALSE, quote=T, sep="", append=T);
};

# force in samples that do not fall into the area selection
if (GEOforcein[1] != FALSE ) {
  write("The following snijmonster(s) has been forced in by the user: ", file=GEOFWarningsFile, append=T);
  tforce <- data.frame(snijID=GEOforcein, IcesArea=rep("forced",length(GEOforcein)));
  NpointlocsUSE <- data.frame(rbind(NpointlocsUSE,tforce));
  write.table(NpointlocsUSE, file=GEOFWarningsFile, quote=T, sep="", col.names=FALSE, append=T);
};

checktab <- table(NpointlocsUSE$snijID);
checkNmonst <- checktab[checktab>1]
if(length(checkNmonst)!=0) {
write("warning: the following snijmonster(s) is included more than once: ", file=GEOFWarningsFile, append=T);
write.table(checktab[checktab>1], file=GEOFWarningsFile, append=T);
};

GEOsnijUSE <- c(GEOsnij$ID%in%NpointlocsUSE$snijID);
GEOsnij <- GEOsnij[GEOsnijUSE,];

return(merge(GEOsnij,Npointlocs,by.x=c("ID"),by.y=c("snijID")));

};



#
#GEOsnij=snij
#GEOSnijArea=catchareas
#GEOforcein=FALSE
#GEOSnijAreadat=Areadat
#GEOICESpolygons=ICESshapefile
#
SNIJSelectGeoNOPRINT <- function(GEOsnij=snij, GEOSnijArea=SnijArea,  GEOforcein=forcein, GEOSnijAreadat=SnijAreadat, GEOICESpolygons=ICESpolygons) {

temX <- tapply(GEOsnij$LONGITUDE,as.factor(GEOsnij$ID),mean);
temY <- tapply(GEOsnij$LATITUDE,as.factor(GEOsnij$ID),mean);
temID <- as.numeric(levels(as.factor(GEOsnij$ID)));

check1 <- c(dimnames(temX)[[1]]==dimnames(temY)[[1]]);
check2 <- c(temID==dimnames(temY)[[1]]);
if ( length(check1[check1==FALSE])!=0 ) { print("warning: snijfiles ID not unique for latitude and longitude"); };
if ( length(check2[check2==FALSE])!=0 ) { print("warning: snijfiles ID not unique for latitude and longitude"); };

pointlocs <- as.EventData(data.frame(EID=as.numeric(temID), X=temX, Y=temY));

polres <- findPolys(pointlocs,GEOICESpolygons);

inind <- temID%in%polres$EID;

notin <- pointlocs[inind==FALSE,]

if( nrow(notin)!=0 ) { print("warning: snijmonster(s) not in an ICES area!"); print(notin$EID); };

if( nrow(notin)==0 ) { print("all snijmonsters allocated to an ICES area!"); };

#print("The following ICES areas have been allocated to the snijmonsters:");
Npointlocs <- data.frame(snijID=polres$EID,IcesArea=GEOSnijAreadat$ICES_area[polres$PID]);

USEind <- c(Npointlocs$IcesArea%in%GEOSnijArea);
NpointlocsUSE <- Npointlocs[USEind,]

print(Npointlocs);


if (  sum(as.numeric(USEind=="FALSE")) == 0 ) {
  temmes <- paste("All of these snijmonsters fall into: ", WriteSQLString(GEOSnijArea),sep="");
};

if (  sum(as.numeric(USEind=="FALSE")) > 0 ) {
  temmes <- paste("The following snijmonsters do not fall into: ", WriteSQLString(GEOSnijArea),sep="");
};


checktab <- table(NpointlocsUSE$snijID);
checkNmonst <- checktab[checktab>1]
if(length(checkNmonst)!=0) {
print("warning: the following snijmonster(s) is included more than once:");
print(checktab[checktab>1]);
};

GEOsnijUSE <- c(GEOsnij$ID%in%NpointlocsUSE$snijID);
GEOsnij <- GEOsnij[GEOsnijUSE,];



#return(data.frame(GEOsnij,AREA=);
return(merge(GEOsnij,Npointlocs,by.x=c("ID"),by.y=c("snijID")));

};

