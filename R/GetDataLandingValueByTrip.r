#tstart <- "01-jan-2008"
#tstop  <- "31-jan-2008"


GetDataLandingValueByTrip <- function(Cstart=Cstart,Cstop=Cstop) {

# This function gets the value of each component of the landings

# Get the total days at sea by trip:

dasbytrip <- GetDataDaysAtSeaByTrip(Cstart=Cstart,Cstop=Cstop)

print ("Got days at sea by trip")

# Get the price data

prices <- GetDataPrice(Cstart=Cstart,Cstop=Cstop)

print ("Got prices")

sv <- tapply(prices$VALUE,list(as.factor(prices$WEEK),as.factor(prices$TXN_ICES_CODE)),sum);
sw <- tapply(prices$WEIGHT,list(as.factor(prices$WEEK),as.factor(prices$TXN_ICES_CODE)),sum);

sp <- sv/sw;


# table with total landings by week
CTAB <- tapply(dasbytrip$WEIGHT,list(as.factor(dasbytrip$WEEK),as.factor(dasbytrip$TXN_ICES_CODE)),sum);
# replace the NAs with zeros
CTAB[is.na(CTAB)] <- 0;


  sp1 <- vectorise(sp)
  dimnames(sp1)[[2]] <- c("PRICE","WEEK","TXN_ICES_CODE")
  
  sp1[is.na(sp1$PRICE),]
  
 # Merge the price data with the landings and effort data

valuebytrip <- merge(dasbytrip,sp1,all.x=T)

 # For missing prices take the mean for that species

 mean.na <- function(x) { mean(x[!is.na(x)])}

 sp2 <- vectorise(tapply(sp1$PRICE,sp1$TXN_ICES_CODE,mean.na))
 dimnames(sp2)[[2]] <- c("PRICE","TXN_ICES_CODE")
 
 # Where are the prices missing in valuebytrip ?
 
 ww <- (1:length(valuebytrip[,1]))[is.na(valuebytrip$PRICE)]
 
 # Insert the averages
 
 valuebytrip$PRICE[ww] <- sp2$PRICE[match(as.character(valuebytrip$TXN_ICES_CODE[ww]),sp2$TXN_ICES_CODE)]

 # Note: some species do not seem to be in the prices table ("FBM","HER","POA","RAZ","RJB","RJH","SBR","SBX","SYC")
 # For the time-being I will make these equal to 1
 
 valuebytrip$PRICE[is.na(valuebytrip$PRICE)] <- 1
 
 valuebytrip$VALUE <- valuebytrip$WEIGHT*valuebytrip$PRICE
 
 valuebytrip
 
 }
 
###############################################################################
##################             Example of DATA EXTRACTION        #########################
###############################################################################

 
# valuebytrip <- GetDataLandingValueByTrip(Cstart="01-jan-2009",Cstop="31-jan-2009")


# take out data for a trip 

## here we try and develop the code partition the effort among the statistical rectangles 
#sum.na<-function(x) { x<-sum(x[!is.na(x)]);x}
#mean.na<-function(x) { x<-mean(x[!is.na(x)]);x}
#
#v <- valuebytrip[valuebytrip$TRIP_NUMBER == 449248,]
#
#tres <- NULL;
#                                             
#ptab <- tapply(valuebytrip$VALUE,list(as.factor(valuebytrip$QUADRANT), as.factor(valuebytrip$TRIP_NUMBER)),sum.na);
#ptab[is.na(ptab)] <- 0;
#
#ptab <- vectorise(prop.table(ptab,margin=2));
#dimnames(ptab)[[2]] <- c("P","QUADRANT","TRIP_NUMBER")
#
#vv<-merge(v,ptab,all.x=T)
#
#
#
#tab2 <- tapply(valuebytrip$DAS,as.factor(valuebytrip$TRIP_NUMBER),mean.na);  
#tab3 <- tapply(valuebytrip$KWDAS,as.factor(valuebytrip$TRIP_NUMBER),mean.na);  
#
#DAS <- ptab%*%tab2;
#DAS <- DAS[DAS!=0,];
#
#KWDAS <- ptab%*%tab3;
#KWDAS <- KWDAS[KWDAS!=0,];
#
## number of trips: at least an occurrence in a quadrant
#ntab <- ptab;
#ntab[ntab!=0] <- 1
#ntrips1 <- rowSums(ntab);
#
## number of trips: proportional allocation according to effort
#ntrips2 <- rowSums(ptab);
#
#
#tempres <- data.frame(country=rep("ned",length(KWDAS)),year=rep(2009,length(KWDAS)),
#metier=rep("TBB_DEM",length(KWDAS)),quadrant=names(DAS),month=rep(qq,length(KWDAS)),das=DAS,kwdas=KWDAS, 
#ntrips1=ntrips1[ntrips1!=0], ntrips2=ntrips2[ntrips2!=0]);
#tres <- rbind(tres,tempres);
#};
#