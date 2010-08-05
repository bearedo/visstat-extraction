#tstart <- "01-jan-2008"
#tstop  <- "31-jan-2008"


GetDataLandingValueByTrip <- function(Cstart=Cstart,Cstop=Cstop,Cmeshmin=Cmeshmin,Cmeshmax=Cmeshmax) {

# This function gets the value of each component of the landings

# Get the total days at sea by trip:

dasbytrip <- GetDataDaysAtSeaByTrip(Cstart=Cstart,Cstop=Cstop,Cmeshmin= Cmeshmin, Cmeshmax= Cmeshmax)

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

 
# valuebytrip <- GetDataLandingValueByTrip(Cstart="01-jan-2008",
#         Cstop="31-jan-2008",Cmeshmin= -1, Cmeshmax= 200)




