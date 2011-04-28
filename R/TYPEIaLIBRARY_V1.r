
ALLGPY <- c("OTB","TBB","TBS","DRB","PTB","SDN","SSC","PTM","PS","LL","LLS","GN","GNS",
            "GND","GTR","LHP","FPO","MIS","LLD","OTM","OTT","BNT","NVT","TGB","LHM")

#######################
# Generic function to write an R vector into a vector which can be used as an argument in 
# an SQL query

WriteSQLString <- function(thevect) {

  if ( length(thevect)==1 ) {
    SQLvect <- paste("('",thevect,"')",sep="");
  };
  
  if ( length(thevect)>1 ) {
    SQLvect <- c("(");
      for ( rr in 1:(length(thevect)-1) ) {
      SQLvect <- paste(SQLvect,"'",thevect[rr],"',",sep="");
      };
    SQLvect <- paste(SQLvect,"'",thevect[rr+1],"'",sep="");
    SQLvect <- paste(SQLvect,")",sep="");
  };

return(SQLvect);
};



###############################################################################################
# FUNCTION 2
# PROCESS CATCH AND MARKET SAMPLING DATA TO GET ESTIMATES OF NUMBERS-AT-AGE
###############################################################################################
#
#
#RNC <- NC;
#Nsnij <- dat$snij;
#Nmarket_category <- dat$market_category;
#Ntab_W_c <- dat$tab_W_c;
#Nagesexplus <- dat$agesexplus;
#Nage <- dat$age;
#Nagesex <- dat$agesex;
#Ntpr <- TRUE;
#WriteRFile <- QRaisingFile;
#intL <- MYintL;
#intLnames <- MYintLnames;
#Ncuryear <- curyear;
#Nplusgroup <- plusgroupage;
#Nspec <- species;
#

RaiseNumbers <- function(Nspec=species, Ncuryear=curyear, Nplusgroup=plusgroupage, Nsnij=dat$snij, Ntab_W_c=dat$tab_W_c, intL=MYintL, intLnames=MYintLnames, Ntpr=TRUE, WriteRFile=Rfile) {

Nage <- Ncuryear - Nsnij$YEARCLASS;

# age variable with plus group

Nageplus <- Nage; Nageplus[Nageplus>=Nplusgroup] <- Nplusgroup;

# age-sex combination

Nagesex <- paste(Nsnij$GENDER,Nage,sep="");

Nagesexplus <- paste(Nsnij$GENDER,Nageplus,sep="");


# ID is de identifier for a visit to a market
tab_N_ID <- tapply(rep(1,nrow(Nsnij)),as.factor(Nsnij$ID),sum);

# ID per SSE_CATEGORY (market category)
tab_N_ID_CAT <- table(as.factor(Nsnij$ID),as.factor(Nsnij$SSE_CATEGORY));

write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);
write("Numbers of fish in samples per market category", file=WriteRFile, append=TRUE);
outm <- as.matrix(cbind(dimnames(tab_N_ID_CAT)[[1]],tab_N_ID_CAT));
dimnames(outm)[[2]][1] <- "id";
write.table(t(dimnames(outm)[[2]]), file=WriteRFile, sep="\t", append=TRUE,col.names=FALSE, row.names=FALSE, quote=FALSE);
write.table(outm, file=WriteRFile, sep="\t", quote=F, col.names=FALSE, row.names=FALSE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);


# numbers of fish sampled per market category
tab_N_c <- tapply(rep(1,nrow(Nsnij)),as.factor(Nsnij$SSE_CATEGORY),sum);
                                                                  
SUMW <- tapply(Nsnij$SSE_WEIGHT,list(as.factor(Nsnij$ID),as.factor(Nsnij$SSE_CATEGORY)),sum);
SUMW[is.na(SUMW)] <- 0;
LC <- tapply(Nsnij$SSE_WEIGHT,list(as.factor(Nsnij$ID),as.factor(Nsnij$SSE_CATEGORY)),length);
TOTW <- SUMW/LC;  TOTW[is.na(TOTW)] <- 0; TOTW <- colSums(TOTW);

  if (Ncuryear>=2000 & Nspec == "'TUR'" & names(TOTW)[1]=="+" & names(TOTW)[2]=="1") { 
  
      Ntab_W_c_new <- array(data=NA, dim=c(7),dimnames=list(c("+","1","2","3","4","5","6")));
      Ntab_W_c_new[c("2","3","4","5","6")] <- Ntab_W_c[c("2","3","4","5","6")];
      propplus <- (TOTW["+"]/sum(TOTW[c("+","1")]))
      Ntab_W_c_new["+"] <-  propplus*Ntab_W_c["1"];
      Ntab_W_c_new["1"] <-  (1-propplus)*Ntab_W_c["1"];

      Ntab_W_c <- Ntab_W_c_new;
      
      write("", file=WriteRFile, append=TRUE);
      write("total landings per market category as used in raising after splitting of + group (kg):", file=WriteRFile, append=TRUE);
      write.table(Ntab_W_c, sep="\t", file=WriteRFile, append=TRUE, col.names=FALSE);
      write("", file=WriteRFile, append=TRUE);

   };

  if (Nspec == "'BLL'" & names(TOTW)[1]=="+" & names(TOTW)[2]=="1") { 
  
      Ntab_W_c_new <- array(data=NA, dim=c(4),dimnames=list(c("+","1","2","3")));
      Ntab_W_c_new[c("2","3")] <- Ntab_W_c[c("2","3")];
      propplus <- (TOTW["+"]/sum(TOTW[c("+","1")]))
      Ntab_W_c_new["+"] <-  propplus*Ntab_W_c["1"];
      Ntab_W_c_new["1"] <-  (1-propplus)*Ntab_W_c["1"];

      Ntab_W_c <- Ntab_W_c_new;

      write("", file=WriteRFile, append=TRUE);
      write("total landings per market category as used in raising after splitting of + group (kg):", file=WriteRFile, append=TRUE);
      write.table(Ntab_W_c, sep="\t", file=WriteRFile, append=TRUE, col.names=FALSE);
      write("", file=WriteRFile, append=TRUE);
      
   };

##############################
# numbers at age and sex
##############################

tab_N_agesexplus_c <- tapply(rep(1,nrow(Nsnij)),list(as.factor(Nagesexplus),as.factor(Nsnij$SSE_CATEGORY)),sum);
tab_N_agesexplus_c[is.na(tab_N_agesexplus_c)] <- 0;

tab_P_agesexplus_c <- tab_N_agesexplus_c;
for ( i in names(Ntab_W_c) ) { tab_P_agesexplus_c[,i] <- tab_N_agesexplus_c[,i]/tab_N_c[i]; };

# mean weight of fish in samples per market category
tab_w_c <- tapply((Nsnij$CSS_WEIGHT/1000),as.factor(Nsnij$SSE_CATEGORY),mean);

# estimated total numbers of fish per market category
tab_Numb_c <- Ntab_W_c[levels(as.factor(Nsnij$SSE_CATEGORY))]/tab_w_c;

# estimated total numbers of fish per age-sex category
tab_Numb_agesexplus <- tab_P_agesexplus_c %*% tab_Numb_c;

##############################
# numbers at age 
##############################

tab_N_age_c <- tapply(rep(1,nrow(Nsnij)),list(as.factor(Nage),as.factor(Nsnij$SSE_CATEGORY)),sum);
tab_N_age_c[is.na(tab_N_age_c)] <- 0;

tab_P_age_c <- tab_N_age_c;
for ( i in names(Ntab_W_c) ) { tab_P_age_c[,i] <- tab_N_age_c[,i]/tab_N_c[i]; };

# estimated total numbers of fish per age-sex category
tab_Numb_age <- tab_P_age_c %*% tab_Numb_c;


##############################
# create an output mask
##############################

sn <- substr(dimnames(tab_N_agesexplus_c)[[1]],start=1,stop=1);
sna <- as.numeric(substr(dimnames(tab_N_agesexplus_c)[[1]],start=2,stop=3));
ordf <- order(sna[sn=="f"]);
ordm <- order(sna[sn=="m"]);

snaf <- sna[sn=="f"]; snafo <- snaf[ordf];
snam <- sna[sn=="m"]; snamo <- snam[ordm];

mask <- paste(sn,c(snafo,snamo),sep="");

agesexmask <- array(data=NA,dim=c(length(sn),1),dimnames=list(mask,c("number")));
agesexcatmask <- array(data=NA,dim=c(length(sn),ncol(tab_N_agesexplus_c)),dimnames=list(mask,dimnames(tab_N_agesexplus_c)[[2]]));

##############################

agesexcatmask[dimnames(tab_N_agesexplus_c)[[1]],dimnames(tab_N_agesexplus_c)[[2]]] <- tab_N_agesexplus_c; 

write("", file=WriteRFile, append=TRUE);
write("Numbers of fish in samples per age, sex and market category", file=WriteRFile, append=TRUE);
outm <- as.matrix(cbind(dimnames(agesexcatmask)[[1]],agesexcatmask));
dimnames(outm)[[2]][1] <- "sex_age";
write.table(t(dimnames(outm)[[2]]), file=WriteRFile, sep="\t", append=TRUE,col.names=FALSE, row.names=FALSE, quote=FALSE);
write.table(outm, file=WriteRFile, sep="\t", quote=F, col.names=FALSE, row.names=FALSE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);

agesexmask[dimnames(tab_Numb_agesexplus)[[1]],"number"] <- tab_Numb_agesexplus; 
write("Numbers @ age and sex", file=WriteRFile, append=TRUE);
write.table(round(agesexmask,digits=1), file=WriteRFile, sep="\t", quote=F, col.names=FALSE,row.names=TRUE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);


write("Numbers @ age", file=WriteRFile, append=TRUE);
write.table(tab_Numb_age, file=WriteRFile, quote=F, sep=" ; ", col.names=FALSE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);


#######################################
# biological (stock) estimates
#######################################

#
# sex ratio
#
write("############", file=WriteRFile, append=TRUE);
write("BIOLOGICAL ESTIMATES", file=WriteRFile, append=TRUE);
write("############", file=WriteRFile, append=TRUE);

tsex <- substr(dimnames(tab_Numb_agesexplus)[[1]],start=1,stop=1);
fratio <- sum(tab_Numb_agesexplus[tsex=="f"])/sum(tab_Numb_agesexplus);

write("Sex ratio", file=WriteRFile, append=TRUE);
write(fratio, file=WriteRFile, append=TRUE);
write("############", file=WriteRFile, append=TRUE);

#
# age and sex
#

# proportions of ages across market categories

tab_PB_agesexplus_c <- tab_N_agesexplus_c;
for ( i in 1:nrow(tab_PB_agesexplus_c) ) { tempv <- tab_P_agesexplus_c[i,]*tab_Numb_c;
                                            tab_PB_agesexplus_c[i,] <- tempv/sum(tempv) };
                                            
# mean length at age and sex 
tab_MLAS <- tapply(Nsnij$LENGTH*100,list(as.factor(Nagesexplus),as.factor(Nsnij$SSE_CATEGORY)),mean);
tab_MLAS[is.na(tab_MLAS)] <- 0;

tab_MLAS <- rowSums(tab_MLAS*tab_PB_agesexplus_c);

# mean weight at age and sex
tab_MWAS <- tapply(Nsnij$CSS_WEIGHT,list(as.factor(Nagesexplus),as.factor(Nsnij$SSE_CATEGORY)),mean);
tab_MWAS[is.na(tab_MWAS)] <- 0;
tab_MWAS <- rowSums(tab_MWAS*tab_PB_agesexplus_c);


agesexmask[names(tab_MLAS),"number"] <- tab_MLAS; 
write("Mean length @ age and sex (cm)", file=WriteRFile, append=TRUE);
write.table(round(agesexmask,digits=3), file=WriteRFile, quote=F, sep=" ; ", col.names=FALSE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);

agesexmask[names(tab_MWAS),"number"] <- tab_MWAS; 
write("Mean weight @ age and sex (grams)", file=WriteRFile, append=TRUE);
write.table(round(agesexmask,digits=3), file=WriteRFile, quote=F, sep=" ; ", col.names=FALSE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);



#
# age only
#


# proportions of ages across market categories

tab_PB_age_c <- tab_N_age_c;
for ( i in 1:nrow(tab_PB_age_c) ) { tempv <- tab_P_age_c[i,]*tab_Numb_c
                                            tab_PB_age_c[i,] <- tempv/sum(tempv) };
   

# mean length at age and sex and market category
tab_MLA <- tapply(Nsnij$LENGTH*100,list(as.factor(Nage),as.factor(Nsnij$SSE_CATEGORY)),mean);
tab_MLA[is.na(tab_MLA)] <- 0;

tab_MLA <- rowSums(tab_MLA*tab_PB_age_c);

# mean weight at age
tab_MWA <- tapply(Nsnij$CSS_WEIGHT,list(as.factor(Nage),as.factor(Nsnij$SSE_CATEGORY)),mean);
tab_MWA[is.na(tab_MWA)] <- 0;
tab_MWA <- rowSums(tab_MWA*tab_PB_age_c);


write("Mean length @ age (cm)", file=WriteRFile, append=TRUE);
write.table(tab_MLA, file=WriteRFile, quote=F, sep=" ; ", col.names=FALSE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);

write("Mean weight @ age (grams)", file=WriteRFile, append=TRUE);
write.table(tab_MWA, file=WriteRFile, quote=F, sep=" ; ", col.names=FALSE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);


#############
# sum of products
#############

write("Total Landed Weight from logbooks (kg)", file=WriteRFile, append=TRUE);
write(sum(Ntab_W_c), file=WriteRFile, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);
write("Total Landed Weight according to sum of product (weight@age * numbers@age) (kg)", file=WriteRFile, append=TRUE);
SOM <- sum( (t(tab_MWA)/1000) * t(tab_Numb_age) )
write(SOM, file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);
write("ratio of the two total landed weights:", file=WriteRFile, append=TRUE);
write(sum(Ntab_W_c) / (sum( (t(tab_MWA)/1000) * t(tab_Numb_age) )), file=WriteRFile, append=TRUE );


##############
# numbers at length
##############
LL <- as.integer(Nsnij$LENGTH*1000);
LLfact <- rep(NA,length(LL));
for ( ii in 1:(length(intL)-1) ) {    
      LLfact[LL>=intL[ii] & LL<intL[ii+1]] <- ii;      
};       


tab_N_length_c <- tapply(rep(1,nrow(Nsnij)),list(as.factor(LLfact),as.factor(Nsnij$SSE_CATEGORY)),sum);
tab_N_length_c[is.na(tab_N_length_c)] <- 0;

tab_P_length_c <- tab_N_length_c;
for ( i in names(Ntab_W_c) ) { tab_P_length_c[,i] <- tab_N_length_c[,i]/tab_N_c[i]; };

# estimated total numbers of fish per length category
tab_Numb_length <- tab_P_length_c %*% tab_Numb_c;
dimnames(tab_Numb_length)[[1]] <- intLnames[as.numeric(dimnames(tab_Numb_length)[[1]])];

write("", file=WriteRFile, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);
write("Numbers @ length ", file=WriteRFile, append=TRUE);
write.table(tab_Numb_length, file=WriteRFile, quote=F, sep="\t", col.names=FALSE, row.names=TRUE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);


##############
# numbers at length and sex
##############

LLfactS <- paste(Nsnij$GENDER,LLfact,sep="");


tab_N_lengthsex_c <- tapply(rep(1,nrow(Nsnij)),list(as.factor(LLfactS),as.factor(Nsnij$SSE_CATEGORY)),sum);
tab_N_lengthsex_c[is.na(tab_N_lengthsex_c)] <- 0;


tab_P_lengthsex_c <- tab_N_lengthsex_c;
for ( i in names(Ntab_W_c) ) { tab_P_lengthsex_c[,i] <- tab_N_lengthsex_c[,i]/tab_N_c[i]; };

# estimated total numbers of fish per length category
tab_Numb_lengthsex <- tab_P_lengthsex_c %*% tab_Numb_c;

temNames <- paste(substr(dimnames(tab_Numb_lengthsex)[[1]],start=1,stop=1),intLnames[as.numeric(substr(dimnames(tab_Numb_lengthsex)[[1]],start=2,stop=3))],sep="");

dimnames(tab_Numb_lengthsex)[[1]] <- temNames;
    
write("Numbers @ length and sex ", file=WriteRFile, append=TRUE);
write.table(tab_Numb_lengthsex, file=WriteRFile, quote=F, sep="\t", col.names=FALSE, row.names=TRUE, append=TRUE);
write("############", file=WriteRFile, append=TRUE);
write("", file=WriteRFile, append=TRUE);


#############

if (Ntpr) { print(paste("Estimates of Numbers@Age obtained")); };

thereslist <- list(fratio=fratio, tab_MLA=tab_MLA, tab_MWA=tab_MWA, tab_MLAS=tab_MLAS, tab_MWAS=tab_MWAS, tab_N_ID=tab_N_ID, tab_N_ID_CAT=tab_N_ID_CAT, tab_N_c=tab_N_c, tab_N_agesexplus_c=tab_N_agesexplus_c, tab_P_age_c=tab_P_age_c, tab_N_age_c=tab_N_age_c, tab_P_agesexplus_c=tab_P_agesexplus_c, tab_P_length_c=tab_P_length_c, tab_N_length_c=tab_N_length_c, tab_w_c=tab_w_c, tab_Numb_c=tab_Numb_c, tab_Numb_length=tab_Numb_length, tab_Numb_lengthsex=tab_Numb_lengthsex, tab_Numb_age=tab_Numb_age, tab_Numb_agesexplus=tab_Numb_agesexplus, Ntab_W_c=Ntab_W_c);

return(thereslist);

};




################################################
#
#
# raising with bootstrapping
#
#
################################################
#
##
#RNC <- NC;
#Nsnij <- dat$snij;
#Nmarket_category <- dat$market_category;
#Ntab_W_c <- dat$tab_W_c;
#Nagesexplus <- dat$agesexplus;
#Nage <- dat$age;
#Nagesex <- dat$agesex;
#Ntpr <- TRUE;
#WriteRFile <- QRaisingFile
#intL <- seq(200,800,25);
#intLnames <- MYintLnames;
#Ncuryear <- curyear;
#Nplusgroup <- plusgroupage;
#Nminage <- minage;
#Nmaxage <- maxage;
#NBOOT <- 10;
#Nspec <- species; 
#


RaiseNumbersBOOT <- function(Nspec=species, Ncuryear=curyear, Nplusgroup=plusgroupage, Nsnij=dat$snij, Ntab_W_c=dat$tab_W_c, intL=seq(minL,maxL,50), intLnames=MYintLnames, Ntpr=TRUE, WriteRFile=Rfile, Nminage=1, Nmaxage=25, NBOOT=10) {


# numbers/weights/lengths at age
Btab_MWA <- Btab_MLA <- Btab_Numb_age <- array(data=NA, dim=c((Nmaxage-Nminage+1),NBOOT),dimnames=list(Nminage:Nmaxage,1:NBOOT));

Bfratio <- NULL;

Btab_Numb_length <- array(data=NA, dim=c(length(MYintLnames),NBOOT),dimnames=list(MYintLnames,1:NBOOT));

BOOTlevs <- as.numeric(as.character(levels(as.factor(Nsnij$ID))));

theT <- qt(0.975,nlevels(as.factor(Nsnij$ID))-1);


for ( BB in 1:NBOOT ) {

#print(BB);
BOOTdat <- NULL;
BOOTID <- sample(BOOTlevs,size=length(BOOTlevs),replace=TRUE);
for ( kki in BOOTID ) {
    BOOTdat <- rbind(BOOTdat,Nsnij[Nsnij$ID==kki,]);
};
#nrow(BOOTdat)
      


Nage <- Ncuryear - BOOTdat$YEARCLASS;

# age variable with plus group

Nageplus <- Nage; Nageplus[Nageplus>=Nplusgroup] <- Nplusgroup;

# age-sex combination

Nagesex <- paste(BOOTdat$GENDER,Nage,sep="");

Nagesexplus <- paste(BOOTdat$GENDER,Nageplus,sep="");


# ID is de identifier for a visit to a market: typicaly 60 fish (15 per market category) but can be a bit more or less
tab_N_ID <- tapply(rep(1,nrow(BOOTdat)),as.factor(BOOTdat$ID),sum);

# ID per SSE_CATEGORY (market category): typically approx. 15 fish
tab_N_ID_CAT <- table(as.factor(BOOTdat$ID),as.factor(BOOTdat$SSE_CATEGORY));

# numbers of fish sampled per market category
tab_N_c <- tapply(rep(1,nrow(BOOTdat)),as.factor(BOOTdat$SSE_CATEGORY),sum);
                                                                  
                                                                 
SUMW <- tapply(Nsnij$SSE_WEIGHT,list(as.factor(Nsnij$ID),as.factor(Nsnij$SSE_CATEGORY)),sum);
LC <- tapply(Nsnij$SSE_WEIGHT,list(as.factor(Nsnij$ID),as.factor(Nsnij$SSE_CATEGORY)),length);
TOTW <- SUMW/LC;  TOTW <- na.omit(TOTW); TOTW <- colSums(TOTW);

  if (Nspec == "'TUR'" & names(TOTW)[1]=="+" & names(TOTW)[2]=="1") { 
  
      Ntab_W_c_new <- array(data=NA, dim=c(7),dimnames=list(c("+","1","2","3","4","5","6")));
      Ntab_W_c_new[c("2","3","4","5","6")] <- Ntab_W_c[c("2","3","4","5","6")];
      propplus <- (TOTW["+"]/sum(TOTW[c("+","1")]))
      Ntab_W_c_new["+"] <-  propplus*Ntab_W_c["1"];
      Ntab_W_c_new["1"] <-  (1-propplus)*Ntab_W_c["1"];

      Ntab_W_c <- Ntab_W_c_new;
   };

  if (Nspec == "'BLL'" & names(TOTW)[1]=="+" & names(TOTW)[2]=="1") { 
  
      Ntab_W_c_new <- array(data=NA, dim=c(4),dimnames=list(c("+","1","2","3")));
      Ntab_W_c_new[c("2","3")] <- Ntab_W_c[c("2","3")];
      propplus <- (TOTW["+"]/sum(TOTW[c("+","1")]))
      Ntab_W_c_new["+"] <-  propplus*Ntab_W_c["1"];
      Ntab_W_c_new["1"] <-  (1-propplus)*Ntab_W_c["1"];

      Ntab_W_c <- Ntab_W_c_new;
   };



##############################
# numbers at age and sex
##############################

tab_N_agesexplus_c <- tapply(rep(1,nrow(BOOTdat)),list(as.factor(Nagesexplus),as.factor(BOOTdat$SSE_CATEGORY)),sum);
tab_N_agesexplus_c[is.na(tab_N_agesexplus_c)] <- 0;

tab_P_agesexplus_c <- tab_N_agesexplus_c;
for ( i in names(Ntab_W_c) ) { tab_P_agesexplus_c[,i] <- tab_N_agesexplus_c[,i]/tab_N_c[i]; };

# mean weight of fish in samples per market category
tab_w_c <- tapply((BOOTdat$CSS_WEIGHT/1000),as.factor(BOOTdat$SSE_CATEGORY),mean);

# estimated total numbers of fish per market category
tab_Numb_c <- Ntab_W_c[levels(as.factor(BOOTdat$SSE_CATEGORY))]/tab_w_c;

# estimated total numbers of fish per age-sex category
tab_Numb_agesexplus <- tab_P_agesexplus_c %*% tab_Numb_c;

##############################
# numbers at age 
##############################

tab_N_age_c <- tapply(rep(1,nrow(BOOTdat)),list(as.factor(Nage),as.factor(BOOTdat$SSE_CATEGORY)),sum);
tab_N_age_c[is.na(tab_N_age_c)] <- 0;

tab_P_age_c <- tab_N_age_c;
for ( i in names(Ntab_W_c) ) { tab_P_age_c[,i] <- tab_N_age_c[,i]/tab_N_c[i]; };

# estimated total numbers of fish per age-sex category
tab_Numb_age <- tab_P_age_c %*% tab_Numb_c;


Btab_Numb_age[dimnames(tab_Numb_age)[[1]],BB] <- tab_Numb_age;

Btab_Numb_age[is.na(Btab_Numb_age[,BB]),BB] <- 0;

#######################################
# biological (stock) estimates
#######################################

#
# sex ratio
#

tsex <- substr(dimnames(tab_Numb_agesexplus)[[1]],start=1,stop=1);
fratio <- sum(tab_Numb_agesexplus[tsex=="f"])/sum(tab_Numb_agesexplus);
Bfratio <- cbind(Bfratio,fratio);

#
# age and sex
#

# proportions of ages across market categories

tab_PB_agesexplus_c <- tab_N_agesexplus_c;
for ( i in 1:nrow(tab_PB_agesexplus_c) ) { tempv <- tab_P_agesexplus_c[i,]*tab_Numb_c
                                            tab_PB_agesexplus_c[i,] <- tempv/sum(tempv) };
                                            
# mean length at age and sex 
tab_MLAS <- tapply(BOOTdat$LENGTH*100,list(as.factor(Nagesexplus),as.factor(BOOTdat$SSE_CATEGORY)),mean);
tab_MLAS[is.na(tab_MLAS)] <- 0;

tab_MLAS <- rowSums(tab_MLAS*tab_PB_agesexplus_c);

# mean weight at age and sex
tab_MWAS <- tapply(BOOTdat$CSS_WEIGHT,list(as.factor(Nagesexplus),as.factor(BOOTdat$SSE_CATEGORY)),mean);
tab_MWAS[is.na(tab_MWAS)] <- 0;
tab_MWAS <- rowSums(tab_MWAS*tab_PB_agesexplus_c);

#
# age only
#


# proportions of ages across market categories

tab_PB_age_c <- tab_N_age_c;
for ( i in 1:nrow(tab_PB_age_c) ) { tempv <- tab_P_age_c[i,]*tab_Numb_c
                                            tab_PB_age_c[i,] <- tempv/sum(tempv) };
   

# mean length at age and sex and market category
tab_MLA <- tapply(BOOTdat$LENGTH*100,list(as.factor(Nage),as.factor(BOOTdat$SSE_CATEGORY)),mean);
tab_MLA[is.na(tab_MLA)] <- 0;

tab_MLA <- rowSums(tab_MLA*tab_PB_age_c);

Btab_MLA[names(tab_MLA),BB] <- tab_MLA;

# mean weight at age
tab_MWA <- tapply(BOOTdat$CSS_WEIGHT,list(as.factor(Nage),as.factor(BOOTdat$SSE_CATEGORY)),mean);
tab_MWA[is.na(tab_MWA)] <- 0;
tab_MWA <- rowSums(tab_MWA*tab_PB_age_c);
Btab_MWA[names(tab_MWA),BB] <- tab_MWA;

##############
# numbers at length
##############
LL <- as.integer(BOOTdat$LENGTH*1000);
LLfact <- rep(NA,length(LL));
for ( ii in 1:(length(intL)-1) ) {
    
      LLfact[LL>=intL[ii] & LL<intL[ii+1]] <- ii;
      
};       


tab_N_length_c <- tapply(rep(1,nrow(BOOTdat)),list(as.factor(LLfact),as.factor(BOOTdat$SSE_CATEGORY)),sum);
tab_N_length_c[is.na(tab_N_length_c)] <- 0;

tab_P_length_c <- tab_N_length_c;
for ( i in names(Ntab_W_c) ) { tab_P_length_c[,i] <- tab_N_length_c[,i]/tab_N_c[i]; };

# estimated total numbers of fish per length category
tab_Numb_length <- tab_P_length_c %*% tab_Numb_c;
dimnames(tab_Numb_length)[[1]] <- intLnames[as.numeric(dimnames(tab_Numb_length)[[1]])];

Btab_Numb_length[dimnames(tab_Numb_length)[[1]],BB] <- tab_Numb_length;
   
Btab_Numb_length[is.na(Btab_Numb_length[,BB]),BB] <- 0;

};

SDBtab_MWA <- SDBtab_MLA <- SDBtab_Numb_age <- array(data=NA, dim=c(Nmaxage-Nminage+1),dimnames=list(Nminage:Nmaxage));

SDBfratio <- var(as.vector(Bfratio));

SDBtab_Numb_length <- array(data=NA, dim=c(length(MYintLnames)),dimnames=list(MYintLnames));

for (ttt in dimnames(SDBtab_MWA)[[1]] ) {
      SDBtab_MWA[ttt] <- var(Btab_MWA[ttt,])
      SDBtab_MLA[ttt] <- var(Btab_MLA[ttt,])
      SDBtab_Numb_age[ttt] <- var(Btab_Numb_age[ttt,]);      
};


for (ttt in dimnames(SDBtab_Numb_length)[[1]] ) {
      SDBtab_Numb_length[ttt] <- var(Btab_Numb_length[ttt,]);
};
      




#############


thereslist <- list(CVfratio=SDBfratio, CVBtab_MWA=SDBtab_MWA, CVBtab_MLA=SDBtab_MLA, CVBtab_Numb_age=SDBtab_Numb_age,CVBtab_Numb_length=SDBtab_Numb_length);

return(thereslist);

};



##############################################################
#
#
#
#
##############################################################



GetDerivativesAgeSex <- function(DNC=NC, Dtab_P_agesexplus_c=RaiseN$tab_P_agesexplus_c, Dtab_W_c=dat$tab_W_c, Dtab_w_c=RaiseN$tab_w_c, Dtab_Numb_c=RaiseN$tab_Numb_c) {

# STEP 1 the vector of first derivatives

temd <- matrix(nrow=2*DNC,ncol=nrow(Dtab_P_agesexplus_c));

for ( jj in 1:nrow(Dtab_P_agesexplus_c) ) {

  temco <- 1;
  for ( ii in 1:DNC ) {

    temd[temco,jj] <- (-Dtab_P_agesexplus_c[jj,ii] * Dtab_W_c[ii])/Dtab_w_c[ii]^2;
    temd[temco+1,jj] <- Dtab_Numb_c[ii];
   
    temco <- temco+2;
  };
  
};
temd <- as.data.frame(temd);
names(temd) <-  dimnames(RaiseN$tab_P_agesexplus_c)[[1]];

print("derivatives Obtained");

return(temd);

};






GetDerivativesAge <- function(DNC=NC, Dtab_P_age_c=RaiseN$tab_P_age_c, Dtab_W_c=dat$tab_W_c, Dtab_w_c=RaiseN$tab_w_c, Dtab_Numb_c=RaiseN$tab_Numb_c) {

# STEP 1 the vector of first derivatives

temd <- matrix(nrow=2*DNC,ncol=nrow(Dtab_P_age_c));

for ( jj in 1:nrow(Dtab_P_age_c) ) {

  temco <- 1;
  for ( ii in 1:DNC ) {

    temd[temco,jj] <- (-Dtab_P_age_c[jj,ii] * Dtab_W_c[ii])/Dtab_w_c[ii]^2;
    temd[temco+1,jj] <- Dtab_Numb_c[ii];
   
    temco <- temco+2;
  };
  
};
temd <- as.data.frame(temd);
names(temd) <-  dimnames(RaiseN$tab_P_age_c)[[1]];

print("derivatives Obtained");

return(temd);

};




GetDerivativesLength <- function(DNC=NC, Dtab_P_length_c=RaiseN$tab_P_length_c, Dtab_W_c=dat$tab_W_c, Dtab_w_c=RaiseN$tab_w_c, Dtab_Numb_c=RaiseN$tab_Numb_c) {

# STEP 1 the vector of first derivatives

temd <- matrix(nrow=2*DNC,ncol=nrow(Dtab_P_length_c));

for ( jj in 1:nrow(Dtab_P_length_c) ) {

  temco <- 1;
  for ( ii in 1:DNC ) {

    temd[temco,jj] <- (-Dtab_P_length_c[jj,ii] * Dtab_W_c[ii])/Dtab_w_c[ii]^2;
    temd[temco+1,jj] <- Dtab_Numb_c[ii];
   
    temco <- temco+2;
  };
  
};
temd <- as.data.frame(temd);
names(temd) <-  dimnames(RaiseN$tab_P_length_c)[[1]];

print("derivatives Obtained");

return(temd);

};




#############################################
#
# FUNCTION 4: the variance_covariance matrix
#             using the Delta method
#############################################


#
#
#VNC=NC
#Vcuryear=curyear
#Vplusgroup=plusgroupage
#Vsnij=dat$snij
#Vtab_P_age_c=RaiseN$tab_P_age_c
#Vtab_N_c=RaiseN$tab_N_c
#Vtab_Numb_age=RaiseN$tab_Numb_age
#Vcurind=1
#VDerivs=Derivs
#VNIDAll=dat$NIDAll
#


VarMatrixSimpleAge <- function(VNC=NC, Vcuryear=curyear,Vplusgroup=plusgroupage, Vsnij=dat$snij, Vtab_P_age_c=RaiseN$tab_P_age_c, Vtab_N_c=RaiseN$tab_N_c, Vtab_Numb_age=RaiseN$tab_Numb_age, VDerivs=Derivs, VNIDAll=dat$NIDAll) {


Vage <- Vcuryear - Vsnij$YEARCLASS;

# age variable with plus group

Vageplus <- Vage; Vageplus[Vageplus>=Vplusgroup] <- Vplusgroup;

# age-sex combination

Vagesex <- paste(Vsnij$GENDER,Vage,sep="");

Vagesexplus <- paste(Vsnij$GENDER,Vageplus,sep="");



# Across-sample and Between-sample variance-covariance matrix
VARCOVARAcr <- VARCOVARBetw <- matrix(rep(0,4*VNC*VNC),nrow=2*VNC, ncol=2*VNC);

# STEP 3b.1: elements on the diagonal

# estimated variances of mean weights of the NC market categories
temvar_W <- tapply(Vsnij$CSS_WEIGHT/1000,as.factor(Vsnij$SSE_CATEGORY),var);
var_meanW <- temvar_W/Vtab_N_c;

# estimated variances of the proportion of age-sex category
temvar <- Vtab_P_age_c * (1-Vtab_P_age_c);
var_meanP <- t(t(temvar)/as.vector(Vtab_N_c)); 

  # variance on mean fish weights computed using sample mean weights
  smw <- t(tapply(Vsnij$CSS_WEIGHT/1000,list(as.factor(Vsnij$SSE_CATEGORY),as.factor(Vsnij$ID)),mean));
  var_meanW_sample <- rep(NA,VNC); 
  for ( ki in 1:VNC ) { var_meanW_sample[ki] <- var(na.omit(smw[,ki]))/length(na.omit(smw)); };

  # variance on proportions computed using sample mean proportions

  tab_N_age_c_sample <- tapply(rep(1,nrow(Vsnij)),list(as.factor(Vsnij$ID),as.factor(Vage),as.factor(Vsnij$SSE_CATEGORY)),sum);
  tab_N_age_c_sample[is.na(tab_N_age_c_sample)] <- 0;

  var_meanP_sample <- NULL;
  for (ggi in 1:VNC ) {
   
   temrs <- rowSums( tab_N_age_c_sample[,,ggi]);
   temtab <- tab_N_age_c_sample[temrs!=0,,ggi] / temrs[temrs!=0];
                                                         
    temvar <- NULL;                                                     
    for ( kki  in 1:ncol(temtab) ) {
      temvar <- c(temvar,var(temtab[,kki])/nrow(temtab));
    };
    
   var_meanP_sample <- cbind(var_meanP_sample, temvar);
    
  };

  var_meanP_sample <- data.frame(var_meanP_sample, row.names=levels(as.factor(Vage)));
  names(var_meanP_sample) <- 1:VNC;


# the diagonal of the variance-covariance matrix

# get the relevent class

resvarianceFISH <- array(data=NA,dim=c(nlevels(as.factor(Vage)),2),dimnames=list(levels(as.factor(Vage)),c("cv","variance")))
resvarianceSAMPLE <- array(data=NA,dim=c(nlevels(as.factor(Vage)),2),dimnames=list(levels(as.factor(Vage)),c("cv","variance")))

for ( aa in levels(as.factor(Vage)) ) {

temco <- 1;
for ( ii in 1:VNC ) {
    VARCOVARAcr[temco,temco] <- var_meanW[ii];
    VARCOVARAcr[temco+1,temco+1] <- var_meanP[aa,ii];

    VARCOVARBetw[temco,temco] <- var_meanW_sample[ii];
    VARCOVARBetw[temco+1,temco+1] <- var_meanP_sample[aa,ii];
       
    temco <- temco+2;
};



# RECOGNISE THAT ALL ELEMENTS IN THE VARIANCE-COVARIANCE MATRIX 
# THAT CORRESPOND TO MARKET CATEGORIES WITH NOT A SINGLE FISH OF THE AGE-SEX CATEGORY
# SHOULD BE SET TO ZERO

derivvect <- VDerivs[,aa];
temind <- ifelse(derivvect==0,1:length(derivvect),0); temind <- temind[temind!=0];
derivvect[c(temind+1)] <- 0;

VARCOVARAcr[c(temind),] <- 0;
VARCOVARAcr[,c(temind)] <- 0;

VARCOVARBetw[c(temind),] <- 0;
VARCOVARBetw[,c(temind)] <- 0;

###################################
#
# Finally: matrix-vector product
#
###################################

estsd1 <- sqrt(t(derivvect)%*%VARCOVARAcr%*%derivvect);
coefv1 <- estsd1/Vtab_Numb_age[aa,];

estsd2 <- sqrt(t(derivvect)%*%VARCOVARBetw%*%derivvect);
coefv2 <- estsd2/Vtab_Numb_age[aa,];

resvarianceSAMPLE[aa,"variance"] <- estsd2;
resvarianceSAMPLE[aa,"cv"] <- coefv2;

resvarianceFISH[aa,"variance"] <- estsd1;
resvarianceFISH[aa,"cv"] <- coefv1;

};


# RETURN
#print("Analytical method Finished");
retlist <- list(varFISH=resvarianceFISH,varSAMPLE=resvarianceSAMPLE);

return(retlist);

};



#
#
#VintL=MYintL
#VNC=NC
#Vcuryear=curyear
#Vplusgroup=plusgroupage
#Vsnij=dat$snij
#Vtab_P_length_c=RaiseN$tab_P_length_c
#Vtab_N_c=RaiseN$tab_N_c
#Vtab_Numb_length=RaiseN$tab_Numb_length
#Vcurind=1
#VDerivs=Derivs
#VNIDAll=dat$NIDAll
#
#

VarMatrixSimpleLength <- function(VintL=MYintL, VNC=NC, Vcuryear=curyear,Vplusgroup=plusgroupage, Vsnij=dat$snij, Vtab_P_length_c=RaiseN$tab_P_length_c, Vtab_N_c=RaiseN$tab_N_c, Vtab_Numb_length=RaiseN$tab_Numb_length, VDerivs=Derivs, VNIDAll=dat$NIDAll) {

LL <- as.integer(Vsnij$LENGTH*1000);
LLfact <- rep(NA,length(LL));
for ( ii in 1:(length(VintL)-1) ) {
    
      LLfact[LL>=VintL[ii] & LL<VintL[ii+1]] <- ii;
      
};       


# Across-sample and Between-sample variance-covariance matrix
VARCOVARAcr <- VARCOVARBetw <- matrix(rep(0,4*VNC*VNC),nrow=2*VNC, ncol=2*VNC);

# STEP 3b.1: elements on the diagonal

# estimated variances of mean weights of the NC market categories
temvar_W <- tapply(Vsnij$CSS_WEIGHT/1000,as.factor(Vsnij$SSE_CATEGORY),var);
var_meanW <- temvar_W/Vtab_N_c;

# estimated variances of the proportion of length category
temvar <- Vtab_P_length_c * (1-Vtab_P_length_c);
var_meanP <- t(t(temvar)/as.vector(Vtab_N_c)); 

  # variance on mean fish weights computed using sample mean weights
  smw <- t(tapply(Vsnij$CSS_WEIGHT/1000,list(as.factor(Vsnij$SSE_CATEGORY),as.factor(Vsnij$ID)),mean));
  var_meanW_sample <- rep(NA,VNC); 
  for ( ki in 1:VNC ) { var_meanW_sample[ki] <- var(na.omit(smw[,ki]))/length(na.omit(smw)); };

  # variance on proportions computed using sample mean proportions

  tab_N_length_c_sample <- tapply(rep(1,nrow(Vsnij)),list(as.factor(Vsnij$ID),as.factor(LLfact),as.factor(Vsnij$SSE_CATEGORY)),sum);
  tab_N_length_c_sample[is.na(tab_N_length_c_sample)] <- 0;

  var_meanP_sample <- NULL;
  for (ggi in 1:VNC ) {
   
   temrs <- rowSums( tab_N_length_c_sample[,,ggi]);
   temtab <- tab_N_length_c_sample[temrs!=0,,ggi] / temrs[temrs!=0];
                                                         
    temvar <- NULL;                                                     
    for ( kki  in 1:ncol(temtab) ) {
      temvar <- c(temvar,var(temtab[,kki])/nrow(temtab));
    };
    
   var_meanP_sample <- cbind(var_meanP_sample, temvar);
    
  };

  var_meanP_sample <- data.frame(var_meanP_sample, row.names=levels(as.factor(LLfact)));
  names(var_meanP_sample) <- 1:VNC;


# the diagonal of the variance-covariance matrix

# get the relevent age-sex class

resvarianceFISH <- array(data=NA,dim=c(nlevels(as.factor(LLfact)),2),dimnames=list(levels(as.factor(LLfact)),c("cv","variance")))
resvarianceSAMPLE <- array(data=NA,dim=c(nlevels(as.factor(LLfact)),2),dimnames=list(levels(as.factor(LLfact)),c("cv","variance")))

for ( aa in levels(as.factor(LLfact)) ) {

temco <- 1;
for ( ii in 1:VNC ) {
    VARCOVARAcr[temco,temco] <- var_meanW[ii];
    VARCOVARAcr[temco+1,temco+1] <- var_meanP[aa,ii];

    VARCOVARBetw[temco,temco] <- var_meanW_sample[ii];
    VARCOVARBetw[temco+1,temco+1] <- var_meanP_sample[aa,ii];
       
    temco <- temco+2;
};



# RECOGNISE THAT ALL ELEMENTS IN THE VARIANCE-COVARIANCE MATRIX 
# THAT CORRESPOND TO MARKET CATEGORIES WITH NOT A SINGLE FISH OF THE AGE-SEX CATEGORY
# SHOULD BE SET TO ZERO

derivvect <- VDerivs[,aa];
temind <- ifelse(derivvect==0,1:length(derivvect),0); temind <- temind[temind!=0];
derivvect[c(temind+1)] <- 0;

VARCOVARAcr[c(temind),] <- 0;
VARCOVARAcr[,c(temind)] <- 0;

VARCOVARBetw[c(temind),] <- 0;
VARCOVARBetw[,c(temind)] <- 0;

###################################
#
# Finally: matrix-vector product
#
###################################

estsd1 <- sqrt(t(derivvect)%*%VARCOVARAcr%*%derivvect);
coefv1 <- estsd1/Vtab_Numb_length[as.character(VintL[as.numeric(aa)]),];

estsd2 <- sqrt(t(derivvect)%*%VARCOVARBetw%*%derivvect);
coefv2 <- estsd2/Vtab_Numb_length[as.character(VintL[as.numeric(aa)]),];


resvarianceSAMPLE[aa,"variance"] <- estsd2;
resvarianceSAMPLE[aa,"cv"] <- coefv2;

resvarianceFISH[aa,"variance"] <- estsd1;
resvarianceFISH[aa,"cv"] <- coefv1;

};


# RETURN
#print("Analytical method Finished");
retlist <- list(varFISH=resvarianceFISH,varSAMPLE=resvarianceSAMPLE);

return(retlist);

};



#
#VNC=NC
#Vcuryear=curyear
#Vplusgroup=plusgroupage
#Vsnij=dat$snij
#Vagesexplus=dimnames(RaiseN$tab_P_agesexplus_c)[[1]]
#Vtab_P_agesexplus_c=RaiseN$tab_P_agesexplus_c
#Vtab_N_c=RaiseN$tab_N_c
#Vtab_Numb_agesexplus=RaiseN$tab_Numb_agesexplus
#Vcurind=1
#VDerivs=Derivs
#VNIDAll=dat$NIDAll
#
#


VarMatrixSimpleAgeSex <- function(VNC=NC, Vcuryear=curyear,Vplusgroup=plusgroupage, Vsnij=dat$snij, Vtab_P_agesexplus_c=RaiseN$tab_P_agesexplus_c, Vtab_N_c=RaiseN$tab_N_c, Vtab_Numb_agesexplus=RaiseN$tab_Numb_agesexplus, VDerivs=Derivs, VNIDAll=dat$NIDAll) {


Vage <- Vcuryear - Vsnij$YEARCLASS;

# age variable with plus group

Vageplus <- Vage; Vageplus[Vageplus>=Vplusgroup] <- Vplusgroup;

# age-sex combination

Vagesex <- paste(Vsnij$GENDER,Vage,sep="");

Vagesexplus <- paste(Vsnij$GENDER,Vageplus,sep="");



# Across-sample and Between-sample variance-covariance matrix
VARCOVARAcr <- VARCOVARBetw <- matrix(rep(0,4*VNC*VNC),nrow=2*VNC, ncol=2*VNC);

# STEP 3b.1: elements on the diagonal

# estimated variances of mean weights of the NC market categories
temvar_W <- tapply(Vsnij$CSS_WEIGHT/1000,as.factor(Vsnij$SSE_CATEGORY),var);
var_meanW <- temvar_W/Vtab_N_c;

# estimated variances of the proportion of age-sex category
temvar <- Vtab_P_agesexplus_c * (1-Vtab_P_agesexplus_c);
var_meanP <- t(t(temvar)/as.vector(Vtab_N_c)); 

  # variance on mean fish weights computed using sample mean weights
  smw <- t(tapply(Vsnij$CSS_WEIGHT/1000,list(as.factor(Vsnij$SSE_CATEGORY),as.factor(Vsnij$ID)),mean));
  var_meanW_sample <- rep(NA,VNC); 
  for ( ki in 1:VNC ) { var_meanW_sample[ki] <- var(na.omit(smw[,ki]))/length(na.omit(smw)); };

  # variance on proportions computed using sample mean proportions

  tab_N_agesexplus_c_sample <- tapply(rep(1,nrow(Vsnij)),list(as.factor(Vsnij$ID),as.factor(Vagesexplus),as.factor(Vsnij$SSE_CATEGORY)),sum);
  tab_N_agesexplus_c_sample[is.na(tab_N_agesexplus_c_sample)] <- 0;

  var_meanP_sample <- NULL;
  for (ggi in 1:VNC ) {
   
   temrs <- rowSums( tab_N_agesexplus_c_sample[,,ggi]);
   temtab <- tab_N_agesexplus_c_sample[temrs!=0,,ggi] / temrs[temrs!=0];
                                                         
    temvar <- NULL;                                                     
    for ( kki  in 1:ncol(temtab) ) {
      temvar <- c(temvar,var(temtab[,kki])/nrow(temtab));
    };
    
   var_meanP_sample <- cbind(var_meanP_sample, temvar);
    
  };

  var_meanP_sample <- data.frame(var_meanP_sample, row.names=levels(as.factor(Vagesexplus)));
  names(var_meanP_sample) <- 1:VNC;


# the diagonal of the variance-covariance matrix

# get the relevent age-sex class

resvarianceFISH <- array(data=NA,dim=c(nlevels(as.factor(Vagesexplus)),2),dimnames=list(levels(as.factor(Vagesexplus)),c("cv","variance")))
resvarianceSAMPLE <- array(data=NA,dim=c(nlevels(as.factor(Vagesexplus)),2),dimnames=list(levels(as.factor(Vagesexplus)),c("cv","variance")))

for ( aa in levels(as.factor(Vagesexplus)) ) {

temco <- 1;
for ( ii in 1:VNC ) {
    VARCOVARAcr[temco,temco] <- var_meanW[ii];
    VARCOVARAcr[temco+1,temco+1] <- var_meanP[aa,ii];

    VARCOVARBetw[temco,temco] <- var_meanW_sample[ii];
    VARCOVARBetw[temco+1,temco+1] <- var_meanP_sample[aa,ii];
       
    temco <- temco+2;
};



# RECOGNISE THAT ALL ELEMENTS IN THE VARIANCE-COVARIANCE MATRIX 
# THAT CORRESPOND TO MARKET CATEGORIES WITH NOT A SINGLE FISH OF THE AGE-SEX CATEGORY
# SHOULD BE SET TO ZERO

derivvect <- VDerivs[,aa];
temind <- ifelse(derivvect==0,1:length(derivvect),0); temind <- temind[temind!=0];
derivvect[c(temind+1)] <- 0;

VARCOVARAcr[c(temind),] <- 0;
VARCOVARAcr[,c(temind)] <- 0;

VARCOVARBetw[c(temind),] <- 0;
VARCOVARBetw[,c(temind)] <- 0;

###################################
#
# Finally: matrix-vector product
#
###################################

estsd1 <- sqrt(t(derivvect)%*%VARCOVARAcr%*%derivvect);
coefv1 <- estsd1/Vtab_Numb_agesexplus[aa,];

estsd2 <- sqrt(t(derivvect)%*%VARCOVARBetw%*%derivvect);
coefv2 <- estsd2/Vtab_Numb_agesexplus[aa,];


resvarianceSAMPLE[aa,"variance"] <- estsd2;
resvarianceSAMPLE[aa,"cv"] <- coefv2;

resvarianceFISH[aa,"variance"] <- estsd1;
resvarianceFISH[aa,"cv"] <- coefv1;

};


# RETURN
#print("Analytical method Finished");
retlist <- list(varFISH=resvarianceFISH,varSAMPLE=resvarianceSAMPLE);

return(retlist);

};

















