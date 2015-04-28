# Extract eflalo formatted data #

EFLALO2 is the data format used in many EU projects and is the default format needed for the VMStools R-package developed within IMARES. The data contains vessel, metier, trip, catch composition and price data.

The starting date and end date need to be specified, as well as the nations for which you want to extract the data.

Extracting the full dataset at once is too complicated, therefore, we need three sql queries which need to be merged afterwards.

# SQL #
Query 1:
```

SELECT  platform_properties.PLM_CODE as VE_REF,
platform_properties.ID as VE_ID
,   platform_properties.PLM_CNY_CODE as VE_COU
,   platform_properties.LENGTH as VE_LEN
,   platform_properties.POWER as VE_KW
,   trips.TRIP_NUMBER as FT_REF
,   trips.PRT_CNY_CODE_DEPARTED_FROM as FT_DCOU
,   trips.PRT_CODE_DEPARTED_FROM as FT_DHAR
,   trips.DEPARTURE_DATE as FT_DDAT
,   TRIM(TO_CHAR(FLOOR(trips.DEPARTURE_TIME/100),'09'))||':'||TRIM(TO_CHAR(MOD(trips.DEPARTURE_TIME,100),'09')) as FT_DTIME
,   trips.PRT_CNY_CODE as FT_LCOU
,   trips.PRT_CODE as FT_LHAR
,   trips.ARRIVAL_DATE as FT_LDAT
,   TRIM(TO_CHAR(FLOOR(trips.ARRIVAL_TIME/100),'09'))||':'||TRIM(TO_CHAR(MOD(trips.ARRIVAL_TIME,100),'09')) as FT_LTIME
,   catches.txn_ices_code as LE_SP
,   catches.weight as LE_KG_SP
,   catches.fish_time as LE_TIME
,   catches.catch_seq_no as LE_SEQ
,   registrations.RGN_DATE as LE_CDAT
,   registrations.GPY_CODE as LE_GEAR
,   registrations.MESHSIZE as LE_MSZ
,   registrations.WIDTH as LE_WIDTH
,   nvl(quadrant_properties.ICES_QUADRANT,'UNKNOWN') AS LE_RECT
,   nvl(quadrant_properties.ICES_AREA,'UNKNOWN') AS LE_DIV
,   nvl(quadrant_properties.ICES_SUBAREA,'UNKNOWN') AS LE_SUBDIV
FROM trips, registrations, catches, platform_properties, quadrant_properties
WHERE trips.arrival_date BETWEEN '01-jan-2010' and '31-jan-2010' AND
platform_properties.plm_cny_code in 'nld' and
trips.ppy_id = platform_properties.id AND
trips.arrival_date between platform_properties.start_date and nvl(platform_properties.end_date,sysdate) AND
registrations.trp_arrival_date = trips.arrival_date  AND
registrations.trp_arrival_time = trips.arrival_time AND
registrations.trp_ppy_id = trips.ppy_id AND
registrations.trp_ppy_plm_code = trips.ppy_plm_code AND
registrations.trp_ppy_plm_cny_code = trips.ppy_plm_cny_code AND
registrations.trp_prt_cny_code = trips.prt_cny_code AND
registrations.trp_prt_code = trips.prt_code AND
registrations.trp_prt_cny_code = trips.prt_cny_code AND
quadrant_properties.ices_quadrant = registrations.qpy_ices_quadrant AND
catches.rgn_sre_code = registrations.sre_code AND
catches.rgn_trp_arrival_date = registrations.trp_arrival_date AND
catches.rgn_trp_arrival_time = registrations.trp_arrival_time AND
catches.rgn_trp_ppy_id = registrations.trp_ppy_id AND
catches.rgn_trp_ppy_plm_code = registrations.trp_ppy_plm_code AND
catches.rgn_trp_ppy_plm_cny_code = registrations.trp_ppy_plm_cny_code AND
catches.rgn_trp_prt_code = registrations.trp_prt_code AND
catches.rgn_trp_prt_cny_code = registrations.trp_prt_cny_code AND
catches.rgn_rgn_date = registrations.rgn_date;")
```
Query 2
```

SELECT metiers.TRIP_NUMBER as FT_REF
,	metiers.METIER as VE_FLT
FROM metiers;
```
Query 3. Note that the dates have shifted 8 days. The unload data can differ at most 7 days from the landings date, hence the shift of 8 days.
```

SELECT  size_classes.TXN_ICES_CODE as LE_SP
, 	size_classes.WEIGHT * size_classes.PRICE_KG as LE_EURO_SP
,   size_classes.WEIGHT as LE_KG_SP
,	  size_classes.TRIP_NUMBER as FT_REF
FROM  size_classes
WHERE size_classes.UNLOAD_DATE between '24-dec-2009' and '08-feb-2010';```
Finally, aggregate the first query over fishtime (LE\_TIME) and catch sequence (LE\_SEQ) for each unique combination. Thereafter, merge the results with the results of query 2 on FT\_REF (keep number of rows of first query only) and merge that result to query 3 on FT\_REF and LE\_SP (after the result of query 3 is aggregated over trip and species).
**Note however, that this data is not yet in the eflalo format and requires a number of transformations**

## In R ##
Use variable selection of startDate and endDate and specify Nations.
```

startDate <- '01-jan-2010'
endDate   <- '31-jan-2010'
Nations   <- 'nld'
```
Now make it flexible inside the code.
```

startDate <- paste("('",startDate,"')",sep="")
endDate   <- paste("('",endDate,"')",  sep="")
if(length(Nations)>1){
SQLvect <- c("(")
for ( rr in 1:(length(Nations)-1) )
SQLvect <- paste(SQLvect,"'",Nations[rr],"',",sep="")
SQLvect <- paste(SQLvect,"'",Nations[rr+1],"'",sep="")
SQLvect <- paste(SQLvect,")",sep="")
Nations <- SQLvect
} else { Nations <- paste("('",Nations,"')",sep="")}
```
Now the first query.
```

frst <- paste("SELECT  platform_properties.PLM_CODE as VE_REF,
platform_properties.ID as VE_ID
,   platform_properties.PLM_CNY_CODE as VE_COU
,   platform_properties.LENGTH as VE_LEN
,   platform_properties.POWER as VE_KW
,   trips.TRIP_NUMBER as FT_REF
,   trips.PRT_CNY_CODE_DEPARTED_FROM as FT_DCOU
,   trips.PRT_CODE_DEPARTED_FROM as FT_DHAR
,   trips.DEPARTURE_DATE as FT_DDAT
,   TRIM(TO_CHAR(FLOOR(trips.DEPARTURE_TIME/100),'09'))||':'||TRIM(TO_CHAR(MOD(trips.DEPARTURE_TIME,100),'09')) as FT_DTIME
,   trips.PRT_CNY_CODE as FT_LCOU
,   trips.PRT_CODE as FT_LHAR
,   trips.ARRIVAL_DATE as FT_LDAT
,   TRIM(TO_CHAR(FLOOR(trips.ARRIVAL_TIME/100),'09'))||':'||TRIM(TO_CHAR(MOD(trips.ARRIVAL_TIME,100),'09')) as FT_LTIME
,   catches.txn_ices_code as LE_SP
,   catches.weight as LE_KG_SP
,   catches.fish_time as LE_TIME
,   catches.catch_seq_no as LE_SEQ
,   registrations.RGN_DATE as LE_CDAT
,   registrations.GPY_CODE as LE_GEAR
,   registrations.MESHSIZE as LE_MSZ
,   registrations.WIDTH as LE_WIDTH
,   nvl(quadrant_properties.ICES_QUADRANT,'UNKNOWN') AS LE_RECT
,   nvl(quadrant_properties.ICES_AREA,'UNKNOWN') AS LE_DIV
,   nvl(quadrant_properties.ICES_SUBAREA,'UNKNOWN') AS LE_SUBDIV
FROM trips, registrations, catches, platform_properties, quadrant_properties
WHERE trips.arrival_date BETWEEN ",startDate," and ",endDate," AND
platform_properties.plm_cny_code in ",Nations," and
trips.ppy_id = platform_properties.id AND
trips.arrival_date between platform_properties.start_date and nvl(platform_properties.end_date,sysdate) AND
registrations.trp_arrival_date = trips.arrival_date  AND
registrations.trp_arrival_time = trips.arrival_time AND
registrations.trp_ppy_id = trips.ppy_id AND
registrations.trp_ppy_plm_code = trips.ppy_plm_code AND
registrations.trp_ppy_plm_cny_code = trips.ppy_plm_cny_code AND
registrations.trp_prt_cny_code = trips.prt_cny_code AND
registrations.trp_prt_code = trips.prt_code AND
registrations.trp_prt_cny_code = trips.prt_cny_code AND
quadrant_properties.ices_quadrant = registrations.qpy_ices_quadrant AND
catches.rgn_sre_code = registrations.sre_code AND
catches.rgn_trp_arrival_date = registrations.trp_arrival_date AND
catches.rgn_trp_arrival_time = registrations.trp_arrival_time AND
catches.rgn_trp_ppy_id = registrations.trp_ppy_id AND
catches.rgn_trp_ppy_plm_code = registrations.trp_ppy_plm_code AND
catches.rgn_trp_ppy_plm_cny_code = registrations.trp_ppy_plm_cny_code AND
catches.rgn_trp_prt_code = registrations.trp_prt_code AND
catches.rgn_trp_prt_cny_code = registrations.trp_prt_cny_code AND
catches.rgn_rgn_date = registrations.rgn_date;")
```
Query 2
```

scnd <- paste("SELECT metiers.TRIP_NUMBER as FT_REF
,	metiers.METIER as VE_FLT
FROM metiers;")
```
Time to extract these
```

library(RODBC)
conn <- odbcConnect(dsn="visstatp",uid=user,pwd=passwd,believeNRows=F)
dat1 <- sqlQuery(conn,frst,believeNRows=F)
dat2 <- sqlQuery(conn,scnd,believeNRows=F)
```
To ensure we do not extract too much price data, we make use of the results of query 1 (resulting in dat1) to get the time frame.
```

startDate <- format(min(dat1$FT_LDAT,na.rm=T) - (8*60*60*24),"%d-%b-%Y")
endDate   <- format(max(dat1$FT_LDAT,na.rm=T) + (8*60*60*24),"%d-%b-%Y")
startDate <- paste("('",startDate,"')",sep="")
endDate   <- paste("('",endDate,"')",  sep="")

thrd <- paste("SELECT  size_classes.TXN_ICES_CODE as LE_SP
, 	size_classes.WEIGHT * size_classes.PRICE_KG as LE_EURO_SP
,   size_classes.WEIGHT as LE_KG_SP
,	  size_classes.TRIP_NUMBER as FT_REF
FROM  size_classes
WHERE size_classes.UNLOAD_DATE between ",startDate," and ",endDate,";")

dat3 <- sqlQuery(conn,thrd,believeNRows=F)
```
All the data is retrieved from the database now. However, to construct the specific eflalo format, we need to aggregate some of these datasets, merge them together and reformat the structure.

First, get the formats right of each column
```

dat1$LE_SP    <- ac(dat1$LE_SP);    dat1$VE_REF   <- ac(dat1$VE_REF);   dat1$VE_COU     <- ac(dat1$VE_COU)
dat1$FT_DCOU  <- ac(dat1$FT_DCOU);  dat1$FT_DHAR  <- ac(dat1$FT_DHAR);  dat1$FT_DTIME   <- ac(dat1$FT_DTIME)
dat1$FT_LCOU  <- ac(dat1$FT_LCOU);  dat1$FT_LHAR  <- ac(dat1$FT_LHAR);  dat1$FT_LTIME   <- ac(dat1$FT_LTIME)
dat1$LE_GEAR  <- ac(dat1$LE_GEAR);  dat1$LE_RECT  <- ac(dat1$LE_RECT);  dat1$LE_DIV     <- ac(dat1$LE_DIV)
dat1$LE_SUBDIV<- ac(dat1$LE_SUBDIV);dat1$VE_ID    <- ac(dat1$VE_ID);
checkRow      <- nrow(dat1)
dat1          <- dat1[!duplicated(paste(dat1$FT_REF,  dat1$VE_REF,  dat1$VE_COU,  dat1$VE_LEN,  dat1$VE_KW, dat1$FT_DCOU,dat1$FT_DHAR,ac(dat1$FT_DDAT),dat1$FT_DTIME,dat1$VE_ID,
dat1$FT_LCOU,dat1$FT_LHAR,ac(dat1$FT_LDAT),dat1$FT_LTIME,
dat1$LE_CDAT, dat1$LE_GEAR, dat1$LE_MSZ,  dat1$LE_WIDTH,dat1$LE_RECT,dat1$LE_DIV,dat1$LE_SUBDIV,dat1$LE_SP,dat1$LE_KG_SP,dat1$LE_TIME,dat1$LE_SEQ)),]
if(checkRow != nrow(dat1)) stop("Extraction contains duplicate records which have now been removed, but clearly this is a bug")
```
Now we have to aggregate over fishtime and catch sequence. However, an easy aggregation it isn't due to the very long unique key.
```

clm2fact      <- function(x){return(as.factor(x))}
fact2clm      <- function(x,type){
if(type=="character") y <- ac(x)
if(type=="numeric")   y <- anf(x)
if(type=="integer")   y <- as.integer(anf(x))
if(type=="POSIXct")   y <- as.POSIXct(ac(x),format="%Y-%m-%d")
return(y)}
idx           <- grep("LE_KG_SP",colnames(dat1))
dat1a         <- dat1
for(i in (1:ncol(dat1))[-idx]) dat1a[,i] <- clm2fact(dat1[,i])

DT                    <- data.table(dat1a)
eq1                   <- c.listquote(paste("sum(","LE_KG_SP",",na.rm=T)",sep=""))
eq2                   <- c.listquote(c("FT_REF","VE_REF","VE_COU","VE_LEN","VE_KW","FT_DCOU","FT_DHAR","FT_DDAT","FT_DTIME",
"FT_LCOU","FT_LHAR","FT_LDAT","FT_LTIME","LE_CDAT","LE_GEAR","LE_MSZ","LE_WIDTH",
"LE_RECT","LE_DIV","LE_SUBDIV","LE_SP","VE_ID"))
byDat1                <- data.frame(DT[,eval(eq1),by=eval(eq2)])
colnames(byDat1)[length(colnames(byDat1))] <- "LE_KG_SP"
byDat1a               <- byDat1
for(i in 1:ncol(byDat1)){
iClass <- class(dat1[,colnames(byDat1)[i]])[1]
byDat1a[,i]         <- fact2clm(byDat1[,i],iClass)
}
dat1                  <- byDat1a
```
Hereafter, we can merge the first two datasets together
```

dat2$VE_FLT   <- ac(dat2$VE_FLT)
dat12               <- merge(dat1,dat2,by="FT_REF",all.x=T,all.y=F)
```
The price data is per disaggregated by size classes. We work towards a price per kilogram per species per trip.
```

dat3$LE_SP    <- ac(dat3$LE_SP)
dat3a               <- aggregate(cbind(dat3$LE_EURO_SP,dat3$LE_KG_SP),by=list(dat3$FT_REF,dat3$LE_SP),sum,na.rm=T)
colnames(dat3a)     <- c("FT_REF","LE_SP","LE_EURO_SP","LE_KG_SP")
dat3a$LE_EURO_KG_SP <- dat3a$LE_EURO_SP / dat3a$LE_KG_SP
dat3b               <- dat3a[,c("FT_REF","LE_SP","LE_EURO_KG_SP")]
```
And then merge the dat12 with the dat3b dataset on tripnumber (FT\_REF) and species (LE\_SP)
```

dat123              <- merge(dat12,dat3b,by=c("FT_REF","LE_SP"),all.x=T,all.y=F)
dat123$LE_EURO_SP   <- dat123$LE_KG_SP * dat123$LE_EURO_KG_SP
dat123              <- dat123[,-grep("LE_EURO_KG_SP",colnames(dat123))]
```
The eflalo format is column oriented rather than row oriented. Therefore, we need to create a new structure and appropriately fill that structure. It is just a lot of bookkeeping!
```

specNames           <- sort(unique(dat123$LE_SP))
colNames            <- c(paste("LE_KG_",specNames,sep=""),paste("LE_EURO_",specNames,sep=""))
dtf                 <- as.data.frame(matrix(NA,nrow=1,ncol=length(colNames),dimnames=list(1,colNames)))


x <- dat123[!duplicated(paste(dat123$FT_REF,  dat123$VE_REF,  dat123$VE_COU,  dat123$VE_LEN,  dat123$VE_KW, dat123$FT_DCOU,dat123$FT_DHAR,ac(dat123$FT_DDAT),dat123$FT_DTIME,dat123$VE_ID,
dat123$FT_LCOU,dat123$FT_LHAR,ac(dat123$FT_LDAT),dat123$FT_LTIME,
dat123$LE_CDAT, dat123$LE_GEAR, dat123$LE_MSZ,  dat123$LE_WIDTH,dat123$LE_RECT,dat123$LE_DIV,dat123$LE_SUBDIV,dat123$VE_FLT)),]
x <- cbind(x,dtf)
x <- x[,-which(colnames(x) %in% c("LE_SP","LE_KG_SP","LE_EURO_SP"))]


for(iSpec in specNames){
spc <- subset(dat123,LE_SP == iSpec)
kg  <- which(paste("LE_KG_",iSpec,sep="")==colnames(x))
eur <- which(paste("LE_EURO_",iSpec,sep="")==colnames(x))

idx <- pmatch(paste(spc$FT_REF,  spc$VE_REF,  spc$VE_COU,  spc$VE_LEN,  spc$VE_KW,  spc$FT_DCOU,spc$FT_DHAR,  ac(spc$FT_DDAT),spc$FT_DTIME,
spc$FT_LCOU,spc$FT_LHAR,  ac(spc$FT_LDAT),spc$FT_LTIME,
spc$LE_CDAT, spc$LE_GEAR, spc$LE_MSZ,  spc$LE_WIDTH,spc$LE_RECT,spc$LE_DIV, spc$LE_SUBDIV,   spc$VE_FLT,  spc$VE_ID),
paste(  x$FT_REF,    x$VE_REF,    x$VE_COU,    x$VE_LEN,    x$VE_KW,    x$FT_DCOU,  x$FT_DHAR,  ac(x$FT_DDAT),    x$FT_DTIME,
x$FT_LCOU,  x$FT_LHAR,  ac(x$FT_LDAT),    x$FT_LTIME,
x$LE_CDAT, x$LE_GEAR, x$LE_MSZ,  x$LE_WIDTH,x$LE_RECT,x$LE_DIV,x$LE_SUBDIV,x$VE_FLT,         x$VE_ID))
x[idx,kg]   <- spc$LE_KG_SP
x[idx,eur]  <- spc$LE_EURO_SP
}
```
Most is done and dusted now. Finally, fill out those columns we don't have in our database yet but are essential for the format and put the columns in the right order.
```

x$VE_TON    <- NA
x$LE_STIME  <- NA
x$LE_ETIME  <- NA
x$LE_SLAT   <- NA
x$LE_SLON   <- NA
x$LE_ELAT   <- NA
x$LE_ELON   <- NA
x$LE_ID     <- paste(x$FT_REF,x$LE_GEAR,x$LE_RECT,sep="-")
x$LE_MET    <- NA
x$FT_DDAT   <- format(x$FT_DDAT,"%d/%m/%Y")
x$FT_LDAT   <- format(x$FT_LDAT,"%d/%m/%Y")
x$LE_CDAT   <- format(x$LE_CDAT,"%d/%m/%Y")

eflaloOrder <- c("VE_REF","VE_ID","VE_FLT","VE_COU","VE_LEN","VE_KW","VE_TON","FT_REF",
"FT_DCOU","FT_DHAR","FT_DDAT","FT_DTIME","FT_LCOU","FT_LHAR",
"FT_LDAT","FT_LTIME","LE_ID","LE_CDAT","LE_STIME","LE_ETIME",
"LE_SLAT","LE_SLON","LE_ELAT","LE_ELON","LE_GEAR","LE_WIDTH",
"LE_MSZ","LE_RECT","LE_DIV","LE_SUBDIV","LE_MET",colNames)
x           <- x[,eflaloOrder]
```


