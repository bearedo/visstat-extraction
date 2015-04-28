# Extract tacsat formatted data #
TACSAT is the data format used in many EU projects and is the default format needed for the VMStools R-package developed within IMARES. It is very similar to 'normal' VMS data, but the column names follow the structure of the agreed TACSAT structure.

The starting date and end date need to be specified. An example is given below, as well as an R example with variable starting date and end date
## SQL ##
```

SELECT PPY_PLM_CNY_CODE as VE_COU,
PPY_ID as VE_REF,
LATITUDE as SI_LATI,
LONGITUDE as SI_LONG,
TO_CHAR(RGN_LOCAL_DATE,'dd/mm/yyyy') as SI_DATE,
TRIM(TO_CHAR(FLOOR(RGN_LOCAL_TIME/100),'09'))||':'||TRIM(TO_CHAR(MOD(RGN_LOCAL_TIME,100),'09')) as SI_TIME,
HEADING as SI_HE,
SPEED as SI_SP FROM vms
WHERE rgn_local_date BETWEEN '01-jan-2010' AND '31-jan-2010';
```

## In R ##
```

startDate <- "('01-jan-2010')"
endDate   <- "('31-dec-2010')"
```
or
```

startDate <- '01-jan-2010'
endDate   <- '31-jan-2010'
startDate <- paste("('",startDate,"')",sep="")
endDate   <- paste("('",endDate,"')",  sep="")
```
A list of countries can be created through a variable as well. However, if the list is longer than one item, the procedure to turn it into a variable SQL compatible string is more complex.
```

countries <- c('fra','gbr','nor')

SQLvect <- c("(")
for ( rr in 1:(length(countries)-1) )
SQLvect <- paste(SQLvect,"'",countries[rr],"',",sep="")
SQLvect <- paste(SQLvect,"'",countries[rr+1],"'",sep="")
SQLvect <- paste(SQLvect,")",sep="")
countries <- SQLvect
```

Now the query itself
```

library(RODBC)

query <- paste("SELECT PPY_PLM_CNY_CODE as VE_COU,
PPY_ID as VE_REF,
LATITUDE as SI_LATI,
LONGITUDE as SI_LONG,
TO_CHAR(RGN_LOCAL_DATE,'dd/mm/yyyy') as SI_DATE,
TRIM(TO_CHAR(FLOOR(RGN_LOCAL_TIME/100),'09'))||':'||TRIM(TO_CHAR(MOD(RGN_LOCAL_TIME,100),'09')) as SI_TIME,
HEADING as SI_HE,
SPEED as SI_SP FROM vms
WHERE PPY_PLM_CNY_CODE IN ",countries," AND              rgn_local_date BETWEEN ",startDate," AND ",endDate,";")
conn <- odbcConnect(dsn="visstatp",uid=user,pwd=passwd,believeNRows=F)
vmsNLD <- sqlQuery(conn,query)
```