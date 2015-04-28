# Extract specific vessels from VMS #
Extract a specific vessel within a certain period from the VMS dataset. An example is given below, as well as an R example with variable starting date and end date

## SQL ##
```

SELECT * FROM vms WHERE PPY_PLM_CNY_CODE = 'nld' AND rgn_local_date BETWEEN '01-jan-2010' AND '31-dec-2010' AND PPY_PLM_CODE in ('SCH22','SCH24');
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
A list of vessels can be created through a variable as well. However, if the list is longer than one item, the procedure to turn it into a variable SQL compatible string is more complex.
```

vessels <- c('SCH22','SCH23','SCH24')

SQLvect <- c("(")
for ( rr in 1:(length(vessels)-1) )
SQLvect <- paste(SQLvect,"'",vessels[rr],"',",sep="")
SQLvect <- paste(SQLvect,"'",vessels[rr+1],"'",sep="")
SQLvect <- paste(SQLvect,")",sep="")
vessels <- SQLvect
```
Now the query itself
```

library(RODBC)

query <- paste("SELECT * FROM vms WHERE PPY_PLM_CODE in ",vessels," AND rgn_local_date BETWEEN ",startDate," AND ",endDate,";")
conn <- odbcConnect(dsn="visstatp",uid=user,pwd=passwd,believeNRows=F)
vmsNLD <- sqlQuery(conn,query)
```