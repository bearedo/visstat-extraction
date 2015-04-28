# Extract Dutch VMS only #
The starting date and end date need to be specified. An example is given below, as well as an R example with variable starting date and end date
## SQL ##
```

SELECT * FROM vms WHERE PPY_PLM_CNY_CODE = 'nld' AND rgn_local_date BETWEEN '01-jan-2010' AND '31-dec-2010';
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
Now the query itself
```

library(RODBC)

query <- paste("SELECT * FROM vms WHERE PPY_PLM_CNY_CODE = 'nld' AND rgn_local_date BETWEEN ",startDate," AND ",endDate,";")
conn <- odbcConnect(dsn="visstatp",uid=user,pwd=passwd,believeNRows=F)
vmsNLD <- sqlQuery(conn,query)
```