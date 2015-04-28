# Extract Foreign VMS only #
The starting date and end date need to be specified. An example is given below, as well as an R example with variable starting date and end date
## SQL ##
Select all but Dutch VMS data.
```

SELECT * FROM vms WHERE PPY_PLM_CNY_CODE <> 'nld' AND rgn_local_date BETWEEN '01-jan-2010' AND '31-dec-2010';
```
Or a list of countries
```

SELECT * FROM vms WHERE PPY_PLM_CNY_CODE IN ('fra','gbr','nor') AND rgn_local_date BETWEEN '01-jan-2010' AND '31-dec-2010';
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

query <- paste("SELECT * FROM vms WHERE PPY_PLM_CNY_CODE IN ",countries," AND rgn_local_date BETWEEN ",startDate," AND ",endDate,";")
conn <- odbcConnect(dsn="visstatp",uid=user,pwd=passwd,believeNRows=F)
vmsNLD <- sqlQuery(conn,query)
```