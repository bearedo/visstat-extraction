# Extract landings #

This query extracts landings from the catches table in visstat. The input selection can be a combination of species, country, gear, ices\_area and a time-interval.

It creates a table from your selection with registrations (landings) per species, country, gear, ices\_quadrant, vessel, vessel length, vessel power, arrival and departure date and time.

## SQL ##

Select Dab, Plaice and Sole from Dutch vessels only between 01-jan-2010 and 31-jan-2010 caught with TBB.

```

select trips.trip_number,
trips.arrival_date
, trips.arrival_time
, trips.departure_date
, trips.departure_time
, registrations.qpy_ices_quadrant as ices_quadrant
, registrations.gpy_code as gear
, registrations.MESHSIZE
, registrations.trp_ppy_plm_code as vessel
, catches.txn_ices_code as species
, catches.weight
, catches.rgn_trp_ppy_plm_cny_code as country
, TO_CHAR(catches.rgn_trp_arrival_date,'Q') as quarter
, TO_CHAR(catches.rgn_trp_arrival_date,'YYYY') as year
, nvl(quadrant_properties.ices_area,'UNKNOWN') as ices_area
, nvl(quadrant_properties.ICES_SUBAREA,'UNKNOWN') as ices_subarea
, platform_properties.power as power
, platform_properties.length as vessel_length

FROM  trips, registrations, catches, platform_properties, quadrant_properties
WHERE trips.arrival_date BETWEEN '01-jan-2010' and '31-jan-2010' AND
platform_properties.plm_cny_code in ('nld') and
trips.ppy_id = platform_properties.id AND
trips.arrival_date between platform_properties.start_date and     nvl(platform_properties.end_date,sysdate) AND
registrations.gpy_code = 'TBB' AND
registrations.trp_arrival_date = trips.arrival_date  AND
registrations.trp_arrival_time = trips.arrival_time AND
registrations.trp_ppy_id = trips.ppy_id AND
registrations.trp_ppy_plm_code = trips.ppy_plm_code AND
registrations.trp_ppy_plm_cny_code = trips.ppy_plm_cny_code AND
registrations.trp_prt_cny_code = trips.prt_cny_code AND
registrations.trp_prt_code = trips.prt_code AND
registrations.trp_prt_cny_code = trips.prt_cny_code AND
quadrant_properties.ices_quadrant = registrations.qpy_ices_quadrant AND
catches.txn_ices_code IN ('PLE','SOL','DAB') AND
catches.rgn_sre_code = registrations.sre_code AND
catches.rgn_trp_arrival_date = registrations.trp_arrival_date AND
catches.rgn_trp_arrival_time = registrations.trp_arrival_time AND
catches.rgn_trp_ppy_id = registrations.trp_ppy_id AND
catches.rgn_trp_ppy_plm_code = registrations.trp_ppy_plm_code AND
catches.rgn_trp_ppy_plm_cny_code = registrations.trp_ppy_plm_cny_code AND
catches.rgn_trp_prt_code = registrations.trp_prt_code AND
catches.rgn_trp_prt_cny_code = registrations.trp_prt_cny_code AND
catches.rgn_rgn_date = registrations.rgn_date;
```

## In R ##
```

startDate <- "('01-jan-2010')"
endDate   <- "('31-dec-2010')"

nations   <- "('nld')"
gear      <- "('TBB')"
species   <- "('DAB','SOL','PLE')"
```
Or if you have a lot of species, try this
```

species   <- c("DAB","SOL","PLE","HER","HOM","MAC")
SQLvect   <- c("(")
for ( rr in 1:(length(species)-1) )
SQLvect <- paste(SQLvect,"'",species[rr],"',",sep="")
SQLvect   <- paste(SQLvect,"'",species[rr+1],"'",sep="")
SQLvect   <- paste(SQLvect,")",sep="")
species   <- SQLvect
```

Now the query itself
```

library(RODBC)

query     <- paste("select trips.trip_number,
trips.arrival_date
, trips.arrival_time
, trips.departure_date
, trips.departure_time
, registrations.qpy_ices_quadrant as ices_quadrant
, registrations.gpy_code as gear
, registrations.MESHSIZE
, registrations.trp_ppy_plm_code as vessel
, catches.txn_ices_code as species
, catches.weight
, catches.rgn_trp_ppy_plm_cny_code as country
, TO_CHAR(catches.rgn_trp_arrival_date,'Q') as quarter
, TO_CHAR(catches.rgn_trp_arrival_date,'YYYY') as year
, nvl(quadrant_properties.ices_area,'UNKNOWN') as ices_area
, nvl(quadrant_properties.ICES_SUBAREA,'UNKNOWN') as ices_subarea
, platform_properties.power as power
, platform_properties.length as vessel_length

FROM  trips, registrations, catches, platform_properties, quadrant_properties
WHERE trips.arrival_date BETWEEN ",startDate," and ",endDate," AND
platform_properties.plm_cny_code in ",nations," and
trips.ppy_id = platform_properties.id AND
trips.arrival_date between platform_properties.start_date and     nvl(platform_properties.end_date,sysdate) AND
registrations.gpy_code IN ",gear," AND
registrations.trp_arrival_date = trips.arrival_date  AND
registrations.trp_arrival_time = trips.arrival_time AND
registrations.trp_ppy_id = trips.ppy_id AND
registrations.trp_ppy_plm_code = trips.ppy_plm_code AND
registrations.trp_ppy_plm_cny_code = trips.ppy_plm_cny_code AND
registrations.trp_prt_cny_code = trips.prt_cny_code AND
registrations.trp_prt_code = trips.prt_code AND
registrations.trp_prt_cny_code = trips.prt_cny_code AND
quadrant_properties.ices_quadrant = registrations.qpy_ices_quadrant AND
catches.txn_ices_code IN ",species," AND
catches.rgn_sre_code = registrations.sre_code AND
catches.rgn_trp_arrival_date = registrations.trp_arrival_date AND
catches.rgn_trp_arrival_time = registrations.trp_arrival_time AND
catches.rgn_trp_ppy_id = registrations.trp_ppy_id AND
catches.rgn_trp_ppy_plm_code = registrations.trp_ppy_plm_code AND
catches.rgn_trp_ppy_plm_cny_code = registrations.trp_ppy_plm_cny_code AND
catches.rgn_trp_prt_code = registrations.trp_prt_code AND
catches.rgn_trp_prt_cny_code = registrations.trp_prt_cny_code AND
catches.rgn_rgn_date = registrations.rgn_date;")



conn <- odbcConnect(dsn="visstatp",uid=user,pwd=passwd,believeNRows=F)
visstat_catches <- sqlQuery(conn,query,believeNRows = FALSE)

```