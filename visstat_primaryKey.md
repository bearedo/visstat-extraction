# Visstat Primary Key #

Each table in Visstat has it's own unique combination of columns to ensure that a record is unique. This is especially important when a combination of tables is needed to retrieve the desired outcome. In these cases, you don't want unnecessary doubling of records when merging two or more tables together. The primary key can help you to appropriately combine different tables.

### Trips table ###
```

Column names:
* ARRIVAL_DATE
* ARRIVAL_TIME
* PPY_ID
* PPY_PLM_CODE
* PPY_PLM_CNY_CODE
* PRT_CNY_CODE
* PRT_CODE
```

### Registrations table ###
```

Column names
* SRE_CODE
* TRP_PPY_PLM_CNY_CODE
* TRP_PRT_CODE
* TRP_PRT_CNY_CODE
* TRP_ARRIVAL_DATE
* TRP_ARRIVAL_TIME
* TRP_PPY_ID
* TRP_PPY_PLM_CODE
* RGN_DATE
```

### Catches table ###
```

Column names
* TXN_ICES_CODE
* RGN_SRE_CODE
* RGN_TRP_ARRIVAL_DATE
* RGN_TRP_ARRIVAL_TIME
* RGN_TRP_PPY_ID
* RGN_TRP_PPY_PLM_CODE
* RGN_TRP_PPY_PLM_CNY_CODE
* RGN_TRP_PRT_CODE
* RGN_TRP_PRT_CNY_CODE
* RGN_RGN_DATE
* FISH_TIME
* CATCH_SEQ_NO
```

## Examples ##

If you want to combine the trips and registrations table 1-to-1, you need the following expressions to do this correctly:

### SQL ###
```

select * from trips, registrations
where
trips.arrival_date     = registrations.trp_arrival_date     AND
trips.arrival_time     = registrations.trp_arrival_time     AND
trips.ppy_id           = registrations.trp_ppy_id           AND
trips.ppy_plm_code     = registrations.trp_ppy_plm_code     AND
trips.ppy_plm_cny_code = registrations.trp_ppy_plm_cny_code AND
trips.prt_cny_code     = registrations.prt_cny_code         AND
trips.prt_code         = registrations.prt_code;
```